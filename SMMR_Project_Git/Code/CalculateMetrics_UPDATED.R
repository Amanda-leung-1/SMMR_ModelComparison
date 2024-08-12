library(dplyr)
library(lubridate)

#created a function to process all data 
#RAVEN, PRMS, and HYPE data is stored as a .csv file in the folder "raw data". Whenever an update to the 
# model results, these csv files need to be updated
process_time_series <- function(gauge_names, RAVEN_Data_All, HYPE_Data_All, PRMS_Data_All, Obs_flow) {
  
  # Initialize lists to store results for each timestep. 
  timeseries_daily_list <- list()
  timeseries_weekly_list <- list()
  timeseries_monthly_list <- list()
  timeseries_irr_list <- list()
  timeseries_seasonal_list <- list()
  timeseries_full_list <- list()  
  
  # combine PRMS, HYPE, and RAVEN results based on gauge name under numerous timesteps.  
  for (gauge_name in gauge_names) {
    # Create initial data frame with dates
    timeseries_day <- data.frame(Date = RAVEN_Data_All$Date)
    # Merge the data frames for each model and observed data
    timeseries_day <- merge(timeseries_day, HYPE_Data_All[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
    timeseries_day <- merge(timeseries_day, PRMS_Data_All[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
    timeseries_day <- merge(timeseries_day, RAVEN_Data_All[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
    timeseries_day <- merge(timeseries_day, Obs_flow[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
    # Rename columns appropriately
    colnames(timeseries_day)[2:5] <- c("HYPE", "PRMS", "RAVEN", "Obs")
    timeseries_day$Date <- as.Date(timeseries_day$Date)
    # Store daily results
    timeseries_daily_list[[gauge_name]] <- timeseries_day
    
    # Create a new 'week' column
    timeseries_day <- timeseries_day %>%
      mutate(week = floor_date(Date - days(3), unit = "week") + days(3))
    
    # Weekly aggregation
    timeseries_weekly <- timeseries_day %>%
      group_by(week) %>%
      summarize(
        week_start = min(Date),
        week_end = max(Date),
        HYPE_mean = mean(HYPE, na.rm = TRUE),
        PRMS_mean = mean(PRMS, na.rm = TRUE),
        Obs_mean = mean(Obs, na.rm = TRUE),
        RAVEN_mean = mean(RAVEN, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(-week_start)  #  remove week_start if not needed
    # Store weekly results
    timeseries_weekly_list[[gauge_name]] <- timeseries_weekly
    
    
    # Monthly aggregation
    timeseries_monthly <- timeseries_day %>%
      group_by(month = floor_date(Date, "month")) %>%
      summarize(
        HYPE_mean = mean(HYPE, na.rm = TRUE),
        PRMS_mean = mean(PRMS, na.rm = TRUE),
        Obs_mean = mean(Obs, na.rm = TRUE),
        RAVEN_mean = mean(RAVEN, na.rm = TRUE)
      ) %>%
      ungroup()
    # Store monthly results
    timeseries_monthly_list[[gauge_name]] <- timeseries_monthly
  
    
    # QUARTERLY aggregation
    timeseries_seasonal <- timeseries_day %>%
    mutate(quarter = as.factor(quarter(Date))) %>%
      group_by(quarter) %>%
      summarize(
        # Aggregate data
        HYPE_mean = mean(HYPE, na.rm = TRUE),
        PRMS_mean = mean(PRMS, na.rm = TRUE),
        Obs_mean = mean(Obs, na.rm = TRUE),
        RAVEN_mean = mean(RAVEN, na.rm = TRUE),
        .groups = "drop"
      )%>%
      ungroup()
    # Store monthly results
    timeseries_seasonal_list[[gauge_name]] <- timeseries_seasonal
    
    #Weekly aggregation of data based on the irrigation season going from April 1 to October 31 (31 weeks)
    #end date may be in November, based on how the weeks work out. This was done based on request of the study group. 
    # Function to filter data for a specific year
    filter_data_for_year <- function(year) {
      start_date <- as.POSIXct(paste0(year, "-04-01 00:00:00"), format="%Y-%m-%d %H:%M:%S")
      end_date <- start_date + weeks(31)
      timeseries_day %>%
        filter(Date >= start_date & Date <= end_date)
    }
    # Apply the filtering function to all years and combine results
    timeseries_irrigation <- map_df(years, filter_data_for_year, .id = "year")
    # Weekly aggregation
    timeseries_irrigation_weekly <- timeseries_irrigation %>%
      mutate(week = floor_date(Date, unit = "week")) %>%
      group_by(week) %>%
      summarize(
        week_start = min(Date),
        week_end = max(Date),
        HYPE_mean = mean(HYPE, na.rm = TRUE),
        PRMS_mean = mean(PRMS, na.rm = TRUE),
        Obs_mean = mean(Obs, na.rm = TRUE),
        RAVEN_mean = mean(RAVEN, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Extract month from week_start and week_end
      mutate(
        start_month = month(week_start),
        end_month = month(week_end)
      ) %>%
      # Filter out rows where both week_start and week_end are in November (month 11)
      filter(!(start_month == 11 & end_month == 11))
    timeseries_irr_list[[gauge_name]] <- timeseries_irrigation_weekly
    
    # 
    timeseries_full <- timeseries_day %>%
      summarize(
        HYPE_mean = mean(HYPE, na.rm = TRUE),
        PRMS_mean = mean(PRMS, na.rm = TRUE),
        Obs_mean = mean(Obs, na.rm = TRUE),
        RAVEN_mean = mean(RAVEN, na.rm = TRUE)
      )
        timeseries_full_list[[gauge_name]] <- timeseries_full
  }
  
  # Return results as a list of lists that contain the mean, upper and lower quartile for each timeseries 
  #at each gauge 
  return(list(
   daily = timeseries_daily_list,
     weekly = timeseries_weekly_list,
    monthly = timeseries_monthly_list,
   irr = timeseries_irr_list,
   seasonal = timeseries_seasonal_list,
    full = timeseries_full_list  
  ))
}

# Call the function with the necessary data
results <- process_time_series(
  gauge_names = name_all,
  RAVEN_Data_All = RAVEN_Data_All,
  HYPE_Data_All = HYPE_Data_All,
  PRMS_Data_All = PRMS_Data_All,
  Obs_flow = Obs_flow
)

# Extract the list of daily data frames
daily_data_list <- results$daily
# Initialize a list to store mean calculations for each gauge
mean_by_gauge_list_daily <- list()
# Loop through each gauge and calculate the mean
for (gauge_name in names(daily_data_list)) {
  # Extract the daily data for the current gauge
  gauge_daily_data <- daily_data_list[[gauge_name]]
  
  # Calculate the mean for each column of interest
  gauge_summary_daily <- gauge_daily_data %>%
    summarize(across(c(HYPE, PRMS, Obs, RAVEN), 
                     list(
                       mean = ~ mean(.x, na.rm = TRUE),
                       lower_quartile = ~ quantile(.x, 0.25, na.rm = TRUE),
                       upper_quartile = ~ quantile(.x, 0.75, na.rm = TRUE)
                     ), 
                     .names = "{col}_{fn}"))
  
  # Add the mean results to the list with gauge name as the key
  mean_by_gauge_list_daily[[gauge_name]] <- gauge_summary_daily
}
# Combine the mean results into a single data frame
summary_results_daily <- bind_rows(mean_by_gauge_list_daily, .id = "gauge_name")

# Extract the list of weekly data frames
weekly_data_list <- results$weekly
# Initialize a list to store mean calculations for each gauge
mean_by_gauge_list_weekly <- list()
# Loop through each gauge and calculate the mean
for (gauge_name in names(weekly_data_list)) {
  # Extract the weekly data for the current gauge
  gauge_weekly_data <- weekly_data_list[[gauge_name]]
  
  # Calculate the mean for each column of interest
  gauge_summary_weekly <- gauge_weekly_data %>%
    summarize(across(c(HYPE_mean, PRMS_mean, Obs_mean, RAVEN_mean), 
                     list(
                       mean = ~ mean(.x, na.rm = TRUE),
                       lower_quartile = ~ quantile(.x, 0.25, na.rm = TRUE),
                       upper_quartile = ~ quantile(.x, 0.75, na.rm = TRUE)
                     ), 
                     .names = "{col}_{fn}"))
  
  # Add the mean results to the list with gauge name as the key
  mean_by_gauge_list_weekly[[gauge_name]] <- gauge_summary_weekly
}
# Combine the mean results into a single data frame
summary_results_weekly <- bind_rows(mean_by_gauge_list_weekly, .id = "gauge_name")

# Extract the list of monthly data frames
monthly_data_list <- results$monthly
# Initialize a list to store mean calculations for each gauge
mean_by_gauge_list_monthly <- list()
# Loop through each gauge and calculate the mean
for (gauge_name in names(monthly_data_list)) {
  # Extract the monthly data for the current gauge
  gauge_monthly_data <- monthly_data_list[[gauge_name]]
  
  # Calculate the mean for each column of interest
  gauge_summary_monthly <- gauge_monthly_data %>%
    summarize(across(c(HYPE_mean, PRMS_mean, Obs_mean, RAVEN_mean), 
                     list(
                       mean = ~ mean(.x, na.rm = TRUE),
                       lower_quartile = ~ quantile(.x, 0.25, na.rm = TRUE),
                       upper_quartile = ~ quantile(.x, 0.75, na.rm = TRUE)
                     ), 
                     .names = "{col}_{fn}"))
  
  # Add the mean results to the list with gauge name as the key
  mean_by_gauge_list_monthly[[gauge_name]] <- gauge_summary_monthly
}
# Combine the mean results into a single data frame
summary_results_monthly <- bind_rows(mean_by_gauge_list_monthly, .id = "gauge_name")


irr_data_list <- results$irr
# Initialize a list to store mean calculations for each gauge
mean_by_gauge_list_irr <- list()
# Loop through each gauge and calculate the mean
for (gauge_name in names(irr_data_list)) {
  # Extract the irr data for the current gauge
  gauge_irr_data <- irr_data_list[[gauge_name]]
  # Calculate the mean for each column of interest
  gauge_summary_irr <- gauge_irr_data %>%
    summarize(across(c(HYPE_mean, PRMS_mean, Obs_mean, RAVEN_mean), 
                     list(
                       mean = ~ mean(.x, na.rm = TRUE),
                       lower_quartile = ~ quantile(.x, 0.25, na.rm = TRUE),
                       upper_quartile = ~ quantile(.x, 0.75, na.rm = TRUE)
                     ), 
                     .names = "{col}_{fn}"))
  
  # Add the mean results to the list with gauge name as the key
  mean_by_gauge_list_irr[[gauge_name]] <- gauge_summary_irr
}
# Combine the mean results into a single data frame
summary_results_irr <- bind_rows(mean_by_gauge_list_irr, .id = "gauge_name")


seasonal_data_list <- results$seasonal
# Initialize a list to store mean calculations for each gauge
mean_by_gauge_list_seasonal <- list()
# Loop through each gauge and calculate the mean
for (gauge_name in names(seasonal_data_list)) {
  # Extract the seasonal data for the current gauge
  gauge_seasonal_data <- seasonal_data_list[[gauge_name]]
  # Calculate the mean for each column of interest
  gauge_summary_seasonal <- gauge_seasonal_data %>%
    summarize(across(c(HYPE_mean, PRMS_mean, Obs_mean, RAVEN_mean), 
                     list(
                       mean = ~ mean(.x, na.rm = TRUE),
                       lower_quartile = ~ quantile(.x, 0.25, na.rm = TRUE),
                       upper_quartile = ~ quantile(.x, 0.75, na.rm = TRUE)
                     ), 
                     .names = "{col}_{fn}"))
  
  # Add the mean results to the list with gauge name as the key
  mean_by_gauge_list_seasonal[[gauge_name]] <- gauge_summary_seasonal
}
# Combine the mean results into a single data frame
summary_results_seasonal <- bind_rows(mean_by_gauge_list_seasonal, .id = "gauge_name")


# Define a function to calculate the log of NSE
calculate_log_nse <- function(sim, obs) {
  # Calculate NSE
  nse_value <- NSE(sim, obs, na.rm = FALSE)
  # Check if NSE is valid for log calculation
  if (nse_value < 0) {
    return(NA)  # Return NA if NSE is zero or negative
  } else {
    return(NSE(sim, obs, fun=log, na.rm=FALSE))  # Return logNSE if NSE is positive
  }
}

test <- NSE(log(HYPE_Data_All$PPCMO), log(PRMS_Data_All$PPCMO))

kge_nse_weekly <- list()
kge_nse_irr <- list()
kge_nse_irr <- list()
kge_nse_seasonal <- list()
kge_nse_daily <- list()

daily_data_list <- results$daily

# calculate for daily timestep. Loop through each gauge name
for (gauge_name in name_all) {
  # Extract the data frame for the current gauge
  df <- daily_data_list[[gauge_name]]
  kge_HYPE_Obs <- KGE(df$HYPE, df$Obs, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_Obs <- KGE(df$PRMS, df$Obs, s = c(1, 1, 1), na.rm = TRUE)
  kge_RAVEN_Obs <- KGE(df$RAVEN,df$Obs, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_RAVEN <- KGE(df$HYPE, df$RAVEN, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_PRMS <- KGE(df$HYPE, df$PRMS, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_RAVEN <- KGE(df$PRMS,df$RAVEN, s = c(1, 1, 1), na.rm = TRUE)
  
  nse_HYPE_Obs <- NSE(df$HYPE, df$Obs, na.rm = FALSE)
  nse_PRMS_Obs <- NSE(df$PRMS, df$Obs, na.rm = FALSE)
  nse_RAVEN_Obs <- NSE(df$RAVEN,df$Obs, na.rm = FALSE)
  nse_HYPE_PRMS <- NSE(df$HYPE, df$PRMS, na.rm = FALSE)
  nse_HYPE_RAVEN <- NSE(df$HYPE, df$RAVEN, na.rm = FALSE)
  nse_PRMS_RAVEN <- NSE(df$PRMS,df$RAVEN, na.rm = FALSE)
  
  lnse_HYPE_Obs <- calculate_log_nse(df$HYPE, df$Obs)
  lnse_PRMS_Obs <- calculate_log_nse(df$PRMS, df$Obs)
  lnse_RAVEN_Obs <- calculate_log_nse(df$RAVEN, df$Obs)
  lnse_HYPE_PRMS <- calculate_log_nse(df$HYPE, df$PRMS)
  lnse_HYPE_RAVEN <- calculate_log_nse(df$HYPE, df$RAVEN)
  lnse_PRMS_RAVEN <- calculate_log_nse(df$PRMS, df$RAVEN)
  
  # Store the results in a named list
  kge_nse_daily[[gauge_name]] <- list(
    kge_HYPE_Obs = kge_HYPE_Obs,
    kge_PRMS_Obs = kge_PRMS_Obs,
    kgeRAVEN_Obs = kge_RAVEN_Obs, 
    kge_HYPE_RAVEN = kge_HYPE_RAVEN,
    kge_HYPE_PRMS = kge_HYPE_PRMS,
    kge_PRMS_RAVEN = kge_PRMS_RAVEN,
    nse_HYPE_Obs  = nse_HYPE_Obs,
    nse_PRMS_Obs  =  nse_PRMS_Obs,
    nse_RAVEN_Obs = nse_RAVEN_Obs,
    nse_HYPE_PRMS =  nse_HYPE_PRMS,
    nse_HYPE_RAVEN = nse_HYPE_RAVEN,
    nse_PRMS_RAVEN= nse_PRMS_RAVEN, 
    lnse_HYPE_Obs = lnse_HYPE_Obs,
    lnse_PRMS_Obs = lnse_PRMS_Obs,
    lnse_RAVEN_Obs = lnse_RAVEN_Obs,
    lnse_HYPE_PRMS = lnse_HYPE_PRMS,
    lnse_HYPE_RAVEN = lnse_HYPE_RAVEN,
    lnse_PRMS_RAVEN = lnse_PRMS_RAVEN
  )
}


# calculate for WEEKLY timestep. Loop through each gauge name
for (gauge_name in name_all) {
  # Extract the data frame for the current gauge
  df <- weekly_data_list[[gauge_name]]
  kge_HYPE_Obs <- KGE(df$HYPE_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_Obs <- KGE(df$PRMS_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_RAVEN_Obs <- KGE(df$RAVEN_mean,df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_RAVEN <- KGE(df$HYPE_mean, df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_PRMS <- KGE(df$HYPE_mean, df$PRMS_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_RAVEN <- KGE(df$PRMS_mean,df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  
  nse_HYPE_Obs <- NSE(df$HYPE_mean, df$Obs_mean, na.rm = FALSE)
  nse_PRMS_Obs <- NSE(df$PRMS_mean, df$Obs_mean, na.rm = FALSE)
  nse_RAVEN_Obs <- NSE(df$RAVEN_mean,df$Obs_mean, na.rm = FALSE)
  nse_HYPE_PRMS <- NSE(df$HYPE_mean, df$PRMS_mean, na.rm = FALSE)
  nse_HYPE_RAVEN <- NSE(df$HYPE_mean, df$RAVEN_mean, na.rm = FALSE)
  nse_PRMS_RAVEN <- NSE(df$PRMS_mean,df$RAVEN_mean, na.rm = FALSE)
  
  lnse_HYPE_Obs <- calculate_log_nse(df$HYPE_mean, df$Obs_mean)
  lnse_PRMS_Obs <- calculate_log_nse(df$PRMS_mean, df$Obs_mean)
  lnse_RAVEN_Obs <- calculate_log_nse(df$RAVEN_mean, df$Obs_mean)
  lnse_HYPE_PRMS <- calculate_log_nse(df$HYPE_mean, df$PRMS_mean)
  lnse_HYPE_RAVEN <- calculate_log_nse(df$RAVEN_mean, df$RAVEN_mean)
  lnse_PRMS_RAVEN <- calculate_log_nse(df$PRMS_mean, df$RAVEN_mean)
  
  # Store the results in a named list
  kge_nse_weekly[[gauge_name]] <- list(
    kge_HYPE_Obs = kge_HYPE_Obs,
    kge_PRMS_Obs = kge_PRMS_Obs,
    kgeRAVEN_Obs = kge_RAVEN_Obs, 
    kge_HYPE_RAVEN = kge_HYPE_RAVEN,
    kge_HYPE_PRMS = kge_HYPE_PRMS,
    kge_PRMS_RAVEN = kge_PRMS_RAVEN,
    nse_HYPE_Obs  = nse_HYPE_Obs,
    nse_PRMS_Obs  =  nse_PRMS_Obs,
    nse_RAVEN_Obs = nse_RAVEN_Obs,
    nse_HYPE_PRMS =  nse_HYPE_PRMS,
    nse_HYPE_RAVEN = nse_HYPE_RAVEN,
    nse_PRMS_RAVEN= nse_PRMS_RAVEN, 
    lnse_HYPE_Obs = lnse_HYPE_Obs,
    lnse_PRMS_Obs = lnse_PRMS_Obs,
    lnse_RAVEN_Obs = lnse_RAVEN_Obs,
    lnse_HYPE_PRMS = lnse_HYPE_PRMS,
    lnse_HYPE_RAVEN = lnse_HYPE_RAVEN,
    lnse_PRMS_RAVEN = lnse_PRMS_RAVEN
  )
}


# calculate for MONTLY timestep. Loop through each gauge name
for (gauge_name in name_all) {
  # Extract the data frame for the current gauge
  df <- irr_data_list[[gauge_name]]
  kge_HYPE_Obs <- KGE(df$HYPE_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_Obs <- KGE(df$PRMS_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_RAVEN_Obs <- KGE(df$RAVEN_mean,df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_RAVEN <- KGE(df$HYPE_mean, df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_PRMS <- KGE(df$HYPE_mean, df$PRMS_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_RAVEN <- KGE(df$PRMS_mean,df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  
  nse_HYPE_Obs <- NSE(df$HYPE_mean, df$Obs_mean, na.rm = FALSE)
  nse_PRMS_Obs <- NSE(df$PRMS_mean, df$Obs_mean, na.rm = FALSE)
  nse_RAVEN_Obs <- NSE(df$RAVEN_mean,df$Obs_mean, na.rm = FALSE)
  nse_HYPE_PRMS <- NSE(df$HYPE_mean, df$PRMS_mean, na.rm = FALSE)
  nse_HYPE_RAVEN <- NSE(df$HYPE_mean, df$RAVEN_mean, na.rm = FALSE)
  nse_PRMS_RAVEN <- NSE(df$PRMS_mean,df$RAVEN_mean, na.rm = FALSE)
  
  lnse_HYPE_Obs <- calculate_log_nse(df$HYPE_mean, df$Obs_mean)
  lnse_PRMS_Obs <- calculate_log_nse(df$PRMS_mean, df$Obs_mean)
  lnse_RAVEN_Obs <- calculate_log_nse(df$RAVEN_mean, df$Obs_mean)
  lnse_HYPE_PRMS <- calculate_log_nse(df$HYPE_mean, df$PRMS_mean)
  lnse_HYPE_RAVEN <- calculate_log_nse(df$RAVEN_mean, df$RAVEN_mean)
  lnse_PRMS_RAVEN <- calculate_log_nse(df$PRMS_mean, df$RAVEN_mean)
  
  # Store the results in a named list
  kge_nse_irr[[gauge_name]] <- list(
    kge_HYPE_Obs = kge_HYPE_Obs,
    kge_PRMS_Obs = kge_PRMS_Obs,
    kgeRAVEN_Obs = kge_RAVEN_Obs, 
    kge_HYPE_RAVEN = kge_HYPE_RAVEN,
    kge_HYPE_PRMS = kge_HYPE_PRMS,
    kge_PRMS_RAVEN = kge_PRMS_RAVEN,
    nse_HYPE_Obs  = nse_HYPE_Obs,
    nse_PRMS_Obs  =  nse_PRMS_Obs,
    nse_RAVEN_Obs = nse_RAVEN_Obs,
    nse_HYPE_PRMS =  nse_HYPE_PRMS,
    nse_HYPE_RAVEN = nse_HYPE_RAVEN,
    nse_PRMS_RAVEN= nse_PRMS_RAVEN, 
    lnse_HYPE_Obs = lnse_HYPE_Obs,
    lnse_PRMS_Obs = lnse_PRMS_Obs,
    lnse_RAVEN_Obs = lnse_RAVEN_Obs,
    lnse_HYPE_PRMS = lnse_HYPE_PRMS,
    lnse_HYPE_RAVEN = lnse_HYPE_RAVEN,
    lnse_PRMS_RAVEN = lnse_PRMS_RAVEN
  )
}


# calculate for irr timestep. Loop through each gauge name
for (gauge_name in name_all) {
  # Extract the data frame for the current gauge
  df <- irr_data_list[[gauge_name]]
  kge_HYPE_Obs <- KGE(df$HYPE_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_Obs <- KGE(df$PRMS_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_RAVEN_Obs <- KGE(df$RAVEN_mean,df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_RAVEN <- KGE(df$HYPE_mean, df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_PRMS <- KGE(df$HYPE_mean, df$PRMS_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_RAVEN <- KGE(df$PRMS_mean,df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  
  nse_HYPE_Obs <- NSE(df$HYPE_mean, df$Obs_mean, na.rm = FALSE)
  nse_PRMS_Obs <- NSE(df$PRMS_mean, df$Obs_mean, na.rm = FALSE)
  nse_RAVEN_Obs <- NSE(df$RAVEN_mean,df$Obs_mean, na.rm = FALSE)
  nse_HYPE_PRMS <- NSE(df$HYPE_mean, df$PRMS_mean, na.rm = FALSE)
  nse_HYPE_RAVEN <- NSE(df$HYPE_mean, df$RAVEN_mean, na.rm = FALSE)
  nse_PRMS_RAVEN <- NSE(df$PRMS_mean,df$RAVEN_mean, na.rm = FALSE)
  
  lnse_HYPE_Obs <- calculate_log_nse(df$HYPE_mean, df$Obs_mean)
  lnse_PRMS_Obs <- calculate_log_nse(df$PRMS_mean, df$Obs_mean)
  lnse_RAVEN_Obs <- calculate_log_nse(df$RAVEN_mean, df$Obs_mean)
  lnse_HYPE_PRMS <- calculate_log_nse(df$HYPE_mean, df$PRMS_mean)
  lnse_HYPE_RAVEN <- calculate_log_nse(df$RAVEN_mean, df$RAVEN_mean)
  lnse_PRMS_RAVEN <- calculate_log_nse(df$PRMS_mean, df$RAVEN_mean)
  
  # Store the results in a named list
  kge_nse_irr[[gauge_name]] <- list(
    kge_HYPE_Obs = kge_HYPE_Obs,
    kge_PRMS_Obs = kge_PRMS_Obs,
    kgeRAVEN_Obs = kge_RAVEN_Obs, 
    kge_HYPE_RAVEN = kge_HYPE_RAVEN,
    kge_HYPE_PRMS = kge_HYPE_PRMS,
    kge_PRMS_RAVEN = kge_PRMS_RAVEN,
    nse_HYPE_Obs  = nse_HYPE_Obs,
    nse_PRMS_Obs  =  nse_PRMS_Obs,
    nse_RAVEN_Obs = nse_RAVEN_Obs,
    nse_HYPE_PRMS =  nse_HYPE_PRMS,
    nse_HYPE_RAVEN = nse_HYPE_RAVEN,
    nse_PRMS_RAVEN= nse_PRMS_RAVEN, 
    lnse_HYPE_Obs = lnse_HYPE_Obs,
    lnse_PRMS_Obs = lnse_PRMS_Obs,
    lnse_RAVEN_Obs = lnse_RAVEN_Obs,
    lnse_HYPE_PRMS = lnse_HYPE_PRMS,
    lnse_HYPE_RAVEN = lnse_HYPE_RAVEN,
    lnse_PRMS_RAVEN = lnse_PRMS_RAVEN
  )
}


# calculate for seasonal timestep. Loop through each gauge name
for (gauge_name in name_all) {
  # Extract the data frame for the current gauge
  df <- seasonal_data_list[[gauge_name]]
  kge_HYPE_Obs <- KGE(df$HYPE_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_Obs <- KGE(df$PRMS_mean, df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_RAVEN_Obs <- KGE(df$RAVEN_mean,df$Obs_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_RAVEN <- KGE(df$HYPE_mean, df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_HYPE_PRMS <- KGE(df$HYPE_mean, df$PRMS_mean, s = c(1, 1, 1), na.rm = TRUE)
  kge_PRMS_RAVEN <- KGE(df$PRMS_mean,df$RAVEN_mean, s = c(1, 1, 1), na.rm = TRUE)
  
  nse_HYPE_Obs <- NSE(df$HYPE_mean, df$Obs_mean, na.rm = FALSE)
  nse_PRMS_Obs <- NSE(df$PRMS_mean, df$Obs_mean, na.rm = FALSE)
  nse_RAVEN_Obs <- NSE(df$RAVEN_mean,df$Obs_mean, na.rm = FALSE)
  nse_HYPE_PRMS <- NSE(df$HYPE_mean, df$PRMS_mean, na.rm = FALSE)
  nse_HYPE_RAVEN <- NSE(df$HYPE_mean, df$RAVEN_mean, na.rm = FALSE)
  nse_PRMS_RAVEN <- NSE(df$PRMS_mean,df$RAVEN_mean, na.rm = FALSE)
  
  lnse_HYPE_Obs <- calculate_log_nse(df$HYPE_mean, df$Obs_mean)
  lnse_PRMS_Obs <- calculate_log_nse(df$PRMS_mean, df$Obs_mean)
  lnse_RAVEN_Obs <- calculate_log_nse(df$RAVEN_mean, df$Obs_mean)
  lnse_HYPE_PRMS <- calculate_log_nse(df$HYPE_mean, df$PRMS_mean)
  lnse_HYPE_RAVEN <- calculate_log_nse(df$RAVEN_mean, df$RAVEN_mean)
  lnse_PRMS_RAVEN <- calculate_log_nse(df$PRMS_mean, df$RAVEN_mean)
  
  # Store the results in a named list
  kge_nse_seasonal[[gauge_name]] <- list(
    kge_HYPE_Obs = kge_HYPE_Obs,
    kge_PRMS_Obs = kge_PRMS_Obs,
    kgeRAVEN_Obs = kge_RAVEN_Obs, 
    kge_HYPE_RAVEN = kge_HYPE_RAVEN,
    kge_HYPE_PRMS = kge_HYPE_PRMS,
    kge_PRMS_RAVEN = kge_PRMS_RAVEN,
    nse_HYPE_Obs  = nse_HYPE_Obs,
    nse_PRMS_Obs  =  nse_PRMS_Obs,
    nse_RAVEN_Obs = nse_RAVEN_Obs,
    nse_HYPE_PRMS =  nse_HYPE_PRMS,
    nse_HYPE_RAVEN = nse_HYPE_RAVEN,
    nse_PRMS_RAVEN= nse_PRMS_RAVEN, 
    lnse_HYPE_Obs = lnse_HYPE_Obs,
    lnse_PRMS_Obs = lnse_PRMS_Obs,
    lnse_RAVEN_Obs = lnse_RAVEN_Obs,
    lnse_HYPE_PRMS = lnse_HYPE_PRMS,
    lnse_HYPE_RAVEN = lnse_HYPE_RAVEN,
    lnse_PRMS_RAVEN = lnse_PRMS_RAVEN
  )
}


