convert_list_to_df <- function(metric_list) {
  # Initialize an empty list to store data frames
  df_list <- list()
  
  # Loop through each gauge in the list
  for (gauge_name in names(metric_list)) {
    # Extract metrics for the current gauge
    metrics <- metric_list[[gauge_name]]
    
    # Convert the metrics list to a data frame and add a column for gauge name
    df <- as.data.frame(metrics)
    df$gauge_name <- gauge_name
    
    # Append the data frame to the list
    df_list[[gauge_name]] <- df
  }
  
  # Combine all data frames into one
  combined_df <- do.call(rbind, df_list)
  
  return(combined_df)
}

# Convert lists to data frames
df_daily <- convert_list_to_df(kge_nse_daily)
df_weekly <- convert_list_to_df(kge_nse_weekly)
df_monthly <- convert_list_to_df(kge_nse_monthly)
df_irr <- convert_list_to_df(kge_nse_irr)
df_seasonal <- convert_list_to_df(kge_nse_seasonal)

# Define file names
file_daily <- "Results/metrics/kge_nse_daily.csv"
file_weekly <- "Results/metrics/kge_nse_weekly.csv"
file_monthly <- "Results/metrics/kge_nse_monthly.csv"
file_irr <- "Results/metrics/kge_nse_irr.csv"
file_seasonal <- "Results/metrics/kge_nse_seasonal.csv"

# Write data frames to CSV files
write.csv(df_daily, file = file_daily, row.names = FALSE)
write.csv(df_weekly, file = file_weekly, row.names = FALSE)
write.csv(df_monthly, file = file_monthly, row.names = FALSE)
write.csv(df_irr, file = file_irr, row.names = FALSE)
write.csv(df_seasonal, file = file_seasonal, row.names = FALSE)

file_metric_daily <- "Results/metrics/metrics_daily.csv"
file_metric_weekly <- "Results/metrics/metrics_weekly.csv"
file_metric_monthly <- "Results/metrics/metrics_monthly.csv"
file_metric_irr <- "Results/metrics/metrics_irr.csv"
file_metric_seasonal <- "Results/metrics/metrics_seasonal.csv"

write.csv(summary_results_daily, file = file_metric_daily, row.names = FALSE)
write.csv(summary_results_weekly, file = file_metric_weekly, row.names = FALSE)
write.csv(summary_results_monthly, file = file_metric_monthly, row.names = FALSE)
write.csv(summary_results_irr, file = file_metric_irr, row.names = FALSE)
write.csv(summary_results_seasonal, file = file_metric_seasonal, row.names = FALSE)
