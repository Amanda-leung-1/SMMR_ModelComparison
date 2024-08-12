library(dplyr)
library(hydroGOF)

# Define a function to calculate metrics for a single gauge
calculate_metrics <- function(name, Obs_flow, PRMS_Data_All, HYPE_Data_All, RAVEN_Data_All, start_date, end_date) {
  
  # Subset the data within the given time frame
  subset_Obs_flow <- Obs_flow[Obs_flow$Date >= start_date & Obs_flow$Date <= end_date, c("Date", name)]
  subset_PRMS_Data <- PRMS_Data_All[PRMS_Data_All$Date >= start_date & PRMS_Data_All$Date <= end_date, c("Date", name)]
  subset_HYPE_Data <- HYPE_Data_All[HYPE_Data_All$Date >= start_date & HYPE_Data_All$Date <= end_date, c("Date", name)]
  subset_RAVEN_Data <- RAVEN_Data_All[RAVEN_Data_All$Date >= start_date & RAVEN_Data_All$Date <= end_date, c("Date", name)]
  
  # Calculate NSE and KGE for each comparison
  NSE_HYPE_Obs <- NSE(subset_HYPE_Data[[name]], subset_Obs_flow[[name]], na.rm = FALSE)
  NSE_PRMS_Obs <- NSE(subset_PRMS_Data[[name]], subset_Obs_flow[[name]], na.rm = FALSE)
  NSE_HYPE_PRMS <- NSE(subset_PRMS_Data[[name]], subset_HYPE_Data[[name]], na.rm = FALSE)
  NSE_HYPE_RAVEN <- NSE(subset_HYPE_Data[[name]], subset_RAVEN_Data[[name]], na.rm = FALSE)
  NSE_PRMS_RAVEN <- NSE(subset_PRMS_Data[[name]], subset_RAVEN_Data[[name]], na.rm = FALSE)
  NSE_RAVEN_Obs <- NSE(subset_RAVEN_Data[[name]], subset_Obs_flow[[name]], na.rm = FALSE)
  
  KGE_HYPE_Obs <- KGE(subset_HYPE_Data[[name]], subset_Obs_flow[[name]], s = c(1, 1, 1), na.rm = TRUE, method = "2009", out.type = "single")
  KGE_PRMS_Obs <- KGE(subset_PRMS_Data[[name]], subset_Obs_flow[[name]], s = c(1, 1, 1), na.rm = TRUE, method = "2009", out.type = "single")
  KGE_HYPE_PRMS <- KGE(subset_HYPE_Data[[name]], subset_PRMS_Data[[name]], s = c(1, 1, 1), na.rm = TRUE, method = "2009", out.type = "single")
  KGE_HYPE_RAVEN <- KGE(subset_HYPE_Data[[name]], subset_RAVEN_Data[[name]], s = c(1, 1, 1), na.rm = TRUE, method = "2009", out.type = "single")
  KGE_PRMS_RAVEN <- KGE(subset_PRMS_Data[[name]], subset_RAVEN_Data[[name]], s = c(1, 1, 1), na.rm = TRUE, method = "2009", out.type = "single")
  KGE_RAVEN_Obs <- KGE(subset_RAVEN_Data[[name]], subset_Obs_flow[[name]], s = c(1, 1, 1), na.rm = TRUE, method = "2009", out.type = "single")
  
  
  return(c(
    Guage_Name = name,
    StartDate = format(start_date, "%Y-%m-%d"),
    EndDate = format(end_date, "%Y-%m-%d"),
    NSE_HYPE_Obs = NSE_HYPE_Obs,
    KGE_HYPE_Obs = KGE_HYPE_Obs,
    NSE_PRMS_Obs = NSE_PRMS_Obs,
    KGE_PRMS_Obs = KGE_PRMS_Obs,
    NSE_RAVEN_Obs = NSE_RAVEN_Obs,
    KGE_RAVEN_Obs = KGE_RAVEN_Obs,
    NSE_HYPE_PRMS = NSE_HYPE_PRMS,
    KGE_HYPE_PRMS = KGE_HYPE_PRMS,
    NSE_HYPE_RAVEN = NSE_HYPE_RAVEN,
    KGE_HYPE_RAVEN = KGE_HYPE_RAVEN,
    NSE_PRMS_RAVEN = NSE_PRMS_RAVEN,
    KGE_PRMS_RAVEN = KGE_PRMS_RAVEN
  ))
}

# Initialize an empty dataframe to store results
results_df <- data.frame(
  Guage_Name = character(),
  StartDate = character(),
  EndDate = character(),
  NSE_HYPE_Obs = numeric(),
  KGE_HYPE_Obs = numeric(),
  NSE_PRMS_Obs = numeric(),
  KGE_PRMS_Obs = numeric(),
  NSE_RAVEN_Obs = numeric(),
  KGE_RAVEN_Obs = numeric(),
  NSE_HYPE_PRMS = numeric(),
  KGE_HYPE_PRMS = numeric(),
  NSE_HYPE_RAVEN = numeric(),
  KGE_HYPE_RAVEN = numeric(),
  NSE_PRMS_RAVEN = numeric(),
  KGE_PRMS_RAVEN = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each gauge location
for (name in name_all) {
  metrics <- calculate_metrics(name, Obs_flow, PRMS_Data_All, HYPE_Data_All, RAVEN_Data_All, start_date, end_date)
  
  # Append the result to the dataframe
  results_df <- rbind(results_df, as.data.frame(t(metrics), stringsAsFactors = FALSE))
}

# Print the dataframe with results
print(results_df)

# Specify the file path where you want to save the CSV file
csv_file <- "Files/SMMR_Comparison_Results/SMMR_Corr_Results_V3.csv"

# Save results_df as a CSV file
write.csv(results_df, file = csv_file, row.names = FALSE)

# Optional: Print a message confirming the file was saved
cat("CSV file saved:", csv_file, "\n")





