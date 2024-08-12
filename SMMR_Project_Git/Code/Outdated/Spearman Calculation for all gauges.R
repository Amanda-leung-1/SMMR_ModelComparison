library(hydroGOF)
start_date <- as.POSIXct("1981-10-02")
end_date <- as.POSIXct("2015-09-30")
# Initialize an empty dataframe to store results
results_df <- data.frame(
  Guage_Name = character(length(name_all)),
  StartDate = character(length(name_all)),
  EndDate = character(length(name_all)),
  NSE_HYPE_Obs = numeric(length(name_all)),
  KGE_HYPE_Obs = numeric(length(name_all)),
  NSE_PRMS_Obs = numeric(length(name_all)),
  NSE_HYPE_Obs = numeric(length(name_all)), 
  NSE_PRMS_Obs = numeric(length(name_all)), 
  NSE_HYPE_PRMS = numeric(length(name_all))
)

results_df[i, "Guage_Name"] <- name
results_df[i, "StartDate"] <- format(start_date, "%Y-%m-%d")
results_df[i, "EndDate"] <- format(end_date, "%Y-%m-%d")
results_df[i, "NSE_HYPE_Obs"] <- NSE_HYPE_Obs
results_df[i, "KGE_HYPE_Obs"] <-KGE_HYPE_Obs
results_df[i, "NSE_PRMS_Obs"] <- NSE_PRMS_Obs
results_df[i, "KGE_PRMS_Obs"] <-KGE_PRMS_Obs
results_df[i, "NSE_RAVEN_Obs"] <-  NSE_RAVEN_Obs
results_df[i, "KGE_RAVEN_Obs"] <- KGE_RAVEN_Obs
results_df[i, "NSE_HYPE_PRMS"] <- NSE_HYPE_PRMS
results_df[i, "KGE_HYPE_PRMS"] <-KGE_HYPE_PRMS
results_df[i, "NSE_HYPE_RAVEN"] <-NSE_HYPE_RAVEN
results_df[i, "KGE_HYPE_RAVEN"] <-KGE_HYPE_RAVEN
results_df[i, "NSE_PRMS_RAVEN"] <- NSE_PRMS_RAVEN
results_df[i, "KGE_PRMS_RAVEN"] <-KGE_PRMS_RAVEN  

# Loop through each gauge location
for (i in seq_along(name_all)) {
  name <- name_all[i]
  
  # Subset the data within the given time frame for each dataset (Obs_flow, PRMS_Data_All, HYPE_Data_All)
  subset_Obs_flow <- Obs_flow[Obs_flow$Date >= start_date & Obs_flow$Date <= end_date, c("Date", name)]
  subset_PRMS_Data <- PRMS_Data_All[PRMS_Data_All$Date >= start_date & PRMS_Data_All$Date <= end_date, c("Date", name)]
  subset_HYPE_Data <- HYPE_Data_All[HYPE_Data_All$Date >= start_date & HYPE_Data_All$Date <= end_date, c("Date", name)]
  subset_RAVEN_Data <- RAVEN_Data_All[RAVEN_Data_All$Date >= start_date & RAVEN_Data_All$Date <= end_date, c("Date", name)]
  
  # Calculate spearman correlation for the subset data
  Spearman_HYPE <- cor(subset_Obs_flow[[name]], subset_HYPE_Data[[name]], method = "spearman", use = "pairwise.complete.obs")
  Spearman_HYPE <- round(Spearman_HYPE, 3)
  
  Spearman_PRMS <- cor(subset_Obs_flow[[name]], subset_PRMS_Data[[name]], method = "spearman", use = "pairwise.complete.obs")
  Spearman_PRMS <- round(Spearman_PRMS, 3)
  
  Spearman_RAVEN <- cor(subset_Obs_flow[[name]], subset_RAVEN_Data[[name]], method = "spearman", use = "pairwise.complete.obs")
  Spearman_RAVEN <- round(Spearman_RAVEN, 3)
  
  # Calculate R2 for the subset data
  lm_HYPE <- lm(subset_Obs_flow[[name]] ~ subset_HYPE_Data[[name]])
  R2_HYPE <- summary(lm_HYPE)$r.squared
  R2_HYPE <- round(R2_HYPE, 3)
  
  lm_PRMS <- lm(subset_Obs_flow[[name]] ~ subset_PRMS_Data[[name]])
  R2_PRMS <- summary(lm_PRMS)$r.squared
  R2_PRMS <- round(R2_PRMS, 3)
  
  lm_RAVEN <- lm(subset_Obs_flow[[name]] ~ subset_RAVEN_Data[[name]])
  R2_RAVEN <- summary(lm_RAVEN)$r.squared
  R2_RAVEN <- round(R2_RAVEN, 3)
  
  #calculate NSE for subset data 
  NSE_HYPE_Obs <- NSE(subset_HYPE_Data[[name]], subset_Obs_flow[[name]], na.rm = FALSE)
  NSE_PRMS_Obs <- NSE(subset_PRMS_Data[[name]], subset_Obs_flow[[name]], na.rm = FALSE)
  NSE_HYPE_PRMS <- NSE(subset_PRMS_Data[[name]], subset_HYPE_Data[[name]], na.rm = FALSE)
  NSE_HYPE_RAVEN <- NSE(subset_HYPE_Data[[name]], subset_RAVEN_Data[[name]], na.rm = FALSE)
  NSE_PRMS_RAVEN <- NSE(subset_PRMS_Data[[name]], subset_RAVEN_Data[[name]], na.rm = FALSE)
  NSE_RAVEN_Obs <- NSE(subset_RAVEN_Data[[name]], subset_Obs_flow[[name]], na.rm = FALSE)

  KGE_HYPE_Obs <- KGE(subset_HYPE_Data[[name]], subset_Obs_flow[[name]], s=c(1,1,1), na.rm=TRUE, method="2009", out.type="single", fun=NULL)
  KGE_PRMS_Obs <- KGE(subset_PRMS_Data[[name]], subset_Obs_flow[[name]], s=c(1,1,1), na.rm=TRUE, method="2009", out.type="single", fun=NULL)
  KGE_HYPE_PRMS <- KGE(subset_HYPE_Data[[name]], subset_PRMS_Data[[name]], s=c(1,1,1), na.rm=TRUE, method="2009", out.type="single", fun=NULL)
  KGE_HYPE_RAVEN  <- KGE(subset_HYPE_Data[[name]], subset_RAVEN_Data[[name]], s=c(1,1,1), na.rm=TRUE, method="2009", out.type="single", fun=NULL)
  KGE_PRMS_RAVEN  <- KGE(subset_PRMS_Data[[name]], subset_RAVEN_Data[[name]], s=c(1,1,1), na.rm=TRUE, method="2009", out.type="single", fun=NULL)
  KGE_RAVEN_Obs <- KGE(subset_RAVEN_Data[[name]], subset_Obs_flow[[name]], s=c(1,1,1), na.rm=TRUE, method="2009", out.type="single", fun=NULL)
  
  # Store the result in the dataframe
  results_df[i, "Guage_Name"] <- name
  results_df[i, "StartDate"] <- format(start_date, "%Y-%m-%d")
  results_df[i, "EndDate"] <- format(end_date, "%Y-%m-%d")
  results_df[i, "NSE_HYPE_Obs"] <- NSE_HYPE_Obs
  results_df[i, "KGE_HYPE_Obs"] <-KGE_HYPE_Obs
  results_df[i, "NSE_PRMS_Obs"] <- NSE_PRMS_Obs
  results_df[i, "KGE_PRMS_Obs"] <-KGE_PRMS_Obs
  results_df[i, "NSE_RAVEN_Obs"] <-  NSE_RAVEN_Obs
  results_df[i, "KGE_RAVEN_Obs"] <- KGE_RAVEN_Obs
  results_df[i, "NSE_HYPE_PRMS"] <- NSE_HYPE_PRMS
  results_df[i, "KGE_HYPE_PRMS"] <-KGE_HYPE_PRMS
  results_df[i, "NSE_HYPE_RAVEN"] <-NSE_HYPE_RAVEN
  results_df[i, "KGE_HYPE_RAVEN"] <-KGE_HYPE_RAVEN
  results_df[i, "NSE_PRMS_RAVEN"] <- NSE_PRMS_RAVEN
  results_df[i, "KGE_PRMS_RAVEN"] <-KGE_PRMS_RAVEN  
}

# Print the dataframe with results
print(results_df)
# Specify the file path where you want to save the CSV file
csv_file <- ("Files/SMMR_Comparison_Results/SMMR_Corr_Results_V2.csv")

# Save results_df as a CSV file
write.csv(results_df, file = csv_file, row.names = FALSE)

# Optional: Print a message confirming the file was saved
cat("CSV file saved:", csv_file, "\n")


#graph results
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)
library(patchwork)

maps_df <- merge(results_df, gauge_info, by = "name_all")

maps_sf <- st_as_sf(maps_df, coords = c("longitude", "latitude"), crs = 4326)

# Plot the map
p4 <- ggplot(data = maps_sf) +
  geom_sf(aes(color = Spearman_PRMS), size = 3) +
  scale_color_viridis_c(option = "plasma", name = "Spearman for PRMS") +
  theme_minimal() +
  labs(title = "Spearman Correlation at Each Gauge",
       x = "Longitude",
       y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

p5 <- ggplot(data = maps_sf) +
  geom_sf(aes(color = Spearman_HYPE), size = 3) +
  scale_color_viridis_c(option = "plasma", name = "Spearman for HYPE") +
  theme_minimal() +
  labs(
       x = "Longitude",
       y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5))

comparison <- p4 / p5
