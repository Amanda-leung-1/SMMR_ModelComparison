

############ Scatterplot

# Subset the data within the given time frame
subset_Obs_flow <- Obs_flow[Obs_flow$Date >= start_date & Obs_flow$Date <= end_date, c("Date", name)]
subset_PRMS_Data <- PRMS_Data_All[PRMS_Data_All$Date >= start_date & PRMS_Data_All$Date <= end_date, c("Date", name)]
subset_HYPE_Data <- HYPE_Data_All[HYPE_Data_All$Date >= start_date & HYPE_Data_All$Date <= end_date, c("Date", name)]

# Plot the subset data
# Calculate the range for the axes
min_value <- min(c(subset_Obs_flow[[name]], subset_PRMS_Data[[name]], subset_HYPE_Data[[name]]), na.rm = TRUE)
max_value <- max(c(subset_Obs_flow[[name]], subset_PRMS_Data[[name]], subset_HYPE_Data[[name]]), na.rm = TRUE)
range_values <- c(min_value, max_value)

# Set up a 1x2 grid for plots
par(mfrow = c(1, 2))

# Create the scatter plot with equal axis limits
plot(subset_Obs_flow[[name]], subset_PRMS_Data[[name]], 
     main = paste("Scatterplot for PRMS"),  
     xlab = "Observed", 
     ylab = "PRMS", 
     pch = 19,
     xlim = range_values, 
     ylim = range_values)

# Add a red, dashed line
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

plot(subset_Obs_flow[[name]], subset_HYPE_Data[[name]],
     main = "Scatterplot for HYPE", 
     xlab = "Observed", 
     ylab = "HYPE", 
     pch = 19,
     xlim = range_values, 
     ylim = range_values)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2) # Red, dashed line

main_title <- paste("Comparison of Models for Gauge", name, "for the time period of", start_date, "to", end_date)
mtext(main_title, side = 3, line = 3, cex = 0.9, adj = 1.1)
