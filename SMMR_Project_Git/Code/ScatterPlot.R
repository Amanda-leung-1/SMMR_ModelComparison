# Define start and end dates
start_date <- as.POSIXct("1981-10-01")
end_date <- as.POSIXct("2015-09-30")

# Function to subset data based on date range
subset_data <- function(data, name) {
  subset(data[data$Date >= start_date & data$Date <= end_date, c("Date", name)])
}

# Function to create scatter plots
create_scatter_plot <- function(observed, model_data, model_name, name) {
  plot(observed, model_data, 
       main = paste("Scatterplot for", model_name),  
       xlab = "Observed", 
       ylab = model_name, 
       pch = 19,
       xlim = range_values, 
       ylim = range_values,
       las = 2)  
  abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
  grid()
}

# File path to save the plots
file_path <- "Results/scatterplots"

# Loop through each name in name_all
for (name in name_all) {
  
  # Subset the data for each model
  subset_Obs_flow <- subset_data(Obs_flow, name)
  subset_PRMS_Data <- subset_data(PRMS_Data_All, name)
  subset_HYPE_Data <- subset_data(HYPE_Data_All, name)
  subset_RAVEN_Data <- subset_data(RAVEN_Data_All, name)
  
  # Calculate the range for the axes
  range_values <- range(c(subset_Obs_flow[[name]], 
                          subset_PRMS_Data[[name]], 
                          subset_HYPE_Data[[name]], 
                          subset_RAVEN_Data[[name]]), na.rm = TRUE)
  
  # Set up a PNG device
  png_filename <- paste0(file_path, "Scatterplot_", name, ".png")
  png(filename = png_filename, width = 1200, height = 600)
  
  # Set up a 1x3 grid for plots
  par(mfrow = c(1, 3))
  
  # Create scatter plots for each model
  create_scatter_plot(subset_Obs_flow[[name]], subset_PRMS_Data[[name]], "PRMS", name)
  create_scatter_plot(subset_Obs_flow[[name]], subset_HYPE_Data[[name]], "HYPE", name)
  create_scatter_plot(subset_Obs_flow[[name]], subset_RAVEN_Data[[name]], "RAVEN", name)
  
  # Add a main title
  main_title <- paste("Comparison of Models for Gauge", name, "for the time period of", start_date, "to", end_date)
  mtext(main_title, side = 3, line = 3, cex = 0.9, adj = 1.1)
  
  # Close the PNG device to save the file
  dev.off()
}

