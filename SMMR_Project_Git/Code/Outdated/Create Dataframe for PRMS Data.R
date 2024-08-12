create_location_dataframe <- function(data, location) {
  # Check if the location column exists in the data frame
  if (!(location %in% colnames(data))) {
    stop(paste("Column", location, "does not exist in the data frame."))
  }
  
  # Select the "Date" and location columns
  location_data <- data[c("Date", location)]
  
  # Rename the location column to include the location name
  colnames(location_data)[2] <- location
  
  return(location_data)
}

PRMS_Data_All <- read.table("Files/Simulated_flow_cfs_out", header = TRUE, sep = "\t")
PRMS_Data_All$Date <- as.Date(paste(PRMS_Data_All$Year, PRMS_Data_All$Month, PRMS_Data_All$Day, sep = "-"))
names(PRMS_Data_All) <- gsub("\\.", " ", names(PRMS_Data_All))

locations <- c("Swiftcurrent Creek at Sherburne Reservoir", "St  Mary River near Babb  MT", 
               "St  Mary River at International Boundary", "Milk River at Western Crossing of International Boundary", 
               "North Fork Milk River above St Mary Canal near Browning", "Milk River at Eastern Crossing", 
               "Clear Creek at Mouth", "Lodge Creek at International Boundary", 
               "Battle Creek at International Boundary", "Peoples Creek at Mouth", 
               "Beaver Creek Bowdoin", "Rock Creek at Mouth")

location_dataframes <- lapply(locations, function(location) {
  create_location_dataframe(PRMS_Data_All, location)
})


# Select the data frame you want to plot (for example, the first one)
df_to_plot <- location_dataframes[[1]]

# Create the plot
plot <- ggplot(df_to_plot) +
  geom_line(aes(Date, `Swiftcurrent Creek at Sherburne Reservoir`)) +
  scale_y_continuous(position = "left", limits = c(-1, 20), expand = c(0, 0))

