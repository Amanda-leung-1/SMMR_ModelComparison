



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


# GGPLOT SHOWING THE HYDROGRAPH THAT CAN CHANGE BASED ON ON THE start_time and end_time
p1 <- ggplot(HYPE_Data_All) +
  geom_line(aes(Date, .data[[name]], colour = "HYPE"), show.legend = TRUE) +
  geom_line(data = PRMS_Data_All, aes(x = Date, y = .data[[name]], colour = "PRMS"))+
  geom_line(data = Obs_flow, aes(x = Date, y = .data[[name]], colour = "Observed"))+
  scale_y_continuous(position = "left", limits = c(0, 20), expand = c(0,0)) +
  guides(x = guide_axis(angle = 90)) +
  labs(y = "Flows [cms]",
       x = "Date",
       title = sprintf("Hydrograph for %s from %s to %s", name, start_date, end_date)) +
  theme_minimal() +
  theme(axis.title.y.left = element_text(hjust = 0),
        legend.position = "right",
        legend.justification = c(0.5, 1),  # Center the legend horizontally
        legend.box = "horizontal") +  # Display legend items horizontally
  scale_x_datetime(limits = c(start_date, end_date), date_breaks = "6 months", date_labels = "%b %Y") +  # Set limits and breaks
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels for better visibility
