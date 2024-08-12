library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)

HYPE_Data <- read.csv("Files/HYPE_CMS_SwiftcurrentCreekSherburneReservoir.csv")
HYPE_Data$DATE <- as.Date(HYPE_Data$DATE, format = "%Y-%m-%d")

PRMS_Data_All <- read.table("Files/Simulated_flow_cfs_out", header = TRUE, sep = "\t")
PRMS_Data_All$Date <- as.Date(paste(PRMS_Data_All$Year, PRMS_Data_All$Month, PRMS_Data_All$Day, sep = "-"))
names(PRMS_Data_All) <- gsub("\\.", " ", names(PRMS_Data_All))

PRMS_Data <- PRMS_Data_All[c("Date", "Swiftcurrent Creek at Sherburne Reservoir")]
# Convert flow rate from cfs to cms
PRMS_Data$`Swiftcurrent Creek at Sherburne Reservoir` <- PRMS_Data$`Swiftcurrent Creek at Sherburne Reservoir` * 0.0283168

# Create the initial plot using HYPE_Data
p1 <- ggplot(HYPE_Data) +
  geom_line(aes(DATE, cout, colour = "HYPE Data"),show.legend = TRUE) +
  scale_y_continuous(position = "left",
                     limits = c(-1, 20),
                     expand = c(0,0)) +
  scale_color_manual(values = c("HYPE Data" = "steelblue", "PRMS Data" = "red"),  # Specify colors for both datasets
                     labels = c("HYPE Data", "PRMS Data")) +  # Specify labels for both datasets
  guides(x = guide_axis(angle = 90)) +
  labs(y = "Discharge [cms]",
       x = "Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Adjust date breaks and labels to display years
  theme_minimal() +
  theme(axis.title.y.left = element_text(hjust = 0),
        legend.position = "right",  
        legend.justification = c(0.5, 1),  # Center the legend horizontally
        legend.box = "horizontal",  # Display legend items horizontally
        legend.title = element_blank())+   # Remove legend title
  ggtitle("Hydrograph at Swiftcurrent Creek by Sherburne Reservoir from 2010 to 2015")


# Add the line for PRMS_Data
p1 <- p1 + 
  geom_line(data = PRMS_Data, aes(x = Date, y = `Swiftcurrent Creek at Sherburne Reservoir`, colour = "PRMS Data"))

# Set the desired date range for the x-axis
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2015-01-01")

# Update the x-axis limits in the plot
p1 <- p1 + xlim(start_date, end_date)

print(p1 +  ggtitle("Hydrograph at Swiftcurrent Creek by Sherburne Reservoir from 2010 to 2015")
)





