#import observed flow cfs file to R
Obs_flow <- read.table("Files/Observed_flow_cfs", sep = "\t")

#rename columns
colnames(Obs_flow) <- c("Year", "Month", "Day", "V4", "V5","V6", 
                        "Swiftcurrent Creek at Sherburne Reservoir", "St Mary River near Babb MT",
                        "St Mary River at International Boundary", "Milk River at Western Crossing of International Boundary",
                        "North Fork Milk River above St Mary Canal near Browning", "Milk River at Eastern Crossing",
                        "Clear Creek at Mouth", "Lodge Creek at International Boundary", 
                        "Battle Creek at International Boundary", "Peoples Creek at Mouth", 
                        "Beaver Creek Bowdoin", "Rock Creek at Mouth")
#add new column called date that combines the year, month, and day
Obs_flow$Date <- as.Date(paste(Obs_flow$Year, Obs_flow$Month, Obs_flow$Day, sep = "-"))

# Extract flow columns (assuming they start from column 7)
flow_columns <- colnames(Obs_flow)[7:18]

# Replace -9999 with NA in flow columns
Obs_flow[flow_columns][Obs_flow[flow_columns] == -9999] <- NA
Obs_flow[flow_columns] <- Obs_flow[flow_columns] * 0.0283168
