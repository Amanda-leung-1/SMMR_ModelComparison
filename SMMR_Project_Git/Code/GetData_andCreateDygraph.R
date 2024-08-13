library(ggplot2)
library(gridExtra)
library(dplyr)
library(dygraphs)
library(xts)
library(htmlwidgets)
library(lubridate)
library(hydroGOF)
library(purrr)

#function to add plugin to allow for timeseries to be toggled 
dyHide <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Hide",
    path = system.file("plugins/hide.js", package = "dygraphs")
  )
}

# Replace ravensubbasin ID with DNRC subbasin ID
replace_subbasin_with_gauge <- function(column_names, raven_names) {
  # Create a named vector for replacement
  replacement_vector <- setNames(raven_names$gauge_id, raven_names$model_subbasin_id)
  
  # Replace Model_Subbasin_ID with Gauge_ID in column names
  updated_names <- sapply(column_names, function(name) {
    # Find all matches of Model_Subbasin_ID in the column name
    for (id in raven_names$model_subbasin_id) {
      if (grepl(paste0("sub", id), name)) {
        name <- gsub(paste0("sub", id), replacement_vector[as.character(id)], name)
      }
    }
    return(name)
  })
  
  return(updated_names)
}

#Setting variable names
start_date <- as.POSIXct("1980-01-01")
end_date <- as.POSIXct("2015-12-31")
#max start date is 1980-01-01
#max end date is 2015-12-31
gauge_name <- "SWCSB"
name <- "SWCSB"
name_all <- c(
  "SWCSB",   # Swiftcurrent Creek at Sherburne Reservoir
  "SMRBB",   # St Mary River near Babb MT
  "SMRIB",   # St Mary River at International Boundary
  "MRWIB",   # Milk River at Western Crossing of International Boundary
  "NFKMR",   # North Fork Milk River above St Mary Canal near Browning
  "MREIB",   # Milk River at Eastern Crossing
  "CLCMO",   # Clear Creek at Mouth
  "LDCIB",   # Lodge Creek at International Boundary
  "BTCIB",   # Battle Creek at International Boundary
  "PPCMO",   # Peoples Creek at Mouth
  "BCBMO",   # Beaver Creek Bowdoin
  "RKCMO"    # Rock Creek at Mouth
)

# Import HYPE Data from hype_simulated folder
file_directory <- "Input/hype_simulated" 
file_list <- list.files(path = file_directory, pattern = "*.txt", full.names = TRUE)
data_frames <- list() # Initialize an empty list to store data frames

# Read and process each file in the folder since it is stored as a .txt file 
for (file in file_list) {
  file_name <- tools::file_path_sans_ext(basename(file))# Extract the file name without extension
  temp_data <- read.table(file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)   # Read the file
  # Filter and select necessary columns
  temp_data <- temp_data %>%
    filter(DATE != "UNITS" & !grepl("m3s", cout)) %>%
    select(DATE, cout)
  temp_data$DATE <- as.POSIXct(temp_data$DATE)
  temp_data$cout <- as.numeric(temp_data$cout)
  colnames(temp_data) <- c("DATE", file_name)   # Rename columns based on file name
  data_frames[[file_name]] <- temp_data   # Store in the list
}
# Combine dataframes into a single dataframe by DATE
HYPE_Data_All <- Reduce(function(x, y) merge(x, y, by = "DATE", all = TRUE), data_frames)
HYPE_Data_All <- HYPE_Data_All %>%
  rename(Date = DATE)
names(HYPE_Data_All) <- gsub("\\.|,", "", names(HYPE_Data_All))
#named in the order that the files appear in the hype_simulated folder 
colnames(HYPE_Data_All) <- c( "Date",
                              "BTCIB",   "BCBMO",   "CLCMO",   "LDCIB",   "MREIB",   "MRWIB",
                              "NFKMR",   "PPCMO",   "RKCMO",   "SMRIB",   "SMRBB",   "SWCSB")

#Extract PRMS data 
PRMS_Data_All <- read.table("Input/prms_simulated", header = TRUE, sep = "\t")
PRMS_Data_All$Date <- as.POSIXct(paste(PRMS_Data_All$Year, PRMS_Data_All$Month, PRMS_Data_All$Day, sep = "-"))
names(PRMS_Data_All) <- gsub("\\.", " ", names(PRMS_Data_All))
colnames(PRMS_Data_All) <- gsub("\\s+", " ", colnames(PRMS_Data_All))
colnames(PRMS_Data_All)[5:16] <- name_all
#convert units (conversion from cfs to cms is 0.0283168)
PRMS_Data_All[name_all] <- PRMS_Data_All[name_all] * 0.0283168


#import observed flow cfs file to R
Obs_flow <- read.table("Input/observed", sep = "\t")
#rename columns
colnames(Obs_flow) <- c("Year", "Month", "Day", "V4", "V5","V6", 
                        "SWCSB","SMRBB","SMRIB", "MRWIB", "NFKMR","MREIB","CLCMO", 
                        "LDCIB","BTCIB", "PPCMO", "BCBMO", "RKCMO")
#add new column called date that combines the year, month, and day
Obs_flow$Date <- as.POSIXct(paste(Obs_flow$Year, Obs_flow$Month, Obs_flow$Day, sep = "-"))
# Extract flow columns (assuming they start from column 7)
Obs_flow_colnames <- colnames(Obs_flow)[7:18]
# Replace -9999 with NA in flow columns
Obs_flow[Obs_flow_colnames][Obs_flow[Obs_flow_colnames] == -9999] <- NA
Obs_flow[Obs_flow_colnames] <- Obs_flow[Obs_flow_colnames] * 0.0283168

#RAVEN DATA IMPORT 
RAVEN_Data_stmary <- read.csv("Input/raven_simulated_stmary.csv", header = TRUE)
RAVEN_Data_milk <- read.csv("Input/raven_simulated_milk.csv", header = TRUE)
raven_names <- read.csv("Input/raven_names.csv", header = TRUE)

names(RAVEN_Data_stmary) <- replace_subbasin_with_gauge(names(RAVEN_Data_stmary), raven_names)
stmary_gauge_id <- raven_names$gauge_id[1:3]
# Define the patterns for the columns to select
stmary_raven <- c("date",stmary_gauge_id)
# Use dplyr to select columns that match the patterns
RAVEN_Data_stmary_select <- RAVEN_Data_stmary %>%
  select(contains(stmary_raven)) %>%
  select(-contains("observed"))

names(RAVEN_Data_milk) <- replace_subbasin_with_gauge(names(RAVEN_Data_milk), raven_names)
milk_gauge_id <- raven_names$gauge_id[4:14]
milk_raven <- c("date",milk_gauge_id)
# select columns in raven data that match the DNRC basin ID in the raven_names file 
RAVEN_Data_milk_select <- RAVEN_Data_milk %>%
  select(contains(milk_raven)) %>%
  select(-contains("observed"))

# Merge the data frames by the 'time' column
RAVEN_Data_All <- merge(RAVEN_Data_stmary_select, RAVEN_Data_milk_select, by = "date", all = TRUE)  # 'all = TRUE' includes all rows from both data frames
# Clean up column names
RAVEN_Data_All <- RAVEN_Data_All %>%
  rename_all(~ gsub("..m3.s.", "", .))  %>%# Remove the specific suffix "_m3.s"
  rename(Date = date) %>%
  mutate(Date = as.POSIXct(paste(Date, "00:00:00"), format = "%Y-%m-%d %H:%M:%S"))




## DYNAMIC PLOT SHOWING THE HYDROGRAPH 
# Define a function to create and save a dygraph plot
create_dygraph <- function(gauge_name) {
  # Create initial data frame with dates
  df_HYPE_PRMS_Obs_RAVEN <- data.frame(Date = Obs_flow$Date)
  
  # Merge the data frames for each model and observed data
  df_HYPE_PRMS_Obs_RAVEN <- merge(df_HYPE_PRMS_Obs_RAVEN, HYPE_Data_All[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
  df_HYPE_PRMS_Obs_RAVEN <- merge(df_HYPE_PRMS_Obs_RAVEN, PRMS_Data_All[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
  df_HYPE_PRMS_Obs_RAVEN <- merge(df_HYPE_PRMS_Obs_RAVEN, Obs_flow[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
  df_HYPE_PRMS_Obs_RAVEN <- merge(df_HYPE_PRMS_Obs_RAVEN, RAVEN_Data_All[, c("Date", gauge_name)], by = "Date", all.x = TRUE)
  
  # Rename columns appropriately
  colnames(df_HYPE_PRMS_Obs_RAVEN)[2:5] <- c("HYPE", "PRMS", "Obs", "RAVEN")
  
  # Convert the data frame to an xts object
  xts_HYPE_PRMS_Obs_RAVEN <- xts(
    x = df_HYPE_PRMS_Obs_RAVEN[, c("HYPE", "PRMS", "Obs", "RAVEN")],
    order.by = df_HYPE_PRMS_Obs_RAVEN$Date
  )
  
  # Create the dygraph
  dygraph_HYPE_PRMS_Obs_RAVEN <- dygraph(xts_HYPE_PRMS_Obs_RAVEN, main = sprintf("Hydrograph for %s", gauge_name)) %>%
    dySeries("HYPE", label = "HYPE") %>%
    dySeries("PRMS", label = "PRMS") %>%
    dySeries("RAVEN", label = "RAVEN") %>%
    dySeries("Obs", label = "Obs") %>%
    dyAxis("y", label = "Flows (cms)", valueRange = c(0, 450)) %>%
    dyAxis("x", label = "Year") %>%
    dyOptions(colors = c("blue", "gray", "red", "black")) %>%
    dyHide() %>%
    dyRangeSelector() %>%
    dyCSS("Code/legend.css")
  
  # Save the dygraph as an HTML file
  file_save <- sprintf("Results/dygraph/%s.html", gauge_name)
  saveWidget(dygraph_HYPE_PRMS_Obs_RAVEN, file = file_save, selfcontained = TRUE)
}

# Loop through all gauge names and create dygraphs
for (gauge_name in name_all) {
  create_dygraph(gauge_name)
}


