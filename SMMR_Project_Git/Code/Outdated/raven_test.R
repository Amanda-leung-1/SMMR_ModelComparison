
RAVEN_Data_milk2 <- read.csv("Hydrographs_milk2.csv", header = TRUE)
RAVEN_Data_milk2 <- RAVEN_Data_milk2 %>%
  rename(Date = date) %>%
  mutate(Date = as.POSIXct(paste(Date, "00:00:00"), format = "%Y-%m-%d %H:%M:%S"))
RAVEN_Data_milk2_graph <- data.frame(Date = Obs_flow$Date)
RAVEN_Data_milk2_graph  <- merge(RAVEN_Data_milk2_graph, Obs_flow[, c("Date", name)], by = "Date", all.x = TRUE)
RAVEN_Data_milk2_graph  <- merge(RAVEN_Data_milk2_graph, RAVEN_Data_milk2[, c("Date", name)], by = "Date", all.x = TRUE)
names(RAVEN_Data_milk2_graph)[2:3] <- c("Obs", "RAVEN")
RAVEN_Data_milk2_xts <- xts(
  x = RAVEN_Data_milk2_graph[,c("Obs", "RAVEN")],
  order.by = RAVEN_Data_milk2_graph$Date
)
RAVEN_Data_milk2_dygraph <- dygraph(RAVEN_Data_milk2_xts, main = sprintf("Hydrograph for %s", name)) %>%
  dyAxis("y", label = "Flows (cms)", valueRange = c(0,450))%>%
  dyAxis("x", label = "Year")%>%
  dyOptions(colors = c("blue", "gray"))%>%  # Set custom colors
  dyRangeSelector()
subset_Obs_flow <- Obs_flow[Obs_flow$Date >= start_date & Obs_flow$Date <= end_date, c("Date", name)]
subset_RAVEN_Data <- RAVEN_Data_milk2[RAVEN_Data_milk2$Date >= start_date & RAVEN_Data_milk2$Date <= end_date, c("Date", name)]
KGE_RAVEN_Obs_2 <- KGE(subset_RAVEN_Data[[name]], subset_Obs_flow[[name]], s=c(1,1,1), na.rm=TRUE, method="2009", out.type="single", fun=NULL)
print(KGE_RAVEN_Obs_2)