library (ggplot2)
library(dplyr)


start_d <- "2020-10-01"
end_d <- "2023-10-31"

#create a date sequence
date_sequence<-seq(as.Date(start_d),as.Date(end_d), by="day")
nrows<-length(date_sequence)
my_data<-data_frame(Date=date_sequence,levels=rep(NA,nrows),outlet=rep(NA,nrows))

#read data from Hydat but don't bring everything in, only select the date and values
GSL_levels<-subset(hy_daily_levels("07SB001",start_date=start_d,end_date=end_d),select=c(Date,Value))
GSL_outlet<-subset(hy_daily_flows("10FB006",start_date=start_d,end_date=end_d),select=c(Date,Value))

#now get real-time data
RT_levels<-subset((realtime_dd("07SB001") %>% realtime_daily_mean()),select=c(Date,Value))
RT_outlet<-subset((realtime_dd("10FB006") %>% realtime_daily_mean()),select=c(Date,Value))

#10FBnow place data in the right spot based on date - this will generate warnings
my_data$levels[my_data$Date == GSL_levels$Date] <- GSL_levels$Value
my_data$outlet[my_data$Date == GSL_outlet$Date] <- GSL_outlet$Value
my_data$levels[my_data$Date == RT_levels$Date] <- RT_levels$Value
my_data$outlet[my_data$Date == RT_outlet$Date] <- RT_outlet$Value

#now append the data
GSL_level<-rbind(GSL_levels,RT_levels)
GSL_outlet<-rbind(GSL_outlet, RT_levels)

#now merge  data into a common database for the entire period 
merged<-merge(GSL_levels,GSL_outlet, by="Date", all=TRUE)

# create the plot for level
plot1<- ggplot(my_data, aes(x=Date,y=outlet)) + geom_line() 

# create the plot for the flow
plot2<- ggplot(my_data, aes(x=Date,y=levels)) + geom_line()

#arrange the plots on 1 page
my_plt<-grid.arrange(plot1, plot2, ncol = 1)

print(my_plt)
