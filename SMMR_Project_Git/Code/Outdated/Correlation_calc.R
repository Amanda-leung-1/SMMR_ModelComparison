library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(patchwork)
library(ggplot2)

#set plot theme for the document so we don't have to do it in every plot
theme_set(theme_classic())
startDate <- "1960-01-01"
endDate <- "2020-01-01"

HYPE_Data %>% ggplot(aes(x = DATE, y = cout))+
  geom_line()
HYPE_Data %>% ggplot(aes(cout))+
  stat_density()+
  scale_x_log10()+
  geom_vline(xintercept = median(HYPE_Data$cout), color = "red")

HYPE_Data %>% ggplot(aes(cout))+
  stat_ecdf()+
  scale_x_log10()+
  geom_vline(xintercept = median(HYPE_Data$cout), color = "red")+
  geom_vline(xintercept = quantile(HYPE_Data$cout)[4], color = "blue")

#Flow is negative in rank() to make high flows ranked low (#1)
HYPE_Data <- HYPE_Data %>%
  mutate(rank = rank(-cout)) %>%
  mutate(P = 100 * (rank / (length(cout) + 1)))

p1 <- HYPE_Data %>% ggplot(aes(x = P, y = cout))+
  geom_line()+
  scale_y_log10()+
  xlab("% Time flow equalled or exceeded")+
  ylab("Q (cfs)")

PRMS_Data <- PRMS_Data %>%
  mutate(rank = rank(-`Swiftcurrent Creek at Sherburne Reservoir`)) %>%
  mutate(P = 100 * (rank / (length(`Swiftcurrent Creek at Sherburne Reservoir`) + 1)))

p1 <- p1 + geom_line(data = PRMS_Data, aes(x = P, y = `Swiftcurrent Creek at Sherburne Reservoir`), color = "red")



