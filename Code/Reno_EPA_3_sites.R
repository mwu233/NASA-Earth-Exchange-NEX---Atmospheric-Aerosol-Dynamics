
# Author: Meiliu Wu
# Summer 2018
# Project: NASA NEX - Atmospheric Aerosol Dynamics

#--------------------------------------------------------------
# Set up packages and libraries
#--------------------------------------------------------------
# install.packages("ggmap")
# install.packages("ggplot")
# install.packages("ggplot2")
# install.packages("leaflet")
# install.packages("magrittr")
# install.packages("ggthemes")
# install.packages("tidyverse")
# install.packages("geojsonR")
# install.packages("tidyr")
# install.packages("RPostgreSQL") # connect to db
# install.packages("rpostgis")
# install.packages("sp")
# install.packages("postGIStools")
# install.packages("EnvStats")
# install.packages("data.table")
# install.packages("mgcv")
# install.packages("voxel")
# install.packages("lattice")
# install.packages("visreg")

library(ggmap)
library(ggplot2)
library(leaflet)
library(magrittr)
library(dplyr)
library(ggthemes)
library(scales)
library(tidyverse)
library(geojsonR)
library(sp)
library(lubridate)
library(reshape2)
library(RPostgreSQL)
library(rpostgis)
library(sp)
library(postGIStools)
library(EnvStats)
library(data.table)
library(mgcv)
library(voxel)
library(lattice)
library(visreg)

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno_22
#--------------------------------------------------------------#
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 4"
Reno_22.EPA_Site <- subset(hourly_pm2.5_allYears,
                           Site.Num == 22 & 
                             State.Name == "Nevada" & 
                             County.Name == "Washoe")

# Create a new column as PST Local time 
# by joining the local date and time together
Reno_22.EPA_Site$DateTime.Local <- as.POSIXct(paste(Reno_22.EPA_Site$Date.Local, 
                                                    Reno_22.EPA_Site$Time.Local), 
                                              format = "%Y-%m-%d %H:%M")

# Plot the hourly-average PM2.5 data from the EPA site around CCNY (Aeronet site)

# TODO: 
# 1. Break the days as monthly-based so that later we can con_pmsolidate them into seasons
# 2. Plot the PM2.5 daily change trend 
# with 24-hour as X-axis and the hourly-based-average values as Y-axis

# Create a new df with year, month, 24-hour as the X-axis, hourly-based average of PM2.5 as the Y-axis
Year.Local <- lubridate::year(Reno_22.EPA_Site$DateTime.Local)
Month.Local <- lubridate::month(Reno_22.EPA_Site$DateTime.Local, label = TRUE, abbr = FALSE)
Hour.Local <- format(Reno_22.EPA_Site$DateTime.Local,"%H:%M")

Reno_22.PM25.diurnal.byMonth <- data.frame(Year.Local,Month.Local,Hour.Local)
Reno_22.PM25.diurnal.byMonth$DateTime.Local <- Reno_22.EPA_Site$DateTime.Local
Reno_22.PM25.diurnal.byMonth$PM2.5 <- Reno_22.EPA_Site$Sample.Measurement

# Remove the rows that have invalid PM2.5 value
Reno_22.PM25.diurnal.byMonth <- subset(Reno_22.PM25.diurnal.byMonth, PM2.5 > 0)

#################################################
###### 1. Aggregated by year, month, hour #######
#################################################
Reno_22.PM25.hourly_geoMean.byMonth <- aggregate(PM2.5 ~ Year.Local + Month.Local + Hour.Local,
                                                 data = Reno_22.PM25.diurnal.byMonth,
                                                 FUN = geoMean)

year <- 2014
start.month <- 1
end.month <- 12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

Reno_22.PM25.hourly_geoMean.byMonth %>% 
  na.omit %>%
  subset(Year.Local == year &
           Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 15), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local) +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", ", 
                 year, collapse = NULL))

#################################################
######### 2. Aggregated by month, hour ##########
#################################################
Reno_22.PM25.hourly_Agg_MH.byMonth <- aggregate(PM2.5 ~ Month.Local + Hour.Local,
                                                data = Reno_22.PM25.diurnal.byMonth,
                                                FUN = geoMean)
start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
Reno_22.PM25.hourly_Agg_MH.byMonth %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 9), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)") +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  #labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\monthly trend_Reno_22.png")

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno_1005
#--------------------------------------------------------------#
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 4"
Reno_1005.EPA_Site <- subset(hourly_pm2.5_allYears,
                             Site.Num == 1005 & 
                               State.Name == "Nevada" & 
                               County.Name == "Washoe")

# Create a new column as PST Local time 
# by joining the local date and time together
Reno_1005.EPA_Site$DateTime.Local <- as.POSIXct(paste(Reno_1005.EPA_Site$Date.Local, 
                                                      Reno_1005.EPA_Site$Time.Local), 
                                                format = "%Y-%m-%d %H:%M")

# Plot the hourly-average PM2.5 data from the EPA site around CCNY (Aeronet site)

# TODO: 
# 1. Break the days as monthly-based so that later we can con_pmsolidate them into seasons
# 2. Plot the PM2.5 daily change trend 
# with 24-hour as X-axis and the hourly-based-average values as Y-axis

# Create a new df with year, month, 24-hour as the X-axis, hourly-based average of PM2.5 as the Y-axis
Year.Local <- lubridate::year(Reno_1005.EPA_Site$DateTime.Local)
Month.Local <- lubridate::month(Reno_1005.EPA_Site$DateTime.Local, label = TRUE, abbr = FALSE)
Hour.Local <- format(Reno_1005.EPA_Site$DateTime.Local,"%H:%M")

Reno_1005.PM25.diurnal.byMonth <- data.frame(Year.Local,Month.Local,Hour.Local)
Reno_1005.PM25.diurnal.byMonth$DateTime.Local <- Reno_1005.EPA_Site$DateTime.Local
Reno_1005.PM25.diurnal.byMonth$PM2.5 <- Reno_1005.EPA_Site$Sample.Measurement

# Remove the rows that have invalid PM2.5 value
Reno_1005.PM25.diurnal.byMonth <- subset(Reno_1005.PM25.diurnal.byMonth, PM2.5 > 0)

#################################################
###### 1. Aggregated by year, month, hour #######
#################################################
Reno_1005.PM25.hourly_geoMean.byMonth <- aggregate(PM2.5 ~ Year.Local + Month.Local + Hour.Local,
                                                   data = Reno_1005.PM25.diurnal.byMonth,
                                                   FUN = geoMean)

year <- 2014
start.month <- 1
end.month <- 12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

Reno_1005.PM25.hourly_geoMean.byMonth %>% 
  na.omit %>%
  subset(Year.Local == year &
           Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 15), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local) +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", ", 
                 year, collapse = NULL))

#################################################
######### 2. Aggregated by month, hour ##########
#################################################
Reno_1005.PM25.hourly_Agg_MH.byMonth <- aggregate(PM2.5 ~ Month.Local + Hour.Local,
                                                  data = Reno_1005.PM25.diurnal.byMonth,
                                                  FUN = geoMean)
start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
Reno_1005.PM25.hourly_Agg_MH.byMonth %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 9), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)") +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  #labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\monthly trend_Reno_1005_geoMean.png")

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno_16
#--------------------------------------------------------------#
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 4"
Reno_16.EPA_Site <- subset(hourly_pm2.5_allYears,
                           Site.Num == 16 & 
                             State.Name == "Nevada" & 
                             County.Name == "Washoe")

# Create a new column as PST Local time 
# by joining the local date and time together
Reno_16.EPA_Site$DateTime.Local <- as.POSIXct(paste(Reno_16.EPA_Site$Date.Local, 
                                                    Reno_16.EPA_Site$Time.Local), 
                                              format = "%Y-%m-%d %H:%M")

# Plot the hourly-average PM2.5 data from the EPA site around CCNY (Aeronet site)

# TODO: 
# 1. Break the days as monthly-based so that later we can con_pmsolidate them into seasons
# 2. Plot the PM2.5 daily change trend 
# with 24-hour as X-axis and the hourly-based-average values as Y-axis

# Create a new df with year, month, 24-hour as the X-axis, hourly-based average of PM2.5 as the Y-axis
Year.Local <- lubridate::year(Reno_16.EPA_Site$DateTime.Local)
Month.Local <- lubridate::month(Reno_16.EPA_Site$DateTime.Local, label = TRUE, abbr = FALSE)
Hour.Local <- format(Reno_16.EPA_Site$DateTime.Local,"%H:%M")

Reno_16.PM25.diurnal.byMonth <- data.frame(Year.Local,Month.Local,Hour.Local)
Reno_16.PM25.diurnal.byMonth$DateTime.Local <- Reno_16.EPA_Site$DateTime.Local
Reno_16.PM25.diurnal.byMonth$PM2.5 <- Reno_16.EPA_Site$Sample.Measurement

# Remove the rows that have invalid PM2.5 value
Reno_16.PM25.diurnal.byMonth <- subset(Reno_16.PM25.diurnal.byMonth, PM2.5 > 0)

#################################################
###### 1. Aggregated by year, month, hour #######
#################################################
Reno_16.PM25.hourly_geoMean.byMonth <- aggregate(PM2.5 ~ Year.Local + Month.Local + Hour.Local,
                                                 data = Reno_16.PM25.diurnal.byMonth,
                                                 FUN = geoMean)

year <- 2014
start.month <- 1
end.month <- 12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

Reno_1005.PM25.hourly_geoMean.byMonth %>% 
  na.omit %>%
  subset(Year.Local == year &
           Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 15), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local) +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", ", 
                 year, collapse = NULL))

#################################################
######### 2. Aggregated by month, hour ##########
#################################################
Reno_16.PM25.hourly_Agg_MH.byMonth <- aggregate(PM2.5 ~ Month.Local + Hour.Local,
                                                data = Reno_16.PM25.diurnal.byMonth,
                                                FUN = geoMean)
start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
Reno_16.PM25.hourly_Agg_MH.byMonth %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 9), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)") +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  #labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\monthly trend_Reno_16_geoMean.png")

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Reno_16_1005
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# normalized? - Since we cannot compare them directly
#--------------------------------------------------------------#

memory.limit()
# might need to increase the memory limit in order to establish this dataset in R
memory.limit(size = 20000)
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 4"
Reno_16_1005.EPA_Site <- subset(hourly_pm2.5_allYears,
                                (Site.Num == 16 | Site.Num == 1005) & 
                                  State.Name == "Nevada" & 
                                  County.Name == "Washoe")

# Create a new column as PST Local time 
# by joining the local date and time together
Reno_16_1005.EPA_Site$DateTime.Local <- as.POSIXct(paste(Reno_16_1005.EPA_Site$Date.Local, 
                                                         Reno_16_1005.EPA_Site$Time.Local), 
                                                   format = "%Y-%m-%d %H:%M")

# Plot the hourly-average PM2.5 data from the EPA site around CCNY (Aeronet site)

# TODO: 
# 1. Break the days as monthly-based so that later we can con_pmsolidate them into seasons
# 2. Plot the PM2.5 daily change trend 
# with 24-hour as X-axis and the hourly-based-average values as Y-axis

# Create a new df with year, month, 24-hour as the X-axis, hourly-based average of PM2.5 as the Y-axis
Year.Local <- lubridate::year(Reno_16_1005.EPA_Site$DateTime.Local)
Month.Local <- lubridate::month(Reno_16_1005.EPA_Site$DateTime.Local, label = TRUE, abbr = FALSE)
Hour.Local <- format(Reno_16_1005.EPA_Site$DateTime.Local,"%H:%M")

Reno_16_1005.PM25.diurnal.byMonth <- data.frame(Year.Local,Month.Local,Hour.Local)
Reno_16_1005.PM25.diurnal.byMonth$DateTime.Local <- Reno_16_1005.EPA_Site$DateTime.Local
Reno_16_1005.PM25.diurnal.byMonth$PM2.5 <- Reno_16_1005.EPA_Site$Sample.Measurement
Reno_16_1005.PM25.diurnal.byMonth$Site.Num <- Reno_16_1005.EPA_Site$Site.Num

# Remove the rows that have invalid PM2.5 value
Reno_16_1005.PM25.diurnal.byMonth <- subset(Reno_16_1005.PM25.diurnal.byMonth, PM2.5 > 0)

Reno_16_1005.PM25.hourly_Agg_MH.byMonth <- aggregate(PM2.5 ~ Month.Local + Hour.Local + Site.Num,
                                                     data = Reno_16_1005.PM25.diurnal.byMonth,
                                                     FUN = geoMean)
start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
Reno_16_1005.PM25.hourly_Agg_MH.byMonth %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Site.Num, color = as.character(Site.Num))) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 9), 
              size = 1,
              se = FALSE,
              aes(group = Site.Num)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Site Number") +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  #labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\monthly trend_Reno_16_1005_geoMean.png")
