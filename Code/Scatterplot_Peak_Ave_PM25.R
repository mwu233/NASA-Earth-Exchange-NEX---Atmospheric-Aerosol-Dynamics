
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
# Scatterplot of the peak and average values in NY
#--------------------------------------------------------------#
NY.EPA_Site_peak <- aggregate(Sample.Measurement ~ Date.Local,
                              data = NY.EPA_Site,
                              FUN = max)

NY.EPA_Site_average <- aggregate(Sample.Measurement ~ Date.Local,
                                 data = NY.EPA_Site,
                                 FUN = mean)

NY.EPA_Site_peak_average = NY.EPA_Site_peak %>%
  left_join(NY.EPA_Site_average, 
            by = c("Date.Local" = "Date.Local"))

# rename column names
names(NY.EPA_Site_peak_average)[1]<-paste("Date")
names(NY.EPA_Site_peak_average)[2]<-paste("Peak")
names(NY.EPA_Site_peak_average)[3]<-paste("Average") 

NY.EPA_Site_peak_average$Ratio = as.numeric(NY.EPA_Site_peak_average$Peak / NY.EPA_Site_peak_average$Average)

# Overall four years scatterplot
NY.EPA_Site_peak_average %>%
  ggplot(aes(x = Average, y = Peak)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.8, linetype = 1, span = 0.4) +
  # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
  labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter", color = "Site") +
  ggtitle(paste0("Scatterplot of Daily Average and Peak Values of PM2.5 in New York, 2014-2017",
                 collapse = NULL))

# Correlation Coefficient
cor(NY.EPA_Site_peak_average$Peak,NY.EPA_Site_peak_average$Average)
# 0.6602249

# Separate the date into seasons
NY.EPA_Site_peak_average$Season.numeric <- 
  lubridate::quarter(NY.EPA_Site_peak_average$Date, 
                     with_year = FALSE, 
                     fiscal_start = 3)

NY.EPA_Site_peak_average$Season <- 
  cut(NY.EPA_Site_peak_average$Season.numeric, 
      breaks = 4, 
      labels = c("1. Spring","2. Summer","3. Fall","4. Winter"))

PM2.5_peak_average(subset(NY.EPA_Site_peak_average, Peak < 100))

PM2.5_peak_average_bySeason(subset(NY.EPA_Site_peak_average, Peak < 100))

NY.EPA_Site_peak_average$Year <- lubridate::year(NY.EPA_Site_peak_average$Date)
PM2.5_ratio_daily_bySeason(subset(NY.EPA_Site_peak_average, Average > 0 & Peak > 35.4 & Ratio > 1.5))

#--------------------------------------------------------------#
# Scatterplot of the peak and average values in Baltimore
#--------------------------------------------------------------#
Baltimore.EPA_Site_peak <- aggregate(Sample.Measurement ~ Date.Local,
                                     data = Baltimore.EPA_Site,
                                     FUN = max)

Baltimore.EPA_Site_average <- aggregate(Sample.Measurement ~ Date.Local,
                                        data = Baltimore.EPA_Site,
                                        FUN = mean)

Baltimore.EPA_Site_peak_average = Baltimore.EPA_Site_peak %>%
  left_join(Baltimore.EPA_Site_average, 
            by = c("Date.Local" = "Date.Local"))

# rename column names
names(Baltimore.EPA_Site_peak_average)[1]<-paste("Date")
names(Baltimore.EPA_Site_peak_average)[2]<-paste("Peak")
names(Baltimore.EPA_Site_peak_average)[3]<-paste("Average") 

# Overall four years scatterplot
Baltimore.EPA_Site_peak_average %>%
  ggplot(aes(x = Average, y = Peak)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
  # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
  labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter", color = "Site") +
  ggtitle(paste0("Scatterplot of Daily Average and Peak Values of PM2.5 in Baltimore, 2014-2017",
                 collapse = NULL))

# Correlation Coefficient
cor(Baltimore.EPA_Site_peak_average$Peak,
    Baltimore.EPA_Site_peak_average$Average)
# 0.8496859

# Separate the date into seasons
Baltimore.EPA_Site_peak_average$Season.numeric <- 
  lubridate::quarter(Baltimore.EPA_Site_peak_average$Date, 
                     with_year = FALSE, 
                     fiscal_start = 3)

Baltimore.EPA_Site_peak_average$Season <- 
  cut(Baltimore.EPA_Site_peak_average$Season.numeric, 
      breaks = 4, 
      labels = c("1. Spring","2. Summer","3. Fall","4. Winter"))

PM2.5_peak_average_bySeason(subset(Baltimore.EPA_Site_peak_average, Peak < 100))

Baltimore.EPA_Site_peak_average$Ratio <- as.numeric(Baltimore.EPA_Site_peak_average$Peak / Baltimore.EPA_Site_peak_average$Average)
Baltimore.EPA_Site_peak_average$Year <- lubridate::year(Baltimore.EPA_Site_peak_average$Date)
PM2.5_ratio_daily_bySeason(subset(Baltimore.EPA_Site_peak_average, Average > 0 & Peak > 35.4 & Ratio > 1.5))

#--------------------------------------------------------------#
# Scatterplot of the peak and average values in Reno
#--------------------------------------------------------------#
Reno_16.EPA_Site_peak <- aggregate(Sample.Measurement ~ Date.Local,
                                   data = Reno_16.EPA_Site,
                                   FUN = max)

Reno_16.EPA_Site_average <- aggregate(Sample.Measurement ~ Date.Local,
                                      data = Reno_16.EPA_Site,
                                      FUN = mean)

Reno_16.EPA_Site_peak_average = Reno_16.EPA_Site_peak %>%
  left_join(Reno_16.EPA_Site_average, 
            by = c("Date.Local" = "Date.Local"))

# rename column names
names(Reno_16.EPA_Site_peak_average)[1]<-paste("Date")
names(Reno_16.EPA_Site_peak_average)[2]<-paste("Peak")
names(Reno_16.EPA_Site_peak_average)[3]<-paste("Average") 

# Overall four years scatterplot
Reno_16.EPA_Site_peak_average %>%
  ggplot(aes(x = Average, y = Peak)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
  # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
  labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter", color = "Site") +
  ggtitle(paste0("Scatterplot of Daily Average and Peak Values of PM2.5 in Reno, 2014-2017",
                 collapse = NULL))

# Correlation Coefficient
cor(Reno_16.EPA_Site_peak_average$Peak,
    Reno_16.EPA_Site_peak_average$Average)
# 0.6602249

# Separate the date into seasons
Reno_16.EPA_Site_peak_average$Season.numeric <- 
  lubridate::quarter(Reno_16.EPA_Site_peak_average$Date, 
                     with_year = FALSE, 
                     fiscal_start = 3)

Reno_16.EPA_Site_peak_average$Season <- 
  cut(Reno_16.EPA_Site_peak_average$Season.numeric, 
      breaks = 4, 
      labels = c("1. Spring","2. Summer","3. Fall","4. Winter"))

PM2.5_peak_average_bySeason(subset(Reno_16.EPA_Site_peak_average, Peak < 100))

Reno_16.EPA_Site_peak_average$Ratio <- as.numeric(Reno_16.EPA_Site_peak_average$Peak / Reno_16.EPA_Site_peak_average$Average)
Reno_16.EPA_Site_peak_average$Year <- lubridate::year(Reno_16.EPA_Site_peak_average$Date)
PM2.5_ratio_daily_bySeason(subset(Reno_16.EPA_Site_peak_average, Average > 0 & Peak > 35.4 & Ratio > 1.5))


#--------------------------------------------------------------#
# Scatterplot of the peak and average values in Denver
#--------------------------------------------------------------#
Denver.EPA_Site_peak <- aggregate(Sample.Measurement ~ Date.Local,
                                  data = Denver.EPA_Site,
                                  FUN = max)

Denver.EPA_Site_average <- aggregate(Sample.Measurement ~ Date.Local,
                                     data = Denver.EPA_Site,
                                     FUN = mean)

Denver.EPA_Site_peak_average = Denver.EPA_Site_peak %>%
  left_join(Denver.EPA_Site_average, 
            by = c("Date.Local" = "Date.Local"))

# rename column names
names(Denver.EPA_Site_peak_average)[1]<-paste("Date")
names(Denver.EPA_Site_peak_average)[2]<-paste("Peak")
names(Denver.EPA_Site_peak_average)[3]<-paste("Average") 

# Overall four years scatterplot
Denver.EPA_Site_peak_average %>%
  ggplot(aes(x = Average, y = Peak)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
  # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
  labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter", color = "Site") +
  ggtitle(paste0("Scatterplot of Daily Average and Peak Values of PM2.5 in New York, 2014-2017",
                 collapse = NULL))

# Correlation Coefficient
cor(Denver.EPA_Site_peak_average$Peak,
    Denver.EPA_Site_peak_average$Average)
# 0.8597218

# Separate the date into seasons
Denver.EPA_Site_peak_average$Season.numeric <- 
  lubridate::quarter(Denver.EPA_Site_peak_average$Date, 
                     with_year = FALSE, 
                     fiscal_start = 3)

Denver.EPA_Site_peak_average$Season <- 
  cut(Denver.EPA_Site_peak_average$Season.numeric, 
      breaks = 4, 
      labels = c("1. Spring","2. Summer","3. Fall","4. Winter"))

PM2.5_peak_average_bySeason(subset(Denver.EPA_Site_peak_average, Peak < 100))

Denver.EPA_Site_peak_average$Ratio <- as.numeric(Denver.EPA_Site_peak_average$Peak / Denver.EPA_Site_peak_average$Average)
Denver.EPA_Site_peak_average$Year <- lubridate::year(Denver.EPA_Site_peak_average$Date)
PM2.5_ratio_daily_bySeason(subset(Denver.EPA_Site_peak_average, Average > 0 & Peak > 35.4 & Ratio > 1.5))

