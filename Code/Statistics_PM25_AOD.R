
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

################################################################
################################################################

# Statistical relationship between AOD and PM2.5

################################################################
################################################################

#--------------------------------------------------------------#
# Four sites
# Overall
# Create the joint table
AOD_PM_Four <- merge(Four.AOD.diurnal.byMonth, 
                     Four.PM25.diurnal.byMonth,
                     by = c("Site", 
                            "Year.Local",
                            "Month.Local",
                            "Date.Local",
                            "Hour.Local"))

AOD_PM_Four$Season.Local2 <-
  lubridate::quarter(AOD_PM_Four$DateTime.Local.x, with_year = FALSE, fiscal_start = 3)

AOD_PM_Four$Season.Local <- cut(AOD_PM_Four$Season.Local2, 
                                breaks = 4, 
                                labels = c("1. Spring","2. Summer","3. Fall","4. Winter"))

CC_AOD_PM25_allseasons(AOD_PM_Four)
# [1] 0.3952645
# If the Pearson correlation coefficient is close to 1, 
# it would indicate that the variables are 
# positively linearly related and 
# the scatter plot falls almost along a 
# straight line with positive slope.

# Spatial differences of the relationship 
AOD_PM_Four %>%
  ggplot(aes(x = AOD, y = PM2.5, group = Site, color = Site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
  # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
  labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter", color = "Site") +
  ggtitle(paste0("Scatterplot of AOD and PM2.5 by Site",
                 collapse = NULL))

# Temporal differences of the relationship 
AOD_PM_Four %>%
  ggplot(aes(x = AOD, y = PM2.5, group = Season.Local, color = Season.Local)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
  # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
  labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter", color = "Season") +
  ggtitle(paste0("Scatterplot of AOD and PM2.5 by Season",
                 collapse = NULL))

#--------------------------------------------------------------#
# New York
# Overall
AOD_PM_NY <- subset(AOD_PM_Four, 
                    Site == "New York")

# Simple Scatterplot of NY
plot_AOD_PM25_allseasons(AOD_PM_NY)
CC_AOD_PM25_allseasons(AOD_PM_NY)
# 0.4401979

plot_AOD_PM25_OneSite_bySeason_noFacet(AOD_PM_NY)

# Temporal differences of the relationship 
# By Season
plot_AOD_PM25_OneSite_bySeason(AOD_PM_NY)

# By 3-h time block + Season
timeBlock <- "4-6 a.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_NY, 
                                                Hour.Local.y == "04:00" |
                                                  Hour.Local.y == "05:00" | 
                                                  Hour.Local.y == "06:00"), timeBlock)
VCtimeBlock <- c("04:00","05:00","06:00")

plot_AOD_PM25_OneSite_TimeBlock_BySeason(AOD_PM_NY[AOD_PM_NY$Hour.Local.y %in% VCtimeBlock,], 
                                         VCtimeBlock)

timeBlock <- "7-9 a.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_NY, 
                                                Hour.Local.y == "07:00" |
                                                  Hour.Local.y == "08:00" | 
                                                  Hour.Local.y == "09:00"), timeBlock)

timeBlock <- "10-12 a.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_NY, 
                                                Hour.Local.y == "10:00" |
                                                  Hour.Local.y == "11:00" | 
                                                  Hour.Local.y == "12:00"), timeBlock)

timeBlock <- "1-3 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_NY, 
                                                Hour.Local.y == "13:00" |
                                                  Hour.Local.y == "14:00" | 
                                                  Hour.Local.y == "15:00"), timeBlock)

timeBlock <- "4-6 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_NY, 
                                                Hour.Local.y == "16:00" |
                                                  Hour.Local.y == "17:00" | 
                                                  Hour.Local.y == "18:00"), timeBlock)

timeBlock <- "7-9 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_NY, 
                                                Hour.Local.y == "19:00" |
                                                  Hour.Local.y == "20:00" | 
                                                  Hour.Local.y == "21:00"), timeBlock)

#--------------------------------------------------------------#
# Baltimore 
# Overall
AOD_PM_Ba <- subset(AOD_PM_Four, 
                    Site == "Baltimore")

# Simple Scatterplot of Baltimore
plot_AOD_PM25_allseasons(AOD_PM_Ba)
CC_AOD_PM25_allseasons(AOD_PM_Ba)
# 0.2800043

# Temporal differences of the relationship 
# By Season
plot_AOD_PM25_OneSite_bySeason(AOD_PM_Ba)

# By 3-h time block + Season
# Change it to Hour.Local.y
timeBlock <- "7-10 a.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Ba, 
                                                Hour.Local.y == "04:00" |
                                                  Hour.Local.y == "05:00" | 
                                                  Hour.Local.y == "06:00" |
                                                  Hour.Local.y == "07:00"), timeBlock)

timeBlock <- "11 a.m. - 1 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Ba, 
                                                Hour.Local.y == "08:00" |
                                                  Hour.Local.y == "09:00" | 
                                                  Hour.Local.y == "10:00"), timeBlock)

timeBlock <- "2-4 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Ba, 
                                                Hour.Local.y == "11:00" |
                                                  Hour.Local.y == "12:00" | 
                                                  Hour.Local.y == "13:00"), timeBlock)

timeBlock <- "5-8 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Ba, 
                                                Hour.Local.y == "14:00" |
                                                  Hour.Local.y == "15:00" | 
                                                  Hour.Local.y == "16:00" |
                                                  Hour.Local.y == "17:00" ), timeBlock)

#--------------------------------------------------------------#
# Reno 
# Overall
AOD_PM_Reno <- subset(AOD_PM_Four, 
                      Site == "UN-Reno")

# Simple Scatterplot of Reno
plot_AOD_PM25_allseasons(AOD_PM_Reno)
CC_AOD_PM25_allseasons(AOD_PM_Reno)
# 0.4199023

# Spring
plot_AOD_PM25_season(AOD_PM_Reno,1)
CC_AOD_PM25_season(AOD_PM_Reno,1)
# 0.1476386

# Summer
plot_AOD_PM25_season(AOD_PM_Reno,2)
CC_AOD_PM25_season(AOD_PM_Reno,2)
# 0.5689221

# Fall
plot_AOD_PM25_season(AOD_PM_Reno,3)
CC_AOD_PM25_season(AOD_PM_Reno,3)
# 0.3871701

# Winter
plot_AOD_PM25_season(AOD_PM_Reno,4)
CC_AOD_PM25_season(AOD_PM_Reno,4)
# 0.3017616

# Temporal differences of the relationship 
# By Season
plot_AOD_PM25_OneSite_bySeason(AOD_PM_Reno)

# By 3-h time block + Season
timeBlock <- "6-9 a.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Reno, 
                                                Hour.Local.y == "06:00" |
                                                  Hour.Local.y == "07:00" |
                                                  Hour.Local.y == "08:00" | 
                                                  Hour.Local.y == "09:00"), timeBlock)

timeBlock <- "10 a.m. - 1 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Reno, 
                                                Hour.Local.y == "10:00" |
                                                  Hour.Local.y == "11:00" | 
                                                  Hour.Local.y == "12:00" | 
                                                  Hour.Local.y == "13:00"), timeBlock)

timeBlock <- "2-5 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Reno, 
                                                Hour.Local.y == "14:00" |
                                                  Hour.Local.y == "15:00" | 
                                                  Hour.Local.y == "16:00" | 
                                                  Hour.Local.y == "17:00"), timeBlock)

timeBlock <- "6-8 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Reno, 
                                                Hour.Local.y == "20:00" |
                                                  Hour.Local.y == "19:00" | 
                                                  Hour.Local.y == "18:00"), timeBlock)

#--------------------------------------------------------------#
# St. Louis
# Overall
AOD_PM_StLouis <- subset(AOD_PM_Four, 
                        Site == "St. Louis")
# Simple Scatterplot of Denver
plot_AOD_PM25_allseasons(AOD_PM_Denver)
CC_AOD_PM25_allseasons(AOD_PM_Denver)
# 0.6499737

# Spring - Denver only has Spring data
plot_AOD_PM25_season(AOD_PM_Denver,1)
CC_AOD_PM25_season(AOD_PM_Denver,1)
# 0.6499737

# Temporal differences of the relationship 
plot_AOD_PM25_OneSite_bySeason(AOD_PM_Denver)
# By 3-h time block + Season
timeBlock <- "6-8 a.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Denver, 
                                                Hour.Local.y == "06:00" |
                                                  Hour.Local.y == "07:00" |
                                                  Hour.Local.y == "08:00"), timeBlock)

timeBlock <- "9-11 a.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Denver, 
                                                Hour.Local.y == "10:00" |
                                                  Hour.Local.y == "11:00" | 
                                                  Hour.Local.y == "09:00"), timeBlock)

timeBlock <- "12-2 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Denver, 
                                                Hour.Local.y == "14:00" |
                                                  Hour.Local.y == "13:00" | 
                                                  Hour.Local.y == "12:00"), timeBlock)

timeBlock <- "3-5 p.m."
plot_AOD_PM25_OneSite_TimeBlock_BySeason(subset(AOD_PM_Denver, 
                                                Hour.Local.y == "15:00" |
                                                  Hour.Local.y == "16:00" | 
                                                  Hour.Local.y == "17:00"), timeBlock)
