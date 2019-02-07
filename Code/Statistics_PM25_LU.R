
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

# Statistical relationship between Land Use and PM2.5

################################################################
################################################################

EPA_Sites <- read.csv("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\Data\\EPA_Sites\\aqs_sites\\aqs_sites.csv", header = TRUE)

PM_LU = hourly_pm2.5_location_allYears %>%
  left_join(EPA_Sites, 
            by = c("Latitude" = "Latitude", 
                   "Longitude" = "Longitude"))

PM_LU <- subset(PM_LU, Land.Use != "")

#--------------------------------------------------------------#
# select daytime out
#--------------------------------------------------------------#
hourly_pm2.5_allYears_daytime <- subset(hourly_pm2.5_allYears, 
                                        Time.Local >= "06:00" &
                                          Time.Local <= "18:00")

hourly_pm2.5_allYears_daytime_Agg <- aggregate(Sample.Measurement ~ Latitude + Longitude + Site.Num + County.Name + State.Name,
                                               data = hourly_pm2.5_allYears_daytime,
                                               FUN = mean)

PM_LU = PM_LU %>%
  left_join(hourly_pm2.5_allYears_daytime_Agg, 
            by = c("Latitude" = "Latitude", 
                   "Longitude" = "Longitude",
                   "Site.Num" = "Site.Num",
                   "County.Name.x" = "County.Name" , 
                   "State.Name.x" = "State.Name"))

chisq.test(PM_LU$Sample.Measurement, 
           PM_LU$Land.Use, 
           rescale.p = TRUE,
           simulate.p.value = TRUE)

# We have a chi-squared value of 4776. 
# Since we get a p-Value less than the significance level of 0.05 (0.0004997501), 
# we reject the null hypothesis and conclude that the two variables are dependent.

#--------------------------------------------------------------#
# select nighttime out
#--------------------------------------------------------------#
memory.limit()
# might need to increase the memory limit in order to establish this dataset in R
memory.limit(size = 20000)

hourly_pm2.5_allYears_nighttime <- subset(hourly_pm2.5_allYears, 
                                          Time.Local > "18:00" |
                                            Time.Local < "06:00")

hourly_pm2.5_allYears_nighttime_Agg <- aggregate(Sample.Measurement ~ Latitude + Longitude + Site.Num + County.Name + State.Name,
                                                 data = hourly_pm2.5_allYears_nighttime,
                                                 FUN = mean)

colnames(hourly_pm2.5_allYears_nighttime_Agg)[colnames(hourly_pm2.5_allYears_nighttime_Agg)=="Sample.Measurement"] <- "Sample.Measurement_nighttime"

PM_LU = PM_LU %>%
  left_join(hourly_pm2.5_allYears_nighttime_Agg, 
            by = c("Latitude" = "Latitude", 
                   "Longitude" = "Longitude",
                   "Site.Num" = "Site.Num",
                   "County.Name.x" = "County.Name" , 
                   "State.Name.x" = "State.Name"))

chisq.test(PM_LU$Sample.Measurement_nighttime, 
           PM_LU$Land.Use, 
           rescale.p = TRUE,
           simulate.p.value = TRUE)

# We have a chi-squared value of 4776. 
# Since we get a p-Value less than the significance level of 0.05 (0.0004997501), 
# we reject the null hypothesis and conclude that the two variables are dependent.

#--------------------------------------------------------------#
# select Summer out
#--------------------------------------------------------------#

hourly_pm2.5_allYears_summer <- subset(hourly_pm2.5_allYears,
                                       (Date.Local > "2014-06-22" & Date.Local < "2014-09-23") | 
                                         (Date.Local > "2015-06-22" & Date.Local < "2015-09-23") |
                                         (Date.Local > "2016-06-22" & Date.Local < "2016-09-23") |
                                         (Date.Local > "2017-06-22" & Date.Local < "2017-09-23"))

hourly_pm2.5_allYears_summer_Agg <- aggregate(Sample.Measurement ~ Latitude + Longitude + Site.Num + County.Name + State.Name,
                                              data = hourly_pm2.5_allYears_summer,
                                              FUN = mean)

colnames(hourly_pm2.5_allYears_summer_Agg)[colnames(hourly_pm2.5_allYears_summer_Agg)=="Sample.Measurement"] <- "Sample.Measurement_summer"

PM_LU = PM_LU %>%
  left_join(hourly_pm2.5_allYears_summer_Agg, 
            by = c("Latitude" = "Latitude", 
                   "Longitude" = "Longitude",
                   "Site.Num" = "Site.Num",
                   "County.Name.x" = "County.Name" , 
                   "State.Name.x" = "State.Name"))

chisq.test(PM_LU$Sample.Measurement_summer, 
           PM_LU$Land.Use, 
           rescale.p = TRUE,
           simulate.p.value = TRUE)

# We have a chi-squared value of 4472. 
# Since we get a p-Value less than the significance level of 0.05 (0.0004997501), 
# we reject the null hypothesis and conclude that the two variables are dependent.
