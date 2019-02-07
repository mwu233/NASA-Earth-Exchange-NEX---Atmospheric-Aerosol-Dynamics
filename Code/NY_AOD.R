
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
#--------------------------------------------------------------#
# Area: CCNY
#--------------------------------------------------------------#
################################################################
#--------------------------------------------------------------#
# Hourly Aeronet AOD Data of CCNY
#--------------------------------------------------------------#
# set the working directory
setwd("~/UW_Study_Work/2018Summer/NASA/Atmospheric Aerosol Dynamics")

# Import the data 
system.time(NY_Aeronet_AOD <- subset(aeronet_allSites_2014_2017, AERONET_Site == "CCNY"))

# Join date and time into a new column "DateTime.GMT"
NY_Aeronet_AOD$DateTime.GMT <- ymd_hms(as.POSIXct(paste(NY_Aeronet_AOD$Date.GMT,
                                                        NY_Aeronet_AOD$Time.GMT),
                                                  format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")

# converting to local time zone "America/New_York" and round the time by hour
NY_Aeronet_AOD$DateTime.Local <- with_tz(NY_Aeronet_AOD$DateTime.GMT, tz = "America/New_York")
NY_Aeronet_AOD$DateTime.Local <- round_date(NY_Aeronet_AOD$DateTime.Local, unit = "hour")

start.date <- "2014-01-01 00:00:00"
end.date <- "2017-12-31 23:59:59"

plot.pointChart.AOD(NY_Aeronet_AOD,
                    start.date,
                    end.date)

NY_Aeronet_AOD.plot.pointChart.AOD <- plot.pointChart.AOD(NY_Aeronet_AOD,
                                                          start.date,
                                                          end.date)
NY_Aeronet_AOD.plot.pointChart.AOD

# Create a new df with year, month, 24-hour as the X-axis, 
# hourly-based average of AOD as the Y-axis
Year.Local <- lubridate::year(NY_Aeronet_AOD$DateTime.Local)
Month.Local <- lubridate::month(NY_Aeronet_AOD$DateTime.Local, label = TRUE, abbr = FALSE)
Hour.Local <- format(NY_Aeronet_AOD$DateTime.Local, "%H:%M")

NY_Aeronet_AOD.diurnal.byMonth <- 
  data.frame(Year.Local,
             Month.Local,
             Hour.Local)
NY_Aeronet_AOD.diurnal.byMonth$AOD <- NY_Aeronet_AOD$AOD_500nm
NY_Aeronet_AOD.diurnal.byMonth$DateTime.Local <- NY_Aeronet_AOD$DateTime.Local

# Remove the rows that have invalid AOD value
NY_Aeronet_AOD.diurnal.byMonth <- subset(NY_Aeronet_AOD.diurnal.byMonth, AOD > 0)

# write it out to Meytar
# write.csv(NY_Aeronet_AOD.diurnal.byMonth,file = "Data/Aeronet/All_Sites_Times_All_Points_AOD20/NY_Aeronet_AOD_diurnal_byMonth_2014_2017.csv", row.names = FALSE)

#################################################
###### 1. Aggregated by year, month, hour #######
#################################################
NY_Aeronet_AOD.hourly_geoMean.byMonth <- aggregate(AOD ~ Year.Local + Month.Local + Hour.Local,
                                                   data = NY_Aeronet_AOD.diurnal.byMonth,
                                                   FUN = geoMean)
year <- 2017
start.month <- 6
end.month <- 8

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for one year
NY.plot.AOD.hourly_geoMean.byMonth <- plot.AOD.diurnal.byMonth(NY_Aeronet_AOD.hourly_geoMean.byMonth,
                                                               year,
                                                               start.month,
                                                               end.month)
NY.plot.AOD.hourly_geoMean.byMonth

#################################################
######### 2. Aggregated by month, hour ##########
#################################################
NY_Aeronet_AOD.hourly_Agg_MH.byMonth <- aggregate(AOD ~ Month.Local + Hour.Local,
                                                  data = NY_Aeronet_AOD.diurnal.byMonth,
                                                  FUN = geoMean)
start.month <- 6
end.month <-8

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
NY.plot.AOD.hourly_Agg_MH.byMonth <- plot.AOD.diurnal.allyears.byMonth(NY_Aeronet_AOD.hourly_Agg_MH.byMonth,
                                                                       start.month,
                                                                       end.month)
NY.plot.AOD.hourly_Agg_MH.byMonth

#################################################
######### 3. Aggregated by year, hour ###########
#################################################
NY_Aeronet_AOD.hourly_Agg_YH.byYear <- aggregate(AOD ~ Year.Local + Hour.Local,
                                                 data = NY_Aeronet_AOD.diurnal.byMonth,
                                                 FUN = geoMean)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for one year
NY.plot.AOD.hourly.byYear <- plot.AOD.diurnal.byYear(NY_Aeronet_AOD.hourly_Agg_YH.byYear)
NY.plot.AOD.hourly.byYear

#################################################
###### 4.1 Aggregated by year, season, hour #####
###### Compare four seasons of one specified year
#################################################

# Add the season column and update the season value based on the DateTime column
NY_Aeronet_AOD.diurnal.byMonth$Season.Local <- 
  lubridate::quarter(NY_Aeronet_AOD.diurnal.byMonth$DateTime.Local, with_year = TRUE, fiscal_start = 3)

NY_Aeronet_AOD.diurnal.byMonth$Season.Local2 <-
  lubridate::quarter(NY_Aeronet_AOD.diurnal.byMonth$DateTime.Local, with_year = FALSE, fiscal_start = 3)

NY_Aeronet_AOD.hourly_Agg_YSH.bySeason <- aggregate(AOD ~ Season.Local + Hour.Local,
                                                    data = NY_Aeronet_AOD.diurnal.byMonth,
                                                    FUN = geoMean)

# Specify the year
year <- 2015

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for one year
NY.plot.AOD.diurnal.bySeason <- plot.AOD.diurnal.bySeason(NY_Aeronet_AOD.hourly_Agg_YSH.bySeason, year)
NY.plot.AOD.diurnal.bySeason

#################################################
###### 4.2 Aggregated by year, season, hour #####
###### Compare one specified season of four years
#################################################

NY_Aeronet_AOD.hourly_Agg_YSH2.bySeason <- aggregate(AOD ~ Year.Local + Season.Local2 + Hour.Local,
                                                     data = NY_Aeronet_AOD.diurnal.byMonth,
                                                     FUN = geoMean)

# Specify the season:
# 1 - Spring (Mar - May)
# 2 - Summer (Jun - Aug)
# 3 - Fall (Sep - Nov)
# 4 - Winter (Dec - Feb) # will it be a problem?
quarter <- 2

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for four years
NY.plot.AOD.diurnal.bySeason2 <- plot.AOD.diurnal.bySeason2(NY_Aeronet_AOD.hourly_Agg_YSH2.bySeason, quarter)
NY.plot.AOD.diurnal.bySeason2

#################################################
######### 5. Aggregated by season, hour #########
#################################################
NY_Aeronet_AOD.hourly_Agg_YSH.allyears.bySeason <- 
  aggregate(AOD ~ Season.Local2 + Hour.Local,
            data = NY_Aeronet_AOD.diurnal.byMonth,
            FUN = geoMean)

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for four years
NY.plot.AOD.diurnal.allyears.bySeason <- 
  plot.AOD.diurnal.allyears.bySeason(NY_Aeronet_AOD.hourly_Agg_YSH.allyears.bySeason)
NY.plot.AOD.diurnal.allyears.bySeason

#################################################
############ 6. Aggregated by hour #############
#################################################
NY_Aeronet_AOD.hourly.byHour <- 
  aggregate(AOD ~ Hour.Local,
            data = NY_Aeronet_AOD.diurnal.byMonth,
            FUN = geoMean)

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for four years
NY.plot.AOD.diurnal.allyears.byHour <- 
  plot.AOD.diurnal.allyears.byHour(NY_Aeronet_AOD.hourly.byHour)
NY.plot.AOD.diurnal.allyears.byHour