
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
install.packages("lutz")

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
library(lutz)

#--------------------------------------------------------------
# Store the PM2.5 data into dataframes
#--------------------------------------------------------------
# Set the working directory
setwd("~/UW_Study_Work/2018Summer/NASA/Atmospheric Aerosol Dynamics")

# Read PM2.5 data from downloaded csv files for each year in 2014-2017
hourly_pm2.5_2014 <- read.csv("Data/PM2.5/hourly_88101_2014/hourly_88101_2014.csv", stringsAsFactors = FALSE)
hourly_pm2.5_2015 <- read.csv("Data/PM2.5/hourly_88101_2015/hourly_88101_2015.csv", stringsAsFactors = FALSE)
hourly_pm2.5_2016 <- read.csv("Data/PM2.5/hourly_88101_2016/hourly_88101_2016.csv", stringsAsFactors = FALSE)
hourly_pm2.5_2017 <- read.csv("Data/PM2.5/hourly_88101_2017/hourly_88101_2017.csv", stringsAsFactors = FALSE)

# Combine all PM2.5 data from 2014 to 2017 into one dataframe
hourly_pm2.5_allYears <- rbind(hourly_pm2.5_2014, hourly_pm2.5_2015)
hourly_pm2.5_allYears <- rbind(hourly_pm2.5_allYears, hourly_pm2.5_2016)
hourly_pm2.5_allYears <- rbind(hourly_pm2.5_allYears, hourly_pm2.5_2017)

#--------------------------------------------------------------
# con_pmvert Date columns to date objects
#--------------------------------------------------------------
hourly_pm2.5_allYears$Date.Local <- as.Date(hourly_pm2.5_allYears$Date.Local, format = "%F")
hourly_pm2.5_allYears$Date.GMT <- as.Date(hourly_pm2.5_allYears$Date.GMT, format = "%F")

#--------------------------------------------------------------
# Join date and time together as single column
#--------------------------------------------------------------
hourly_pm2.5_allYears$DateTime.GMT <- as.POSIXct(paste(hourly_pm2.5_allYears$Date.GMT, 
                                                       hourly_pm2.5_allYears$Time.GMT), 
                                                 format = "%Y-%m-%d %H:%M")

#--------------------------------------------------------------#
# Selet all EPA site locations
#--------------------------------------------------------------#
# Create pm2.5 all sites
hourly_pm2.5_location_allYears <- unique(select(subset(hourly_pm2.5_allYears), 
                                                c(Longitude, 
                                                  Latitude, 
                                                  Site.Num, 
                                                  State.Name, 
                                                  County.Name)))

# mapping EPA sites, 2014 - 2017
# leaflet(hourly_pm2.5_location_allYears) %>%
#   addCircles(~Longitude, ~Latitude,
#              label = ~paste("Site Num: ", Site.Num, ";",
#                             "State Name: ", State.Name, ";",
#                             "County Name: ", County.Name)) %>%
#   addTiles() %>%
#   addProviderTiles(providers$CartoDB.Positron)

#--------------------------------------------------------------#
# Aeronet data: all points and all sites (24G)
#--------------------------------------------------------------#
# Set the working directory
setwd("~/UW_Study_Work/2018Summer/NASA/Atmospheric Aerosol Dynamics")

### Create a connection between R and PostgreSQL ###
# Save the password that we can "hide" it as best as we can by collapsing it
pw <- "postgres"

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# note that "con_aeronet" will be used later 
# in each connection to the database "Aeronet"
con_aeronet <- dbConnect(drv, dbname = "Aeronet",
                         host = "localhost", port = 5432,
                         user = "postgres", password = pw)

# removes the password
rm(pw) 

# Set the all-sites aeronet .dat file path
aeronet.file <- file.path("Data/Aeronet/All_Sites_Times_All_Points_AOD20/All_Sites_Times_All_Points_AOD20.dat")

# Check metadata at the beginning of the .dat file
top <- readLines(aeronet.file, n = 8)
# It is comma delimited, and the headers are on line 7
top

# skip the first 6 rows (metadata)
# now we guess the column types by reading in a small fraction of the rows
guess <- read.csv(aeronet.file, stringsAsFactors=FALSE, nrows=1000, skip = 6)

# Use this guess to set up the dbDataType
create <- sprintf("CREATE TABLE aeronet ( %s )", 
                  paste0(sprintf('"%s" %s', colnames(guess), 
                                 sapply(guess, dbDataType, dbObj=con_aeronet)), collapse=","))

# From guess we can see which are interesting fields
names(guess)

# check the fields that might have AOD data
aodnames <- grep("AOD_[0-9]{3,4}nm", names(guess), value = TRUE)
# return the number of fields that have AOD data
length(aodnames)

# and how many non-missing do we have per row (in this first sample of data)
table(apply(guess[, aodnames], 1, function(x) sum(x != -999)))

## pulling all of the data into PostgreSQL ####
# we build the table creation dynamically from what we've learned from guessing
system.time(invisible(dbSendQuery(con_aeronet, create)))

# Failed: permission denied because of the user setting
# output.file <- file.path("Data/Aeronet/All_Sites_Times_All_Points_AOD20/output_withHeader.csv")
# ## pulling all of the data (.dat) into PostgreSQL ##
# system.time(invisible(dbSendQuery(con_aeronet, paste0("COPY aeronet 
#                                                       FROM '",
#                                                       output.file,
#                                                       "' USING DELIMITERS ',' CSV HEADER;"))))

## Solution: 
## use psql's \copy in terminal to surpass the user limitation as client-side
#--------------------------------------------------------------#
# Import .dat file as a table to the PostgreSQL database 
# i.e., Aeronet 
# Finished by Java programming and cmd - psql '\copy'
#--------------------------------------------------------------#
#--------------------------------------------------------------#
# Aeronet data (V3 L2.0): hourly for all sites
#--------------------------------------------------------------#

# Change date format of Date.GMT. Corrects to "yyyy-mm-dd"
dbSendQuery(con_aeronet, 'ALTER TABLE aeronet RENAME COLUMN "Date.dd.mm.yyyy." TO "Date.GMT";')
dbSendQuery(con_aeronet, 'ALTER TABLE aeronet RENAME COLUMN "Time.hh.mm.ss." TO "Time.GMT";')

# Rename lat/lon columns
dbSendQuery(con_aeronet, 'ALTER TABLE aeronet RENAME COLUMN "Site_Longitude.Degrees." TO "Longitude";')
dbSendQuery(con_aeronet, 'ALTER TABLE aeronet RENAME COLUMN "Site_Latitude.Degrees." TO "Latitude";')

# Create a table index for the dataset
# 134.12 seconds to run
system.time(invisible(dbSendQuery(con_aeronet, 'CREATE INDEX aeronet_idx ON aeronet ("Longitude", "Latitude");')))

# Change the column Date.GMT as DATE data type and format it from "DD:MM:YYYY" to "YYYY-MM-DD"
# UPDATE 23382113. Query returned successfully in 10 min.
system.time(invisible(dbSendQuery(con_aeronet, 
                                  'UPDATE aeronet SET "Date.GMT" = to_date("Date.GMT", "DD:MM:YYYY");')))

# Extract the data from the database Aeronet
#    user  system elapsed 
#  260.69  215.33 1374.64 
memory.limit()

# might need to increase the memory limit in order to establish this dataset in R
memory.limit(size = 20000)
system.time(aeronet_allSites_2014_2017 <- 
              dbGetQuery(con_aeronet,
                         paste0('SELECT * from aeronet 
                         where "Date.GMT" <= ', "'2017-12-31'", 'and "Date.GMT" >=', "'2013-10-01'", collapse=",")))

# Write aeronet_allSites_2014_2017 to the database PM2.5 in postgresql
# user  system elapsed 
# 454.66   16.04  471.09 
system.time(dbWriteTable(con_aeronet,"aeronet_allSites_2014_2017", value = aeronet_allSites_2014_2017, append = TRUE, row.names = FALSE))

# Unique location for all aeronet sites, i.e.,569 sites
system.time(aeronet_allSites_location_2014_2017 <- 
              aeronet_allSites_2014_2017[
                !duplicated(aeronet_allSites_2014_2017$AERONET_Site), ])

# Write aeronet_allSites_location_2014_2017 to the database PM2.5 in postgresql
system.time(dbWriteTable(con_aeronet,"aeronet_allSites_location_2014_2017", value = aeronet_allSites_location_2014_2017, append = FALSE, row.names = FALSE))

# Map out all aeronet sites from 2014 to 2017
# leaflet(aeronet_allSites_location_2014_2017) %>%
#   addCircles(~Longitude, ~Latitude,
#              label = ~paste("Site Name: ", AERONET_Site)) %>%
#   addTiles() %>%
#   addProviderTiles(providers$CartoDB.Positron)

# close all connections and the driver of PostgreSQL
lapply(dbListConnections(drv = dbDriver("PostgreSQL")), 
       function(x) {dbDisconnect(conn = x)})
dbUnloadDriver(drv)

#--------------------------------------------------------------#
# Import R's dataframes into PostgreSQL database 
# i.e., AOD (Atmospheric Aerosal Dynamics Project)
#--------------------------------------------------------------#

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "postgres"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

# creates a con_pmnection to the postgres database
# note that "con_pm" will be used later in each con_pmnection to the database
con_pm <- dbConnect(drv, dbname = "PM2.5",
                    host = "localhost", port = 5432,
                    user = "postgres", password = pw)

rm(pw) # removes the password

# check for an existing table
# dbExistsTable(con_pm, "country")
# TRUE

# remove a specific table from the db
# dbRemoveTable(con_pm,"aeronet_allSites_2014_2017")

# # create df of all aeronet sites 2014-2017 
aeronet_allSites_location_2014_2017_4 <- unique(select(aeronet_allSites_2014_2017,
                                                       c(Longitude,
                                                         Latitude)))

# write hourly_pm2.5_allyears to the database PM2.5 in postgresql
dbWriteTable(con_pm,"hourly_pm2.5_allyears", value = hourly_pm2.5_allyears, append = TRUE, row.names = FALSE)

# write hourly_pm2.5_location_allYears 2014-2017 to the database PM2.5 in postgresql
dbWriteTable(con_pm,"hourly_pm2.5_location_allYears", value = hourly_pm2.5_location_allYears, append = TRUE, row.names = FALSE)

# write hourly PM2.5 data 2014-2017 individually to the database PM2.5 in postgresql
dbWriteTable(con_pm,"hourly_pm2.5_2014", value = hourly_pm2.5_2014, append = TRUE, row.names = FALSE)
dbWriteTable(con_pm,"hourly_pm2.5_2015", value = hourly_pm2.5_2015, append = TRUE, row.names = FALSE)
dbWriteTable(con_pm,"hourly_pm2.5_2016", value = hourly_pm2.5_2016, append = TRUE, row.names = FALSE)
dbWriteTable(con_pm,"hourly_pm2.5_2017", value = hourly_pm2.5_2017, append = TRUE, row.names = FALSE)

# query the data from postgreSQL 
# df_postgres <- dbGetQuery(con_pm, "SELECT * from tableName")
# dbGetQuery(con_pm, 'SELECT * from public."hourly_pm2.5_allyears" limit 1')

# close the con_pmnection
lapply(dbListConnections(drv = dbDriver("PostgreSQL")), 
       function(x) {dbDisconnect(conn = x)})
dbUnloadDriver(drv)

#--------------------------------------------------------------#
# Aeronet data: all aeronet sites and EPA sites, 2014-2017 
#--------------------------------------------------------------#
library(leaflet)
leaflet(hourly_pm2.5_location_allYears) %>%
  addCircles(data = aeronet_allSites_location_2014_2017,
             lng = ~Longitude,
             lat = ~Latitude,
             radius = 5000, # in meters
             color = "red",
             label = ~paste("[AERONET] Site Name: ", AERONET_Site)) %>%
  addCircles(~Longitude, ~Latitude,
             label = ~paste("[EPA] Site Num: ", Site.Num, ", ",
                            "State Name: ", State.Name, ", ",
                            "County Name: ", County.Name)) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addScaleBar()

################################################################
#--------------------------------------------------------------#
# 1. Area: CCNY
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

# converting to local time zone and round the time by hour
tz_lookup <- tz_lookup_coords(NY_Aeronet_AOD$Latitude[1], NY_Aeronet_AOD$Longitude[1], method = "fast", warn = FALSE)
NY_Aeronet_AOD$DateTime.Local <- with_tz(NY_Aeronet_AOD$DateTime.GMT, tz = tz_lookup[1])

# round to the next hour
NY_Aeronet_AOD$DateTime.Local <- ceiling_date(NY_Aeronet_AOD$DateTime.Local, unit = "hour")
NY_Aeronet_AOD$Hour.Local <- as.character(format(NY_Aeronet_AOD$DateTime.Local,"%H:%M"))

#--------------------------------------------------------------#
# Hourly PM2.5 Data of NY
#--------------------------------------------------------------#
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 4"
NY.EPA_Site <- subset(hourly_pm2.5_allYears,
                      Site.Num == 110 & 
                        State.Name == "New York" & 
                        County.Name == "Bronx")

# Create a new column as PST Local time 
# by joining the local date and time together
NY.EPA_Site$DateTime.Local <- as.POSIXct(paste(NY.EPA_Site$Date.Local, 
                                               NY.EPA_Site$Time.Local), 
                                         format = "%Y-%m-%d %H:%M")

################################################################
#--------------------------------------------------------------#
# 2. Area: Baltimore, MD
#--------------------------------------------------------------#
################################################################
#--------------------------------------------------------------#
# Hourly Aeronet AOD Data of Baltimore (MD_Science_Center)
#--------------------------------------------------------------#
# set the working directory
setwd("~/UW_Study_Work/2018Summer/NASA/Atmospheric Aerosol Dynamics")

# Import the data 
system.time(Baltimore_Aeronet_AOD <- subset(aeronet_allSites_2014_2017, AERONET_Site == "MD_Science_Center"))

# Join date and time into a new column "DateTime.GMT"
Baltimore_Aeronet_AOD$DateTime.GMT <- ymd_hms(as.POSIXct(paste(Baltimore_Aeronet_AOD$Date.GMT,
                                                               Baltimore_Aeronet_AOD$Time.GMT),
                                                         format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")

# converting to local time zone and round the time by hour
tz_lookup <- tz_lookup_coords(Baltimore_Aeronet_AOD$Latitude[1], Baltimore_Aeronet_AOD$Longitude[1], method = "fast", warn = FALSE)
Baltimore_Aeronet_AOD$DateTime.Local <- with_tz(Baltimore_Aeronet_AOD$DateTime.GMT, tz = tz_lookup[1])

# round to the next hour
Baltimore_Aeronet_AOD$DateTime.Local <- ceiling_date(Baltimore_Aeronet_AOD$DateTime.Local, unit = "hour")
Baltimore_Aeronet_AOD$Hour.Local <- as.character(format(Baltimore_Aeronet_AOD$DateTime.Local,"%H:%M"))

#--------------------------------------------------------------#
# Hourly PM2.5 Data of Baltimore, MD
#--------------------------------------------------------------#
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 3"
Baltimore.EPA_Site <- subset(hourly_pm2.5_allYears,
                             Site.Num == 40 & 
                               State.Name == "Maryland" & 
                               County.Name == "Baltimore (City)")

# Create a new column as PST Local time 
# by joining the local date and time together
Baltimore.EPA_Site$DateTime.Local <- as.POSIXct(paste(Baltimore.EPA_Site$Date.Local, 
                                                      Baltimore.EPA_Site$Time.Local),
                                                format = "%Y-%m-%d %H:%M")


################################################################
#--------------------------------------------------------------#
# 3. Area: Reno, NV
#--------------------------------------------------------------#
################################################################
#--------------------------------------------------------------#
# Hourly Aeronet AOD Data of UNR
#--------------------------------------------------------------#
# set the working directory
setwd("~/UW_Study_Work/2018Summer/NASA/Atmospheric Aerosol Dynamics")

# Import the data 
system.time(UNR_Aeronet_AOD <- subset(aeronet_allSites_2014_2017, AERONET_Site == "Univ_of_Nevada-Reno"))

# Join date and time into a new column "DateTime.GMT"
UNR_Aeronet_AOD$DateTime.GMT <- ymd_hms(as.POSIXct(paste(UNR_Aeronet_AOD$Date.GMT,
                                                         UNR_Aeronet_AOD$Time.GMT),
                                                   format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")

# converting to local time zone and round the time by hour
tz_lookup <- tz_lookup_coords(UNR_Aeronet_AOD$Latitude[1], UNR_Aeronet_AOD$Longitude[1], method = "fast", warn = FALSE)
UNR_Aeronet_AOD$DateTime.Local <- with_tz(UNR_Aeronet_AOD$DateTime.GMT, tz = tz_lookup[1])

# round to the next hour
UNR_Aeronet_AOD$DateTime.Local <- ceiling_date(UNR_Aeronet_AOD$DateTime.Local, unit = "hour")
UNR_Aeronet_AOD$Hour.Local <- as.character(format(UNR_Aeronet_AOD$DateTime.Local,"%H:%M"))

#--------------------------------------------------------------#
# Hourly PM2.5 Data of UNR
#--------------------------------------------------------------#
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 3"
UNR.EPA_Site <- subset(hourly_pm2.5_allYears,
                       Site.Num == 16 & 
                         State.Name == "Nevada" & 
                         County.Name == "Washoe")
# LU: Residential
# Create a new column as PST Local time 
# by joining the local date and time together
UNR.EPA_Site$DateTime.Local <- as.POSIXct(paste(UNR.EPA_Site$Date.Local, 
                                                UNR.EPA_Site$Time.Local), 
                                          format = "%Y-%m-%d %H:%M")

################################################################
#--------------------------------------------------------------#
# 8. Area: St. Louis
#--------------------------------------------------------------#
################################################################
#--------------------------------------------------------------#
# Hourly Aeronet AOD Data of CCNY
#--------------------------------------------------------------#
# set the working directory
setwd("~/UW_Study_Work/2018Summer/NASA/Atmospheric Aerosol Dynamics")

# Import the data 
system.time(StLouis_Aeronet_AOD <- subset(aeronet_allSites_2014_2017, AERONET_Site == "St_Louis_University"))

# Join date and time into a new column "DateTime.GMT"
StLouis_Aeronet_AOD$DateTime.GMT <- ymd_hms(as.POSIXct(paste(StLouis_Aeronet_AOD$Date.GMT,
                                                             StLouis_Aeronet_AOD$Time.GMT),
                                                  format = "%Y-%m-%d %H:%M:%S"), tz = "GMT")

# converting to local time zone and round the time by hour
tz_lookup <- tz_lookup_coords(StLouis_Aeronet_AOD$Latitude[1], StLouis_Aeronet_AOD$Longitude[1], method = "fast", warn = FALSE)
StLouis_Aeronet_AOD$DateTime.Local <- with_tz(StLouis_Aeronet_AOD$DateTime.GMT, tz = tz_lookup[1])

# round to the next hour
StLouis_Aeronet_AOD$DateTime.Local <- ceiling_date(StLouis_Aeronet_AOD$DateTime.Local, unit = "hour")
StLouis_Aeronet_AOD$Hour.Local <- as.character(format(StLouis_Aeronet_AOD$DateTime.Local,"%H:%M"))

#--------------------------------------------------------------#
# Hourly PM2.5 Data of St. Louis
#--------------------------------------------------------------#
# Extract Hourly PM2.5 data from the nearest EPA site around this Aeronet site 
# All observations are "POC = 3"
StLouis.EPA_Site1 <- subset(hourly_pm2.5_allYears,
                          Site.Num == 85 & 
                            State.Name == "Missouri" & 
                            County.Name == "St. Louis City")

# Create a new column as PST Local time 
# by joining the local date and time together
StLouis.EPA_Site1$DateTime.Local <- as.POSIXct(paste(StLouis.EPA_Site1$Date.Local, 
                                                     StLouis.EPA_Site1$Time.Local), 
                                             format = "%Y-%m-%d %H:%M")

