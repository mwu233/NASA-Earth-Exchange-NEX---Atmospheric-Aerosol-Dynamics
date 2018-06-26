# Author: Meiliu Wu
# Date: 06/19/2018

# open the daily data in 2014

install.packages("ggmap")
install.packages("ggplot2")

#load ggmap
library(ggmap)
library(ggplot2)

#--------------------------------------------------------------
# data pre-processing
# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origTable <- read.csv(fileToLoad, stringsAsFactors = FALSE)
# names(origTable)
df <- as.data.frame(origTable,row.names = NULL)

#--------------------------------------------------------------
# map the point data with unique sites
uniqueSites <- df[!duplicated(df$Latitude), ]

#--------------------------------------------------------------
# write the unique sites as an csv table
# rename
# colnames(uniqueSites) <- c("StateCode","CountyCode","SiteNum","Longitude","Latitude")
write.csv(uniqueSites,file = file.choose(new = T),row.names = FALSE)

#--------------------------------------------------------------
# data analysis 
# factorsToLoad is the csv 
# that has the info of land use, pop, hh income, traffic, and meteorological factors.
WindToLoad <- file.choose(new = TRUE)
Wind <- read.csv(WindToLoad, stringsAsFactors = FALSE)

stateCode <- data.frame(Wind$State.Code)
countyCode <- data.frame(Wind$County.Code)
siteNum <- data.frame(Wind$Site.Num)
date_local <-data.frame(Wind$Date.Local)
time_local <-data.frame(Wind$Time.Local)
Windvalue <-data.frame(Wind$Sample.Measurement)
Wind <- as.data.frame(cbind(stateCode,countyCode,siteNum,date_local,time_local,Windvalue))

df_wind <- as.data.frame(Wind,row.names = NULL)

#--------------------------------------------------------------
PressToLoad <- file.choose(new = TRUE)
Press <- read.csv(PressToLoad, stringsAsFactors = FALSE)

stateCode <- data.frame(Press$State.Code)
countyCode <- data.frame(Press$County.Code)
siteNum <- data.frame(Press$Site.Num)
date_local <-data.frame(Press$Date.Local)
time_local <-data.frame(Press$Time.Local)
Pressvalue <-data.frame(Press$Sample.Measurement)
Press <- as.data.frame(cbind(stateCode,countyCode,siteNum,date_local,time_local,Pressvalue))

df_press <- as.data.frame(Press,row.names = NULL)

#--------------------------------------------------------------
TempToLoad <- file.choose(new = TRUE)
Temp <- read.csv(TempToLoad, stringsAsFactors = FALSE)
df_temp <- as.data.frame(Temp,row.names = NULL)

#--------------------------------------------------------------
RHToLoad <- file.choose(new = TRUE)
RH <- read.csv(RHToLoad, stringsAsFactors = FALSE)
df_rh <- as.data.frame(RH,row.names = NULL)

wind_press <- merge(df_wind, df_press, by=c("State.Code","County.Code","Site.Num","Date.Local","Time.Local"))
wind_press_temp <- merge(wind_press, df_temp, by=c("State.Code","County.Code","Site.Num","Date.Local","Time.Local"))
all_meteorological <- merge(wind_press_temp, df_rh, by=c("State.Code","County.Code","Site.Num","Date.Local","Time.Local"))

df_meteo <- merge(all_meteorological, df, by=c("State.Code","County.Code","Site.Num","Date.Local","Time.Local"))
write.csv(df_meteo,file = file.choose(new = T),row.names = FALSE)
