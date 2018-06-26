# Author: Meiliu Wu
# Date: 06/25/2018

# open the daily data in 2014

install.packages("ggmap")
install.packages("ggplot2")
install.packages("leaflet")

#load ggmap
library(ggmap)
library(ggplot2)
library(leaflet)

#--------------------------------------------------------------
# data pre-processing
# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origTable <- read.csv(fileToLoad, stringsAsFactors = FALSE)
names(origTable)
df <- as.data.frame(origTable,row.names = NULL)

stateCode <- data.frame(df$State.Code)
countyCode <- data.frame(df$County.Code)
siteNum <- data.frame(df$Site.Num)
date_local <-data.frame(df$Date.Local)
time_local <-data.frame(df$Time.Local)
PMvalue <-data.frame(df$Sample.Measurement)
df_pm <- as.data.frame(cbind(stateCode,countyCode,siteNum,date_local,time_local,PMvalue))

df_pm <- as.data.frame(df_pm,row.names = NULL)
colnames(df_pm) <- c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal","PM2.5")

#--------------------------------------------------------------
# map the point data with unique sites
uniqueSites <- df[!duplicated(df$Latitude), ]

# use leaflet to map 
m <- leaflet()
m <- addTiles(m)


m <- addMarkers(m, lng=174.768, lat=-36.852, popup="The birthplace of R")


m  # Print the map







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
colnames(df_wind) <- c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal","Wind")

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
colnames(df_press) <- c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal","Press")

#--------------------------------------------------------------
TempToLoad <- file.choose(new = TRUE)
Temp <- read.csv(TempToLoad, stringsAsFactors = FALSE)

stateCode <- data.frame(Temp$State.Code)
countyCode <- data.frame(Temp$County.Code)
siteNum <- data.frame(Temp$Site.Num)
date_local <-data.frame(Temp$Date.Local)
time_local <-data.frame(Temp$Time.Local)
Tempvalue <-data.frame(Temp$Sample.Measurement)
Temp <- as.data.frame(cbind(stateCode,countyCode,siteNum,date_local,time_local,Tempvalue))

df_temp <- as.data.frame(Temp,row.names = NULL)
colnames(df_temp) <- c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal","Temp")

#--------------------------------------------------------------
RHToLoad <- file.choose(new = TRUE)
RH <- read.csv(RHToLoad, stringsAsFactors = FALSE)

stateCode <- data.frame(RH$State.Code)
countyCode <- data.frame(RH$County.Code)
siteNum <- data.frame(RH$Site.Num)
date_local <-data.frame(RH$Date.Local)
time_local <-data.frame(RH$Time.Local)
RHvalue <-data.frame(RH$Sample.Measurement)
RH <- as.data.frame(cbind(stateCode,countyCode,siteNum,date_local,time_local,RHvalue))

df_rh <- as.data.frame(RH,row.names = NULL)
colnames(df_rh) <- c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal","RH")

#--------------------------------------------------------------
wind_press <- merge(df_wind, df_press, by=c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal"))
temp_rh <- merge(df_temp, df_rh, by=c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal"))
all_meteorological <- merge(wind_press, temp_rh, by=c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal"))
df_pm_meteo <- merge(all_meteorological, df_pm, by=c("StateCode","CountyCode","SiteNum","DateLocal","TimeLocal"))

#--------------------------------------------------------------
write.csv(df_meteo,file = file.choose(new = T),row.names = FALSE)
