
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

#--------------------------------------------------------------
# Scatterplot of the peak values and the average values in NY
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

# Overall four years scatterplot
NY.EPA_Site_peak_average %>%
  ggplot(aes(x = Average, y = Peak)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
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

PM2.5_peak_average_bySeason(subset(NY.EPA_Site_peak_average, Peak < 100))

NY.EPA_Site_peak_average %>%
  subset(Peak < 100) %>%
  ggplot(aes(x = Average, y = Peak)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
  geom_text(data = 
              data.frame(x = Inf,
                         y = Inf, 
                         label = c(cor(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==1),]$Peak,
                                       NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==1),]$Average), 
                                   cor(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==2),]$Peak,
                                       NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==2),]$Average), 
                                   cor(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==3),]$Peak,
                                       NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==3),]$Average),
                                   cor(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==4),]$Peak,
                                       NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==4),]$Average)),
                         Season = c("1. Spring", 
                                    "2. Summer", 
                                    "3. Fall",
                                    "4. Winter")),
            mapping = aes(x = 0, 
                          y = 70, 
                          label = paste("R^2 = ",round(label,digits = 2))),
            vjust = "inward",
            hjust = "inward",
            size = 5,
            color = "darkblue") +
  
  geom_text(data = 
              data.frame(x = Inf,
                         y = Inf, 
                         count = c(nrow(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==1),]),
                                   nrow(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==2),]),
                                   nrow(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==3),]),
                                   nrow(NY.EPA_Site_peak_average[which(NY.EPA_Site_peak_average$Season.numeric==4),])), 
                         Season = c("1. Spring", 
                                    "2. Summer", 
                                    "3. Fall",
                                    "4. Winter")),
            mapping = aes(x = 0, 
                          y = 65, 
                          label = paste("Count = ",count)),
            vjust = "inward",
            hjust = "inward",
            size = 5,
            color = "darkblue") +
  facet_wrap(~Season, ncol = 2) +
  labs(x = "Daily Average Value of PM2.5 [Unit: Micrograms/cubic meter]", 
       y = "Daily Peak Value of PM2.5 [Unit: Micrograms/cubic meter]", 
       color = "Season") +
  ggtitle(paste0("Scatterplot of Daily Average and Peak Values of PM2.5 in New York by Season, 2014-2017",
                 collapse = NULL))

#--------------------------------------------------------------
# Aggregate the dataset 
# Create a new df with year, month, 24-hour as the X-axis, hourly-based average of PM2.5 as the Y-axis
Year.Local <- lubridate::year(NY.EPA_Site$DateTime.Local)
Month.Local <- lubridate::month(NY.EPA_Site$DateTime.Local, label = TRUE, abbr = FALSE)
Hour.Local <- format(NY.EPA_Site$DateTime.Local,"%H:%M")

NY.PM25.diurnal.byMonth <- data.frame(Year.Local,Month.Local,Hour.Local)
NY.PM25.diurnal.byMonth$DateTime.Local <- NY.EPA_Site$DateTime.Local
NY.PM25.diurnal.byMonth$PM2.5 <- NY.EPA_Site$Sample.Measurement

# Remove the rows that have invalid PM2.5 value
NY.PM25.diurnal.byMonth <- subset(NY.PM25.diurnal.byMonth, PM2.5 > 0)

#################################################
###### 1. Aggregated by year, month, hour #######
#################################################
NY.PM25.hourly_geoMean.byMonth <- aggregate(PM2.5 ~ Year.Local + Month.Local + Hour.Local,
                                            data = NY.PM25.diurnal.byMonth,
                                            FUN = geoMean)

year <- 2017
start.month <- 1
end.month <- 12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

NY.PM25.hourly_geoMean.byMonth %>% 
  na.omit %>%
  subset(Year.Local == year &
           Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 15), 
              size = 1,
              se = FALSE) +
  facet_wrap(~Month.Local, ncol = 4) +
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
#-----------------------------------------------------------
# geoMean
#-----------------------------------------------------------
NY.PM25.hourly_Agg_MH.byMonth <- aggregate(PM2.5 ~ Month.Local + Hour.Local,
                                           data = NY.PM25.diurnal.byMonth,
                                           FUN = geoMean)
start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

NY.PM25.hourly_Agg_MH.byMonth.meanY <- 
  NY.PM25.hourly_Agg_MH.byMonth %>% 
  group_by(Month.Local) %>% 
  summarise(mean_PM2.5 = mean(PM2.5))

NY.PM25.hourly_Agg_MH.byMonth.maxY <- 
  NY.PM25.hourly_Agg_MH.byMonth %>% 
  group_by(Month.Local) %>% 
  summarise(max_PM2.5 = max(PM2.5))

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
NY.PM25.hourly_Agg_MH.byMonth %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  #(aes(x = Hour.Local, y = PM2.5)) +
  # geom_point(size = 0.8) +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 9), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  geom_hline(data = NY.PM25.hourly_Agg_MH.byMonth.meanY, 
             aes(yintercept=mean_PM2.5),
             color="red", linetype="dashed", size=1) +
  geom_text(data = NY.PM25.hourly_Agg_MH.byMonth.maxY,
            aes(0,
                max_PM2.5,
                label = round(max_PM2.5,digits = 2),
                hjust = -3),
            size = 4,
            color = "darkblue") +
  geom_text(data = NY.PM25.hourly_Agg_MH.byMonth.meanY,
            aes(0,
                mean_PM2.5,
                label = round(mean_PM2.5,digits = 2), 
                vjust = -0.5,
                hjust = -0.5),
            color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)") +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  #labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\mobthly trend.png")

#---------------------------------------------------------------
# visreg for visualization
#---------------------------------------------------------------
NY.PM25.hourly_Agg_MH.byMonth$Hour.Local.Num <- 
  as.numeric(NY.PM25.hourly_Agg_MH.byMonth$Hour.Local) - 1

fit <- gam(PM2.5 ~ 
             s(Hour.Local.Num,
               by=Month.Local, 
               bs="cc",
               sp=0.1), 
           data=NY.PM25.hourly_Agg_MH.byMonth)

visreg(fit, 
       "Hour.Local.Num", 
       by="Month.Local", 
       gg=TRUE, 
       ylab="PM2.5 Unit: Micrograms/cubic meter (LC)") +
  theme_bw() +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\mobthly trend predition_visgre.png")


#-----------------------------------------------------------
# Max/Peak value
#-----------------------------------------------------------
NY.PM25.hourly_Agg_MH.byMonth.max <- aggregate(PM2.5 ~ Month.Local + Hour.Local,
                                               data = NY.PM25.diurnal.byMonth,
                                               FUN = max)

NY.PM25.hourly_Agg_MH.byMonth.max <- subset(NY.PM25.hourly_Agg_MH.byMonth.max, PM2.5 < 50)

start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

NY.PM25.hourly_Agg_MH.byMonth.max.meanY <- 
  NY.PM25.hourly_Agg_MH.byMonth.max %>% 
  group_by(Month.Local) %>% 
  summarise(mean_PM2.5 = mean(PM2.5))

NY.PM25.hourly_Agg_MH.byMonth.max.maxY <- 
  NY.PM25.hourly_Agg_MH.byMonth.max %>% 
  group_by(Month.Local) %>% 
  summarise(max_PM2.5 = max(PM2.5))

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
NY.PM25.hourly_Agg_MH.byMonth.max %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  #geom_point(size = 0.8) +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", sp =0.1), 
              size = 1,
              se = FALSE,
              aes(group = 1)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  geom_hline(data = NY.PM25.hourly_Agg_MH.byMonth.max.meanY, 
             aes(yintercept=mean_PM2.5),
             color="red", linetype="dashed", size=1) +
  geom_text(data = NY.PM25.hourly_Agg_MH.byMonth.max.maxY,
            aes(0,
                max_PM2.5,
                label = paste("Max: ", round(max_PM2.5,digits = 2)),
                hjust = -1,
                vjust = 1),
            size = 4,
            color = "darkblue") +
  geom_text(data = NY.PM25.hourly_Agg_MH.byMonth.max.meanY,
            aes(0,
                mean_PM2.5,
                label = round(mean_PM2.5,digits = 2), 
                vjust = -0.5,
                hjust = -0.5),
            color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)") +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  #labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Peak Values of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\monthly trend (max).png")

#################################################
######### 3. Aggregated by year, hour ###########
#################################################
NY.PM25.hourly_Agg_YH.byYear <- aggregate(PM2.5 ~ Year.Local + Hour.Local,
                                          data = NY.PM25.diurnal.byMonth,
                                          FUN = geoMean)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for one year
NY.PM25.hourly_Agg_YH.byYear %>%
  na.omit %>%
  subset(!is.null(PM2.5)) %>% 
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Year.Local, color = as.character(Year.Local))) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 9), 
              size = 1,
              se = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Year") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5 2014-2017", collapse = NULL))

#################################################
###### 4.1 Aggregated by year, season, hour #####
###### Compare four seasons of one specified year
#################################################

# Add the season column and update the season value based on the DateTime column
NY.PM25.diurnal.byMonth$Season.Local <- 
  lubridate::quarter(NY.PM25.diurnal.byMonth$DateTime.Local, with_year = TRUE, fiscal_start = 3)

NY.PM25.diurnal.byMonth$Season.Local2 <-
  lubridate::quarter(NY.PM25.diurnal.byMonth$DateTime.Local, with_year = FALSE, fiscal_start = 3)

NY.PM25.hourly_Agg_YSH.bySeason <- aggregate(PM2.5 ~ Season.Local + Hour.Local,
                                             data = NY.PM25.diurnal.byMonth,
                                             FUN = geoMean)

# Specify the year
year <- 2016

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for one year
NY.PM25.hourly_Agg_YSH.bySeason %>% 
  na.omit %>%
  subset(Season.Local >= year + 0.1 &
           Season.Local <= year + 0.4 &
           !is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Season.Local, color = as.character(Season.Local))) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc"), 
              size = 1,
              se = FALSE) +
  #facet_wrap(~Season.Local, ncol = 4) +
  #scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Season/Quarter") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5 by Season ", 
                 year, collapse = NULL))

#################################################
###### 4.2 Aggregated by year, season, hour #####
###### Compare one specified season of four years
#################################################

NY.PM25.hourly_Agg_YSH2.bySeason <- aggregate(PM2.5 ~ Year.Local + Season.Local2 + Hour.Local,
                                              data = NY.PM25.diurnal.byMonth,
                                              FUN = geoMean)

# Specify the season:
# 1 - Spring (Mar - May)
# 2 - Summer (Jun - Aug)
# 3 - Fall (Sep - Nov)
# 4 - Winter (Dec - Feb) # will it be a problem?
quarter <- 2

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for one year
NY.PM25.hourly_Agg_YSH2.bySeason %>% 
  na.omit %>%
  subset(Season.Local2 == quarter &
           !is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Year.Local, color = as.character(Year.Local))) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc"), 
              size = 1,
              se = FALSE) +
  #facet_wrap(~Season.Local, ncol = 4) +
  #scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Year") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5 by Quarter ", 
                 quarter, 
                 " , 2014-2017",
                 collapse = NULL))

#################################################
######### 5. Aggregated by season, hour #########
#################################################
NY.PM25.hourly_Agg_YSH.allyears.bySeason <- 
  aggregate(PM2.5 ~ Season.Local2 + Hour.Local,
            data = NY.PM25.diurnal.byMonth,
            FUN = geoMean)

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for four years
NY.PM25.hourly_Agg_YSH.allyears.bySeason %>% 
  na.omit %>%
  subset(!is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Season.Local2, color = as.character(Season.Local2))) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc"), 
              size = 1,
              se = FALSE) +
  facet_wrap(~Season.Local2, ncol = 2) +
  scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Season") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5 by Season, 2014-2017",
                 collapse = NULL))

#################################################
############ 6. Aggregated by hour ##############
#################################################
NY.PM25.hourly.byHour <- aggregate(PM2.5 ~ Hour.Local,
                                   data = NY.PM25.diurnal.byMonth,
                                   FUN = geoMean)

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for four years
NY.PM25.hourly.byHour %>% 
  na.omit %>%
  subset(!is.null(PM2.5)) %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5)) +
  geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc"), 
              size = 1,
              se = TRUE,
              aes(group = 1)) +
  #facet_wrap(~Season.Local, ncol = 4) +
  #scale_x_discrete(breaks = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, 2014-2017",
                 collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\hourly trend.png")

