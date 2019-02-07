
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

# Spatial differences exploration - PM2.5

################################################################
################################################################

# Create a df combining the four sites PM2.5 data
EPA_Sites_four <- rbind(NY.EPA_Site, Baltimore.EPA_Site, StLouis.EPA_Site1, UNR.EPA_Site)

# Create a new df with year, month, 24-hour as the X-axis, hourly-based average of PM2.5 as the Y-axis
Year.Local <- lubridate::year(EPA_Sites_four$DateTime.Local)
Month.Local <- lubridate::month(EPA_Sites_four$DateTime.Local, label = TRUE, abbr = FALSE)
Date.Local <- lubridate::date(EPA_Sites_four$DateTime.Local)
Hour.Local <- as.character(format(EPA_Sites_four$DateTime.Local,"%H:%M"))

Four.PM25.diurnal.byMonth <- data.frame(Year.Local,Month.Local,Date.Local,Hour.Local)
Four.PM25.diurnal.byMonth$DateTime.Local <- EPA_Sites_four$DateTime.Local
Four.PM25.diurnal.byMonth$PM2.5 <- EPA_Sites_four$Sample.Measurement
Four.PM25.diurnal.byMonth$Site.Num <- EPA_Sites_four$Site.Num
Four.PM25.diurnal.byMonth$County.Name <- EPA_Sites_four$County.Name
Four.PM25.diurnal.byMonth$State.Name <- EPA_Sites_four$State.Name

Four.PM25.diurnal.byMonth$Site <- NULL

Four.PM25.diurnal.byMonth$Site[which(Four.PM25.diurnal.byMonth$Site.Num == 85 &
                                       Four.PM25.diurnal.byMonth$State.Name == "Missouri" &
                                       Four.PM25.diurnal.byMonth$County.Name == "St. Louis City")] <- "St. Louis"

Four.PM25.diurnal.byMonth$Site[which(Four.PM25.diurnal.byMonth$Site.Num == 16 &
                                       Four.PM25.diurnal.byMonth$State.Name == "Nevada" &
                                       Four.PM25.diurnal.byMonth$County.Name == "Washoe")] <- "UN-Reno"

Four.PM25.diurnal.byMonth$Site[which(Four.PM25.diurnal.byMonth$Site.Num == 110 &
                                       Four.PM25.diurnal.byMonth$State.Name == "New York" &
                                       Four.PM25.diurnal.byMonth$County.Name == "Bronx")] <- "New York"

Four.PM25.diurnal.byMonth$Site[which(Four.PM25.diurnal.byMonth$Site.Num == 40 &
                                       Four.PM25.diurnal.byMonth$State.Name == "Maryland" &
                                       Four.PM25.diurnal.byMonth$County.Name == "Baltimore (City)")] <- "Baltimore"

# Remove the rows that have invalid PM2.5 value
Four.PM25.diurnal.byMonth <- subset(Four.PM25.diurnal.byMonth, PM2.5 > 0)

#################################################
##########  Aggregated by month, hour ###########
#################################################
Four.PM25.hourly_Agg_MH.byMonth <- aggregate(PM2.5 ~ Month.Local + Hour.Local + Site,
                                             data = Four.PM25.diurnal.byMonth,
                                             FUN = geoMean)
start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
Four.PM25.hourly_Agg_MH.byMonth %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(PM2.5)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Site, color = Site)) +
  #geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc", k = 9), 
              size = 1,
              se = FALSE,
              aes(group = Site)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Site") +
  scale_x_discrete(breaks = c("00:00", 
                              "03:00", 
                              "06:00", 
                              "09:00", 
                              "12:00", 
                              "15:00",
                              "18:00", 
                              "21:00")) + 
  #labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Month") +
  ggtitle(paste0("Diurnal Dynamics of EPA PM2.5, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\FourSites\\monthly_trend_four_geoMean_test.png")

#################################################
##########  Aggregated by season, hour ##########
#################################################
Four.PM25.diurnal.byMonth$Season.Local2 <-
  lubridate::quarter(Four.PM25.diurnal.byMonth$DateTime.Local, with_year = FALSE, fiscal_start = 3)

Four.PM25.hourly_Agg_YSH.allyears.bySeason <- 
  aggregate(PM2.5 ~ Season.Local2 + Hour.Local + Site,
            data = Four.PM25.diurnal.byMonth,
            FUN = mean)

# Create a column "season" string
Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 1)] <- "1. Spring"
Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 2)] <- "2. Summer"
Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 3)] <- "3. Fall"
Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.PM25.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 4)] <- "4. Winter"

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for four years
Four.PM25.hourly_Agg_YSH.allyears.bySeason %>% 
  na.omit %>%
  subset(!is.null(PM2.5) & Site != "Denver") %>% 
  #ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = PM2.5, group = Site, color = as.character(Site))) +
  #geom_point() +
  stat_smooth(method = "gam", 
              formula = y ~ s(x, bs="cc"), 
              size = 1,
              se = FALSE) +
  facet_wrap(~Season, ncol = 2) +
  scale_x_discrete(breaks = c("00:00", 
                              "04:00", 
                              "08:00", 
                              "12:00", 
                              "16:00",
                              "20:00" 
                              )) +
  theme_bw() +
  theme(plot.title = element_text(size = 15, 
                                  face = "bold",
                                  margin = margin(t = 0, r = 0, b = 10, l = 0)),
        text = element_text(size=15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(plot.margin=unit(c(1,1,1,1),"cm")) +
  labs(x = "Hour", y = "PM2.5 Unit: Micrograms/cubic meter (LC)", color = "Site") +
  ggtitle(paste0(paste("Diurnal Dynamics of EPA PM2.5 by Season, 2014-2017", 
                       "at Baltimore, New York, Reno", 
                       sep = "\n"))) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\FourSites\\PM2.5_seasonal_four2.png")


################################################################
################################################################

# Spatial differences exploration - Aeronet AOD

################################################################
################################################################

# Create a df combining the four sites PM2.5 data
# Aeronet_AOD_four <- rbind(NY_Aeronet_AOD, Baltimore_Aeronet_AOD, Denver_Aeronet_AOD, UNR_Aeronet_AOD)
Aeronet_AOD_four <- rbind(NY_Aeronet_AOD, Baltimore_Aeronet_AOD, StLouis_Aeronet_AOD, UNR_Aeronet_AOD)

# Create a new df with year, month, 24-hour as the X-axis, hourly-based average of PM2.5 as the Y-axis
Year.Local <- lubridate::year(Aeronet_AOD_four$DateTime.Local)
Month.Local <- lubridate::month(Aeronet_AOD_four$DateTime.Local, label = TRUE, abbr = FALSE)
Date.Local <- lubridate::date(Aeronet_AOD_four$DateTime.Local)

Four.AOD.diurnal.byMonth <- data.frame(Year.Local,Month.Local,Date.Local)
Four.AOD.diurnal.byMonth$Hour.Local <- Aeronet_AOD_four$Hour.Local
Four.AOD.diurnal.byMonth$DateTime.Local <- Aeronet_AOD_four$DateTime.Local
Four.AOD.diurnal.byMonth$AOD <- Aeronet_AOD_four$AOD_500nm
Four.AOD.diurnal.byMonth$Site <- Aeronet_AOD_four$AERONET_Site

Four.AOD.diurnal.byMonth$Site[which(Four.AOD.diurnal.byMonth$Site == "St_Louis_University")] <- "St. Louis"
Four.AOD.diurnal.byMonth$Site[which(Four.AOD.diurnal.byMonth$Site == "Univ_of_Nevada-Reno")] <- "UN-Reno"
Four.AOD.diurnal.byMonth$Site[which(Four.AOD.diurnal.byMonth$Site == "CCNY")] <- "New York"
Four.AOD.diurnal.byMonth$Site[which(Four.AOD.diurnal.byMonth$Site == "MD_Science_Center")] <- "Baltimore"

# Remove the rows that have invalid AOD value
Four.AOD.diurnal.byMonth <- subset(Four.AOD.diurnal.byMonth, AOD > 0)

Four.AOD.diurnal.byMonth <- aggregate(AOD ~ DateTime.Local + Year.Local + Month.Local + Date.Local + Hour.Local + Site,
                                      data = Four.AOD.diurnal.byMonth,
                                      FUN = mean)

#################################################
##########  Aggregated by month, hour ###########
#################################################
Four.AOD.hourly_Agg_MH.byMonth <- aggregate(AOD ~ Month.Local + Hour.Local + Site,
                                            data = Four.AOD.diurnal.byMonth,
                                            FUN = mean)
start.month <- 1
end.month <-12

start.month <- lubridate::month(start.month, label = TRUE, abbr = FALSE)
end.month <- lubridate::month(end.month, label = TRUE, abbr = FALSE)

# Plot hourly geoMean PM2.5 data from the EPA site around CCNY (Aeronet site) for 4 years
Four.AOD.hourly_Agg_MH.byMonth %>% 
  na.omit %>%
  subset(Month.Local >= start.month  &
           Month.Local <= end.month & 
           !is.null(AOD)) %>% 
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local, color = Month.Local)) +
  # ggplot(aes(x = Hour.Local, y = PM2.5, group = Month.Local)) +
  ggplot(aes(x = Hour.Local, y = AOD, group = Site, color = Site)) +
  #geom_point() +
  stat_smooth(method = "loess", 
              formula = y ~ x, 
              size = 0.8,
              se = FALSE,
              aes(group = Site)) +
  facet_wrap(~Month.Local, ncol = 4, nrow = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 3)) +
  labs(x = "Hour", y = "AOD_500nm", color = "Site") +
  scale_x_discrete(breaks = c("03:00", 
                              "06:00", 
                              "09:00", 
                              "12:00", 
                              "15:00",
                              "18:00", 
                              "21:00")) + 
  ggtitle(paste0("Diurnal Dynamics of Aeronet AOD, Month: ", 
                 start.month,"~", 
                 end.month,", 2014-2017", collapse = NULL)) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\AOD_monthly_four.png")

#################################################
##########  Aggregated by season, hour ##########
#################################################
Four.AOD.diurnal.byMonth$Season.Local2 <-
  lubridate::quarter(Four.AOD.diurnal.byMonth$DateTime.Local, with_year = FALSE, fiscal_start = 3)

Four.AOD.hourly_Agg_YSH.allyears.bySeason <- 
  aggregate(AOD ~ Season.Local2 + Hour.Local + Site,
            data = Four.AOD.diurnal.byMonth,
            FUN = mean)

# Create a column "season" string
Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 1)] <- "1. Spring"
Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 2)] <- "2. Summer"
Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 3)] <- "3. Fall"
Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season[which(Four.AOD.hourly_Agg_YSH.allyears.bySeason$Season.Local2 == 4)] <- "4. Winter"

# Plot hourly AOD data from the EPA site around CCNY (Aeronet site) for four years
Four.AOD.hourly_Agg_YSH.allyears.bySeason %>% 
  na.omit %>%
  subset(!is.null(AOD) & Site != "Denver" ) %>% 
  ggplot(aes(x = Hour.Local, y = AOD, group = Site, color = as.character(Site))) +
  #geom_point() +
  stat_smooth(method = "loess", 
              formula = y ~ x, 
              size = 0.8,
              se = FALSE,
              aes(group = Site)) +
  facet_wrap(~Season, ncol = 2) +
  scale_x_discrete(breaks = c("03:00", 
                              "06:00", 
                              "09:00", 
                              "12:00", 
                              "15:00",
                              "18:00", 
                              "21:00")) + 
  theme_bw() +
  theme(plot.title = element_text(size = 15, 
                                  face = "bold",
                                  margin = margin(t = 0, r = 0, b = 10, l = 0)),
        text = element_text(size=15),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  theme(plot.margin=unit(c(1,1,1,1),"cm")) +
  labs(x = "Hour", y = "AOD_500nm", color = "Site") +
  ggtitle(paste0(paste("Diurnal Dynamics of Aeronet AOD by Season, 2014-2017", 
                       "at Baltimore, New York, Reno", 
                       sep = "\n"))) +
  ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\AOD_seasonal_four2.png")





