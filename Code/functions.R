
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

#--------------------------------------------------------------
# functions
#--------------------------------------------------------------

#--------------------------------------------------------------
# @desc: Plots linechart for hourly trends within a certain period for pm2.5
# @param: dataset, start.time, end.time, data.method.code
#--------------------------------------------------------------
plot.linechart.pm25 <- function(pm25_data, start.time, end.time, pm25_data.POC) {
  pm25_data %>% 
    subset(DateTime.Local >= start.time & DateTime.Local <= end.time  & POC == pm25_data.POC) %>% 
    ggplot(aes(x = DateTime.Local, y = Sample.Measurement, group = Site.Num, color = as.character(Site.Num))) +
    #geom_point(position = position_dodge(width = 0.75)) +
    geom_smooth(method = "loess", se = FALSE, linetype = 2, span = 0.2, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Date/Time", y = "Micrograms/cubic meter", color = "Sites") 
  # geom_point(data = aeronet_data, aes(x = Date.Local, y = X440.870_Angstrom_Exponent), colour = 'red')
  # ggtitle(paste0(data.date, ", ", data$State.Name, ", Method Code: ", data.method.code))
}

#--------------------------------------------------------------
# @desc: Plots pointchart for diurnal trends 
# within a certain period for AOD at one specified site
# @param: dataset, start.time, end.time
#--------------------------------------------------------------
plot.pointChart.AOD <- function(data, start.time, end.time) {
  data %>%
    na.omit %>%
    subset(DateTime.Local >= start.time & DateTime.Local <= end.time & AOD_500nm >= 0.00) %>%
    ggplot(aes(x = DateTime.Local, y = AOD_500nm)) +
    geom_point() +
    # geom_smooth(method = "loess", se = FALSE, linetype = 2, aes(group = 1)) +
    labs(x = "Time", y = "AOD_500nm") +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(size = 15,
                                    face = "bold",
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
          text = element_text(size=15),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
    theme(plot.margin=unit(c(1,1,1,1),"cm")) +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD at ", 
                   data$AERONET_Site[1],
                   ", 2014-2017", collapse = NULL)) +
    ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\AOD_PM2.5\\AOD_PM2.5_plot.png",
           dpi = 300)
  
}

#--------------------------------------------------------------
# @desc: Plots linechart for diurnal trends within a certain period of a year for AOD
# @param: dataset, year, start.time, end.time
#--------------------------------------------------------------
plot.AOD.diurnal.byMonth <- function(data, year, start.month, end.month){
  data %>% 
    na.omit %>%
    subset(Year.Local == year &
             Month.Local >= start.month  &
             Month.Local <= end.month & 
             !is.null(AOD)) %>% 
    ggplot(aes(x = Hour.Local, y = AOD, group = Month.Local, color = Month.Local)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm", color = "Month") +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD, Month: ", 
                   start.month,"~", 
                   end.month,", ", 
                   year, collapse = NULL))
}

#--------------------------------------------------------------
# @desc: Plots linechart for diurnal trends of four years for AOD 
# @param: dataset, start.time, end.time
#--------------------------------------------------------------
plot.AOD.diurnal.allyears.byMonth <- function(data, start.month, end.month){
  data %>% 
    na.omit %>%
    subset(Month.Local >= start.month  &
             Month.Local <= end.month & 
             !is.null(AOD)) %>% 
    ggplot(aes(x = Hour.Local, y = AOD, group = Month.Local, color = Month.Local)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm", color = "Month") +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD, Month: ", 
                   start.month,"~", 
                   end.month,", 2014-2017", collapse = NULL))
}

#--------------------------------------------------------------
# @desc: Plots linechart for diurnal trends by year of four years for AOD 
# @param: dataset
#--------------------------------------------------------------
plot.AOD.diurnal.byYear <- function(data){
  data %>% 
    na.omit %>%
    subset(Year.Local != 2013 & !is.null(AOD)) %>% 
    ggplot(aes(x = Hour.Local, y = AOD, group = Year.Local, color = as.character(Year.Local))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm", color = "Year") +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD 2014-2017", collapse = NULL))
}

#--------------------------------------------------------------
# @desc: Plots linechart for diurnal trends by season of one year 
# for AOD comparison - four seasons of one specified year
# @param: dataset, year
#--------------------------------------------------------------
plot.AOD.diurnal.bySeason <- function(data, year){
  data %>%
    na.omit %>%
    subset(Season.Local <= year + 0.4 & Season.Local >= year + 0.1 & 
             !is.null(AOD)) %>% 
    ggplot(aes(x = Hour.Local, y = AOD, group = Season.Local, color = as.character(Season.Local))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm", color = "Season/Quarter") +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD by Season, ",
                   year,
                   collapse = NULL))
}

#--------------------------------------------------------------
# @desc: Plots linechart for diurnal trends by season of four years 
# for AOD comparison - one specified season of four years
# @param: dataset, year
#--------------------------------------------------------------
plot.AOD.diurnal.bySeason2 <- function(data, season){
  data %>%
    na.omit %>%
    subset(Season.Local2 == season & 
             Year.Local != 2013 &
             !is.null(AOD)) %>% 
    ggplot(aes(x = Hour.Local, y = AOD, group = Year.Local, color = as.character(Year.Local))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm", color = "Season/Quarter") +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD of Quarter ",
                   season,
                   ", from 2014 to 2017",
                   collapse = NULL))
}

#--------------------------------------------------------------
# @desc: Plots linechart for diurnal trends by season of four years for AOD 
# @param: dataset, year
#--------------------------------------------------------------
plot.AOD.diurnal.allyears.bySeason <- function(data){
  data %>%
    na.omit %>%
    subset(!is.null(AOD)) %>% 
    ggplot(aes(x = Hour.Local, y = AOD, group = Season.Local2, color = as.character(Season.Local2))) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm", color = "Season/Quarter") +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD by Season from 2014 to 2017",
                   collapse = NULL))
}

plot.AOD.diurnal.allyears.byHour <- function(data){
  data %>%
    na.omit %>%
    subset(!is.null(AOD)) %>% 
    ggplot(aes(x = Hour.Local, y = AOD)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1, aes(group = 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour", y = "AOD 500nm") +
    ggtitle(paste0("Diurnal Dynamics of Aeronet AOD by Hour from 2014 to 2017",
                   collapse = NULL))
}

#--------------------------------------------------------------
# @desc: Plots Scatterplot of AOD and PM2.5 of one specific season
# @param: dataset, season
#--------------------------------------------------------------
plot_AOD_PM25_season <- function(data, season){
  data %>%
    subset(Season.Local2 == season) %>%
    ggplot(aes(x = AOD, y = PM2.5)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
    # geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1) +
    labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter") +
    ggtitle(paste0("Scatterplot of AOD and PM2.5, Season ", 
                   season,
                   " in ",
                   data$Site,
                   collapse = NULL))
}

CC_AOD_PM25_season <- function(data, season){
  cor(subset(data, Season.Local2 == season)$AOD,
      subset(data, Season.Local2 == season)$PM2.5,
      method = "spearman")
}

#--------------------------------------------------------------
# @desc: Plots Scatterplot of AOD and PM2.5
# @param: dataset
#--------------------------------------------------------------
plot_AOD_PM25_allseasons <- function(data){
  data %>%
    ggplot(aes(x = AOD, y = PM2.5)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
    # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter") +
    ggtitle(paste0("Scatterplot of AOD and PM2.5 in ",
                   data$Site,
                   collapse = NULL))
}

CC_AOD_PM25_allseasons <- function(data){
  cor(data$AOD,data$PM2.5, method = "spearman")
}

#--------------------------------------------------------------
# @desc: Plots Scatterplot of AOD and PM2.5 by Season 
# @param: dataset
#--------------------------------------------------------------
plot_AOD_PM25_OneSite_bySeason_noFacet <- function(data){
  data %>%
    ggplot(aes(x = AOD, y = PM2.5, group = Season.Local, color = Season.Local)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, size = 1, linetype = 1) +
    # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter", color = "Season") +
    ggtitle(paste0("Scatterplot of AOD and PM2.5 by Season in ",
                   data$Site,
                   ", 2014-2017",
                   collapse = NULL)) +
    theme_bw() +
    theme(plot.title = element_text(size = 18, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 0, b = 12, l = 0)),
          text = element_text(size=18),
          axis.title.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))) +
    theme(plot.margin=unit(c(1,1,1,1),"cm")) +
    
    ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\New_York\\AOD_PM2.5\\Scatterplot.png",
           dpi = 300)
}

#--------------------------------------------------------------
# @desc: Plots Scatterplot of AOD and PM2.5 by Season 
# @param: dataset
#--------------------------------------------------------------
plot_AOD_PM25_OneSite_bySeason <- function(data){
  data %>%
    ggplot(aes(x = AOD, y = PM2.5)) +
    geom_point() +
    # geom_smooth(method = "lm", se = FALSE, size = 1.2, linetype = 1, span = 0.4) +
    # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    geom_text(
      data.frame(x=Inf,
                 y=Inf, 
                 label=c(cor(data[which(data$Season.Local2.x==1),]$AOD,
                             data[which(data$Season.Local2.x==1),]$PM2.5, method = "spearman"), 
                         cor(data[which(data$Season.Local2.x==2),]$AOD,
                             data[which(data$Season.Local2.x==2),]$PM2.5, method = "spearman"), 
                         cor(data[which(data$Season.Local2.x==3),]$AOD,
                             data[which(data$Season.Local2.x==3),]$PM2.5, method = "spearman"),
                         cor(data[which(data$Season.Local2.x==4),]$AOD,
                             data[which(data$Season.Local2.x==4),]$PM2.5, method = "spearman")),
                 Season.Local = c("1. Spring", 
                                  "2. Summer", 
                                  "3. Fall",
                                  "4. Winter")),       
      mapping = aes(x = 0.75, 
                    y = 200, 
                    label = paste("Spearman's r = ",round(label,digits = 2))),
      vjust = "inward",
      hjust = "inward",
      size = 5,
      color = "darkblue") +
    
    geom_text(
      data.frame(x=Inf,
                 y=Inf, 
                 label=c(nrow(data[which(data$Season.Local2.x==1),]), 
                         nrow(data[which(data$Season.Local2.x==2),]), 
                         nrow(data[which(data$Season.Local2.x==3),]),
                         nrow(data[which(data$Season.Local2.x==4),])),
                 Season.Local = c("1. Spring", 
                                  "2. Summer", 
                                  "3. Fall",
                                  "4. Winter")),       
      mapping = aes(x = 0.75, 
                    y = 170, 
                    label = paste("Count = ",label)),
      vjust = "inward",
      hjust = "inward",
      size = 5,
      color = "darkblue") +
    facet_wrap(~Season.Local, ncol = 2) +
    labs(x = "AOD", y = "pm2.5 Unit:Micrograms/cubic meter") +
    ggtitle(paste0("Scatterplot of AOD and PM2.5 by Season in ",
                   data$Site,
                   ", 2014-2017",
                   collapse = NULL))
}

#--------------------------------------------------------------
# @desc: Plots Scatterplot of AOD and PM2.5 Morning with time block 
# @param: dataset, timeBlock
#--------------------------------------------------------------
plot_AOD_PM25_OneSite_TimeBlock_BySeason <- function(data, timeBlock){
  data %>%
    ggplot(aes(x = AOD, y = PM2.5)) +
    geom_point(alpha = 0.2, position = "jitter") +
    geom_smooth(method = "lm", se = FALSE, size = 1, linetype = 2, color = "red") +
    # geom_smooth(method = "loess", se = FALSE, size = 1.2, linetype = 1) +
    geom_text(
      data.frame(x=-Inf,
                 y=-Inf,
                 label=c(cor(data[which(data$Season.Local2==1),]$AOD,
                             data[which(data$Season.Local2==1),]$PM2.5, method = "spearman"),
                         cor(data[which(data$Season.Local2==2),]$AOD,
                             data[which(data$Season.Local2==2),]$PM2.5, method = "spearman"),
                         cor(data[which(data$Season.Local2==3),]$AOD,
                             data[which(data$Season.Local2==3),]$PM2.5, method = "spearman"),
                         cor(data[which(data$Season.Local2==4),]$AOD,
                             data[which(data$Season.Local2==4),]$PM2.5, method = "spearman")),
                 Season.Local = c("1. Spring",
                                  "2. Summer",
                                  "3. Fall",
                                  "4. Winter")),
      mapping = aes(x = 1.5,
                    y = 250,
                    label = paste("Spearman's r = ",round(label,digits = 2))),
      vjust = "inward",
      hjust = "inward",
      size = 8,
      color = "darkblue") +
    
    geom_text(
      data.frame(x=Inf,
                 y=Inf,
                 label=c(nrow(data[which(data$Season.Local2==1),]),
                         nrow(data[which(data$Season.Local2==2),]),
                         nrow(data[which(data$Season.Local2==3),]),
                         nrow(data[which(data$Season.Local2==4),])),
                 Season.Local = c("1. Spring",
                                  "2. Summer",
                                  "3. Fall",
                                  "4. Winter")),
      mapping = aes(x = 1.5,
                    y = 200,
                    label = paste("Count = ",label)),
      vjust = "inward",
      hjust = "inward",
      size = 8,
      color = "darkblue") +
    facet_wrap(~Season.Local, ncol = 2) +
    labs(x = "AOD", y = "PM2.5 Unit:Micrograms/cubic meter") +
    ggtitle(paste0(
      paste("Scatterplot of AOD and PM2.5 by Season in",
            data$Site),
      paste(", 2014-2017",
            "at ",
            sep="\n"),
      paste(timeBlock, collapse = " "))
    ) +
    theme_bw() + 
    theme(plot.title = element_text(size = 25, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
          text = element_text(size=25),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
    theme(plot.margin=unit(c(1,1.2,1.5,1),"cm"))
}

#--------------------------------------------------------------
# @desc: Plots Scatterplot of Peak and Average of PM2.5
# @param: dataset
#--------------------------------------------------------------
PM2.5_peak_average<- function(data){
  data %>%
    subset(Average > 0 & Peak > 0) %>%
    ggplot(aes(x = Average, y = Peak)) +
    geom_point(alpha = 0.1) +
    geom_abline(slope = 1, intercept = 0,
                color = "blue",
                linetype = 2) +
    labs(x = "Daily Average Value of PM2.5 [Unit: Micrograms/cubic meter]", 
         y = "Daily Peak Value of PM2.5 [Unit: Micrograms/cubic meter]", 
         color = "Season") +
    scale_x_continuous(name="Daily Average Value of PM2.5 [Unit: Micrograms/cubic meter]", 
                                               limits = c(0, 100),
                                               breaks = c(0,20,40,60,80)) +
    scale_y_continuous(name="Daily Peak Value of PM2.5 [Unit: Micrograms/cubic meter]", 
                       limits=c(0, 100),
                       breaks = c(0,20,40,60,80))+
    ggtitle(paste0("Scatterplot of PM2.5 Daily Average and Peak Values by Season, 2014-2017",
                   collapse = NULL)) +
    theme(plot.title = element_text(size = 12, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
          text = element_text(size=12),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
    theme(plot.margin=unit(c(1,1,1,1),"cm")) 
}
  

#--------------------------------------------------------------
# @desc: Plots Scatterplot of Peak and Average of PM2.5 by Season 
# @param: dataset
#--------------------------------------------------------------
PM2.5_peak_average_bySeason <- function(data){
  data %>%
    subset(Average > 0 & Peak > 0) %>%
    ggplot(aes(x = Average, y = Peak)) +
    geom_point(alpha = 0.1) +
    geom_abline(slope = 1, intercept = 0,
                color = "blue",
                linetype = 2) +
    #geom_smooth(method = "lm", se = FALSE, size = 0.8, linetype = 2, color = "red") +
    geom_text(data = 
                data.frame(x = Inf,
                           y = Inf, 
                           label = c(cor(data[which(data$Season.numeric==1),]$Peak,
                                         data[which(data$Season.numeric==1),]$Average), 
                                     cor(data[which(data$Season.numeric==2),]$Peak,
                                         data[which(data$Season.numeric==2),]$Average), 
                                     cor(data[which(data$Season.numeric==3),]$Peak,
                                         data[which(data$Season.numeric==3),]$Average),
                                     cor(data[which(data$Season.numeric==4),]$Peak,
                                         data[which(data$Season.numeric==4),]$Average)),
                           Season = c("1. Spring", 
                                      "2. Summer", 
                                      "3. Fall",
                                      "4. Winter")),
              mapping = aes(x = 0, 
                            y = 100, 
                            label = paste("Pearson's r = ",round(label,digits = 2))),
              vjust = "inward",
              hjust = "inward",
              size = 5,
              color = "darkblue") +
    
    geom_text(data = 
                data.frame(x = Inf,
                           y = Inf, 
                           count = c(nrow(data[which(data$Season.numeric==1),]),
                                     nrow(data[which(data$Season.numeric==2),]),
                                     nrow(data[which(data$Season.numeric==3),]),
                                     nrow(data[which(data$Season.numeric==4),])), 
                           Season = c("1. Spring", 
                                      "2. Summer", 
                                      "3. Fall",
                                      "4. Winter")),
              mapping = aes(x = 0, 
                            y = 90, 
                            label = paste("Count = ",count)),
              vjust = "inward",
              hjust = "inward",
              size = 5,
              color = "darkblue") +
    facet_wrap(~Season, ncol = 2) +
    scale_x_continuous(name="Daily Average Value of PM2.5 [Unit: Micrograms/cubic meter]", 
                       limits = c(0, 100),
                       breaks = c(0,20,40,60,80,100)) +
    scale_y_continuous(name="Daily Peak Value of PM2.5 [Unit: Micrograms/cubic meter]", 
                       limits=c(0, 100),
                       breaks = c(0,20,40,60,80,100))+
    labs(x = "Daily Average Value of PM2.5 [Unit: Micrograms/cubic meter]", 
         y = "Daily Peak Value of PM2.5 [Unit: Micrograms/cubic meter]", 
         color = "Season") +
    ggtitle(paste0(paste("Scatterplot of PM2.5 Daily Average and Peak Values", 
                         "in New York by Season, 2014-2017",
                         sep = "\n"))) +
    theme_bw() +
    theme(plot.title = element_text(size = 15, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 0, b = 10, l = 0)),
          text = element_text(size=15),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(plot.margin=unit(c(1,1,1,1),"cm")) +
    
    ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\Reno\\Peak_Ave_PM2.5_plot_themesBW.png",
           dpi = 300)
}

#--------------------------------------------------------------
# @desc: Plot of the ratio btw daily PM2.5 Peak and Average along time
# @param: dataset
#--------------------------------------------------------------

PM2.5_ratio_daily_bySeason <- function(data){
  data %>%
    ggplot(aes(x = Date, y = Ratio, color = Season, size = Ratio)) +
    geom_point(alpha = 0.6) +
    #geom_line(color = "royalblue") +
    #geom_smooth(method = "gam", se = FALSE, size = 1.2, linetype = 2, color = "red") +
    #facet_wrap(~Year) +
    labs(x = "Date", 
         y = "Ratio of Daily PM2.5 Peak and Average", 
         color = "Season") +
    ggtitle(paste0("Plot of Ratio of Daily PM2.5 Peak and Average in Reno, 2014-2017 (Peak > 35.4 ug/m^3 and Ratio > 1.5)",
                   collapse = NULL)) +
    theme_bw() +
    theme(plot.title = element_text(size = 15,
                                    face = "bold",
                                    margin = margin(t = 0, r = 0, b = 15, l = 0)),
          text = element_text(size=15),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
    theme(plot.margin=unit(c(1,1,1,1),"cm")) +
    ggsave("C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\R\\Results\\FourSites\\Peak_Ave_PM2.5\\plot.png",
           dpi = 300)
}

