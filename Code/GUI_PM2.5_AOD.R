
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
# install.packages("shiny")

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
library(shiny)

#--------------------------------------------------------------
# input and output functions are put into fluidPage
# this fluidPage interprets functions as html
#--------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Diurnal Analysis of AOD and PM2.5 by Season"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "location", 
                  label = "Select a location: ",
                  choices = list(
                    c("St. Louis, MO"),
                    c("Reno, NV"),
                    'East Coast' = c("New York, NY","Balitmore, MD")),
                  
                  selected = "New York, NY",
                  width = "100%",
                  multiple = FALSE),
      
      selectInput(inputId = "timeBlock", 
                  label = "Select time blocks: ",
                  choices = c("07:00","08:00","09:00",
                              "10:00","11:00","12:00",
                              "13:00","14:00","15:00",
                              "16:00","17:00","18:00",
                              "19:00","20:00"),
                  width = "100%",
                  multiple = TRUE),
      
      # update the dataset and then plot
      actionButton(inputId = "update",
                   label = "Update"),
      
      # download the plot 
      downloadButton(outputId = "download", label = "Download the plot")
      
    ),
    
    mainPanel(
      # plot
      plotOutput("scatterPlot",
                 width = 800, 
                 height = 800)
    )
  )
)


#--------------------------------------------------------------
# Tell the server how to assemble inputs into outputs in ui(fluidPage)
#--------------------------------------------------------------
server <- function(input, output){
  
  # the button will update the data 
  data <- eventReactive(input$update,{
    if(input$location == "New York, NY"){
      AOD_PM_NY[AOD_PM_NY$Hour.Local %in% input$timeBlock,]
    }
    else if (input$location == "Balitmore, MD"){
      AOD_PM_Ba[AOD_PM_Ba$Hour.Local %in% input$timeBlock,]
    }
    else if (input$location == "Reno, NV"){
      AOD_PM_Reno[AOD_PM_Reno$Hour.Local %in% input$timeBlock,]
    }
    else if (input$location == "St. Louis, MO"){
      AOD_PM_StLouis[AOD_PM_StLouis$Hour.Local %in% input$timeBlock,]
    }
  })
  
  plotFun = function(){
    plot_AOD_PM25_OneSite_TimeBlock_BySeason(data(),input$timeBlock)
  }
  
  # scatterplot of the data specified with location and time blocks 
  output$scatterPlot <- renderPlot(
    {
      plotFun()
    }
  )
  
  # save the plot 
  output$download <- downloadHandler(
    
    filename = function() {
      paste(data()$Site, "_plot.png", sep = "")
    },
    
    # content writes the plot to the device
    content = function(file) {
      plot_test = plotFun()
      device <- function(..., width, height) {
        grDevices::png(..., width = 1000, height = 1000, units = "px")
      }
      ggsave(file, plot_test, device = device, width = 10, height = 10, dpi = 300, units = "in")
    },
    contentType = "image/png"
    
  )
}

#--------------------------------------------------------------
# Run the shiny app
#--------------------------------------------------------------
shinyApp(ui = ui, server = server)

