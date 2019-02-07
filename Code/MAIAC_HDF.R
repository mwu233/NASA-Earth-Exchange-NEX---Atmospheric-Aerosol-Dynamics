
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
# install.packages("raster")
# install.packages("MODIS")

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
library(raster)
library(MODIS)
library(gdalUtils)

#--------------------------------------------------------------------------#
# https://stackoverflow.com/questions/36772341/reading-hdf-files-into-r-and-converting-them-to-geotiff-rasters
#--------------------------------------------------------------------------#

# read in all SDS strings (layer descriptors)
# using the MODIS package
filename <- "C:\\Users\\Meiliu\\Documents\\UW_Study_Work\\2018Summer\\NASA\\Atmospheric Aerosol Dynamics\\Data\\MODIS_Data\\MOD04_3K.A2015190.1825.061.2017321175335.pscs_000501246217.hdf"
sds <- get_subdatasets(filename)
sds

# my_hdf_layer <- readGDAL(sds[12])
# my_hdf_layer

# interested the 12th subdataset - Image_Optical_Depth_Land_And_Ocean
# use gdal_translate to convert it to a .tif
setwd("~/UW_Study_Work/2018Summer/NASA/Atmospheric Aerosol Dynamics/R/Results/HDF")
gdal_translate(sds[12], dst_dataset = "AOD.tif", of = "GTiff")

# Load and plot the new .tif
rast <- raster("AOD.tif")
plot(rast,
     main = "Image Optical Depth Land And Ocean",
     xlab="Longitude",
     ylab="Latitude",
     col=rev(heat.colors(10)))
rast
crs(rast) <- CRS('+init=EPSG:4326')
rast
plot(rast)

xp <- projectRaster(rast, crs = "+proj=utm +zone=11 +datum=WGS84") 
plot(xp)
