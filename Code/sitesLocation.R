# Author: Meiliu Wu
# Date: 06/19/2018

install.packages("ggmap")
install.packages("ggplot2")

#load ggmap
library(ggmap)
library(ggplot2)

# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <- read.csv(fileToLoad, stringsAsFactors = FALSE)
names(origAddress)

origAddress$Latitude = as.numeric(origAddress$Latitude)
origAddress$Longitude = as.numeric(origAddress$Longitude)
origAddress$Land.Use = as.character(origAddress$Land.Use)
origAddress$Location.Setting = as.character(origAddress$Location.Setting)
origAddress$State.Name = as.character(origAddress$State.Name)
origAddress$City.Name = as.character(origAddress$City.Name)

# Initialize the data frame
lat <-data.frame(origAddress$Latitude)
lon <-data.frame(origAddress$Longitude)
LandUse <-data.frame(origAddress$Land.Use)
Setting <-data.frame(origAddress$Location.Setting)
State <-data.frame(origAddress$State.Name)
City <-data.frame(origAddress$City.Name)

df <- as.data.frame(cbind(lon,lat,LandUse,Setting,State,City))
df = df[df$origAddress.Longitude != 0.0 & df$origAddress.Latitude != 0 ,] 

# map the point data
#usa_center = as.numeric(geocode("United States"))
#USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal")

# Houston
houston_center = as.numeric(geocode("Houston"))
ggmap(get_googlemap(center = houston_center, 
                    #zoom = "auto",
                    size = c(640, 640), 
                    #scale = 2,
                    maptype = "roadmap",
                    extent = "panel")) + 
  geom_point(aes(x = df$origAddress.Longitude, y = df$origAddress.Latitude), 
             data = df, 
             alpha = .5, 
             color="darkred")

# San Francisco
sanFrancisco_center = as.numeric(geocode("San Francisco"))
ggmap(get_googlemap(center = sanFrancisco_center, 
                    zoom = 9,
                    size = c(640, 640), 
                    scale = 2,
                    maptype = "roadmap",
                    extent = "panel")) + 
  geom_point(aes(x = df$origAddress.Longitude, y = df$origAddress.Latitude), 
             data = df, 
             alpha = .5, 
             color="darkred")
