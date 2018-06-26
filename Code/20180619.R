# Author: Meiliu Wu
# Date: 06/19/2018

# open the daily data

install.packages("ggmap")
install.packages("ggplot2")

#load ggmap
library(ggmap)
library(ggplot2)

#--------------------------------------------------------------
# data pre-processig
# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <- read.csv(fileToLoad, stringsAsFactors = FALSE)
names(origAddress)

origAddress$State.Code = as.numeric(origAddress$State.Code)
origAddress$County.Code = as.numeric(origAddress$County.Code)
origAddress$Site.Num = as.numeric(origAddress$Site.Num)

origAddress$Latitude = as.numeric(origAddress$Latitude)
origAddress$Longitude = as.numeric(origAddress$Longitude)
origAddress$Sample.Measurement = as.numeric(origAddress$Sample.Measurement)

# Initialize the data frame
stateCode <- data.frame(origAddress$State.Code)
countyCode <- data.frame(origAddress$County.Code)
siteNum <- data.frame(origAddress$Site.Num)
lat <-data.frame(origAddress$Latitude)
lon <-data.frame(origAddress$Longitude)
PMvalue <-data.frame(origAddress$Sample.Measurement)
df <- as.data.frame(cbind(stateCode,countyCode,siteNum,lon,lat,PMvalue))
df <- as.data.frame(cbind(stateCode,countyCode,siteNum,lon,lat))

#--------------------------------------------------------------
# map the point data with unique sites
uniqueSites <- df[!duplicated(df$origAddress.Latitude), ]

# San Francisco
sanFrancisco_center = as.numeric(geocode("San Francisco"))
ggmap(get_googlemap(center = sanFrancisco_center, 
                    zoom = 9,
                    size = c(640, 640), 
                    scale = 2,
                    maptype = "roadmap",
                    extent = "panel")) + 
  geom_point(aes(x = uniqueSites$origAddress.Longitude, y = uniqueSites$origAddress.Latitude), 
             data = uniqueSites, 
             alpha = .5, 
             color="darkred")

#--------------------------------------------------------------
# write the unique sites as an csv table
#uniqueSites$origAddress.Sample.Measurement <- NULL
#head(uniqueSites)
colnames(uniqueSites) <- c("StateCode","CountyCode","SiteNum","Longitude","Latitude")
write.csv(uniqueSites,file = file.choose(new = T),row.names = FALSE)

#--------------------------------------------------------------
# data analysis
colnames(df) <- c("StateCode","CountyCode","SiteNum","Longitude","Latitude","PMValue")
