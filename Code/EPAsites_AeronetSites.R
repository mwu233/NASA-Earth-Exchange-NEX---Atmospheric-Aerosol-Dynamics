# Author: Meiliu Wu
# Date: 06/22/2018

# open the aeronet data in 2014

fileToLoad <- file.choose(new = TRUE)
readLines(fileToLoad, n=10)
aeronet_ames <- read.table(fileToLoad, sep=",", header=TRUE, skip=6)
names(aeronet_ames)
# map the point data with unique sites
AmesSites <- aeronet_ames[!duplicated(aeronet_ames$Site_Latitude.Degrees.), ]

#--------------------------------------------------------------
fileToLoad <- file.choose(new = TRUE)
aeronet_CCNY <- read.table(fileToLoad, sep=",", header=TRUE, skip=6)
# map the point data with unique sites
CCNYSites <- aeronet_CCNY[!duplicated(aeronet_CCNY$Site_Latitude.Degrees.), ]

#--------------------------------------------------------------
fileToLoad <- file.choose(new = TRUE)
aeronet_CCNY <- read.table(fileToLoad, sep=",", header=TRUE, skip=6)
# map the point data with unique sites
CCNYSites <- aeronet_CCNY[!duplicated(aeronet_CCNY$Site_Latitude.Degrees.), ]

#--------------------------------------------------------------
fileToLoad <- file.choose(new = TRUE)
aeronet_CCNY <- read.table(fileToLoad, sep=",", header=TRUE, skip=6)
# map the point data with unique sites
CCNYSites <- aeronet_CCNY[!duplicated(aeronet_CCNY$Site_Latitude.Degrees.), ]

#--------------------------------------------------------------
fileToLoad <- file.choose(new = TRUE)
aeronet_CCNY <- read.table(fileToLoad, sep=",", header=TRUE, skip=6)
# map the point data with unique sites
CCNYSites <- aeronet_CCNY[!duplicated(aeronet_CCNY$Site_Latitude.Degrees.), ]

Aeronet_sites <- rbind(AmesSites, CCNYSites) 
# write the unique sites as an csv table
# rename
# colnames(uniqueSites) <- c("StateCode","CountyCode","SiteNum","Longitude","Latitude")
write.csv(AmesSites,file = file.choose(new = T),row.names = FALSE)

# Date: 06/25/2018 update
# Time formatting
library(stringr)
dateMatrix <- str_split_fixed(aeronet_ames$Date.dd.mm.yyyy., ":", 3)
date_day <- data.frame(dateMatrix[,1])
date_month <- data.frame(dateMatrix[,2])
date_year <- data.frame(dateMatrix[,3])
dateMatrix <- as.data.frame(cbind(date_day,date_month,date_year))
names(dateMatrix)
dateMatrix <- within(dateMatrix,  correctFormat <- paste(dateMatrix...2., dateMatrix...1., dateMatrix...3., sep="-"))

aeronet_ames$Date.dd.mm.yyyy. <- dateMatrix$correctFormat

