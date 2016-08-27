
rm(list = ls())

##############
## Packages ##
##############
## Basic Packages
library(ggplot2)
library(ggmap)
library(dplyr)

## GIS Packages
library(raster)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(maptools)


###########################
## McDonald's Data Mngmt ##
###########################
## Set up data for manipulation
mcd <- read.csv2("McDonalds_USA_CAN.csv", header = FALSE, sep = ",")  # Bring in the raw data
names(mcd) <- c("long", "lat", "type", "address")  # Rename columns
mcd$type <- as.character(mcd$type)  # column to apply regex must be character

## Regular Exressions are Dope!
mcd$city  <- sapply(strsplit(mcd$type, "-"), "[", 2)  # separate store type and location
mcd$type  <- sapply(strsplit(mcd$type, "-"), "[", 1)  # save store type which will be cleaned later
mcd$state <- sapply(strsplit(mcd$city, ","), "[", 2)  # now let's separate city from state
mcd$city  <- sapply(strsplit(mcd$city, ","), "[", 1)  # save city name

## Fix input errors into factor w/ levels 2, normal McRestaurant and one with playplace
mcd <- within(mcd, {
  
  type[type == "McDonalds]"]           <- "McDonalds"
  type[type == "McDonalds [WM]"]       <- "McDonalds"
  type[type == "McDonald [WM]"]        <- "McDonalds"
  type[type == "McDonalds [WM"]        <- "McDonalds"
  type[type == "MdDonalds"]            <- "McDonalds"
  type[type == "McDonalds"]            <- "McDonalds"
  type[type == "McDonalds PlayPlace"]  <- "McDonalds PlayPlace"
  type[type == "McDonalds PlayPlace]"] <- "McDonalds PlayPlace"
  type[type == "McDonalds Play Place"] <- "McDonalds PlayPlace"
  type[type == "McDonalds Playplace"]  <- "McDonalds PlayPlace"
  type[type == "McDonalds PlacePlay"]  <- "McDonalds PlayPlace"
  
})

## More Regex to get street addres, city, state, and phone
mcd$phone <- sapply(strsplit(as.character(mcd$address), ", "), "[", 3)
mcd$address <- sapply(strsplit(as.character(mcd$address), ", "), "[", 1)

## Fix data types
mcd$long  <- as.numeric(levels(mcd$long))[mcd$long]
mcd$lat   <- as.numeric(levels(mcd$lat))[mcd$lat]
mcd$type  <- as.factor(mcd$type)
mcd$state <- as.factor(mcd$state)

####################
## USA Shapefiles ##
####################
states   <- spTransform(readOGR(dsn = "USAshp", layer = "states", stringsAsFactors = FALSE, verbose = FALSE), CRS("+proj=longlat +datum=WGS84"))
statesdf <- states@data # Bring slotted data into dataframe
statesdf <- statesdf[order(statesdf$DRAWSEQ),] # order ID, low to high
statesdf <- filter(statesdf, STATE_ABBR != "HI" & STATE_ABBR != "AK")  # Remove states out of bounds
statesdf$DRAWSEQ <- as.character(statesdf$DRAWSEQ) # join columns must have same variable type, convert to string
statesFortified  <- fortify(states, region = "DRAWSEQ")
statesFortified  <- statesFortified[order(as.numeric(statesFortified$id)),]
USAmain <- inner_join(statesdf, statesFortified, by = c("DRAWSEQ" = "id"))
rm(states, statesdf, statesFortified)

## Remove spaces in State abbreviations
USAmain$STATE_ABBR <- gsub(" ", "", USAmain$STATE_ABBR, fixed = T)
mcd$state          <- gsub(" ", "", mcd$state, fixed = T)

## Remove Canada from data
statesUS  <- unique(USAmain$STATE_ABBR)
statesCAN <- unique(mcd$state)
statediff <- setdiff(statesCAN, statesUS)
mcd <- unique(mcd[mcd$state %in% statesUS,])
rm(statesUS, statediff, statesCAN)

## Add id column
mcd$id    <- 1:nrow(mcd)


########################
## Visualize McMerica ##
########################

source("NicerMaps.R")  ## Procedure to make functions and values for blank slate for dot map

## Build Map
US <- ggplot()
US <- US + geom_polygon(data = USAmain, aes(x = long, y = lat, group = group), fill = "#000505", color = "#000505")
US <- US + geom_point(data = mcd, aes(x = long, y = lat, alpha = id, size = id), color = "gold")
US <- US + scale_alpha_continuous(range  = alpha_range, trans = "log", limits = range(mcd$id))
US <- US + scale_size_continuous(range  = size_range, trans = "log", limits = range(mcd$id))
US <- US + scale_color_manual(values = c("yellow", "green", "gray", "red", "blue"))
US <- US + coord_map(xlim = range(USAmain$long), ylim = range(USAmain$lat))
US <- US + ggtitle_subtitle("McMerica", "Our Nation of 14,000 McDonald's")  # Problem with fonts
US <- US + BWtheme(base_size = 24)
US <- US + theme(legend.position = "none")
ggsave("Output/McMerica.pdf", US)
ggsave("Output/McMerica.png", US)



