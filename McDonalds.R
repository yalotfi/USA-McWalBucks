
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
## Set up data for cleaning
mcd <- read.csv2("McDonalds_USA_CAN.csv", header = FALSE, sep = ",")  # Bring in the raw data
names(mcd) <- c("long", "lat", "type", "address")  # Rename columns
mcd$type <- as.character(mcd$type)  # column to apply regex must be character

## Regular Exressions are Dope!
mcd$city <- sapply(strsplit(mcd$type, "-"), "[", 2)  # separate store type and location
mcd$type <- sapply(strsplit(mcd$type, "-"), "[", 1)  # save store type which will be cleaned later
mcd$state <- sapply(strsplit(mcd$city, ","), "[", 2)  # separate city from state
mcd$city <- sapply(strsplit(mcd$city, ","), "[", 1)  # save city name

## Fix input errors into factor w/ levels 2, normal McRestaurant and one with playplace
mcd <- within(mcd, {
  
  type[type == "McDonalds]"]     <- "McDonalds"
  type[type == "McDonalds [WM]"] <- "McDonalds"
  type[type == "McDonald [WM]"]  <- "McDonalds"
  type[type == "McDonalds [WM"]  <- "McDonalds"
  type[type == "MdDonalds"]      <- "McDonalds"
  type[type == "McDonalds"]      <- "McDonalds"
  type[type == "McDonalds PlayPlace"]  <- "McDonalds PlayPlace"
  type[type == "McDonalds PlayPlace]"] <- "McDonalds PlayPlace"
  type[type == "McDonalds Play Place"] <- "McDonalds PlayPlace"
  type[type == "McDonalds Playplace"]  <- "McDonalds PlayPlace"
  type[type == "McDonalds PlacePlay"]  <- "McDonalds PlayPlace"
  
})

# levels <- c("McDonalds", "McDonalds PlayPalace")
# mcd$type <- as.factor(mcd$type)

## More Regex to get street address and phone
mcd$phone <- sapply(strsplit(as.character(mcd$address), ", "), "[", 3) # Phone Number
mcd$address <- sapply(strsplit(as.character(mcd$address), ", "), "[", 1) # Street Address

## Set appropriate data types for mcd
mcd$long <- as.numeric(levels(mcd$long))[mcd$long]
mcd$lat <- as.numeric(levels(mcd$lat))[mcd$lat]
mcd$type <- as.factor(mcd$type)
# address, city, and phone number remain as chr strings

## USA Shapefiles
states <- spTransform(readOGR(dsn = "USAshp", layer = "states", stringsAsFactors = FALSE, verbose = FALSE), CRS("+proj=longlat +datum=WGS84"))
statesdf <- states@data # Bring slotted data into dataframe
statesdf <- statesdf[order(statesdf$DRAWSEQ),] # order ID, low to high
statesdf$DRAWSEQ <- as.character(statesdf$DRAWSEQ) # join columns must have same variable type, convert to string
statesFortified <- fortify(states, region = "DRAWSEQ")
statesFortified <- statesFortified[order(as.numeric(statesFortified$id)),]
USAmain <- inner_join(statesdf, statesFortified, by = c("DRAWSEQ" = "id"))
rm(states, statesdf, statesFortified)

## Separate Canada and US McDnlds
statesUS <- unique(USAmain$STATE_ABBR)
mcd$state <- sub("WI ", "WI", mcd$state)
mcd$state <- sub("OR ", "OR", mcd$state)
mcd$state <- sub("PA ", "PA", mcd$state)
statesCAN <- unique(mcd$state)
statediff <- setdiff(statesCAN, statesUS)

## US McDonalds Dataset
mcdUS <- mcd[which(mcd$state == statesUS[1]),] # starting point for loop
i <- 2
for(i in 1:length(statesUS)) 
{
  temp <- mcd[which(mcd$state == statesUS[i]),]
  mcdUS <- rbind(mcdUS, temp)
  i <- i + 1
}
rm(temp, i)

## Canada McDonalds Dataset
mcdCAN <- mcd[which(mcd$state == statediff[1]),] # starting point for loop
i <- 2
for(i in 1:length(statediff)) 
{
  temp <- mcd[which(mcd$state == statediff[i]),]
  mcdCAN <- rbind(mcdCAN, temp)
  i <- i + 1
}
rm(temp, i)

## Remove superfluous datasets
rm(statesUS, statesCAN, statediff)

## Let's not map Alaska or Hawaii
mcdHawaii <- filter(mcdUS, state == "HI")
mcdAlaska <- filter(mcdUS, state == "AK")
mcdUSmain <- filter(mcdUS, state != "HI" & state != "AK")
rm(mcd, mcdUS)

## Add ID column and Data Type fix
mcdUSmain$id <- 1:nrow(mcdUSmain)
mcdUSmain$state <- as.factor(mcdUSmain$state)
mcdCAN$id <- 1:nrow(mcdCAN)
mcdCAN$city <- as.factor(mcdCAN$city)
mcdHawaii$id <- 1:nrow(mcdHawaii)
mcdHawaii$city <- as.factor(mcdHawaii$city)
mcdAlaska$id <- 1:nrow(mcdAlaska)
mcdAlaska$city <- as.factor(mcdAlaska$city)


#############
## Mapping ##
#############
## Basic tools
bbox <- c(-130,25, -65, 50) # Set Bounding Box Limits for mainland USA
waterMap <- get_map(location = bbox, zoom = 4, maptype = "watercolor") # Had to work without Internet connection

McMap <- waterMap
McMap <- ggmap(McMap)
McMap <- McMap + geom_point(aes(long, lat), data = mcdUSmain, color = "orangered4")
McMap <- McMap + ggtitle("McDonalds Location Across the USA")
McMap <- McMap + BWtheme(base_size = 24)
ggsave("Output/McMap.png", McMap)

## Help with nicer map
alpha_range = c(0.14, 0.75)
size_range = c(0.134, 0.173)

## Create a title with subtitle in ggplot/ggmap
ggtitle_subtitle = function(title, subtitle = "") {
  
  ggtitle(bquote(atop(bold(.(title)), atop(.(subtitle)))))
  
}

## Black Theme Map
fontCheck <- names(pdfFonts())

BlackTheme = function(base_size = 24) {
  
  theme_bw(base_size) +
    theme(text = element_text(color = "#ffffff"),
          rect = element_rect(fill = "#000000", color = "#000000"),
          plot.background = element_rect(fill = "#000000", color = "#000000"),
          panel.background = element_rect(fill = "#000000", color = "#000000"),
          plot.title = element_text(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  
}

# Mainland USA
McUS <- ggplot()
McUS <- McUS + geom_polygon(data = USAmain, aes(x = long, y = lat, group = group), fill = "#080808", color = "#080808")
McUS <- McUS + geom_point(data = mcdUSmain, aes(x = long, y = lat, alpha = id, size = id), color = "gold")
McUS <- McUS + scale_alpha_continuous(range  = alpha_range, trans = "log", limits = range(mcdUSmain$id))
McUS <- McUS + scale_size_continuous(range  = size_range, trans = "log", limits = range(mcdUSmain$id))
McUS <- McUS + coord_map(xlim = range(mcdUSmain$long), ylim = range(mcdUSmain$lat))
McUS <- McUS + ggtitle_subtitle("McMerica", "14,000 McDonalds Locations in the USA")  # Problem with fonts
McUS <- McUS + BWtheme(base_size = 24)
McUS <- McUS + theme(legend.position = "none")
ggsave("Output/McMerica.png", McUS)


