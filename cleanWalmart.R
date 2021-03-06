source("sourcePackages.R")

########################
## Parse Walmart Data ##
########################
## Bring Data and Make Helper
wmData <- readLines("data/walmarts.xml") # view raw data to see variables
strsplits <- function(x, splits, ...) {   # Rewrite strsplit() function
  for (split in splits) {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}

## name, cha NOTE: Name and Address Same
name <- regmatches(wmData, regexpr("<name>.+</name>", wmData))
name <- strsplits(name, c("<name>","</name>"))

## postal_code, num NOTE: Fewer observations than rest, not necessary bc plenty of spatial data, could find missing ones
postalCode <- regmatches(wmData, regexpr("<postal_code>.+</postal_code>", wmData))
rm(postalCode)

## opening_date, num
year <- regmatches(wmData, regexpr("<opening_date>.+</opening_date>", wmData))
year <- as.numeric(strsplits(year, c("<opening_date>"," </opening_date>")))

## latitude, num
lat <- regmatches(wmData, regexpr("<latitude>.+</latitude>", wmData))
lat <- as.numeric(strsplits(lat, c("<latitude>","</latitude>")))

## longitude, num
long <- regmatches(wmData, regexpr("<longitude>.+</longitude>", wmData))
long <- as.numeric(strsplits(long, c("<longitude>","</longitude>")))

## Create Data Frame
wmData <- cbind.data.frame(name, year, lat, long)
wmData$id <- 1:nrow(wmData)
rm(name, year, lat, long)

## Bring in US Spatial Data to df
states <- spTransform(readOGR(dsn = "shapefiles", layer = "states", stringsAsFactors = FALSE, verbose = FALSE), CRS("+proj=longlat +datum=WGS84"))
statesdf <- states@data # Bring slotted data into dataframe
statesdf <- statesdf[order(statesdf$DRAWSEQ),] # order ID, low to high
statesdf <- filter(statesdf, STATE_ABBR != "HI" & STATE_ABBR != "AK")  # Remove states out of bounds
statesdf$DRAWSEQ <- as.character(statesdf$DRAWSEQ) # join columns must have same variable type, convert to string
statesFortified <- fortify(states, region = "DRAWSEQ")
statesFortified <- statesFortified[order(as.numeric(statesFortified$id)),]
USAmain <- inner_join(statesdf, statesFortified, by = c("DRAWSEQ" = "id"))
rm(states, statesdf, statesFortified)


######################
## Explore the Data ##
######################
Growth <- wmData$year
time <- max(Growth) - min(Growth) # 44 years
pdf("Output/Walmart_Histogram.pdf")
wmHist <- hist(Growth, breaks = time, freq = TRUE) # Get sense of large growth periods for Walmart (How many locations opened each year?)
dev.off()
rm(Growth, time, wmHist)

## Breakdown expansion by each decade since 1962
attach(wmData)
wm1970 <- subset(wmData, year < 1970)
wm1980 <- subset(wmData, year < 1980 & year >= 1970)
wm1990 <- subset(wmData, year < 1990 & year >= 1980)
wm2000 <- subset(wmData, year < 2000 & year >= 1990)
wm2010 <- subset(wmData, year < 2010 & year >= 2000)
detach(wmData)


#################
## Map Helpers ##
#################
## Basic tools
bbox <- c(-130,25, -65, 50) # Set Bounding Box Limits for mainland USA
waterMap <- get_map(location = bbox, zoom = 4, maptype = "watercolor") # Had to work without Internet connection


##################
## Map Building ##
##################
## All Locations
# pdf("Output/allWM.pdf")
# WMmap <- waterMap
# WMmap <- ggmap(WMmap)
# WMmap <- WMmap + geom_point(aes(long, lat), data = wmData, color = "blue")
# WMmap <- WMmap + ggtitle("All Walmart Locations Between 1962 and 2006")
# WMmap
# dev.off()

## 1962 - 1970
pdf("Output/WM1970.pdf")
map1970 <- waterMap
map1970 <- ggmap(map1970)
map1970 <- map1970 + geom_point(aes(long, lat), data = wm1970, color = "blue")
map1970 <- map1970 + ggtitle("Walmarts Built Between 1962 and 1970")
map1970
dev.off()

## 1970 - 1980
pdf("Output/WM1980.pdf")
map1980 <- waterMap
map1980 <- ggmap(map1980)
map1980 <- map1980 + geom_point(aes(long, lat), data = wm1980, color = "blue")
map1980 <- map1980 + ggtitle("Walmarts Built Between 1970 and 1980")
map1980
dev.off()

## 1980 - 1990
pdf("Output/WM1990.pdf")
map1990 <- waterMap
map1990 <- ggmap(map1990)
map1990 <- map1990 + geom_point(aes(long, lat), data = wm1990, color = "blue")
map1990 <- map1990 + ggtitle("Walmarts Built Between 1980 and 1990")
map1990
dev.off()

## 1990 - 2000
pdf("Output/WM2000.pdf")
map2000 <- waterMap
map2000 <- ggmap(map2000)
map2000 <- map2000 + geom_point(aes(long, lat), data = wm2000, color = "blue")
map2000 <- map2000 + ggtitle("Walmarts Built Between 1990 and 2000")
map2000
dev.off()

## 2000 - 2010
pdf("Output/WM2010.pdf")
map2010 <- waterMap
map2010 <- ggmap(map2010)
map2010 <- map2010 + geom_point(aes(long, lat), data = wm2010, color = "blue")
map2010 <- map2010 + ggtitle("Walmarts Built Between 2000 and 2010")
map2010
dev.off()

## remove subsets + maps objs
rm(wm1970, wm1980, wm1990, wm2000, wm2010) # remove subsets
rm(map1970, map1980, map1990, map2000, map2010) # remove maps

## remove objs
rm(bbox, waterMap)

#####################
## Spatial Density ##
#####################
## Create Point Pattern Window
# states <- spTransform(readOGR(dsn = "USAshp", layer = "states", stringsAsFactors = FALSE, verbose = FALSE), CRS("+proj=longlat +datum=WGS84"))
# states@data <- filter(states@data, STATE_ABBR != "HI" & STATE_ABBR != "AK")
# win <- as(states,"owin")
# 
# ## Create Point Pattern Class Objs
# pppWM <- ppp(x = wmData$long, y = wmData$lat, window = win, marks = wmData$year)
# plot(density(pppWM))
# 
# ## remove objs
# rm(states, win, pppWM)




