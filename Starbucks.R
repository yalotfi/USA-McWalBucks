
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


###################
## Data Cleaning ##
###################
starbucks <- read.csv("Starbucks.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
names(starbucks) <- c("long", "lat", "storename", "address")
starbucks$id <- 1:nrow(starbucks)

## USA Shapefiles
states <- spTransform(readOGR(dsn = "USAshp", layer = "states", stringsAsFactors = FALSE, verbose = FALSE), CRS("+proj=longlat +datum=WGS84"))
statesdf <- states@data # Bring slotted data into dataframe
statesdf <- statesdf[order(statesdf$DRAWSEQ),] # order ID, low to high
statesdf$DRAWSEQ <- as.character(statesdf$DRAWSEQ) # join columns must have same variable type, convert to string
statesFortified <- fortify(states, region = "DRAWSEQ")
statesFortified <- statesFortified[order(as.numeric(statesFortified$id)),]
USAmain <- inner_join(statesdf, statesFortified, by = c("DRAWSEQ" = "id"))
rm(states, statesdf, statesFortified)

states <- unique(USAmain$STATE_ABBR) # Filter based on this list of state abbreviations

## Regex to Get US states
starbucks$state <- sapply(strsplit(starbucks$address, ", "), "[", 2) # Pattern consistent for US locations, not Canadian ones due to format
starbucks$state <- sapply(strsplit(starbucks$state, " "), "[", 1)

## Create Dataset for US locations only
i <- 1
for(i in 1:length(states))
{
  if(!exists("starbucksUSA"))
  {
    starbucksUSA <- subset(starbucks, starbucks$state == states[i])
  }
  if(exists("starbucksUSA"))
  {
    temp <- subset(starbucks, starbucks$state == states[i])
    starbucksUSA <- rbind.data.frame(starbucksUSA, temp)
    rm(temp)
  }
  i <- i + 1
}
rm(i, states)

## Geographic subsets into Alaska, Hawaii, Mainland USA, Canada
starbucksUSA$state <- as.factor(starbucksUSA$state)
starbucksHI <- filter(starbucksUSA, starbucksUSA$state == "HI")
starbucksAK <- filter(starbucksUSA, starbucksUSA$state == "AK")
starbucksmain <- filter(starbucksUSA, starbucksUSA$state != "HI" & starbucksUSA$state != "AK")

#############
## MAPPING ##
#############
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
sbUSA <- ggplot()
sbUSA <- sbUSA + geom_polygon(data = USAmain, aes(x = long, y = lat, group = group), fill = "#080808", color = "#080808")
sbUSA <- sbUSA + geom_point(data = starbucksmain, aes(x = long, y = lat, alpha = id, size = id), color = "forestgreen")
sbUSA <- sbUSA + scale_alpha_continuous(range  = alpha_range, trans = "log", limits = range(starbucksmain$id))
sbUSA <- sbUSA + scale_size_continuous(range  = size_range, trans = "log", limits = range(starbucksmain$id))
sbUSA <- sbUSA + coord_map(xlim = range(starbucksmain$long), ylim = range(starbucksmain$lat))
sbUSA <- sbUSA + ggtitle_subtitle("Starbucks Runs America", "10,000 Starbucks Locations in the USA")
sbUSA <- sbUSA + BlackTheme(base_size = 24)
sbUSA <- sbUSA + theme(legend.position = "none")
ggsave("Output/Starbucks.png", sbUSA)
