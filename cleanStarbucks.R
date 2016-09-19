###################
## Data Cleaning ##
###################
starbucks <- read.csv("Data/Starbucks.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
names(starbucks) <- c("long", "lat", "storename", "address")
starbucks$id <- 1:nrow(starbucks)

## USA Shapefiles
states <- spTransform(readOGR(dsn = "USAshp", layer = "states", stringsAsFactors = FALSE, verbose = FALSE), 
                      CRS("+proj=longlat +datum=WGS84"))

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
