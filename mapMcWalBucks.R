
rm(list = ls())

#####################
### Plotting Maps ###
#####################

source("mapHelpers.R") # Basic Map Plotting Functions
source("sourcePackages.R") # call necessary libraries, probably redundant since they are called in cleaning scripts

## Plot McMerica ##
source("cleanMcDonalds.R")

McUS <- ggplot()
McUS <- McUS + geom_polygon(data = USAmain, aes(x = long, y = lat, group = group), fill = "#080808", color = "#080808")
McUS <- McUS + geom_point(data = mcdUSmain, aes(x = long, y = lat, alpha = id, size = id), color = "gold")
McUS <- McUS + scale_alpha_continuous(range  = alpha_range, trans = "log", limits = range(mcdUSmain$id))
McUS <- McUS + scale_size_continuous(range  = size_range, trans = "log", limits = range(mcdUSmain$id))
McUS <- McUS + coord_map(xlim = range(mcdUSmain$long), ylim = range(mcdUSmain$lat))
McUS <- McUS + ggtitle_subtitle("McMerica", "14,000 McDonalds Locations in the USA")  # Problem with fonts
McUS <- McUS + BlackTheme(base_size = 24)
McUS <- McUS + theme(legend.position = "none")
ggsave("Output/McMerica.png", McUS)


## Starbucks
source("cleanStarbucks.R")

sbUSA <- ggplot()
sbUSA <- sbUSA + geom_polygon(data = USAmain, aes(x = long, y = lat, group = group), fill = "#000000", color = "#000000")
sbUSA <- sbUSA + geom_point(data = starbucksmain, aes(x = long, y = lat, alpha = id, size = id), color = "#00704a")
sbUSA <- sbUSA + scale_alpha_continuous(range  = alpha_range, trans = "log", limits = range(starbucksmain$id))
sbUSA <- sbUSA + scale_size_continuous(range  = size_range, trans = "log", limits = range(starbucksmain$id))
sbUSA <- sbUSA + coord_map(xlim = range(starbucksmain$long), ylim = range(starbucksmain$lat))
sbUSA <- sbUSA + ggtitle_subtitle("Starbucks Runs America", "10,000 Starbucks Locations in the USA")
sbUSA <- sbUSA + BlackTheme(base_size = 24)
sbUSA <- sbUSA + theme(legend.position = "none")
ggsave("Output/Starbucks.png", sbUSA)


## Walmart
source("cleanWalmart.R")

US <- ggplot()
US <- US + geom_polygon(data = USAmain, aes(x = long, y = lat, group = group), fill = "#000000", color = "#000000")
US <- US + geom_point(data = wmData, aes(x = long, y = lat, alpha = id, size = id), color = "dodgerblue")
US <- US + scale_alpha_continuous(range  = alpha_range, trans = "log", limits = range(wmData$id))
US <- US + scale_size_continuous(range  = size_range, trans = "log", limits = range(wmData$id))
US <- US + scale_color_manual(values = c("yellow", "green", "gray", "red", "blue"))
US <- US + coord_map(xlim = range(USAmain$long), ylim = range(USAmain$lat))
US <- US + ggtitle_subtitle("3,000 Walmart Locations", "Built Between 1962 and 2006")  # Problem with fonts
US <- US + BlackTheme(base_size = 24)
US <- US + theme(legend.position = "none")
# ggsave("Output/WalMartUSA.pdf", US)
ggsave("Output/WalMartUSA.png", US)


