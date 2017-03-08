library(dplyr)
library(shiny)
library(ggplot2)
library(ggiraph)
library(rgdal)
library(leaflet)

# initialize food world cup data frame
food <- read.csv("Data/food-world-cup-data.csv", stringsAsFactors = FALSE, fileEncoding = "cp932")
#cleaning up column names
food <- food[,2:ncol(food)]
food.colnames <- colnames(food)
food.colnames <- gsub('Please.rate.how.much.you.like.the.traditional.cuisine.of.', '', food.colnames)
countries.names <- gsub('[.]', '', food.colnames[3:42])
colnames(food) <- c('level.of.knowledge', 'interest', countries.names, 'Gender', 'Age', 'Household.Income', 'Education', 'census.region')


colnames(food)[2] <- "Cuisine.Knowledge"

# initialize US map
us.map <-  map_data('state')
regions <- c('New England', 'Middle Atlantic', 'East North Central', 'West North Central', 'South Atlantic', 'East South Central', 'West South Central', 'Mountain', 'Pacific')

# add US Census Regions to us.maps data frame
us.map$census.region[us.map$region %in% 
                       c("maine", "vermont", "new hampshire", "massachusetts", "connecticut", "rhode island")] <- "New England"
us.map$census.region[us.map$region %in% 
                       c("new jersey", "new york", "pennsylvania")] <- "Middle Atlantic"
us.map$census.region[us.map$region %in% 
                       c("illinois", "indiana", "michigan", "ohio", "wisconsin")] <- "East North Central"
us.map$census.region[us.map$region %in% 
                       c("iowa", "kansas", "minnesota", "missouri", "nebraska", "north dakota", "south dakota")] <- "West North Central"
us.map$census.region[us.map$region %in% 
                       c("delaware", "district of columbia", "maryland",
                         "west virginia", "virginia", "north carolina", "south carolina", "georgia", "florida")] <- "South Atlantic"
us.map$census.region[us.map$region %in% 
                       c("alabama", "kentucky", "mississippi", "tennessee")] <- "East South Central"
us.map$census.region[us.map$region %in% 
                       c("arkansas", "louisiana", "oklahoma", "texas")] <- "West South Central"
us.map$census.region[us.map$region %in% 
                       c("arizona", "montana", "idaho", "wyoming", "utah", "colorado", "nevada", "new mexico")] <- "Mountain"
us.map$census.region[us.map$region %in% 
                       c("washington", "oregon", "alaska", "california", "hawaii")] <- "Pacific"

# subset the dataframe by census regions and move lat/lon accordingly
for(region in regions){
  us.map$lat.transp[us.map$census.region == region] <- us.map$lat[us.map$census.region == region]
  us.map$long.transp[us.map$census.region == region] <- us.map$long[us.map$census.region == region]
}

#getting lat/long 
GetRegionRange <- function(region.name) {
  region.coord <- us.map %>% select(long, lat, census.region) %>% unique()
  region <- region.coord %>% filter(census.region == region.name)
  region.range <- data.frame(long = range(region$long), 
                              lat = range(region$lat), 
                              census.region = c(region.name))
  return(region.range)
}

#data frame of all regions ranges
region.range <- data.frame()
for(region in regions) {
  region.range <- rbind(region.range, GetRegionRange(region))
}

# creates list of labels for each of the census regions
regs <- aggregate(cbind(long.transp, lat.transp) ~ census.region, data=us.map, 
                  FUN=function(x)mean(range(x)))

g <- ggplot(us.map, aes(x=long.transp, y=lat.transp)) + 
  geom_polygon_interactive(aes(group = group, fill=census.region), tooltip = region) +
  geom_text(data=regs, aes(long.transp, lat.transp, label=census.region), size=3) +
  theme(panel.background = element_blank(),  # remove background
        panel.grid = element_blank(), 
        axis.line = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") + # remove legend
  coord_equal()

shinyApp(my.ui, my.server)
