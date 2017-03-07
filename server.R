library(shiny)
library(ggplot2)
library(dplyr)
library("mapproj")

my.server <- function(input, output) {
  # initialize food world cup data frame
  food <- read.csv("data/food-world-cup-data.csv")
  colnames(food)[2] <- "Cuisine.Knowledge"
  
  
  # initialize US map
  us.map <-  map_data('state')
  
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
  us.map$lat.transp[us.map$census.region == "New England"] <- us.map$lat[us.map$census.region == "New England"]
  us.map$long.transp[us.map$census.region == "New England"] <- us.map$long[us.map$census.region == "New England"]
  
  us.map$lat.transp[us.map$census.region == "Middle Atlantic"] <- us.map$lat[us.map$census.region == "Middle Atlantic"]
  us.map$long.transp[us.map$census.region == "Middle Atlantic"] <- us.map$long[us.map$census.region == "Middle Atlantic"]
  
  us.map$lat.transp[us.map$census.region == "East North Central"] <- us.map$lat[us.map$census.region == "East North Central"]
  us.map$long.transp[us.map$census.region == "East North Central"] <- us.map$long[us.map$census.region == "East North Central"]
  
  us.map$lat.transp[us.map$census.region == "West North Central"] <- us.map$lat[us.map$census.region == "West North Central"]
  us.map$long.transp[us.map$census.region == "West North Central"] <- us.map$long[us.map$census.region == "West North Central"]
  
  us.map$lat.transp[us.map$census.region == "South Atlantic"] <- us.map$lat[us.map$census.region == "South Atlantic"]
  us.map$long.transp[us.map$census.region == "South Atlantic"] <- us.map$long[us.map$census.region == "South Atlantic"]
  
  us.map$lat.transp[us.map$census.region == "East South Central"] <- us.map$lat[us.map$census.region == "East South Central"]
  us.map$long.transp[us.map$census.region == "East South Central"] <- us.map$long[us.map$census.region == "East South Central"]
  
  us.map$lat.transp[us.map$census.region == "West South Central"] <- us.map$lat[us.map$census.region == "West South Central"]
  us.map$long.transp[us.map$census.region == "West South Central"] <- us.map$long[us.map$census.region == "West South Central"]
  
  us.map$lat.transp[us.map$census.region == "Mountain"] <- us.map$lat[us.map$census.region == "Mountain"]
  us.map$long.transp[us.map$census.region == "Mountain"] <- us.map$long[us.map$census.region == "Mountain"]
  
  us.map$lat.transp[us.map$census.region == "Pacific"] <- us.map$lat[us.map$census.region == "Pacific"]
  us.map$long.transp[us.map$census.region == "Pacific"] <- us.map$long[us.map$census.region == "Pacific"]
  
  # creates list of labels for each of the census regions
  regs <- aggregate(cbind(long.transp, lat.transp) ~ census.region, data=us.map, 
                    FUN=function(x)mean(range(x)))
  
  
  # create the US map with the census regions separated
  output$map <- renderPlot({
    region.gg <- ggplot(us.map, aes(x=long.transp, y=lat.transp), colour="white") + 
      geom_polygon(aes(group = group, fill=census.region)) +
      geom_text(data=regs, aes(long.transp, lat.transp, label=census.region), size=3) +
      theme(panel.background = element_blank(),  # remove background
            panel.grid = element_blank(), 
            axis.line = element_blank(), 
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.position = "none") + # remove legend
      coord_equal()
    
    return(region.gg)
  })
}
shinyServer(my.server)
