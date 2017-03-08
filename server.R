library(shiny)
library(ggplot2)
library(dplyr)
library("mapproj")

my.server <- function(input, output) {
  # initialize food world cup data frame
  food <- read.csv("Data/food-world-cup-data.csv", stringsAsFactors = FALSE)
  
  # cleaning up column names
  food <- food[,2:ncol(food)]
  food.colnames <- colnames(food)
  food.colnames <- gsub('Please.rate.how.much.you.like.the.traditional.cuisine.of.', '', food.colnames)
  countries.names <- gsub('[.]', '', food.colnames[3:42])
  colnames(food) <- c('level.of.knowledge', 'interest', countries.names, 'Gender', 'Age', 'Household.Income', 'Education', 'census.region')
  
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
  regs <- aggregate(cbind(long.transp, lat.transp) ~ census.region, data = us.map, 
                    FUN = function(x)mean(range(x)))
  
  # create the US map with the census regions separated
  output$map <- renderPlotly({
    region.gg <- ggplot(us.map, aes(x = long.transp, y = lat.transp), colour = "white") + 
      geom_polygon(aes(text = census.region, group = group, fill = census.region), colour = 'white') +
      geom_text(data = regs, aes(long.transp, lat.transp, label = census.region), size = 3) +
      theme(panel.background = element_blank(),  # remove background
            panel.grid = element_blank(), 
            axis.line = element_blank(), 
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.position = "none")
      coord_equal()
    
    region.gg <- ggplotly(region.gg)
    
    return(region.gg)
  })
  
  # creates a reactive variable for the clicked region, converting the curveNumber to the region name

  
  chosen.region <- reactive({
    region.number <- event_data("plotly_click")
    region.number <- region.number$curveNumber
    
    # dictionary of region curveNumbers to corresponding region names
    region.names <- list("0" = "East North Central", "1" = "East South Central", "2" = "Middle Atlantic", "3" = "Mountain", "4" = "New England", "5" = "Pacific", "6" = "South Atlantic", "7" = "West North Central", "8" = "West South Central", "9" = "East South Central")
    
    name <- region.names[[region.number]]
    
    return(name)
  })

}
shinyServer(my.server)
