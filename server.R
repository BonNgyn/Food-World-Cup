library(shiny)
library(ggplot2)
library(dplyr)
library("mapproj")

my.server <- function(input, output) {
  # initialize food world cup data frame
  food <- read.csv("Data/food-world-cup-data.csv", stringsAsFactors = FALSE, fileEncoding = "cp932")
  
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
  regs <- aggregate(cbind(long.transp, lat.transp) ~ census.region, data=us.map, 
                    FUN=function(x)mean(range(x)))
  
  # calculates counts for each state
  test <- select(na.omit(food), Household.Income, census.region) %>% 
    group_by_("census.region") %>% 
    summarise(
      count = n() 
    )
  
  # updates the us.map data frame with the calculated counts from test
  us.map <- left_join(us.map, test, by = "census.region")
  
  # create the US map with the census regions separated
  output$map <- renderPlotly({
    region.gg <- ggplot(us.map, aes(x = long.transp, y = lat.transp), colour = "white") + 
      geom_polygon(aes(text = census.region, group = group, fill = count)) +
      geom_text(data = regs, aes(long.transp, lat.transp, label = census.region), size = 3) +
      theme(panel.background = element_blank(),  # remove background
            panel.grid = element_blank(), 
            axis.line = element_blank(), 
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())
      coord_equal()
    
    region.gg <- ggplotly(region.gg)
    
    return(region.gg)
  })
  
  output$plot2 <- renderPlot({
    if (input$dem == "Gender") {
      # Males
      males.data <- getFilteredGender(filtered(), "Male")
      
      p <- ggplot(data = males.data[1:10,]) +
        geom_bar(mapping = aes(x = `Country`, y=`Average`, width = 0.4, fill = `Average`),
                 stat = "identity") + 
        scale_x_discrete(limits= males.data[1:10,]$Country) + 
        labs(title = "Top 10 Countries' Cuisines enjoyed by Males",
             x = "Country that Traditional Cuisine is From",
             y = "Average Rating (Scale 1-5)")
      return(p)
    }
    
  })
  
  output$plot <- renderPlot({

    if (input$dem == "Household.Income" || input$dem == "Age") {
      #age.and.income <- ggplot(data = filtered()) + 
        #geom_point(mapping = aes_string(x = "place", 
                                        #y = "holder")) 


    } else if (input$dem == "Education") {
      education.data <- filtered() %>% 
        group_by(`Education`) %>% 
        summarise_each(funs(mean(as.numeric(.), na.rm = TRUE)))
      education.long <- gather(education.data, key = Country, value = Average, 
                              Algeria:Ireland)
      education.long <- education.long %>% 
        filter(Education != "") %>% 
        group_by(`Education`) %>% 
        arrange(desc(`Average`))%>% 
        top_n(10)
      
      p <- ggplot(data = education.long) +
        geom_point(mapping = aes(x = `Education`, y = `Average`, color = `Country`))
      return(p)
      
    } else { #gender 
      
      # Females
      females.data <- getFilteredGender(filtered(), "Female")
      
      p <- ggplot(data = females.data) +
        geom_bar(mapping = aes(x = `Country`, y=`Average`, width = 0.4, fill = `Average`),
                 stat = "identity") + 
        scale_x_discrete(limits= females.data$Country) + 
        labs(title = "Top 10 Countries' Cuisines enjoyed by Females",
             x = "Country that Traditional Cuisine is From",
             y = "Average Rating (Scale 1-5)")
      return(p)
    }
  })
    
  filtered <- reactive({
    data <- food %>% 
      filter(census.region == "Pacific") %>% 
      select_(input$dem, "Algeria:Ireland")
    
    return(data)
  })
  
  getFilteredGender <- function(data, gender) {
    
    filtered.data <- data %>% 
      group_by(`Gender`) %>% 
      summarise_each(funs(mean(as.numeric(.), na.rm = TRUE)))
    
    gender.data <- filtered.data %>% 
      filter(`Gender` == gender)
    gender.data <- gender.data[-1] %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(Country = rownames(.)) %>% 
      arrange(desc(`V1`))
    colnames(gender.data) <- c("Average", "Country")  
    return(gender.data[1:10,])
  }
  
}
shinyServer(my.server)
