library(shiny)
library(ggplot2)
library(dplyr)
library(mapproj)

my.server <- function(input, output) {
  # create the US map with the census regions separated
  output$map <- renderPlotly({
    region.gg <- ggplot(us.map, aes(x = long.transp, y = lat.transp), colour = "white") + 
      geom_polygon(aes(text = census.region, group = group, fill = census.region), colour = 'white') +
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
    if (!is.null(region.number)) {
      region.number <- region.number$curveNumber
      
      # dictionary of region curveNumbers to corresponding region names
      region.names <- c("East North Central", "East South Central", "Middle Atlantic", 
                        "Mountain", "New England", "Pacific", "South Atlantic", 
                        "West North Central", "West South Central")
      
      name <- region.names[region.number + 1]
      return(name)
    } else {
      return("")
    }
  })
  
  output$gender.male <- renderPlot({
    if (!is.null(filtered())) {
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
    }
  })
  
  output$plot <- renderPlot({
    if (!is.null(filtered())) {
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
    }  
  })
    
  filtered <- reactive({
    if (chosen.region() != "") {
    data <- food %>% 
      filter(census.region == chosen.region()) %>% 
      select_(input$dem, "Algeria:Ireland")
    
    return(data)
    }
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
  
  GetFilteredTop <- reactive({
    #Getting average of all cuisines 
    top.food <- food %>% filter(census.region == chosen.region()) %>% 
      select(3:42)
    
    #making dataframe 
    means <- c()
    for(country in countries.names) {
      means <- c(means, mean(as.numeric(top.food[,country]), na.rm = TRUE))
    }
    food.mean <- data.frame(countries.names, means) %>% 
      arrange(desc(means)) %>% 
      top_n(n = 10, means)
    return(food.mean)
  })
  
  output$top.food <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 16
    )
    x <- list(
      title = "Top Rated Cuisines",
      titlefont = f
    )
    y <- list(
      title = "Average Rating (1-5)",
      titlefont = f
    )
    
    countries <- factor(GetFilteredTop()$countries.names, 
                        levels = unique(GetFilteredTop()$countries.names)[order(GetFilteredTop()$means, decreasing = TRUE)])
    p <- plot_ly(x = countries, y = GetFilteredTop()$means, type = "bar") %>% 
      layout(title = "Top 10 Rated Cuisines") %>% 
      layout(xaxis = x, yaxis = y)
    
    p2 <- ggplot(food.mean, aes(x = reorder(countries.names, -means), y = means)) +
      geom_bar(stat = "identity")
    return(p)
  })
}
shinyServer(my.server)
