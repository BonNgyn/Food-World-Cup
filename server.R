library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(mapproj)

my.server <- function(input, output) {
  ########################
  ## Reactive Functions ##
  ########################
  
  # Creates a reactive variable for filtered data frames based on which demographic 
  # the user selects
  filtered <- reactive({
    if (chosen.region() != "") {
      data <- food %>% 
        filter(census.region == chosen.region()) %>% 
        select_(input$dem, "Algeria:Ireland")
      
      return(data)
    }
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
      
      # Creates a reactive render text so that the displayed, current selected region can update
      output$region.name = renderText({
        return(paste("You are currently looking at the ", name, " region"))
      })
      
      return(name)
    } else {
      return("")
    }
  })
  ######################
  
  # Accepts a data frame and gender and filters the df returned by filtered() to only
  # include gender and average ratings for all traditional cuisines
  getFilteredGender <- function(data, gender) {
    
    # Computes the average rating for each traditional cuisine
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
  
  # create the US map with the census regions separated
  output$map <- renderPlotly({
    region.gg <- ggplot(us.map, aes(x = long.transp, y = lat.transp), colour = "white") + 
      geom_polygon(aes(group = group, fill = census.region), colour = 'white') +
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
  
  output$plot2 <- renderPlotly({
    if (!is.null(filtered())) { # checks if any region has been clicked yet
      if (input$dem == "Gender") {
        
        # Uses the getFilteredGender function to filter the food df to only include the Male gender
        # and average ratings for cuisines
        males.data <- getFilteredGender(filtered(), "Male")
         
        # Plots the males.data df on a bar graph  
        p <- ggplot(data = males.data[1:10,]) +
          geom_bar(mapping = aes(x = `Country`, y=`Average`, width = 0.4, fill = `Average`),
                  stat = "identity") + 
          theme(legend.position="none") +
          scale_x_discrete(limits= males.data[1:10,]$Country) + 
          labs(title = "Top 10 Countries' Cuisines Enjoyed by Males",
              x = "Country that Traditional Cuisine is From",
              y = "Average Rating (Scale 1-5)")
        p <- ggplotly(p)
        return(p)
      }
    }
  })
  
  output$plot <- renderPlotly({
    if (!is.null(filtered())) { # checks if any region has been clicked yet
      if (input$dem == "Age") {
        
        # Outputs the description for the age plot
        output$plot.desc = renderText({
          return(paste("The plot below displays the top 5 highest rated cuisines for various
                      age buckets (18-29, 30-44, 45-60, >60). For each bucket, the top 5 highest 
                      rated cuisines are displayed with circles, the highest rated cuisine at 
                      the top and the lowest rated cuisine at the bottom."))
        })
        
        age.data <- filtered() %>% group_by(Age) %>% 
          summarise_each(funs(mean(as.numeric(.), na.rm = TRUE)))
        age.long <- gather(age.data, key = Country, value = Average, 
                                 Algeria:Ireland)
        age.long <- age.long %>% 
          group_by(Age) %>% 
          arrange(desc(`Average`))%>% 
          top_n(5)
        
        age.plot <- ggplot(data = age.long) +
          geom_point(mapping = aes(x = Age, y = Average, color = Country), size = 4) +
          labs(title = "Top 5 Countries' Cuisines by Age",
               x = "Age Range",
               y = "Average Rating (Scale 1-5)")
        age.plot <- ggplotly(age.plot)
        return(age.plot)
        
      } else if(input$dem == "Household.Income") {
        
        output$plot.desc = renderText({
          return(paste("The plot below displays the top 5 highest rated cuisines for various income 
                       buckets. For each bucket, the top 5 highest rated cuisines are displayed 
                       with circles, the highest rated cuisine at the top and the lowest rated 
                       cuisine at the bottom."))
        })
        
        income.data <- filtered() %>% filter(Household.Income != "") %>% 
          group_by(Household.Income) %>% 
          summarise_each(funs(mean(as.numeric(.), na.rm = TRUE)))
        income.long <- gather(income.data, key = Country, value = Average, Algeria:Ireland)
        income.long <- income.long %>% 
          group_by(Household.Income) %>% 
          arrange(desc(`Average`)) %>% 
          top_n(5)
        
        income <- (income.long$Household.Income)
        income <- gsub('[$]', '', income)
        
        income.plot <- ggplot(data = income.long) +
          geom_point(mapping = aes(income, income.long$Average, color = income.long$Country), 
                     size = 4) + labs(title = "Top 5 Countries' Cuisines by Household Income",
                                      x = "Household Income Range", y = "Average Rating (Scale 1-5)")
        income.plot <- ggplotly(income.plot)
        return(income.plot)
        
      } else if (input$dem == "Education") {
        
        # Outputs the description for the level of education plot
        output$plot.desc = renderText({
          return(paste("The plot below displays the top 5 highest rated cuisines for various levels 
                       of education. For each bucket, the top 5 highest rated cuisines are
                       displayed with circles, the highest rated cuisine at the top and the lowest
                       rated cuisine at the bottom."))
        })
        
        # Filters the education data to only include the level of education and food ratings;
        # computes the average rating for each type of cuisine
        education.data <- filtered() %>% 
          group_by(`Education`) %>% 
          summarise_each(funs(mean(as.numeric(.), na.rm = TRUE)))
        education.long <- gather(education.data, key = Country, value = Average, 
                                  Algeria:Ireland)
        education.long <- education.long %>% 
          filter(Education != "") %>% 
          group_by(`Education`) %>% 
          arrange(desc(`Average`))%>% 
          top_n(5)
        
        # Plots the education data frame on a scatter plot  
        p <- ggplot(data = education.long) +
          geom_point(mapping = aes(x = `Education`, y = `Average`, color = `Country`), size = 4) +
          labs(title = "Top 5 Countries' Cuisines based on Level of Education",
               x = "Level of Education (Degree)",
               y = "Average Rating (Scale 1-5)")
        
        p <- ggplotly(p)
        return(p)

      } else { #gender 
        
        # Outputs the description for the gender plot
        output$plot.desc = renderText({
          return(paste("The plot below displays the top 10 highest rated cuisines for both genders.
                       Each bar's height represents that cuisines average rating. The highest rated
                       cuisine is on the left side of the graph, and the lowest rated cuisine on the
                       right side."))
        })
          
        # Uses the getFilteredGender function to filter the food df to only include the Female gender
        # and average ratings for cuisines
        females.data <- getFilteredGender(filtered(), "Female")
        
        # Plots the filtered female data frame  
        p <- ggplot(data = females.data) +
        geom_bar(mapping = aes(x = `Country`, y=`Average`, width = 0.4, fill = `Average`),
                stat = "identity") + 
          theme(legend.position="none") +
        scale_x_discrete(limits= females.data$Country) +
        labs(title = "Top 10 Countries' Cuisines Enjoyed by Females",
            x = "Country that Traditional Cuisine is From",
            y = "Average Rating (Scale 1-5)")
        p <- ggplotly(p)
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
    title <- "Top 10 Rated Cuisines in the... "
    if(is.null(chosen.region)) {
      title <- paste0(title, 'United States')
    } else {
      title <- paste0(title, chosen.region())
    }
    
    countries <- factor(GetFilteredTop()$countries.names, 
                        levels = unique(GetFilteredTop()$countries.names)[order(GetFilteredTop()$means, decreasing = TRUE)])
    p <- plot_ly(x = countries, y = GetFilteredTop()$means, type = "bar") %>% 
      layout(title = title, chosen.region(), titlefont = f) %>% 
      layout(xaxis = x, yaxis = y)
    return(p)
  })
}
shinyServer(my.server)
