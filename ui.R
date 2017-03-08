library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)

my.ui <- fluidPage(
  # Creates a title panel to introduce an overview of the report
  titlePanel("Food World Cup"),
  
  # Includes a sidebar layout in order to organize the information into the widgets for filtering 
  # the iris data, as well as a main area for the actual data visualization to be displayed
  sidebarLayout(
    
    # Initializes the widgets sidebar that includes radio buttons, sliders, and checkboxes
    sidebarPanel(
      # Creates radio buttons for users to be able to choose what percentage of the orignal raw 
      # data values they wish to filter and view
      radioButtons("dem", "Demographical Measure",
                   c("Education Level" = "Education",
                     "Household Income" = "Household.Income",
                     "Age" = "Age",
                     "Knowledge of World Cuisines" = "Cuisine.Knowledge")),
      verbatimTextOutput(outputId="region")
    ),
    
    # Initializes the main panel of the report, which should have two tabs to let the user revert 
    # between a plot of the filtered data or a table of the filterd data. Both data forms have a 
    # short summary attached to the end to extend its usability.
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Map", plotOutput("map", click = "plot_click"))
      )
    )
  )
)
shinyUI(my.ui)