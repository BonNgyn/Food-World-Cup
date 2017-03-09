library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rsconnect)
library(plotly)

my.ui <- fluidPage(
  # Creates a title panel to introduce an overview of the report
  titlePanel("Food World Cup"),
  
  # Includes a sidebar layout in order to organize the information into the widgets for filtering 
  # the food world cup data, as well as a main area for the actual data visualization to be displayed
  sidebarLayout(
    
    # Initializes the widgets sidebar 
    sidebarPanel(
      # Creates radio buttons for users to be able to choose what percentage of the orignal raw 
      # data values they wish to filter and view
      radioButtons("dem", "Demographical Measure",
                   c("Education Level" = "Education",
                     "Household Income" = "Household.Income",
                     "Age" = "Age",
                     "Gender" = "Gender"))
    ),
    
    # Initializes the main panel of the report, which should have two tabs to let the user revert 
    # between a plot of the filtered data or a table of the filterd data. Both data forms have a 
    # short summary attached to the end to extend its usability.
    mainPanel(tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"),
      p("Welcome to Food World Cup! Ever wonder what traditional cuisines people prefer based on what region
        of the U.S. they reside in? Or maybe if individuals with higher/lower income prefer a certain cuisine?
        Our interactive plot attempts to answer some (and more) of these questions, by presenting the average 
        rating of certain traditional cuisines, based on various demographics such as residing region, gender,
        age, income, and level of education. Just select a region you would like to look at to begin exploring!"),
      p("Please select a region!"),
      plotlyOutput("map"), hr(),
      plotlyOutput("plot"),
      plotlyOutput("plot2")
    )
  )
)
shinyUI(my.ui)

