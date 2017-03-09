#install.packages('shinydashboard')
library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(plotly)
library(shinydashboard)

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
    mainPanel(
      plotlyOutput("map"),
      plotlyOutput('top.food'),
      plotOutput("plot"),
      plotOutput("gender.male")
    )
  )
)
shinyUI(my.ui)

