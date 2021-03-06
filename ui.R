#install.packages('shinydashboard')
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rsconnect)
library(plotly)
library(shinydashboard)

my.ui <- fluidPage(theme = "bootstrap.css",
  # Creates a title panel to introduce an overview of the report
  column(12, align = 'center', 
         titlePanel("Food World Cup")
  ),
  tags$style("body {background-color: #ECEAE0; }"),
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  column(12,
         # Introduction paragraph                  
         p("Welcome to Food World Cup! Ever wonder what traditional cuisines people prefer based
           on what region of the U.S. they reside in? Or maybe if individuals with higher/lower 
           income prefer a certain cuisine? Our interactive plot attempts to answer some (and more) 
           of these questions, by presenting the highest rated traditional cuisines, based on
           individuals' various demographics such as residing region, gender, age, income, and level of 
           education. Just select a region you would like to look at to begin exploring!"),
         
         # Sources paragraph
         p("The data set used for this interactive visualization is  from FiveThirtyEight, a popular
           interactive news and sports site that writes data-driven articles. Original data - 
           (https://github.com/fivethirtyeight/data/tree/master/food-world-cup)"),
         hr()
  ),

  # Displays the map of the U.S.
  column(12, align="center",
         # Asks the user to select a region on the map
         h4(strong("Please select a region!")),
         
         plotlyOutput("map", width = 900, height = 650), hr(),
         tags$h5('Top Cuisines'), 
         tags$body('This bar graph initially displays the top ten rated cuisines from all regions, meaning the whole United States. 
                   Selecting a region will tailor the bar graph to display the top ten rated cuisines from that region.'),
         plotlyOutput('top.food'), hr()
  ), 
  # Outputs the various demographic plot(s)
  fluidRow(
    column(5, offset = 4, 
           radioButtons("dem", "Demographical Measure",
                        c("Education Level" = "Education",
                          "Household Income" = "Household.Income",
                          "Age" = "Age",
                          "Gender" = "Gender"))
    )
  ),p(strong(textOutput('region.name', inline=TRUE))),
  # Outputs the various demographic plot descriptions
  p(textOutput('plot.desc', inline=TRUE)),
  plotlyOutput("plot"),
  plotlyOutput("plot2")
)
shinyUI(my.ui)

