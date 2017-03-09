library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rsconnect)
library(plotly)

my.ui <- fluidPage(theme = "bootstrap.css",
  # Creates a title panel to introduce an overview of the report
  titlePanel("Food World Cup"),
  
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  p("Welcome to Food World Cup! Ever wonder what traditional cuisines people prefer based on what region
    of the U.S. they reside in? Or maybe if individuals with higher/lower income prefer a certain cuisine?
    Our interactive plot attempts to answer some (and more) of these questions, by presenting the average 
    rating of certain traditional cuisines, based on various demographics such as residing region, gender,
    age, income, and level of education. Just select a region you would like to look at to begin exploring!"),
  hr(),
  h4(strong("Please select a region!")),
  plotlyOutput("map"), hr(),
  fluidRow(
    column(5, offset = 4, 
           radioButtons("dem", "Demographical Measure",
                        c("Education Level" = "Education",
                          "Household Income" = "Household.Income",
                          "Age" = "Age",
                          "Gender" = "Gender"))
    )
  ), p(strong(textOutput('region.name', inline=TRUE))),
  p(textOutput('plot.desc', inlin=TRUE)),
  plotlyOutput("plot"),
  plotlyOutput("plot2")
)
shinyUI(my.ui)

