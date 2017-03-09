<snippet>
  <content><![CDATA[
# ${1:Food World Cup}
Ever wondered what traditional cuisines people prefer based on what region of the U.S. they reside in? Or maybe if individuals with higher/lower income prefer a certain cuisine? Our interactive plot attempts to answer some (and more) of these questions, by presenting the highest rated traditional cuisines, based on individuals' various demographics such as residing region, gender, age, income, and level of education. Just select a region you would like to look at to begin exploring from [here](https://bonngyn.shinyapps.io/food-world-cup/)!

## Purpose
Although this app provides casual entertainment for curious users, it also has some beneficial applications to real world issues. We can use this information to:
- generalize the kind of cuisine to minimize/maximize in each region to eliminate food waste
- understand consumer preferences to maximize profit for individuals interested in opening their own eatery
- simply make some cultural inferences about the relationship between community demographics and their culinary preferences.

Our hope is that this app, although limited in scope, can provide useful insights to individuals who are interested in delving into a sector of data that may not have as much presence in today's society. As people become more aware of the value of data science, even something like cuisine preference has the possibility of raising important questions about the significance of demographical measures. Our team recognized that there is very limited information about food preferences and what this could correlate to, and we want to help catalyze new research questions for this seemingly trivial field of information.

## How to Use the App
It's pretty simple.

As you come into the front page of the app, click on a census region of the U.S. that interests you (e.g. Pacific region). Then, select any of the demographical measures from the radio buttons provided to get different plots displaying various information regarding the correlation between cuisine preferences and the selected demographic within the specified region.

If you want to change the region or any of the filters, simply go back to the widget and select another one, and have fun!

## The Development Process
Our raw data was collected from the fivethirtyeight [Food World Cup repo](https://github.com/fivethirtyeight/data/tree/master/food-world-cup). In order to wrangle with this data to make it work with our intended purpose, we also had to utilize the `map_data('state')` data frame that is provided in the R library.

Each team member had a primary portion of the project to complete.
**Dustin L.**
- built `ggplot` visualizations for `Age` and  `Household.Income` correlations with cuisine preferences
- contributed to UI layout
  - fixed color schemes of plots
  - applied `shinydashboard` to main page improve UI aesthetics

**Bonnie N.**
- created the visualization for the top 10 cuisines for the selected US Census region
- contributed to UI layout with `shinydashboard` to improve UI aesthetics
- published `Shiny` app

**Julia P.**
- worked on the `Gender` and `Education.Level` demographics
  - selected appropriate `ggplot` visualization to represent filtered `Gender` and cuisine preference data (same process for `Education.Level`)
  - incorporated interactive features with `plotly` to allow for user interaction (`hover` feature)


**Erika Y.**
- constructed US Census Region map with `ggplot` to add filtering capabilities to visualizations
  - add `hover` features with `plotly` to incorporate interactive tooltips to each region
- converted `curveNumber`s from the map into a reactive variable US Census region names to enable other functions to access the value

## Key Variables
`age.plot`, `income.plot` (Dustin L.)
- data frames displaying the correlation between the cuisine preferences and the age (`age.plot`) or household income (`income.plot`)
- the data was grouped, converted from wide to long, arranged in descending order, and then the top 5 from each group was selected to be displayed

`filtered` (Erika Y.)
- reactive variable that collects the `curveNumber` of the clicked region of the map, then converts that to the respective US Census Region name to allow filtering of the plots of the demographic measures

`getFilteredGender` (Julia P.)
- accepts a data frame and a gender, and filters and returns the given data frame to only include information about the given gender and their average ratings for each traditional cuisine

`GetFilteredTop` (Bonnie N.)
- gets the average of every rating for every country and filters for the top 10 rated cuisines based on region

`us.map` (Erika Y.)
- a data frame that contains all of the US states (inherited from the `map_data('state')` data frame provided by R), and groups them by their respective US Census Region.
- the framework for making the actual map visualization with `plotly` for `output$plot`

]]></content>
  <tabTrigger>readme</tabTrigger>
</snippet>
