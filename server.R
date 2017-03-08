#install.packages('ggiraph')
library(shiny)
library(ggplot2)
library(dplyr)
library(ggiraph)


my.server <- function(input, output) {
  # create the US map with the census regions separated
  output$map <- renderPlot({
    region.gg <- ggplot(us.map, aes(x=long, y=lat)) + 
      geom_polygon(aes(group = group, fill=census.region)) +
      geom_text(data=regs, aes(long.transp, lat.transp, label=census.region), size=3) +
      theme(panel.background = element_blank(),  # remove background
            panel.grid = element_blank(), 
            axis.line = element_blank(), 
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.position = "none") + # remove legend
      coord_equal()
    return(region.gg)
  })
  
  output$coords <- renderText({
    paste0(input$plot_click)
  })
}
shinyServer(my.server)
