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
  
  region.select <- reactive({
    x <- input$plot_click$x
    y <- input$plot_click$y
    if(x > 48 & x < 130 & y > 51 & y < 255) {
      return('Pacific')
    } else if(x > 144 & x < 320 & y > 49 & y < 263) {
      return('Mountain')
    } else if(x > 311 & x < 468 & y > 48 & y < 206) {
      return('West North Central')
    } else if(x > 323 & x < 469 & y > 204 & y < 344) {
      return('West South Central')
    } else if(x > 463 & x < 599 & y > 78 & y < 177) {
      return('East North Central')
    } else if(x > 483 & x < 548 & y > 188 & y < 277) {
      return('East South Central')
    } else if((x > 596 | x > 511) & (x < 676 | x < 627) & (y > 173 | y > 266) & (y < 354 | y < 365)) {
      return('South Atlantic')
    } else if(x > 609 & x < 693 & y > 99 & y < 166) {
      return('Middle Atlantic')
    } else if(x > 699 & x < 773 & y > 70 & y < 149) {
      return('New England')
    }
  })
}
shinyServer(my.server)
