#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(ggplot2)

regions <- readRDS("data/processed/regions_sf.rds")

source('R/plot_loader.R')

data <- data.frame(
  y = c(6, 7, 7, 9, 12, 13, 13, 15, 16, 19, 22, 23, 23, 25, 26),
  x = c(1, 2, 2, 3, 4, 4, 5, 6, 6, 8, 9, 9, 11, 12, 12)
)

# Define UI for application
ui <- fluidPage(navset_tab(
  nav_panel("Map", page_sidebar(
    sidebar = sidebar(
      selectInput("select", "Choose species:", list("Hirundo rustica", "2", "3")),
      sliderInput(
        "slider",
        "Year",
        min = 1966,
        max = 2024,
        value = 1966,
        animate = animationOptions(interval = 100, loop = FALSE)
      ),
      plotOutput("plot")
      
    ),
    leafletOutput("map")
  ), ),
  nav_panel("Tree", "Content"),
), id = "tab", )

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(regions) %>%
      setView(-98, 41, zoom = 4) %>%
      addPolygons(
        color = "grey",
        fillOpacity = 0.5,
        weight = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = 'black')
      ) %>%
      addTiles()
  })
  output$plot <- renderPlot({
    get_plot(1)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)