#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(ggplot2)
library(dplyr)

regions <- readRDS("data/processed/regions_sf.rds")

# Load data objects
plot_data <- readRDS("./tests/plot_data2.rds")

# Define UI for application
ui <- fluidPage(navset_tab(
  nav_panel("Map", page_sidebar(
    sidebar = sidebar(
      selectInput("select", "Choose species:", c("Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", "Columba_livia", "Vireo_gilvus", "Falco_sparverius", "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", "Geothlypis_trichas")),
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
  # Load data
  plotDataInput <- reactive({
    req(input$select)
    selected_species <- input$select
    d <- plot_data %>%
      filter(species == selected_species)
  })
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
    ggplot(plotDataInput(), aes(x = year, y = pred)) +
      geom_line(linewidth = 1, alpha = 0.7) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      theme_classic()
  },
  width = 200,
  height = 200
  )


}



# Run the application
shinyApp(ui = ui, server = server)