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

regions <- readRDS("data/processed/bcr_clean.rds")

# Load data objects
plot_data <- readRDS("./tests/plot_data_allBCR.rds")

# Define UI for application
ui <- fluidPage(navset_tab(
  nav_panel("Map", page_sidebar(
    sidebar = sidebar(
      selectInput("select", "Choose species:", c("Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", "Columba_livia", "Vireo_gilvus", "Falco_sparverius", "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", "Geothlypis_trichas")),
      selectInput("region", "Choose region:", c("BCR2", "BCR4", "BCR5", "BCR6", "BCR8", "BCR9", "BCR10", "BCR11", "BCR12", "BCR13", "BCR14", "BCR15",
                "BCR16", "BCR17", "BCR18", "BCR19", "BCR20", "BCR21", "BCR22", "BCR23", "BCR24",
                "BCR25", "BCR26", "BCR27", "BCR28", "BCR29", "BCR30", "BCR31", "BCR32", "BCR33", "BCR34", "BCR35", "BCR36", "BCR37")),
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
    leafletOutput("map"),
    textOutput("click_coords")
  ), ),
  nav_panel("Tree", "Content"),
), id = "tab", )

# Define server logic
server <- function(input, output, session) {
  # Load data
  plotDataInput <- reactive({
    req(input$select)
    req(input$select)
    selected_species <- input$select
    selected_region <- input$region
    d <- plot_data %>%
      filter(species == selected_species, region == selected_region)
  })
  # Leaflet map output
  output$map <- renderLeaflet({
    leaflet(regions) %>%
      setView(-98, 41, zoom = 4) %>%
      addPolygons(
        layerId = ~BCR_clean,
        color = "grey",
        fillOpacity = 0.5,
        weight = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = 'black')
      ) %>%
      addTiles()
  })
  output$plot <- renderPlot({
    data <- plotDataInput()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") +
               theme_void())
    }
    
    ggplot(data, aes(x = year, y = pred)) +
      geom_line(linewidth = 1, alpha = 0.7) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
      theme_classic() 
  },
  width = 200,
  height = 200
  )

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)){
      # Update region selector based on map click
      updateSelectInput(session, "region", selected = click$id)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)