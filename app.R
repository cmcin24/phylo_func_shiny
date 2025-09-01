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
library(DT)
library(RColorBrewer)

# source some functions
source("./R/functions.R")

# Load data objects
plot_data <- readRDS("./tests/plot_data_allBCR.rds")

# Calculate trends for all species-region combinations
trend_data <- calculate_trends(plot_data)

regions <- readRDS("data/processed/bcr_clean.rds")



# Define UI for application
ui <- fluidPage(navset_tab(
  nav_panel("Map", page_sidebar(
    sidebar = sidebar(
      width = 300,
      selectInput("select", "Choose species:", c("Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", "Columba_livia", "Vireo_gilvus", "Falco_sparverius", "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", "Geothlypis_trichas")),
      selectInput("region", "Choose region:", c("BCR2", "BCR4", "BCR5", "BCR6", "BCR8", "BCR9", "BCR10", "BCR11", "BCR12", "BCR13", "BCR14", "BCR15",
                "BCR16", "BCR17", "BCR18", "BCR19", "BCR20", "BCR21", "BCR22", "BCR23", "BCR24",
                "BCR25", "BCR26", "BCR27", "BCR28", "BCR29", "BCR30", "BCR31", "BCR32", "BCR33", "BCR34", "BCR35", "BCR36", "BCR37")),
      # sliderInput(
      #   "slider",
      #   "Year",
      #   min = 1992,
      #   max = 2024,
      #   value = 1992,
      #   animate = animationOptions(interval = 100, loop = FALSE)
      # ),
      #checkboxInput("show_only_significant", "Show only significant trends (p < 0.05)", 
       #             value = FALSE),
      plotOutput("plot")
      
    ),
    leafletOutput("map"),
  ), ),
  nav_panel("Tree", "Content"),
  nav_panel("Species Information", 
            fluidRow(
              column(8,
                     h3("Species Details"),
                     uiOutput("species_info_detailed")
              ),
              column(4,
                     h3("Quick Reference"),
                     DTOutput("species_table_compact")
              )
            ),
            hr(),
            fluidRow(
              column(12,
                     h3("All Species Database"),
                     DTOutput("species_table_full")
              )
            )
  ),
  nav_panel("Species Comparison"),
), id = "tab", )

# Define server logic
server <- function(input, output, session) {
  # Load data for plots
  plotDataInput <- reactive({
    req(input$select)
    req(input$select)
    selected_species <- input$select
    selected_region <- input$region
    d <- plot_data %>%
      filter(species == selected_species, region == selected_region)
  })
  # Prepare trend data for the selected species
  trendMapData <- reactive({
    req(input$select)
    
    # Get trend data for selected species
    species_trends <- trend_data %>%
      filter(species == input$select)
    
    # # Apply significance filter if requested
    # if (input$show_only_significant) {
    #   species_trends <- species_trends %>%
    #     mutate(
    #       trend_category = ifelse(p_value >= 0.05 | is.na(p_value), "Not Significant", trend_category),
    #       slope = ifelse(p_value >= 0.05 | is.na(p_value), NA, slope),
    #       r_squared = ifelse(p_value >= 0.05 | is.na(p_value), NA, r_squared)
    #     )
    # }
    
    # Join species data with spatial data
    map_data <- regions %>%
      left_join(species_trends, by = c("BCR_clean" = "region"))
    
    return(map_data)
  })
  
  # # Leaflet map output
  # output$map <- renderLeaflet({
  #   leaflet(regions) %>%
  #     setView(-98, 41, zoom = 4) %>%
  #     addPolygons(
  #       layerId = ~BCR_clean,
  #       color = "grey",
  #       fillOpacity = 0.5,
  #       weight = 0.8,
  #       highlightOptions = highlightOptions(weight = 3, color = 'black')
  #     ) %>%
  #     addTiles()
  # })
  
  output$map <- renderLeaflet({
    leaflet(regions) %>%
      setView(-98, 41, zoom = 4) %>%
      addTiles()
  })
  # Update map colors when data changes
  observe({
    data <- trendMapData()
    
    # Get the range of slopes for all significant trends
    significant_data <- data %>%
      filter(!is.na(slope) & !is.na(p_value) & p_value < 0.05)
    
    # Calculate symmetric range around zero
    max_abs_slope <- max(abs(significant_data$slope), na.rm = TRUE)
    slope_range <- c(-max_abs_slope, max_abs_slope)
    
    # Create color palette for slopes (diverging: red-white-green)
    pal3 <- colorNumeric(
      palette = c("#8B0000", "#CD5C5C", "#FFFFFF", "#90EE90", "#006400"),
      domain = slope_range
    )
    

      
      
    # Create color palette
    pal <- colorFactor(
      palette = c("Decreasing" = "#CD5C5C", 
                  "Increasing" = "#2E8B57", 
                  "No Data" = "#D3D3D3",
                  "Not Significant" = "#808080",
                  "Stable" = "#808080"),
      domain = c("Decreasing", "Increasing", "No Data", "Not Significant", "Stable"),
      na.color = "#D3D3D3"
    )
    
    pal2 <- colorNumeric(
      palette = "RdYlGn",
      domain = data$slope
    )
    
    # Create popup content
    popup_content <- paste0(
      "<strong>Region:</strong> ", data$BCR_clean, "<br>",
      "<strong>Species:</strong> ", gsub("_", " ", input$select), "<br>",
      "<strong>Trend:</strong> ", ifelse(is.na(data$trend_category), "No Data", data$trend_category), "<br>",
      "<strong>Slope:</strong> ", round(data$slope, 4)
      #, "<br>",
      #"<strong>P-value:</strong> ", round(data$p_value, 4)
      #, "<br>",
      #"<strong>R²:</strong> ", round(data$r_squared, 3)
    )
    
    # Update polygons using leafletProxy
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        layerId = ~BCR_clean,
        #fillColor = ~pal(ifelse(is.na(trend_category), "No Data", trend_category)),
        color = ~pal2(slope),
        #color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        popup = popup_content,
        label = ~BCR_clean,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        colors = c("#2E8B57", "#CD5C5C", "#808080", "#D3D3D3"),
        labels = c("Increasing", "Decreasing", "Stable", "No Data"),
        title = "Population Trend",
        position = "bottomright",
        opacity = 0.8,
        layerId = "trend_legend"
      )
  })
  
  output$plot <- renderPlot({
    data <- plotDataInput()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available") +
               theme_void())
    }
    
    ggplot(data, aes(x = year, y = pred)) +
      geom_line(linewidth = 1, alpha = 0.8, color = "darkblue") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightblue") +
      #geom_vline(xintercept = input$slider, color = "red", linetype = "dashed", alpha = 0.7) +
      labs(title = paste("Expected Trend for", gsub("_", " ", input$select)),
           x = "Year", y = "Expected Trend") +
      theme_classic() +
      theme(plot.title = element_text(size = 10))
  },
  width = 220,
  height = 220
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