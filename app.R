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
library(shinythemes)


# source some functions
source("./R/functions.R")

# Load data objects
plot_data <- readRDS("./tests/plot_data_allBCR.rds")

# Calculate trends for all species-region combinations
trend_data <- calculate_trends(plot_data)

regions <- readRDS("data/processed/bcr_simplified.rds")


# Load the bird species information we collected
bird_species_info <- readRDS("data/bird_species_info.rds")



# Define UI for application
ui <- fillPage(theme = shinytheme("flatly"), navset_tab(
  nav_panel("Map", 
            tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}", "#controls {background-color: white; padding: 20px 20px 20px 20px; opacity: 0.8; zoom: 0.9}"),
            leafletOutput("map"),
            absolutePanel(id = "controls", class = "panel panel-default",
                          top = 75, left = 55, width = 270, fixed=TRUE,
                          draggable = TRUE, height = 400,
                          selectInput("select", "Choose species:", c("", "Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", "Columba_livia", "Vireo_gilvus", "Falco_sparverius", "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", "Geothlypis_trichas")),
                          selectInput("region", "Choose region:", c("", "BCR2", "BCR4", "BCR5", "BCR6", "BCR8", "BCR9", "BCR10", "BCR11", "BCR12", "BCR13", "BCR14", "BCR15",
                                                                    "BCR16", "BCR17", "BCR18", "BCR19", "BCR20", "BCR21", "BCR22", "BCR23", "BCR24",
                                                                    "BCR25", "BCR26", "BCR27", "BCR28", "BCR29", "BCR30", "BCR31", "BCR32", "BCR33", "BCR34", "BCR35", "BCR36", "BCR37")),
                          plotOutput("plot")
            ), ),
  nav_panel("Tree", "Content"),
  nav_panel("Species Information", 
            fluidRow(
              column(8, style = "padding: 30px;",
                     uiOutput("species_info_detailed")
              ),
              column(4,
                     uiOutput("species_image")
              )
            ),
            hr(),
            fluidRow(
              column(12, style = "padding: 30px;",
                     h3("All Species Database"),
                     DTOutput("species_table_full")
              )
            )
  ),
  nav_panel("Species Comparison", 
            tags$style(type = "text/css", "#region_map {height: calc(100vh - 80px) !important;}", "#controls2 {background-color: white; padding: 0 20px 20px 20px; opacity: 0.65; zoom: 0.9}"),
            leafletOutput("region_map"),
            absolutePanel(id = "controls2", class = "panel panel-default",
                          top = 75, left = 55, width = 270, fixed=TRUE,
                          draggable = TRUE, height = "auto",
                          selectInput("region2", "Choose region:", c("", "BCR2", "BCR4", "BCR5", "BCR6", "BCR8", "BCR9", "BCR10", "BCR11", "BCR12", "BCR13", "BCR14", "BCR15",
                                                                    "BCR16", "BCR17", "BCR18", "BCR19", "BCR20", "BCR21", "BCR22", "BCR23", "BCR24",
                                                                    "BCR25", "BCR26", "BCR27", "BCR28", "BCR29", "BCR30", "BCR31", "BCR32", "BCR33", "BCR34", "BCR35", "BCR36", "BCR37")),
                          selectInput("select2", "Choose species:", c("", "Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", "Columba_livia", "Vireo_gilvus", "Falco_sparverius", "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", "Geothlypis_trichas")),
                          h3(""),
                          h4(""),
                          ),
            ), 
), )

# Define server logic
server <- function(input, output, session) {
  # Load data for plots
  plotDataInput <- reactive({
    req(input$select)
    req(input$region)
    selected_species <- input$select
    selected_region <- input$region
    d <- plot_data %>%
      filter(species == selected_species, region == selected_region)
  })
  
  # Get current species info
  current_species_info <- reactive({
    req(input$select)
    bird_species_info %>%
      filter(scientific_name == input$select)
  })
  
  # Prepare trend data for the selected species
  speciesTrendMapData <- reactive({
    req(input$select)
    
    # Get trend data for selected species
    species_trends <- trend_data %>%
      filter(species == input$select)
    
    # Join species data with spatial data
    map_data <- regions %>%
      left_join(species_trends, by = c("BCR_clean" = "region"))
    
    return(map_data)
  })
  
  output$map <- renderLeaflet({
    leaflet(regions) %>%
      setView(-104, 41, zoom = 4) %>%
      addTiles()
  })
  
  
  # Update map colors when data changes
  observe({
    data <- speciesTrendMapData()
    
    # Get all available slope data (remove p-value filtering)
    available_data <- data %>%
      filter(!is.na(slope))
    
    # Calculate symmetric range around zero for relative scaling
    max_abs_slope <- max(abs(available_data$slope), na.rm = TRUE)
    
    # Create custom color mapping function based on slope values only
    get_color <- function(slope, trend_category) {
      # Handle missing data first
      if (is.na(slope) || is.na(trend_category)) return("#F4F4F4")  # No Data - Very Light Gray
      
      # Handle stable trends (very small slopes)
      if (trend_category == "Stable") return("#D3D3D3")  # Stable - Light Gray
      
      # For all other trends, color based on relative magnitude
      if (max_abs_slope > 0) {
        slope_intensity <- abs(slope) / max_abs_slope  # Scale from 0 to 1
        
        if (slope > 0) {
          # Increasing: Light Green to Dark Green
          if (slope_intensity <= 0.5) {
            return("#b6d7a8")  # Light Green
          } else {
            return("#006600")  # Dark Green
          }
        } else {
          # Decreasing: Light Red to Dark Red  
          if (slope_intensity <= 0.5) {
            return("#ea9999")  # Light Red
          } else {
            return("#CC0000")  # Dark Red
          }
        }
      }
      
      return("#F4F4F4")  # Default fallback
    }
    
    # Apply colors to data
    data$map_color <- mapply(get_color, data$slope, data$trend_category)
    

    # Create popup content
    popup_content <- paste0(
      "<strong>Region:</strong> ", data$BCR_clean, "<br>",
      "<strong>Species:</strong> ", gsub("_", " ", input$select), "<br>",
      "<strong>Trend:</strong> ", ifelse(is.na(data$trend_category), "No Data", data$trend_category), "<br>",
      "<strong>Slope:</strong> ", round(data$slope, 4)
    )
    
    # Update polygons using leafletProxy
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        layerId = ~BCR_clean,
        fillColor = ~map_color,
        color = "white",
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
      clearControls() %>%
      addLegend(
        colors = c("#006600", "#b6d7a8", "#D3D3D3", "#ea9999", "#CC0000", "#F4F4F4"),
        labels = c("Strong Increase", "Moderate Increase", "Stable", "Moderate Decrease", "Strong Decrease", "No Data"),
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
  
  # Detailed Species Information Panel
  output$species_info_detailed <- renderUI({
    species_info <- current_species_info()
    
    if (nrow(species_info) == 0) return(div("No species selected"))
    
    div(
      style = "border: 1px solid #ddd; border-radius: 12px; padding: 25px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);",
      
      # Header 
      div(
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        div(
          style = "flex: 1;",
          h2(species_info$common_name, style = "color: #2E8B57; margin: 0;"),
          h5(em(species_info$scientific_name_display), style = "color: #666; margin: 5px 0;")
        )
      ),
      
      # All About Birds Species Description
      if (!is.na(species_info$description) && 
          !grepl("not available|please visit", species_info$description, ignore.case = TRUE)) {
        div(
          style = "margin-bottom: 20px; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
          p(species_info$description, 
            style = "line-height: 1.7; margin: 0; font-size: 16px; color: #333;"),
          p(em("Source: All About Birds"), 
            style = "font-size: 0.9em; color: #666; margin-top: 15px; margin-bottom: 0;")
        )
      }
    )
  })
  
  # Bird image display - centered and larger
  output$species_image <- renderUI({
    species_info <- current_species_info()
    
    if (nrow(species_info) == 0) return(div("No species selected"))
    
    # Display centered and larger image
    div(
      style = "padding: 30px; height: 100%;",
      if (!is.na(species_info$photo_url) && species_info$photo_url != "") {
        img(src = species_info$photo_url, 
            style = "max-width: 100%; max-height: 100%; width: auto; height: auto; object-fit: contain; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.2);")
      } else {
        div(
          style = "width: 350px; height: 300px; border: 2px dashed #ccc; border-radius: 12px; display: flex; align-items: center; justify-content: center; margin: 0 auto; color: #666;",
          p("No image available", style = "margin: 0;")
        )
      }
    )
  })
  
  # Full species table - fixed to properly display data
  output$species_table_full <- renderDT({
    # Add error handling and debugging
    tryCatch({
      if (nrow(bird_species_info) == 0) {
        return(datatable(data.frame("Message" = "No data available"), 
                         options = list(dom = 't'), 
                         rownames = FALSE))
      }
      
      # Check if required columns exist
      required_cols <- c("common_name", "scientific_name_display")
      missing_cols <- setdiff(required_cols, names(bird_species_info))
      
      if (length(missing_cols) > 0) {
        # If scientific_name_display doesn't exist, try scientific_name
        if ("scientific_name_display" %in% missing_cols && "scientific_name" %in% names(bird_species_info)) {
          display_data <- bird_species_info %>%
            select(common_name, scientific_name) %>%
            rename("Common Name" = common_name,
                   "Scientific Name" = scientific_name)
        } else {
          # Show what columns are available
          return(datatable(data.frame("Available columns" = names(bird_species_info)), 
                           options = list(pageLength = 10), 
                           rownames = FALSE))
        }
      } else {
        display_data <- bird_species_info %>%
          select(common_name, scientific_name_display) %>%
          rename("Common Name" = common_name,
                 "Scientific Name" = scientific_name_display)
      }
      
      datatable(display_data,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  autoWidth = TRUE,
                  columnDefs = list(
                    list(width = '300px', targets = c(0, 1))
                  )
                ),
                rownames = FALSE)
    }, error = function(e) {
      # Return error information
      datatable(data.frame("Error" = paste("Error loading data:", e$message),
                           "Data structure" = paste("Columns available:", paste(names(bird_species_info), collapse = ", ")),
                           "Rows" = nrow(bird_species_info)), 
                options = list(dom = 't'), 
                rownames = FALSE)
    })
  })
  
  # Prepare trend data for the selected species
  regionTrendMapData <- reactive({
    req(input$region2)
    
    # Get trend data for selected species
    species_trends <- trend_data %>%
      filter(species == input$select)
    
    # Join species data with spatial data
    map_data <- regions %>%
      left_join(species_trends, by = c("BCR_clean" = "region"))
    
    return(map_data)
  })
  
  output$region_map <- renderLeaflet({
    data <- regions
    leaflet() %>%
      setView(-98, 41, zoom = 4) %>%
      addTiles() %>%
      addPolygons(
        data = data,
        layerId = ~BCR_clean,
        #fillColor = ~map_color,
        color = "grey",
        weight = 1,
        opacity = 0.4,
        fillOpacity = 0.2,
        label = ~BCR_clean,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          bringToFront = TRUE
        )
      )
  })
  

  
  #   # Observer for region selection
  # observeEvent(input$region2, {
  #   if (input$region2 != "") {
  #     # Filter to selected region
  #     selected_region <- regions[regions$BCR_clean == input$region2, ]
  #     
  #     # Get bounding box of selected region
  #     bbox <- st_bbox(selected_region)
  #     
  #     # Extract coordinates as individual values to avoid named vector issues
  #     xmin <- as.numeric(bbox["xmin"])
  #     ymin <- as.numeric(bbox["ymin"])
  #     xmax <- as.numeric(bbox["xmax"])
  #     ymax <- as.numeric(bbox["ymax"])
  #     
  #     # Zoom to selected region
  #     leafletProxy("region_map") %>%
  #       fitBounds(lng1 = xmin, 
  #                 lat1 = ymin,
  #                 lng2 = xmax, 
  #                 lat2 = ymax,
  #                 options = list(padding = c(50, 50)))
  #   }
  # })
  
  

  
  # Observer for region selection
  observeEvent(input$region2, {
    if (input$region2 != "") {
      # Filter to selected region
      selected_region <- regions[regions$BCR_clean == input$region2, ]
      
      # Get bounding box of selected region
      bbox <- st_bbox(selected_region)
      
      # Extract coordinates as individual values to avoid named vector issues
      xmin <- as.numeric(bbox["xmin"])
      ymin <- as.numeric(bbox["ymin"])
      xmax <- as.numeric(bbox["xmax"])
      ymax <- as.numeric(bbox["ymax"])
      
      # Zoom to selected region
      leafletProxy("region_map") %>%
        # Clear existing highlights (optional)
        clearGroup("highlight") %>%
        # Highlight selected region
        addPolygons(data = selected_region,
                    #fillColor = "red",
                    #fillOpacity = 0.6,
                    color = "black",
                    weight = 3,
                    group = "highlight") %>%
        fitBounds(lng1 = xmin, 
                  lat1 = ymin,
                  lng2 = xmax, 
                  lat2 = ymax)
      }
  }) 
  
}


# Run the application
shinyApp(ui = ui, server = server)