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

# Load the bird species and bcr information we collected
bird_species_info <- readRDS("data/bird_species_info.rds")
bcr_info <- readRDS("data/processed/bcr_info_text.rds")


# Define UI for application
ui <- fillPage(theme = shinytheme("flatly"), navset_tab(
  nav_panel("Map", 
            tags$style(type = "text/css", 
                       "#map {height: calc(100vh - 80px) !important;}", 
                       "#controls {background-color: white; padding: 20px 20px 20px 20px; opacity: 0.8; zoom: 0.9}",
                       "#controls:hover {opacity: 0.95; transition-delay: 0}",
                       "#species_info_panel {background-color: white; padding: 15px; opacity: 0.95; zoom: 0.85; max-height: 600px; overflow-y: auto;}",
                       "#species_info_panel img {max-width: 200px; max-height: 150px; object-fit: cover; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.15);}",
                       "#species_info_panel .species-description {max-height: 300px; overflow-y: auto; font-size: 13px; line-height: 1.4;}"),
            leafletOutput("map"),
            absolutePanel(id = "controls", class = "panel panel-default",
                          top = 75, left = 55, width = 270, fixed=TRUE,
                          draggable = TRUE, height = 400,
                          selectInput("select", "Choose species:", c("", "Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", "Columba_livia", "Vireo_gilvus", "Falco_sparverius", "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", "Geothlypis_trichas")),
                          selectInput("region", "Choose region:", c("", "BCR2", "BCR4", "BCR5", "BCR6", "BCR8", "BCR9", "BCR10", "BCR11", "BCR12", "BCR13", "BCR14", "BCR15",
                                                                    "BCR16", "BCR17", "BCR18", "BCR19", "BCR20", "BCR21", "BCR22", "BCR23", "BCR24",
                                                                    "BCR25", "BCR26", "BCR27", "BCR28", "BCR29", "BCR30", "BCR31", "BCR32", "BCR33", "BCR34", "BCR35", "BCR36", "BCR37")),
                          plotOutput("plot")
            ), 
            conditionalPanel(
              condition = "input.select != ''",
              absolutePanel(id = "species_info_panel", class = "panel panel-default",
                            top = 75, right = 55, width = 340, fixed = TRUE,
                            draggable = TRUE, height = "auto",
                            uiOutput("species_info_map_panel")
              )
            )
  ),
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
            tags$style(type = "text/css", "#region_map {height: calc(100vh - 80px) !important;}", 
                       "#controls2 {background-color: white; padding: 0 20px 20px 20px; opacity: 0.65; zoom: 0.9}",
                       "#controls2:hover {opacity: 0.95; transition-delay: 0}",
                       "#comparison_plots {height: calc(50vh - 40px); overflow-y: auto;}"),
            leafletOutput("region_map"),
            absolutePanel(id = "controls2", class = "panel panel-default",
                          top = 75, left = 55, width = 350, fixed=TRUE,
                          draggable = TRUE, height = "auto",
                          selectInput("region2", "Choose region:", c("", "BCR2", "BCR4", "BCR5", "BCR6", "BCR8", "BCR9", "BCR10", "BCR11", "BCR12", "BCR13", "BCR14", "BCR15",
                                                                     "BCR16", "BCR17", "BCR18", "BCR19", "BCR20", "BCR21", "BCR22", "BCR23", "BCR24",
                                                                     "BCR25", "BCR26", "BCR27", "BCR28", "BCR29", "BCR30", "BCR31", "BCR32", "BCR33", "BCR34", "BCR35", "BCR36", "BCR37")),
                          conditionalPanel(
                            condition = "input.region2 != ''",
                            h4("Select Species (up to 4):"),
                            selectInput("species_selection", "",
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = TRUE)
                          ),
                          conditionalPanel(
                            condition = "input.region2 != '' && input.species_selection.length > 0",
                            checkboxInput("combined_plot", "Display as combined plot", value = TRUE),
                            hr(),
                            # Combined plot
                            conditionalPanel(
                              condition = "input.combined_plot",
                              plotOutput("combined_trends_plot", height = "300px")
                            ),
                            # Separate plots
                            conditionalPanel(
                              condition = "!input.combined_plot",
                              uiOutput("separate_plots")
                            )
                          )
            ),
            conditionalPanel(
              condition = "input.region2 != ''",
              absolutePanel(id = "bcr_info_panel", class = "panel panel-default",
                            top = 75, right = 55, width = 380, fixed = TRUE,
                            draggable = TRUE, height = "auto",
                            uiOutput("bcr_info_map_panel")
              )
            )
  ), 
  
  nav_panel("Methods & Data",
            tags$style(type = "text/css", 
                       ".methods-content {padding: 30px; max-width: 1200px; margin: 0 auto; height: calc(100vh - 120px); overflow-y: auto;}",
                       ".section-header {color: #2E8B57; border-bottom: 2px solid #2E8B57; padding-bottom: 10px; margin-bottom: 20px;}",
                       ".highlight-box {background: #f8f9fa; border-left: 4px solid #2E8B57; padding: 15px; margin: 15px 0; border-radius: 5px;}",
                       ".code-box {background: #f4f4f4; border: 1px solid #ddd; padding: 15px; border-radius: 5px; font-family: 'Courier New', monospace; font-size: 12px; overflow-x: auto;}"),
            div(class = "methods-content",
                h1("Modeling Methodology & Dataset Information", style = "text-align: center; color: #2E8B57; margin-bottom: 40px;"),
                
                # BBS Dataset Section
                div(
                  h2("North American Breeding Bird Survey (BBS)", class = "section-header"),
                  
                  div(class = "highlight-box",
                      h4("Dataset Overview", style = "color: #2E8B57; margin-top: 0;"),
                      p("The North American Breeding Bird Survey (BBS) is a cooperative effort between the U.S. Geological Survey and Environment and Climate Change Canada to monitor the status and trends of North American bird populations. Since 1966, the BBS has provided the scientific foundation for hundreds of conservation decisions and policies."),
                      
                      h5("Key Features:", style = "color: #2E8B57;"),
                      tags$ul(
                        tags$li("Long-term dataset spanning over 50 years (1966-present)"),
                        tags$li("Continental scale coverage across North America"),
                        tags$li("Standardized methodology ensuring data consistency"),
                        tags$li("Annual surveys conducted during peak breeding season (May-July)"),
                        tags$li("Over 4,000 survey routes across the continent")
                      )
                  ),
                  
                  h4("Survey Methodology"),
                  p("Each BBS route is 24.5 miles long with 50 stops spaced 0.5 miles apart. At each stop, trained observers conduct a 3-minute point count, recording all birds seen or heard within a 0.25-mile radius. Routes are surveyed once per year during peak breeding season, typically starting one-half hour before sunrise."),
                  
                  h4("Bird Conservation Regions (BCRs)"),
                  p("The analysis is organized by Bird Conservation Regions, which are ecologically distinct areas with similar bird communities, habitats, and resource management issues. BCRs provide a biologically meaningful framework for analyzing population trends across North America's diverse landscapes."),
                  
                  div(class = "highlight-box",
                      h5("Data Processing:", style = "color: #2E8B57; margin-top: 0;"),
                      tags$ul(
                        tags$li("Routes with adequate sampling effort and data quality"),
                        tags$li("Species-region combinations with sufficient data for trend estimation"),
                        tags$li("Standardization for observer effects and survey conditions"),
                        tags$li("Integration across multiple spatial and temporal scales")
                      )
                  )
                ),
                
                # GAM Modeling Section
                div(
                  h2("Generalized Additive Model (GAM) Approach", class = "section-header"),
                  
                  div(class = "highlight-box",
                      h4("Why GAMs for Bird Population Trends?", style = "color: #2E8B57; margin-top: 0;"),
                      p("Generalized Additive Models are particularly well-suited for analyzing bird population trends because they:"),
                      tags$ul(
                        tags$li("Handle non-linear temporal patterns without assuming specific functional forms"),
                        tags$li("Incorporate spatial correlation across regions"),
                        tags$li("Account for phylogenetic relationships among species"),
                        tags$li("Provide uncertainty quantification for trend estimates"),
                        tags$li("Scale efficiently to large, complex datasets")
                      )
                  ),
                  
                  h4("Model Architecture"),
                  p("Our modeling approach uses a hierarchical GAM framework that decomposes population trends into multiple components:"),
                  
                  div(class = "code-box",
                      "count ~ s(year) + ti(year, region) + ti(year, species_phylo) + \n",
                      "        ti(year, species) + ti(year, region, species_phylo) + \n",
                      "        ti(year, region, species) + offset(log(n_routes))"
                  ),
                  
                  h5("Model Components:", style = "color: #2E8B57;"),
                  tags$ul(
                    tags$li(strong("Temporal smooth (s(year)):"), " Overall continental trend pattern"),
                    tags$li(strong("Spatiotemporal effects (ti(year, region)):"), " Region-specific deviations from continental trend"),
                    tags$li(strong("Phylogenetic effects (ti(year, species_phylo)):"), " Shared trends among related species"),
                    tags$li(strong("Species-specific effects (ti(year, species)):"), " Individual species deviations"),
                    tags$li(strong("Higher-order interactions:"), " Species-specific spatiotemporal patterns informed by phylogeny"),
                    tags$li(strong("Survey effort offset:"), " Accounts for variation in sampling intensity")
                  ),
                  
                  h4("Advanced Features"),
                  
                  h5("Phylogenetic Information", style = "color: #2E8B57;"),
                  p("The model incorporates evolutionary relationships through Markov Random Field (MRF) smooths based on phylogenetic distance. This allows closely related species to share information, improving trend estimates for rare species while respecting evolutionary constraints."),
                  
                  h5("Spatial Correlation", style = "color: #2E8B57;"),
                  p("Spatial relationships among Bird Conservation Regions are modeled using neighborhood-based MRF smooths, ensuring that geographically adjacent regions with similar ecological conditions share information appropriately."),
                  
                  div(class = "highlight-box",
                      h5("Model Fitting Details:", style = "color: #2E8B57; margin-top: 0;"),
                      tags$ul(
                        tags$li("Fast Restricted Maximum Likelihood (fREML) estimation"),
                        tags$li("Automatic smoothness selection via generalized cross-validation"),
                        tags$li("Bayesian posterior simulation for uncertainty quantification"),
                        tags$li("10% holdout validation set for model assessment"),
                        tags$li("Computational efficiency through discrete fitting methods")
                      )
                  )
                ),
                
                # Trend Interpretation Section
                div(
                  h2("Trend Interpretation", class = "section-header"),
                  
                  h4("Expected vs. Observed Trends"),
                  p("The visualizations show 'expected trends' which represent the model's estimate of what population changes would have occurred with standardized survey effort across all years. This approach:"),
                  tags$ul(
                    tags$li("Controls for changes in the number of survey routes over time"),
                    tags$li("Provides more interpretable relative population changes"),
                    tags$li("Enables fair comparison across regions and species"),
                    tags$li("Reduces confounding between survey effort and true population changes")
                  ),
                  
                  h4("Uncertainty Visualization"),
                  p("Confidence ribbons around trend lines represent 95% credible intervals from the model's posterior distribution. Wider ribbons indicate greater uncertainty, typically occurring for:"),
                  tags$ul(
                    tags$li("Species or regions with limited data"),
                    tags$li("Time periods with high environmental variability"),
                    tags$li("Beginning and end of time series (edge effects)"),
                    tags$li("Species undergoing rapid population changes")
                  ),
                  
                  div(class = "highlight-box",
                      h5("Trend Categories:", style = "color: #2E8B57; margin-top: 0;"),
                      tags$ul(
                        tags$li(strong("Strong Increase:"), " Substantial upward trend with high confidence"),
                        tags$li(strong("Moderate Increase:"), " Positive trend with moderate confidence"),  
                        tags$li(strong("Stable:"), " No significant directional change"),
                        tags$li(strong("Moderate Decrease:"), " Negative trend with moderate confidence"),
                        tags$li(strong("Strong Decrease:"), " Substantial downward trend with high confidence"),
                        tags$li(strong("No Data:"), " Insufficient data for reliable trend estimation")
                      )
                  )
                ),
                
                # Applications and Limitations
                div(
                  h2("Applications & Limitations", class = "section-header"),
                  
                  h4("Conservation Applications"),
                  tags$ul(
                    tags$li("Identifying species and regions of conservation concern"),
                    tags$li("Tracking progress toward conservation targets"),
                    tags$li("Informing habitat management decisions"),
                    tags$li("Supporting policy development and resource allocation"),
                    tags$li("Detecting early warning signals of population declines")
                  ),
                  
                  h4("Model Limitations"),
                  div(class = "highlight-box",
                      tags$ul(style = "margin: 0;",
                              tags$li("Trends reflect breeding populations along roadsides, not total populations"),
                              tags$li("Species detectability may vary across regions and time"),
                              tags$li("Limited coverage in remote areas and certain habitat types"),
                              tags$li("Potential biases from observer effects and route accessibility"),
                              tags$li("Causal interpretation requires additional ecological context")
                      )
                  )
                ),
                
                # References
                div(
                  h2("References & Data Sources", class = "section-header"),
                  
                  h4("Primary Data Source"),
                  p("Pardieck, K.L., D.J. Ziolkowski Jr., M. Lutmerding, V.I. Aponte, and M.-A.R. Hudson. 2020. North American Breeding Bird Survey Dataset 1966-2019, version 2019.0. U.S. Geological Survey, Patuxent Wildlife Research Center."),
                  
                  h4("Methodological References"),
                  tags$ul(
                    tags$li("Wood, S.N. (2017). Generalized Additive Models: An Introduction with R, Second Edition. CRC Press."),
                    tags$li("Sauer, J.R. et al. (2017). The first 50 years of the North American Breeding Bird Survey. The Condor 119(3): 576-593."),
                    tags$li("Bird Conservation Regions: North American Bird Conservation Initiative. (2022). The State of Canada's Birds."),
                    tags$li("Phylogenetic analysis methods based on contemporary phylogenetic reconstruction techniques.")
                  ),
                  
                  div(class = "highlight-box",
                      p(strong("Data Availability:"), " Raw BBS data are freely available through the USGS Patuxent Wildlife Research Center. This analysis represents a subset of the full BBS dataset, processed and analyzed using advanced statistical methods to provide robust trend estimates.", style = "margin: 0;")
                  )
                )
            )
  ),
  
  nav_panel("Info",
            tags$style(type = "text/css", 
                       ".methods-content {padding: 30px; max-width: 900px; margin: 0 auto; height: calc(100vh - 120px); overflow-y: auto;}
              .section-header {color: #2E8B57; border-bottom: 2px solid #2E8B57; padding-bottom: 10px; margin-bottom: 20px;}
              .highlight-box {background: #f8f9fa; border-left: 4px solid #2E8B57; padding: 15px; margin: 15px 0; border-radius: 5px;}
              details summary {
                background: #e9f5ef;
                border: 1px solid #2E8B57;
                border-radius: 5px;
                padding: 10px;
                margin: 10px 0;
                cursor: pointer;
                font-weight: bold;
                color: #2E8B57;
                list-style: none;
              }
              details[open] summary {
                background: #d3efe0;
              }
              details summary::before {
                content: '▶ ';
                font-size: 0.9em;
              }
              details[open] summary::before {
                content: '▼ ';
              }"),
            
            div(class = "methods-content",
                
                h1("How We Studied Bird Populations", 
                   style = "text-align: center; color: #2E8B57; margin-bottom: 40px;"),
                
                # DATA SECTION
                div(
                  h2("About the Data", class = "section-header"),
                  p("The data come from the North American Breeding Bird Survey, 
           a project running since 1966. Volunteers cover fixed routes each year, 
           stopping to count all birds they see or hear. 
           This creates one of the largest and longest-running bird monitoring programs in the world."),
                  
                  div(class = "highlight-box",
                      tags$ul(
                        tags$li("📅 Over 50 years of data"),
                        tags$li("🌎 Coverage across North America"),
                        tags$li("🔁 Same method every year for consistency")
                      )
                  ),
                  
                  tags$details(
                    tags$summary("Show technical details"),
                    p("Each survey route is 24.5 miles long with 50 stops. 
             At each stop, observers record all birds within 0.25 miles in 3 minutes. 
             Data are organized by Bird Conservation Regions (BCRs).")
                  )
                ),
                
                # MODELLING SECTION
                div(
                  h2("How We Estimate Trends", class = "section-header"),
                  p("Bird populations don’t always change in a straight line. 
           We use a flexible model that captures curved trends over time 
           and accounts for differences between regions and related species."),
                  
                  div(class = "highlight-box",
                      tags$ul(
                        tags$li("📊 Detects non-linear (wiggly) trends"),
                        tags$li("📍 Accounts for regional differences"),
                        tags$li("🧬 Shares information across related species")
                      )
                  ),
                  
                  tags$details(
                    tags$summary("Show technical details"),
                    p("We use a hierarchical Generalized Additive Model (GAM):"),
                    tags$pre(
                      "count ~ s(year) + ti(year, region) + ti(year, species_phylo) + 
                   ti(year, species) + ti(year, region, species_phylo) + 
                   ti(year, region, species) + offset(log(n_routes))"
                    ),
                    p("The model uses fREML estimation, automatic smoothness selection, 
             and Bayesian simulation for uncertainty.")
                  )
                ),
                
                # INTERPRETING RESULTS
                div(
                  h2("How to Read the Graphs", class = "section-header"),
                  p("The solid line shows the estimated population trend. 
           The shaded ribbon shows uncertainty — wider ribbons mean less certainty."),
                  
                  div(class = "highlight-box",
                      tags$ul(
                        tags$li("⬆️ Strong Increase – populations rising confidently"),
                        tags$li("➖ Stable – no significant change"),
                        tags$li("⬇️ Strong Decrease – clear population decline")
                      )
                  ),
                  
                  tags$details(
                    tags$summary("Show technical details"),
                    p("Trends are standardized for survey effort. 
             Uncertainty intervals represent 95% credible intervals 
             from the model’s posterior distribution.")
                  )
                ),
                
                # APPLICATIONS & LIMITATIONS
                div(
                  h2("Applications & Limitations", class = "section-header"),
                  h4("Why This Matters"),
                  tags$ul(
                    tags$li("Identify species and regions needing conservation"),
                    tags$li("Track progress toward conservation goals"),
                    tags$li("Provide early warning of population declines")
                  ),
                  
                  h4("Limitations"),
                  div(class = "highlight-box",
                      tags$ul(
                        tags$li("Counts reflect birds along roadsides, not all habitats"),
                        tags$li("Some species are harder to detect than others"),
                        tags$li("Remote areas have limited coverage")
                      )
                  )
                ),
                
                # REFERENCES
                div(
                  h2("Data Access", class = "section-header"),
                  p("The raw survey data are freely available from the USGS Patuxent Wildlife Research Center. 
           Our analysis uses a processed subset for more reliable estimates."),
                  
                  tags$details(
                    tags$summary("References"),
                    tags$ul(
                      tags$li("Pardieck, K.L. et al. (2020). North American Breeding Bird Survey Dataset 1966-2019."),
                      tags$li("Wood, S.N. (2017). Generalized Additive Models: An Introduction with R."),
                      tags$li("Sauer, J.R. et al. (2017). The first 50 years of the North American Breeding Bird Survey.")
                    )
                  )
                )
            )
  )

), 

)

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
  
  output$species_info_map_panel <- renderUI({
    species_info <- current_species_info()
    
    if (nrow(species_info) == 0) return(div("No species selected"))
    
    div(
      # Header with species names
      div(
        style = "margin-bottom: 15px; text-align: center;",
        h4(species_info$common_name, 
           style = "color: #2E8B57; margin: 0; font-size: 16px; font-weight: bold;"),
        p(em(ifelse(is.na(species_info$scientific_name_display) | species_info$scientific_name_display == "", 
                    species_info$scientific_name, 
                    species_info$scientific_name_display)), 
          style = "color: #666; margin: 5px 0; font-size: 13px;")
      ),
      
      # Image
      div(
        style = "text-align: center; margin-bottom: 15px;",
        if (!is.na(species_info$photo_url) && species_info$photo_url != "") {
          img(src = species_info$photo_url)
        } else {
          div(
            style = "width: 200px; height: 150px; border: 2px dashed #ccc; border-radius: 8px; display: flex; align-items: center; justify-content: center; margin: 0 auto; color: #666;",
            p("No image available", style = "margin: 0; font-size: 12px;")
          )
        }
      ),
      
      # Description
      if (!is.na(species_info$description) && 
          !grepl("not available|please visit", species_info$description, ignore.case = TRUE)) {
        div(
          class = "species-description",
          style = "padding: 10px; background: #f8f9fa; border-radius: 5px; border-left: 3px solid #2E8B57;",
          p(species_info$description, 
            style = "margin: 0; color: #333; font-size: 14px;"),
          p(em("Source: All About Birds"), 
            style = "font-size: 11px; color: #666; margin-top: 8px; margin-bottom: 0;")
        )
      } else {
        div(
          style = "padding: 10px; background: #f8f9fa; border-radius: 5px; color: #666; font-style: italic;",
          p("Description not available", style = "margin: 0; font-size: 13px;")
        )
      }
    )
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
  
  

  
  # Observer for drop-down region selection
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
  
  # Observer for mouse-click region selection
  observeEvent(input$region_map_shape_click, {
    click <- input$region_map_shape_click
    if (!is.null(click$id)){
      # Update region selector based on map click
      updateSelectInput(session, "region2", selected = click$id)
    }
  })
  
  # Update species options based on selected region
  observeEvent(input$region2, {
    if (input$region2 != "") {
      # Get available species for the selected region
      available_species <- plot_data %>%
        filter(region == input$region2) %>%
        distinct(species) %>%
        pull(species)
      
      # Update the dropdown with available species (scientific names only)
      updateSelectInput(session, "species_selection",
                        choices = available_species,
                        selected = NULL)
    } else {
      # Clear species selection when no region is selected
      updateSelectInput(session, "species_selection",
                        choices = NULL,
                        selected = NULL)
    }
  })
  
  # Limit species selection to 4
  observeEvent(input$species_selection, {
    if (length(input$species_selection) > 4) {
      updateSelectInput(session, "species_selection",
                        selected = input$species_selection[1:4])
    }
  })
  
  # Get comparison data for selected species and region
  comparisonData <- reactive({
    req(input$region2)
    req(length(input$species_selection) > 0)
    
    # Get plot data for selected species and region
    comparison_data <- plot_data %>%
      filter(species %in% input$species_selection, 
             region == input$region2)
    
    return(comparison_data)
  })
  
  # Combined trends plot
  output$combined_trends_plot <- renderPlot({
    data <- comparisonData()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for selected species and region") +
               theme_void())
    }
    
    # Define colors for up to 4 species
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
    
    ggplot(data, aes(x = year, y = pred, color = species, fill = species)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
      geom_line(linewidth = 1, alpha = 0.8) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      labs(title = paste("Population Trends in", input$region2),
           x = "Year", 
           y = "Expected Trend",
           color = "Species",
           fill = "Species") +
      theme_classic() +
      theme(plot.title = element_text(size = 14, hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10)) +
      guides(color = guide_legend(override.aes = list(alpha = 1)),
             fill = guide_legend(override.aes = list(alpha = 0.3)))
  })
  
  # Separate plots
  output$separate_plots <- renderUI({
    data <- comparisonData()
    
    if (nrow(data) == 0) {
      return(div("No data available for selected species and region"))
    }
    
    # Create individual plots for each species
    species_list <- unique(data$species)
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
    
    # Calculate height based on number of species (minimum 400px for 1 species, add 200px for each additional)
    total_height <- max(400, 200 * length(species_list))
    
    plot_outputs <- lapply(seq_along(species_list), function(i) {
      species <- species_list[i]
      output_id <- paste0("species_plot_", i)
      
      output[[output_id]] <- renderPlot({
        species_data <- data %>% filter(species == !!species)
        
        ggplot(species_data, aes(x = year, y = pred)) +
          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = colors[i]) +
          geom_line(linewidth = 1, alpha = 0.8, color = colors[i]) +
          labs(title = paste(species, "in", input$region2),
               x = "Year", 
               y = "Expected Trend") +
          theme_classic() +
          theme(plot.title = element_text(size = 12, hjust = 0.5))
      }, height = 180)
      
      plotOutput(output_id, height = "180px")
    })
    
    do.call(tagList, plot_outputs)
  })
  
  # Get current region info
  current_region_info <- reactive({
    req(input$region2)
    number_only <- gsub("[^0-9]", "", input$region2)
    bcr_info %>%
      filter(bcr_number == number_only)
  })
  
  # Display region info 
  output$bcr_info_map_panel <- renderUI({
    bcr_info <- current_region_info()
    
    if (nrow(bcr_info) == 0) return(div("No region selected"))
    
    div(
      # Header with species names
      div(
        style = "padding: 10px; margin-bottom: 15px; text-align: center;",
        h4(bcr_info$bcr_name, 
           style = "color: #2E8B57; margin: 0; font-size: 16px; font-weight: bold;"),
        #p(em(ifelse(is.na(species_info$scientific_name_display) | species_info$scientific_name_display == "", 
        #            species_info$scientific_name, 
        #            species_info$scientific_name_display)), 
        #  style = "color: #666; margin: 5px 0; font-size: 13px;")
      ),
      
      # Description
      if (!is.na(bcr_info$bcr_description)) {
        div(
          class = "bcr-description",
          style = "padding: 10px; background: #f8f9fa; border-radius: 5px; border-left: 3px solid #2E8B57;",
          p(bcr_info$bcr_description, 
            style = "margin: 0; color: #333; font-size: 14px;"),
          p(em("Source: North American Bird Conservation Initiative"), 
            style = "font-size: 11px; color: #666; margin-top: 8px; margin-bottom: 0;")
        )
      } else {
        div(
          style = "padding: 10px; background: #f8f9fa; border-radius: 5px; color: #666; font-style: italic;",
          p("Description not available", style = "margin: 0; font-size: 13px;")
        )
      }
    )
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)