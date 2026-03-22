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
library(shinycssloaders)


# Load data objects
bird_species_info <- readRDS("data/bird_species_info.rds")
bcr_info <- readRDS("data/processed/bcr_info_text_short.rds")
bcr_images <- read.csv("bcr_image.csv")
regions <- readRDS("data/processed/bcr_simplified.rds")
plot_data <- readRDS("data/processed/plot_data.rds")
family_info <- readRDS("data/processed/bird_family_info_manual.rds")
order_info <- readRDS("data/processed/bird_order_info_manual.rds")
group_info <- readRDS("data/processed/grouping_info.rds")
load("data/processed/model_objects.rda")

# Load ecological data
species_ecological_data <- readRDS("data/species_ecological_avonet_pif.rds")
grouping_choices <- readRDS("data/grouping_choices_avonet_pif.rds")

# Load pre-calculated trends
trend_data <- readRDS("data/processed/trend_data_precalc.rds")
group_trends_precalc <- readRDS("data/processed/group_trends_precalc.rds")
group_trend_stats <- readRDS("data/processed/group_trend_stats.rds")
plot_data <- readRDS("data/processed/plot_data_with_counts.rds")

# Create a list of grouping types for the UI
grouping_types <- names(grouping_choices)

# List of all species and regions available, sorted
species_list <- sort(as.character(unique(mod_data$sp_latin)))
region_list <- levels(mod_data$strata_name)


# Define UI for application
ui <- fillPage(theme = shinytheme("spacelab"), navset_tab(
  id = "main_tabs",
  nav_panel("Getting Started",
            tags$style(type = "text/css", 
                       ".getting-started {
      padding: 40px 60px;
      max-width: 1000px;
      margin: 0 auto;
      font-size: 15px;
      line-height: 1.7;
      height: calc(100vh - 80px);
      overflow-y: auto;
    }
    .hero-section {
      text-align: center;
      padding: 40px 20px;
      background: linear-gradient(135deg, #e8f5e9 0%, #f1f8e9 100%);
      border-radius: 10px;
      margin-bottom: 40px;
    }
    .hero-section h1 {
      color: #2E8B57;
      font-size: 32px;
      margin-bottom: 15px;
    }
    .hero-section p {
      font-size: 18px;
      color: #555;
      max-width: 700px;
      margin: 0 auto 25px;
    }
    .quick-start-box {
      background: #fff;
      border-left: 5px solid #2E8B57;
      padding: 25px 30px;
      margin: 30px 0;
      border-radius: 5px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }
    .quick-start-box h3 {
      color: #2E8B57;
      margin-top: 0;
      font-size: 22px;
    }
    .quick-start-box ol {
      font-size: 16px;
      line-height: 2;
    }
    .quick-start-box ol li {
      margin: 10px 0;
    }
    .quick-start-box strong {
      color: #2E8B57;
    }
    .feature-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
      gap: 20px;
      margin: 30px 0;
    }
    .feature-card {
      background: white;
      border: 1px solid #ddd;
      padding: 20px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.08);
      transition: transform 0.2s, box-shadow 0.2s;
    }
    .feature-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    }
    .feature-card h4 {
      color: #2E8B57;
      margin-top: 0;
      font-size: 18px;
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .feature-icon {
      font-size: 24px;
    }
    .use-case-box {
      background: #f8f9fa;
      border-radius: 8px;
      padding: 20px;
      margin: 15px 0;
    }
    .use-case-box h5 {
      color: #2E8B57;
      margin-top: 0;
      font-size: 16px;
    }
    .use-case-box p {
      margin: 5px 0;
      color: #555;
    }
    .use-case-box .steps {
      color: #666;
      font-size: 14px;
      margin-top: 10px;
      padding-left: 20px;
    }
    .help-section {
      background: #fff3cd;
      border: 1px solid #ffc107;
      border-radius: 8px;
      padding: 20px;
      margin: 30px 0;
    }
    .help-section h3 {
      color: #856404;
      margin-top: 0;
    }
    .cta-buttons {
      display: flex;
      gap: 15px;
      justify-content: center;
      margin: 40px 0;
      flex-wrap: wrap;
    }
    .btn-primary-custom {
      background: #2E8B57;
      color: white;
      padding: 15px 35px;
      font-size: 18px;
      border: none;
      border-radius: 8px;
      cursor: pointer;
      text-decoration: none;
      display: inline-block;
      transition: background 0.2s;
    }
    .btn-primary-custom:hover {
      background: #267347;
      color: white;
      text-decoration: none;
    }
    .btn-secondary-custom {
      background: white;
      color: #2E8B57;
      padding: 15px 35px;
      font-size: 18px;
      border: 2px solid #2E8B57;
      border-radius: 8px;
      cursor: pointer;
      text-decoration: none;
      display: inline-block;
      transition: all 0.2s;
    }
    .btn-secondary-custom:hover {
      background: #f0f8ff;
      text-decoration: none;
    }
    .tip-box {
      background: #e3f2fd;
      border-left: 4px solid #2196F3;
      padding: 15px;
      margin: 15px 0;
      border-radius: 4px;
    }
    .tip-box strong {
      color: #1976D2;
    }
    @media (max-width: 768px) {
      .getting-started {
        padding: 20px 15px;
      }
      .hero-section h1 {
        font-size: 24px;
      }
      .hero-section p {
        font-size: 16px;
      }
      .feature-grid {
        grid-template-columns: 1fr;
      }
      .cta-buttons {
        flex-direction: column;
      }
    }
  "),
            
            div(class = "getting-started",
                
                # Hero Section
                div(class = "hero-section",
                    h1("North American Bird Population Trends"),
                    p("Explore 30+ years of bird population data across North America. 
         Discover how species are changing, compare trends across regions, 
         and understand conservation challenges."),
                    div(class = "cta-buttons",
                        actionButton("go_to_map_hero", "Start Exploring →", 
                                     class = "btn-primary-custom"),
                        actionButton("go_to_methods", "How It Works", 
                                     class = "btn-secondary-custom")
                    )
                ),
                
                # Quick Start
                div(class = "quick-start-box",
                    h3("Quick Start Guide"),
                    p("How to generate your first population trend:"),
                    tags$ol(
                      tags$li("Click the ", strong("Trend Map"), " tab at the top of this page"),
                      tags$li("In ", strong("'Choose species'"), ", start typing a bird name 
                (e.g., 'robin', 'hawk'), or use the dropdown"),
                      tags$li("Click a ", strong("coloured region"), " on the map, or use the 
                ", strong("'Choose region'"), " dropdown"),
                      tags$li(strong("View the trend!"), " The plot shows how the population 
                has changed over time")
                    )
                ),
                
                
                
                # Use Cases
                h2("Example Use Cases", style = "color: #2E8B57; margin-top: 50px;"),
                p("Not sure where to start? Here are some questions this app can answer:"),
                
                div(class = "use-case-box",
                    h5("\"Are robins increasing in my region?\""),
                    p(strong("Goal:"), " Track a familiar backyard species"),
                    p(class = "steps", 
                      "→ Map tab → Select 'American Robin' → Click your region → View trend")
                ),
                
                div(class = "use-case-box",
                    h5("\"Which grassland birds are declining?\""),
                    p(strong("Goal:"), " Conservation planning for habitat management"),
                    p(class = "steps", 
                      "→ Map tab → Choose 'Species Group' → Select 'Habitat Type: Grassland' 
        → Click a grassland region → See combined trend")
                ),
                
                div(class = "use-case-box",
                    h5("\"How do different warbler species compare in New England?\""),
                    p(strong("Goal:"), " Multi-species comparison in one area"),
                    p(class = "steps", 
                      "→ Region Comparison tab → Select a northeastern BCR (e.g., BCR14) 
        → Choose 3 warbler species → Compare their trends")
                ),
                
                div(class = "use-case-box",
                    h5("\"Are any raptor families doing well?\""),
                    p(strong("Goal:"), " Taxonomic group analysis"),
                    p(class = "steps", 
                      "→ Map tab → 'Species Group' → 'Taxonomic Family: Accipitridae' (hawks) 
        → Explore different regions")
                ),
                
                div(class = "use-case-box",
                    h5("\"What's happening with birds of conservation concern?\""),
                    p(strong("Goal:"), " Priority species monitoring"),
                    p(class = "steps", 
                      "→ Map tab → 'Species Group' → 'Conservation Status: High Concern' 
        → Map shows regional patterns")
                )
              
            )
  ),
  nav_panel("Trend Map", 
            tags$style(type = "text/css", 
                       "#map {height: calc(100vh - 80px) !important;}", 
                       "#controls {background-color: white; padding: 20px 20px 20px 20px; opacity: 0.8; zoom: 0.9}",
                       "#controls:hover {opacity: 0.95; transition-delay: 0}",
                       "#species_info_panel {background-color: white; padding: 15px; opacity: 0.95; zoom: 0.85; max-height: 850px; overflow-y: auto;}",
                       "#species_info_panel img {max-width: 200px; max-height: 150px; object-fit: cover; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.15);}",
                       "#species_info_panel .species-description {max-height: 300px; overflow-y: auto; font-size: 13px; line-height: 1.4;}"),
            leafletOutput("map"),
            absolutePanel(id = "controls", class = "panel panel-default",
                          top = 75, left = 55, width = 350, fixed=TRUE,
                          draggable = TRUE, height = "auto",
                          
                          # Add radio buttons for selection type 
                          div(
                            style = "margin-bottom: 10px;",
                            div(
                              style = "display: flex; align-items: center; gap: 5px; margin-bottom: 5px;",
                              tags$label("Analysis Type:", style = "margin: 0;"),
                              tags$span(
                                title = "Individual Species: View trends for a single species across regions. Species Group: View combined trends for groups of species with shared characteristics (e.g., same family, habitat, or migration pattern).",
                                style = "cursor: help; color: #2E8B57; font-size: 16px;",
                                "ⓘ"
                              )
                            ),
                            radioButtons("selection_type", label = NULL,
                                         choices = list("Individual Species" = "species",
                                                        "Species Group" = "group"),
                                         selected = "species")
                          ),
                          # Add radio buttons for name display preference
                          radioButtons("name_format", "Species names:",
                                       choices = list("Common names" = "common",
                                                      "Scientific names" = "scientific"),
                                       selected = "common",
                                       inline = TRUE),
                          
                          # Species selection (show when individual species selected)
                          conditionalPanel(
                            condition = "input.selection_type == 'species'",
                            selectizeInput("select", "Choose species (start typing to search):", 
                                        choices = NULL,
                                        options = list(placeholder = "Type or scroll to select a species..."))
                          ),
                        
                          
                          # Group selection (show when group selected)
                          conditionalPanel(
                            condition = "input.selection_type == 'group'",
                            selectInput("grouping_type", "Group by:",
                                        choices = c("", grouping_types),
                                        selected = ""),
                            conditionalPanel(
                              condition = "input.grouping_type != ''",
                              uiOutput("group_selector")
                            )
                          ),
                          
                          div(
                            style = "display: flex; align-items: center; gap: 5px;",
                            selectInput("region", "Choose region:", 
                                        choices = c("", region_list),
                                        selected = ""),
                            tags$span(
                              title = "Bird Conservation Regions (BCRs) are ecologically distinct regions in North America with similar bird communities, habitats, and resource management issues.",
                              style = "cursor: help; color: #2E8B57; font-size: 16px; position: relative; top: 8px;",
                              "ⓘ"
                            )
                          ),
                          tags$div(
                            style = "margin-top: 30px;",
                            
                            # Plot
                            div(
                              style = "width: 300px; height: 300px;",
                              plotOutput("plot", width = "100%", height = "100%") %>%
                                withSpinner(color = "#2E8B57", type = 4, size = 0.8)
                            ),
                            
                            # Info box below plot
                            div(
                              style = "margin-top: 10px; padding: 8px; background: #e3f2fd; border-left: 4px solid #2196F3; border-radius: 4px;",
                              p(
                                strong("Reading the Y-axis:"), 
                                style = "margin: 0 0 3px 0; color: #1976D2; font-size: 12px;"
                              ),
                              p(
                                "0 = long-term average | ±1 = one standard deviation from average",
                                style = "margin: 0; font-size: 11px; line-height: 1.3;"
                              )
                            )
                          )
   
            ),
            conditionalPanel(
              condition = "((input.selection_type == 'species' && input.select != '') || (input.selection_type == 'group' && input.grouping_type != '' && input.selected_group != ''))",
              absolutePanel(id = "species_info_panel", class = "panel panel-default",
                            top = 75, right = 55, width = 380, fixed = TRUE,
                            draggable = FALSE, height = "auto",
                            uiOutput("info_panel") %>%
                              withSpinner(color = "#2E8B57", type = 4, size = 0.8)
              )
            )
  ),
  nav_panel("Region Comparison", 
            tags$style(type = "text/css", "#region_map {height: calc(100vh - 80px) !important;}", 
                       "#controls2 {background-color: white; padding: 20px 20px 20px 20px; opacity: 0.8; zoom: 0.9}",
                       "#controls2:hover {opacity: 0.95; transition-delay: 0}",
                       "#comparison_plots {height: calc(50vh - 40px); overflow-y: auto;}"),
            leafletOutput("region_map"),
            absolutePanel(id = "controls2", class = "panel panel-default",
                          top = 75, left = 55, width = 350, fixed=TRUE,
                          draggable = TRUE, height = "auto",
                          div(
                            style = "display: flex; align-items: center; gap: 5px;",
                            selectInput("region2", "Choose region:", 
                                        choices = region_list,
                                        selected = character(0)),
                            tags$span(
                              title = "Bird Conservation Regions (BCRs) are ecologically distinct regions in North America with similar bird communities, habitats, and resource management issues.",
                              style = "cursor: help; color: #2E8B57; font-size: 16px; position: relative; top: 8px;",
                              "ⓘ"
                            )
                          ),
                          # Add radio buttons for name display preference
                          radioButtons("name_format_region", "Species names:",
                                       choices = list("Common names" = "common",
                                                      "Scientific names" = "scientific"),
                                       selected = "common",
                                       inline = TRUE),
                          conditionalPanel(
                            condition = "input.region2 != ''",
                            selectInput("species_selection", "Choose species (up to 3):",
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
                              div(
                                style = "width: 310px; height: 310px;",
                                plotOutput("combined_trends_plot", width = "100%", height = "100%") %>%
                                  withSpinner(color = "#2E8B57", type = 4, size = 0.8)
                              )
                            ),
                            # Separate plots
                            conditionalPanel(
                              condition = "!input.combined_plot",
                              div(
                                style = "width: 310px;",
                                uiOutput("separate_plots")
                              )
                            )
                          )
            ),
            conditionalPanel(
              condition = "input.region2 != ''",
              absolutePanel(id = "bcr_info_panel", class = "panel panel-default",
                            top = 75, right = 55, width = 380, fixed = TRUE,
                            draggable = TRUE, height = "auto",
                            uiOutput("bcr_info_map_panel") %>%
                              withSpinner(color = "#2E8B57", type = 4, size = 0.8)
              )
            )
  ), 
  
  nav_panel("About",
              tags$style(type = "text/css", 
                         ".methods-content {padding: 30px; max-width: 900px; margin: 0 auto; height: calc(100vh - 120px); overflow-y: auto;}
              .section-header {color: #2E8B57; border-bottom: 2px solid #2E8B57; padding-bottom: 10px; margin-bottom: 20px;}
              .highlight-box {background: #f8f9fa; border-left: 4px solid #2E8B57; padding: 15px; margin: 15px 0; border-radius: 5px;}
              .info-callout {background: #e3f2fd; border-left: 4px solid #2196F3; padding: 15px; margin: 15px 0; border-radius: 5px;}
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
                  
                  h1("Modeling Methodology & Dataset Information", 
                     style = "text-align: center; color: #2E8B57; margin-bottom: 40px;"),
                  
                  # DATA SECTION
                  div(
                    h2("About the Data", class = "section-header"),
                    p("The data come from the ", strong("North American Breeding Bird Survey,"), 
                      "a volunteer-based program that has been tracking bird populations since 1966. 
                    Each year, trained observers follow the same routes during breeding season, 
                    stopping at regular intervals to count all birds they see or hear."),
                    
                    p("This creates one of the largest and longest-running bird monitoring datasets in the world, 
                    giving us reliable information about how bird populations are changing over time."),
                    
                    div(class = "highlight-box",
                        h4("Key Features:", style = "color: #2E8B57; margin-top: 0;"),
                        tags$ul(
                          tags$li(strong("50+ years"), " of consistent data collection"),
                          tags$li(strong("Continental coverage"), " across North America"),
                          tags$li(strong("Standardized methods"), " for reliable comparisons"),
                          tags$li(strong("4,000+ routes"), " surveyed annually")
                        )
                    ),
                    
                    tags$details(
                      tags$summary("Show technical details"),
                      h5("Survey Methodology", style = "color: #2E8B57;"),
                      p("Each BBS route is 24.5 miles long with 50 stops spaced 0.5 miles apart. 
                      At each stop, observers conduct a 3-minute point count, recording all birds 
                      seen or heard within a 0.25-mile radius. Routes are surveyed once per year 
                      during peak breeding season, typically starting one-half hour before sunrise."),
                      
                      h5("Bird Conservation Regions", style = "color: #2E8B57;"),
                      p("Data are organized by Bird Conservation Regions (BCRs), which are ecologically 
                      distinct areas with similar bird communities, habitats, and environmental conditions. 
                      BCRs provide a biologically meaningful framework for analyzing population trends 
                      across North America's diverse landscapes."),
                      
                      h5("Data Processing", style = "color: #2E8B57;"),
                      p("We use data from 1992-2023 and focus on species-region combinations with 
                      sufficient observations for reliable trend estimation. This ensures the trends 
                      you see are based on solid evidence rather than sparse data.")
                    )
                  ),
                  
                  # MODELLING SECTION
                  div(
                    h2("How We Estimate Trends", class = "section-header"),
                    
                    h4("The Challenge"),
                    p("Bird populations don't change in simple, straight lines. They can increase rapidly 
                    for a few years, then stabilize, or show complex patterns influenced by weather, 
                    habitat changes, and many other factors. We need a method that can capture these 
                    real-world complexities."),
                    
                    h4("Our Approach"),
                    p("We use a sophisticated statistical model called a ", 
                      strong("Generalized Additive Model (GAM)"), 
                      " that can detect curved, non-linear trends in the data. Think of it as drawing 
                    a flexible line through the data points rather than forcing everything to fit 
                    a straight line."),

                    
                    p("The model also controls for ", strong("survey effort"), " — ensuring that trends 
                    reflect real population changes rather than changes in how many routes were surveyed."),
                    
                    tags$details(
                      tags$summary("Show technical details"),
                      h5("Model Structure", style = "color: #2E8B57;"),
                      p("We fit a hierarchical GAM that decomposes bird counts into multiple components:"),
                      tags$pre(style = "background: #f4f4f4; padding: 10px; border-radius: 5px; font-size: 11px;",
                               "count ~ s(year) + ti(year, region) + ti(year, species_phylo) + 
        ti(year, species) + ti(year, region, species_phylo) + 
        ti(year, region, species) + offset(log(n_routes))"
                      ),
                      
                      h5("Key Components:", style = "color: #2E8B57;"),
                      tags$ul(
                        tags$li(strong("s(year):"), " Overall continental trend"),
                        tags$li(strong("ti(year, region):"), " How each region deviates from the continental trend"),
                        tags$li(strong("ti(year, species_phylo):"), " Trends shared among evolutionarily related species"),
                        tags$li(strong("ti(year, species):"), " Species-specific deviations"),
                        tags$li(strong("ti(year, region, species_phylo):"), " Phylogenetically-informed regional patterns for each species"),
                        tags$li(strong("ti(year, region, species):"), " Unique spatiotemporal patterns for each species"),
                        tags$li(strong("offset(log(n_routes)):"), " Controls for sampling effort")
                      ),
                      
                      h5("Phylogenetic Smoothing", style = "color: #2E8B57;"),
                      p("The model uses Markov Random Field (MRF) smooths based on the evolutionary tree 
                      (phylogeny) of bird species. This means closely related species (e.g., two warbler species) 
                      'borrow strength' from each other, improving trend estimates especially for rare species 
                      while respecting evolutionary constraints."),
                      
                      h5("Spatial Smoothing", style = "color: #2E8B57;"),
                      p("Similarly, geographically adjacent regions with similar ecological conditions share 
                      information through neighborhood-based MRF smooths. This prevents overfitting and 
                      produces more stable regional estimates."),
                      
                      h5("Model Fitting", style = "color: #2E8B57;"),
                      tags$ul(
                        tags$li("Fast Restricted Maximum Likelihood (fREML) estimation"),
                        tags$li("Automatic smoothness selection"),
                        tags$li("Bayesian posterior simulation for uncertainty quantification (500 draws)"),
                        tags$li("10% holdout validation set for model assessment")
                      )
                    )
                  ),
                  
                  # MAP COLORS SECTION
                  div(
                    h2("Understanding Map Colors", class = "section-header"),
                    
                    p("When you select a species or species group on the Trend Map, each region is colored 
                    based on its population trend. But how do we determine these trend categories?"),
                    
                    h4("Trend Classification"),
                    p("While the detailed trend plots show the full, non-linear patterns estimated by our GAM, 
                    the map colors are determined using a ", strong("simple linear model"), " fit to the 
                    GAM predictions. This gives us a single number — the ", strong("slope"), 
                      " — that summarizes the overall direction and strength of change."),
                    
                    div(class = "info-callout",
                        h5("Why Use a Linear Summary?", style = "color: #1976D2; margin-top: 0;"),
                        p("While populations may show complex patterns year-to-year, some applicaabouttions 
                        need more simple estimates: ", em("Is this species increasing or decreasing overall?"), 
                          " The linear slope gives us that summary while the detailed plots preserve the nuance.", 
                          style = "margin: 0;")
                    ),
                    
                    h4("The Color Categories"),
                    p("Based on the slope from the linear model, we classify each region into one of six categories:"),
                    
                    div(style = "margin: 20px 0;",
                        div(style = "display: grid; grid-template-columns: 40px 1fr; gap: 10px; align-items: start; margin: 8px 0;",
                            div(style = "width: 30px; height: 30px; background: #006600; border-radius: 4px;"),
                            div(strong("Strong Increase:"), " Slope ≥ 0.05 (population growing substantially)")
                        ),
                        div(style = "display: grid; grid-template-columns: 40px 1fr; gap: 10px; align-items: start; margin: 8px 0;",
                            div(style = "width: 30px; height: 30px; background: #b6d7a8; border-radius: 4px;"),
                            div(strong("Moderate Increase:"), " Slope between 0.02 and 0.05 (population growing)")
                        ),
                        div(style = "display: grid; grid-template-columns: 40px 1fr; gap: 10px; align-items: start; margin: 8px 0;",
                            div(style = "width: 30px; height: 30px; background: #D3D3D3; border-radius: 4px;"),
                            div(strong("Stable:"), " Slope between -0.02 and 0.02 (no significant change)")
                        ),
                        div(style = "display: grid; grid-template-columns: 40px 1fr; gap: 10px; align-items: start; margin: 8px 0;",
                            div(style = "width: 30px; height: 30px; background: #ea9999; border-radius: 4px;"),
                            div(strong("Moderate Decrease:"), " Slope between -0.05 and -0.02 (population declining)")
                        ),
                        div(style = "display: grid; grid-template-columns: 40px 1fr; gap: 10px; align-items: start; margin: 8px 0;",
                            div(style = "width: 30px; height: 30px; background: #CC0000; border-radius: 4px;"),
                            div(strong("Strong Decrease:"), " Slope ≤ -0.05 (population declining substantially)")
                        ),
                        div(style = "display: grid; grid-template-columns: 40px 1fr; gap: 10px; align-items: start; margin: 8px 0;",
                            div(style = "width: 30px; height: 30px; background: #F4F4F4; border: 1px solid #ccc; border-radius: 4px;"),
                            div(strong("No Data:"), " Insufficient data for trend estimation")
                        )
                    ),
                    
                    tags$details(
                      tags$summary("Show technical details"),
                      h5("Linear Model Details", style = "color: #2E8B57;"),
                      p("For each species-region combination, we fit a simple linear regression:"),
                      tags$pre(style = "background: #f4f4f4; padding: 10px; border-radius: 5px;",
                               "lm(predicted_abundance ~ year)"
                      ),
                      p("where ", tags$code("predicted_abundance"), " comes from the GAM's predictions. 
                      The slope coefficient tells us the average annual rate of change in standardized 
                      abundance units."),
                      
                      h5("Why These Thresholds?", style = "color: #2E8B57;"),
                      p("The threshold values (0.02 and 0.05) were chosen based on ecological significance. 
                      Since the data are standardized (see below), these represent changes in standard 
                      deviation units per year:"),
                      tags$ul(
                        tags$li("A slope of 0.02 means the population changes by 0.02 standard deviations per year"),
                        tags$li("Over 30 years, this amounts to a 0.6 SD change (0.02 × 30)"),
                        tags$li("A slope of 0.05 represents more substantial change: 1.5 SD over 30 years")
                      ),
                      p("These cutoffs help distinguish biologically meaningful trends from natural variation.")
                    )
                  ),
                  
                  # INTERPRETING PLOTS SECTION
                  div(
                    h2("Reading the Trend Plots", class = "section-header"),
                    
                    h4("The Basic Components"),
                    p(strong("Solid line:"), " The estimated population trend. This shows our best estimate 
                    of how the population has changed, assuming constant survey effort across all years."),
                    
                    p(strong("Shaded ribbon:"), " The uncertainty around our estimate. Wider ribbons mean 
                    we're less certain about the exact trend."),
                    
                    p(strong("Dashed horizontal line:"), " Zero, representing the long-term average. Trends 
                    above the line indicate above-average abundance; trends below indicate below-average abundance."),
                    
                    h4("What Causes Uncertainty?"),
                    p("The width of the confidence ribbon varies based on:"),
                    tags$ul(
                      tags$li("Amount of data (more data = narrower ribbons)"),
                      tags$li("Natural population variability"),
                      tags$li("Time period (uncertainty often increases at the edges)"),
                      tags$li("Degree of population change (rapid changes create more uncertainty)")
                    ),
                    
                    tags$details(
                      tags$summary("Show technical details"),
                      h5("Uncertainty Quantification", style = "color: #2E8B57;"),
                      p("The shaded ribbons represent 95% credible intervals from the model's posterior 
                      distribution. We generate 500 posterior samples of model coefficients and propagate 
                      uncertainty through predictions, giving fully Bayesian uncertainty estimates."),
                      
                      h5("Standardized Counts (Y-Axis)", style = "color: #2E8B57;"),
                      p("The Y-axis shows ", strong("standardized counts"), " rather than raw bird numbers. 
                      For each species-region combination, we transform the data using:"),
                      tags$pre(style = "background: #f4f4f4; padding: 10px; border-radius: 5px;",
                               "(count - mean) / standard_deviation"
                      ),
                      p("This creates a common scale where:"),
                      tags$ul(
                        tags$li(strong("0"), " = long-term average for this species in this region"),
                        tags$li(strong("+1"), " = one standard deviation above average"),
                        tags$li(strong("-1"), " = one standard deviation below average")
                      ),
                      p(strong("Why standardize?"), " It allows fair comparisons between common species 
                      (hundreds of birds) and rare species (just a few birds). We're interested in 
                      ", em("relative change"), ", not absolute numbers."),
                      
                      h5("'Expected' vs 'Observed' Trends", style = "color: #2E8B57;"),
                      p("The plots show ", strong("'expected' trends"), " — what we would have seen if 
                      survey effort (number of routes) had been constant across all years. This approach removes confounding between true population changes and changes 
                      in sampling intensity, giving clearer insights into biological patterns")
                    )
                  ),
                  
                  # APPLICATIONS & LIMITATIONS
                  div(
                    h2("Applications & Limitations", class = "section-header"),
                    
                    h4("How This Information can be Used"),
                    tags$ul(
                      tags$li(strong("Conservation prioritization:"), " Identifying species and regions that need immediate attention"),
                      tags$li(strong("Policy development:"), " Providing evidence for habitat protection and management decisions"),
                      tags$li(strong("Progress tracking:"), " Monitoring whether conservation actions are working"),
                      tags$li(strong("Early warning:"), " Detecting population declines before they become severe"),
                      tags$li(strong("Public awareness:"), " Helping people understand local bird populations")
                    ),
                    
                    h4("Important Limitations"),
                    tags$ul(
                      tags$li(strong("Roadside bias:"), " Surveys occur along roads, which may not represent all habitats"),
                      tags$li(strong("Detection variability:"), " Some species are easier to detect than others"),
                      tags$li(strong("Geographic coverage:"), " Remote areas and certain habitat types have limited data"),
                      tags$li(strong("Breeding season focus:"), " Only captures populations during breeding season"),
                      tags$li(strong("Relative trends:"), " Shows population changes, not absolute population sizes"),
                      tags$li(strong("Correlation vs causation:"), " Trends don't explain why populations are changing")
                    )
                  ),
                  
                  # REFERENCES
                  div(
                    h2("Data Access & References", class = "section-header"),
                    
                    h4("Data Source"),
                    p("The raw survey data are freely available from the USGS Patuxent Wildlife Research Center. 
                    Our analysis uses data from 1992-2023, filtered to include species-region combinations 
                    with sufficient observations for reliable trend estimation."),
                    p(a("Access BBS data →", href = "https://www.pwrc.usgs.gov/bbs/RawData/", 
                        target = "_blank", style = "color: #2E8B57;")),
                    
                    tags$details(
                      tags$summary("Key References"),
                      tags$ul(
                        tags$li("Ziolkowski Jr, D. J., Lutmerding, M., English, W., & Hudson, M.-A. (2024). North American Breeding Bird Survey dataset,
                                1966–2023. U.S. Geological Survey. https://doi.org/10.5066/P136CRBV"),
                        tags$li("Wood, S.N. (2017). Generalized Additive Models: An Introduction with R, Second Edition. CRC Press."),
                        tags$li("Sauer, J. R., Niven, D. K., Hines, J. E., Ziolkowski Jr., D. J., Pardieck, K. L., Fallon, J. E., & 
                                Link, W. A. (2017). The North American Breeding Bird Survey: Results and analysis, 1966–2015 (Version 2.07.2017). 
                                U.S. Geological Survey, Patuxent Wildlife Research Center.")
                      )
                    )
                  )
              )
  )
 )

)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamic UI for group selection
  output$group_selector <- renderUI({
    req(input$grouping_type)
    
    # Get choices for the selected grouping type
    group_choices <- grouping_choices[[input$grouping_type]]$choices
    
    #grouping_info <- grouping_choices[[input$grouping_type]]
    
    labels <- names(group_choices)
    
    # For Body Size, keep the full label (including mass range in parentheses)
    # For other grouping types, remove the species count in parentheses
    if (input$grouping_type == "Body Size") {
      values <- labels  # Keep full label like "Small (<20g)"
    } else {
      values <- gsub("\\s*\\([^)]*\\)", "", labels)  # Remove (n species)
    }
    
    selectInput("selected_group",
                paste("Choose", tolower(input$grouping_type), ":"),
                choices = c("Select a group..." = "", setNames(values, labels)),
                selected = "")  # Always start with placeholder
  })
  
  # Reactive expression for formatted species choices
  formatted_species_choices <- reactive({
    if (input$name_format == "common") {
      # Create named vector: value = scientific_name, label = common_name
      choices_df <- bird_species_info %>%
        filter(scientific_name %in% species_list) %>%
        select(scientific_name, common_name) %>%
        arrange(common_name)
      
      setNames(choices_df$scientific_name, choices_df$common_name)
    } else {
      # Use scientific names (formatted)
      setNames(species_list, gsub("_", " ", species_list))
    }
  })
  
  # observer to reset selection when grouping type changes
  observeEvent(input$grouping_type, {
    updateSelectInput(session, "selected_group", selected = "")
  }, ignoreInit = TRUE)
  
  # Reactive for getting species list in selected group
  group_species_list <- reactive({
    req(input$grouping_type, input$selected_group, input$selected_group != "")
    
    # Check that the grouping type exists
    if (!input$grouping_type %in% names(grouping_choices)) {
      return(NULL)
    }
    
    # Check that the selected group exists in the current grouping type
    if (!input$selected_group %in% unname(grouping_choices[[input$grouping_type]]$choices)) {
      return(NULL)
    }
    
    group_column <- grouping_choices[[input$grouping_type]]$column
    group_value <- input$selected_group
    
    species_list <- species_ecological_data %>%
      filter(!!sym(group_column) == group_value) %>%
      pull(species)
    
    if (length(species_list) == 0) {
      return(NULL)
    }
    
    return(species_list)
  })
  
  # Observer to update species dropdown when name preference changes
  observeEvent(input$name_format, {
    current_selection <- input$select
    new_selected <- if (!is.null(current_selection) && current_selection != "") current_selection else ""
    
    updateSelectizeInput(
      session, 
      "select",
      choices = formatted_species_choices(),
      selected = new_selected
    )
  }, ignoreInit = TRUE)
  
  # Only runs once:
  observeEvent(formatted_species_choices(), {
    updateSelectizeInput(
      session,
      "select",
      choices = formatted_species_choices(),
      selected = ""
    )
  }, once = TRUE, ignoreNULL = FALSE)
  
  # observeEvent(formatted_species_choices(), {
  #   updateSelectInput(
  #     session,
  #     "select",
  #     choices = formatted_species_choices()
  #   )
  # }, once = TRUE, ignoreNULL = FALSE)
  
  # Helper function to get display name for a species
  get_species_display_name <- reactive({
    function(scientific_name) {
      if (input$name_format == "common") {
        common <- bird_species_info %>%
          filter(scientific_name == !!scientific_name) %>%
          pull(common_name)
        if (length(common) > 0 && !is.na(common[1])) {
          return(common[1])
        }
      }
      return(gsub("_", " ", scientific_name))
    }
  })
  
  # Load data for plots - handles both species and group selection
  plotDataInput <- reactive({
    req(input$region, input$region != "")
    
    if (input$selection_type == "species") {
      req(input$select, input$select != "")
      
      # Use the pre-joined data (already has counts)
      d <- plot_data %>%
        filter(species == input$select, region == input$region)
      
      if (nrow(d) == 0) {
        return(data.frame())
      }
      
    } else if (input$selection_type == "group") {
      req(input$grouping_type, input$grouping_type != "")
      req(input$selected_group, input$selected_group != "")
      
      # Use pre-calculated group trends
      cache_key <- paste(input$grouping_type, input$selected_group, input$region, sep = "||")
      
      if (cache_key %in% names(group_trends_precalc)) {
        d <- group_trends_precalc[[cache_key]]
      } else {
        return(data.frame())
      }
      
      if (is.null(d) || nrow(d) == 0) {
        return(data.frame())
      }
    }
    
    return(d)
  })
  
  # Get current species info
  current_species_info <- reactive({
    req(input$select)
    bird_species_info %>%
      filter(scientific_name == input$select)
  })
  
  # Prepare trend data for the selected species
  speciesTrendMapData <- reactive({
    if (input$selection_type == "species") {
      req(input$select)
      
      # Use pre-calculated trend data
      species_trends <- trend_data %>%
        filter(species == input$select)
      
      # Join with spatial data
      map_data <- regions %>%
        left_join(species_trends, by = c("BCR_clean" = "region"))
      
      return(map_data)
      
    } else if (input$selection_type == "group") {
      req(input$grouping_type, input$grouping_type != "")
      req(input$selected_group, input$selected_group != "")
      
      # Use pre-calculated group trend stats
      group_trends <- group_trend_stats %>%
        filter(grouping_type == input$grouping_type,
               group_value == input$selected_group)
      
      if (nrow(group_trends) == 0) {
        return(NULL)
      }
      
      # Join with spatial data
      map_data <- regions %>%
        left_join(group_trends, by = c("BCR_clean" = "region"))
      
      return(map_data)
    }
    
    return(NULL)
  })
  
  output$map <- renderLeaflet({
    leaflet(regions) %>%
      setView(-104, 41, zoom = 4) %>%
      addTiles()
  })
  
  
  # Update map colors when data changes (for both species and groups)
  observe({
    req(input$selection_type)
    # Only update map when valid selections are made
    if (input$selection_type == "species") {
      req(input$select, input$select != "")
    } else if (input$selection_type == "group") {
      req(input$grouping_type, input$grouping_type != "")
      req(input$selected_group, input$selected_group != "")
    }
    
    data <- speciesTrendMapData()
    
    if (is.null(data)) return()
    
    # Get all available slope data
    available_data <- data %>%
      filter(!is.na(slope), !is.na(trend_category))
    
    # Calculate symmetric range around zero for relative scaling
    max_abs_slope <- max(abs(available_data$slope), na.rm = TRUE)
    
    # Create custom color mapping function with absolute intensity
    get_color <- function(slope, trend_category) {
      # Handle missing data
      if (is.na(slope) || is.na(trend_category)) return("#F4F4F4")
      
      # Stable trends are always grey
      if (trend_category == "Stable") return("#D3D3D3")
      
      # No data cases
      if (trend_category %in% c("No Data", "Insufficient Data")) return("#F4F4F4")
      
      # Increasing trends - green scale
      if (trend_category == "Strong Increase") {
        return("#006600")  # Dark green for strong increase
        }
      if (trend_category == "Moderate Increase") {
        return("#b6d7a8")  # Light green for moderate increase
      }
      # Decreasing trends - red scale
      if (trend_category == "Moderate Decrease") {
        return("#ea9999")  # Light red for moderate decrease
      }
      if (trend_category == "Strong Decrease") {
        return("#CC0000")  # Dark red for strong decrease
      }

      return("#F4F4F4")  # Default fallback
    }
    
    # Apply colors to data
    data$map_color <- mapply(get_color, data$slope, data$trend_category)
    
    # Create popup content based on selection type
    if (input$selection_type == "species") {
      display_name <- get_species_display_name()(input$select)
      popup_content <- paste0(
        "<strong>Region:</strong> ", data$BCR_clean, "<br>",
        "<strong>Species:</strong> ", display_name, "<br>",
        "<strong>Trend:</strong> ", ifelse(is.na(data$trend_category), "No Data", data$trend_category), "<br>",
        "<strong>Slope:</strong> ", ifelse(is.na(data$slope), "N/A", round(data$slope, 4)), "<br>"
      )
    } else {
      popup_content <- paste0(
        "<strong>Region:</strong> ", data$BCR_clean, "<br>",
        "<strong>Group:</strong> ", input$grouping_type, " - ", input$selected_group, "<br>",
        "<strong>Species in region:</strong> ", ifelse(is.na(data$n_species), "0", data$n_species), "<br>",
        "<strong>Trend:</strong> ", ifelse(is.na(data$trend_category), "No Data", data$trend_category), "<br>",
        "<strong>Avg Slope:</strong> ", ifelse(is.na(data$slope), "N/A", round(data$slope, 4)), "<br>"
      )
    }
    popup_content <- rep_len(popup_content, nrow(data))
    
    # Update polygons
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        layerId = ~BCR_clean,
        fillColor = ~map_color,
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        popup = popup_content,
        label = ~BCR_clean,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 0.8,
          bringToFront = TRUE,
          sendToBack = TRUE
        )
      ) %>%
      clearControls() %>%
      addLegend(
        colors = c("#006600", "#b6d7a8", "#D3D3D3", "#ea9999", "#CC0000", "#F4F4F4"),
        labels = c("Strong Increase", "Moderate Increase", "Stable", "Moderate Decrease", "Strong Decrease", "No Data"),
        title = "Population Trend",
        position = "bottomright",
        opacity = 0.7,
        layerId = "trend_legend"
      )
  })
  
  output$plot <- renderPlot({
    # Check if selections are made
    if (input$selection_type == "species" && 
        (is.null(input$select) || input$select == "")) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Please select a species",
                        size = 5, color = "gray50") +
               theme_void())
    }
    
    if (input$selection_type == "group" && 
        (is.null(input$grouping_type) || input$grouping_type == "" ||
         is.null(input$selected_group) || input$selected_group == "")) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Please select a group",
                        size = 5, color = "gray50") +
               theme_void())
    }
    
    if (is.null(input$region) || input$region == "") {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Please select a region",
                        size = 5, color = "gray50") +
               theme_void())
    }
    
    data <- plotDataInput()
    
    if (nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No data available",
                        size = 5, color = "gray50") +
               theme_void())
    }
    
    # Create appropriate title based on selection type
    if (input$selection_type == "species") {
      display_name <- get_species_display_name()(input$select)
      plot_title <- paste("Population Trend for", display_name, "\n in", input$region, "(1992-2023)")
    } else if (input$selection_type == "group") {
      plot_title <- paste("Population Trend for", input$selected_group, "\n in", input$region, "(1992-2023)")
    }
    
    ggplot(data, aes(x = year, y = pred)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5, linewidth = 0.5) +
      geom_line(linewidth = 1, alpha = 0.8, color = "darkblue") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightblue") +
      labs(title = plot_title,
           x = "Year", y = "Relative Abundance") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
        plot.margin = margin(10, 10, 10, 10),
        axis.title.x = element_text(size = 11, margin = margin(t = 5)),
        axis.title.y = element_text(size = 11, margin = margin(r = 5)),
        axis.text = element_text(size = 8)
      )
  }, width = 300, height = 300
  )

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)){
      updateSelectInput(session, "region", selected = click$id)
      
      # Highlight selected region
      selected_region <- regions %>% filter(BCR_clean == click$id)
      leafletProxy("map") %>%
        clearGroup("selected") %>%
        addPolygons(
          data = selected_region,
          fillColor = "transparent",
          color = "black",
          #opacity = 1,
          fillOpacity = 1,
          weight = 2,
          group = "selected"
        )
    }
  })
  
  output$info_panel <- renderUI({
    if (input$selection_type == "species") {
      # Existing species info code
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
        },
        
        # Ecological characteristics
        hr(style = "margin: 15px 0 10px 0;"),
        
        div(
          style = "padding: 10px; background: #f9f9f9; border-radius: 5px;",
          p(strong("Ecological Characteristics"), 
            style = "color: #2E8B57; margin: 0 0 10px 0; font-size: 14px;"),
          
          # Get species ecological data
          {
            species_eco <- species_ecological_data %>%
              filter(species == input$select)
            
            if (nrow(species_eco) > 0) {
              tagList(
                # Taxonomic info
                if (!is.na(species_eco$order[1]) && species_eco$order[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Order: "), species_eco$order[1])
                },
                if (!is.na(species_eco$family[1]) && species_eco$family[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Family: "), species_eco$family[1])
                },
                
                # Body size
                if (!is.na(species_eco$body_size_category[1]) && species_eco$body_size_category[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Body Size: "), species_eco$body_size_category[1])
                },
                
                # Habitat
                if (!is.na(species_eco$habitat_category[1]) && species_eco$habitat_category[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Habitat: "), species_eco$habitat_category[1])
                },
                
                # Diet
                if (!is.na(species_eco$diet_type[1]) && species_eco$diet_type[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Diet: "), species_eco$diet_type[1])
                },
                
                # Migration
                if (!is.na(species_eco$migration_status[1]) && species_eco$migration_status[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Migration: "), species_eco$migration_status[1])
                },
                
                # Conservation status
                if (!is.na(species_eco$conservation_category[1]) && species_eco$conservation_category[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Conservation Status: "), species_eco$conservation_category[1])
                }
              )
            } else {
              p("Ecological data not available", 
                style = "margin: 0; font-size: 12px; color: #666; font-style: italic;")
            }
          }
        )
        
      )
      
    } else if (input$selection_type == "group") {
      # Group info
      group_species <- tryCatch({
        group_species_list()
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(group_species) || length(group_species) == 0) {
        return(div("No group selected or no species in group"))
      }

      total_in_group <- length(group_species)
      
      # Get list of species names for display - need to get full data for names
      species_display_list <- if (input$name_format == "common") {
        species_ecological_data %>%
          filter(species %in% group_species) %>%
          left_join(bird_species_info, by = c("species" = "scientific_name")) %>%
          arrange(common_name) %>%
          pull(common_name)
      } else {
        species_ecological_data %>%
          filter(species %in% group_species) %>%
          arrange(species) %>%
          pull(species) %>%
          gsub("_", " ", .)
      }
      
      # Create named vector for selectInput, handling NULL case
      species_choices <- if (length(group_species) > 0) {
        if (input$name_format == "common") {
          choices_df <- species_ecological_data %>%
            filter(species %in% group_species) %>%
            left_join(bird_species_info, by = c("species" = "scientific_name")) %>%
            select(species, common_name) %>%
            arrange(common_name)
          c("Select a species..." = "", 
            setNames(choices_df$species, choices_df$common_name))
        } else {
          c("Select a species..." = "", 
            setNames(group_species, gsub("_", " ", group_species)))
        }
      } else {
        c("Select a species..." = "")
      }
      
      # Check if grouping is by family, order, or ecological category and get info
      group_description <- NULL
      
      if (input$grouping_type == "Taxonomic Family") {
        taxon_data <- family_info %>% 
          filter(taxon == input$selected_group, page_exists == TRUE)
        if (nrow(taxon_data) > 0) {
          group_description <- list(
            title = taxon_data$common_name[1],
            description = taxon_data$description[1],
            url = taxon_data$url[1]
          )
        }
      } else if (input$grouping_type == "Taxonomic Order") {
        taxon_data <- order_info %>% 
          filter(taxon == input$selected_group, page_exists == TRUE)
        if (nrow(taxon_data) > 0) {
          group_description <- list(
            title = taxon_data$common_name[1],
            description = taxon_data$description[1],
            url = taxon_data$url[1]
          )
        }
      } else {
        # Check for ecological grouping descriptions
        # Map grouping types to their column names in group_info
        grouping_column_map <- list(
          "Habitat Type" = "habitat_category",
          "Diet Type" = "diet_type",
          "Migration Pattern" = "migration_status",
          "Conservation Status" = "conservation_category"
        )
        
        if (input$grouping_type %in% names(grouping_column_map)) {
          column_name <- grouping_column_map[[input$grouping_type]]
          
          group_desc <- group_info %>%
            filter(category_type == column_name, 
                   category_value == input$selected_group)
          
          if (nrow(group_desc) > 0) {
            group_description <- list(
              title = NULL,  # No title for ecological groups
              description = group_desc$description[1],
              url = NULL  # No URL for ecological groups
            )
          }
        }
      }
      
      div(
        h4(input$grouping_type, 
           style = "color: #2E8B57; margin: 0; font-size: 16px; font-weight: bold;"),
        h5(input$selected_group,
           style = "color: #666; margin: 5px 0 15px 0; font-size: 14px;"),
        
        # Display group description if available
        if(!is.null(group_description)) {
          tagList(
            if(!is.null(group_description$title) && !is.na(group_description$title) && group_description$title != "") {
              p(strong(group_description$title), 
                style = "color: #666; font-size: 13px; margin: 5px 0;")
            },
            div(
              style = "padding: 10px; background: #f8f9fa; border-radius: 5px; border-left: 3px solid #2E8B57; margin-bottom: 10px;",
              p(group_description$description, 
                style = "margin: 0; color: #333; font-size: 13px; line-height: 1.4;"),
              if(!is.null(group_description$url) && !is.na(group_description$url)) {
                p(
                  a("Learn more", href = group_description$url, target = "_blank", 
                    style = "color: #2E8B57; font-size: 11px;"),
                  style = "margin: 8px 0 0 0;"
                )
              }
            )
          )
        },
        
        hr(style = "margin: 10px 0;"),
        
        hr(style = "margin: 10px 0;"),
        
        div(
          style = "margin-top: 10px;",
          p(strong("Switch to individual species:"), style = "margin-bottom: 5px; color: #2E8B57; font-size: 13px;"),
          selectInput("switch_to_species", 
                      label = NULL,
                      choices = species_choices,
                      selected = ""),
          
          hr(style = "margin: 10px 0;"),
          
          div(
            style = "max-height: 300px; overflow-y: auto; font-size: 12px;",
            p(strong("All species in this group:"), style = "margin-bottom: 5px; color: #2E8B57;"),
            tags$ul(
              style = "padding-left: 20px; margin: 0;",
              lapply(species_display_list, function(sp) {
                tags$li(sp, style = "margin: 2px 0;")
              })
            )
          )
        )
      )
    }
  })
  
  # Observer to switch to individual species from group view
  observeEvent(input$switch_to_species, {
    req(input$switch_to_species != "")
    
    updateRadioButtons(session, "selection_type", selected = "species")
    updateSelectizeInput(session, "select", selected = input$switch_to_species)  # <- change here
    
    updateSelectInput(session, "switch_to_species", selected = "")
  }, ignoreInit = TRUE)
  
  
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
    req(plot_data)
    
    if (input$region2 != "") {
      # Get available species for the selected region
      available_species <- plot_data %>%
        filter(region == input$region2) %>%
        distinct(species) %>%
        pull(species)
      
      # Keep the previous selection if it exists in this region
      current_selection <- isolate(input$species_selection)
      if (!is.null(current_selection) && all(current_selection %in% available_species)) {
        selected_species <- current_selection
      } else {
        selected_species <- NULL
      }
      
      # Create choices based on name preference (use region-specific radio buttons)
      species_choices <- if (input$name_format_region == "common") {
        choices_df <- bird_species_info %>%
          filter(scientific_name %in% available_species) %>%
          select(scientific_name, common_name) %>%
          arrange(common_name)
        setNames(choices_df$scientific_name, choices_df$common_name)
      } else {
        setNames(available_species, gsub("_", " ", available_species))
      }
      
      # Update the dropdown with available species
      updateSelectInput(
        session, "species_selection",
        choices = species_choices,
        selected = selected_species
      )
    } else {
      # Clear species selection when no region is selected
      updateSelectInput(session, "species_selection",
                        choices = NULL,
                        selected = NULL)
    }
  })
  
  # Observer to update region2 species dropdown when name preference changes
  observeEvent(input$name_format_region, {
    req(input$region2 != "")
    
    current_selection <- input$species_selection
    
    # Get available species for the selected region
    available_species <- plot_data %>%
      filter(region == input$region2) %>%
      distinct(species) %>%
      pull(species)
    
    # Create choices based on name preference (use region-specific radio buttons)
    species_choices <- if (input$name_format_region == "common") {
      choices_df <- bird_species_info %>%
        filter(scientific_name %in% available_species) %>%
        select(scientific_name, common_name) %>%
        arrange(common_name)
      setNames(choices_df$scientific_name, choices_df$common_name)
    } else {
      setNames(available_species, gsub("_", " ", available_species))
    }
    
    updateSelectInput(
      session,
      "species_selection",
      choices = species_choices,
      selected = current_selection
    )
  }, ignoreInit = TRUE)
  
  # Limit species selection to 3
  observeEvent(input$species_selection, {
    if (length(input$species_selection) > 3) {
      showNotification(
        "Maximum 3 species allowed. Keeping first 3 selections.",
        type = "warning",
        duration = 3
      )
      updateSelectInput(session, "species_selection",
                        selected = head(input$species_selection, 3))
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
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")
    
    # Create display names for legend (use region-specific radio buttons)
    data <- data %>%
      left_join(bird_species_info, by = c("species" = "scientific_name"))
    
    data$display_name <- if (input$name_format_region == "common") {
      data$common_name
    } else {
      gsub("_", " ", data$species)
    }
    
    # Ensure consistent ordering based on input selection order
    data$species <- factor(data$species, levels = input$species_selection)
    data$display_name <- factor(data$display_name, 
                                levels = unique(data$display_name[order(data$species)]))
    
    # Create named color vector based on species order
    species_colors <- setNames(colors[1:length(input$species_selection)], 
                               levels(data$display_name))
    
    ggplot(data, aes(x = year, y = pred, color = display_name, fill = display_name)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5, linewidth = 0.5) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
      geom_line(linewidth = 1, alpha = 0.8) +
      scale_color_manual(values = species_colors) +
      scale_fill_manual(values = species_colors) +
      labs(title = paste("Population Trends in", input$region2, "\n(1992-2023)"),
           x = "Year", 
           y = "Relative Abundance",
           color = "Species",
           fill = "Species") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.6, "lines"),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(3, 0, 0, 0)
      ) +
      guides(
        color = guide_legend(override.aes = list(alpha = 1), nrow = 3, byrow = TRUE),
        fill = "none"
      )
  }, width = 310, height = 310)
  
  # Separate plots
  output$separate_plots <- renderUI({
    data <- comparisonData()
    
    if (nrow(data) == 0) {
      return(div("No data available for selected species and region"))
    }
    
    # Define colors for up to 3 species
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c")
    
    # Use the selection order to maintain consistent colors
    species_list <- input$species_selection
    
    # Calculate height based on number of species (minimum 400px for 1 species, add 200px for each additional)
    total_height <- max(400, 200 * length(species_list))
    
    plot_outputs <- lapply(seq_along(species_list), function(i) {
      species <- species_list[i]
      output_id <- paste0("species_plot_", i)
      
      output[[output_id]] <- renderPlot({
        species_data <- data %>% filter(species == !!species)
        
        # Get display name (use region-specific radio buttons)
        display_name <- if (input$name_format_region == "common") {
          bird_species_info %>%
            filter(scientific_name == !!species) %>%
            pull(common_name) %>%
            first()
        } else {
          gsub("_", " ", species)
        }
        
        ggplot(species_data, aes(x = year, y = pred)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5, linewidth = 0.5) +
          geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = colors[i]) +
          geom_line(linewidth = 1, alpha = 0.8, color = colors[i]) +
          labs(title = paste(display_name, "in", input$region2),
               x = "Year", 
               y = "Relative Abundance") +
          theme_classic() +
          theme(
            plot.title = element_text(size = 10, hjust = 0.5),
            plot.margin = margin(5, 5, 5, 5),
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8)
          )
      }, width = 310, height = 170)
      
      div(
        style = "margin-bottom: 8px;",
        plotOutput(output_id, width = "310px", height = "170px")
      )
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
  
  # Display region info with image
  output$bcr_info_map_panel <- renderUI({
    bcr_info_row <- current_region_info()
    if (nrow(bcr_info_row) == 0) return(div("No region selected"))
    
    # Match corresponding Flickr image by region number
    bcr_img <- bcr_images %>% 
      filter(bcr_number == bcr_info_row$bcr_number) %>%
      slice_head(n = 1)  # take first match if multiple
    
    div(
      # Header
      div(
        style = "padding: 10px; margin-bottom: 15px; text-align: center;",
        h4(bcr_info_row$bcr_name,
           style = "color: #2E8B57; margin: 0; font-size: 16px; font-weight: bold;")
      ),
      
      # Optional image section
      if (nrow(bcr_img) > 0 && !is.na(bcr_img$photo_url)) {
        div(
          style = "text-align: center; margin-bottom: 10px;",
          tags$img(
            src = bcr_img$photo_url,
            style = "max-width: 85%; height: auto; border-radius: 8px; display: flex; align-items: center; justify-content: center; margin: 0 auto; color: #666;"
          ),
          p(
            HTML(paste0(
              "Photo: <b>", bcr_img$photographer, "</b> (", bcr_img$license, ")"
            )),
            style = "font-size: 11px; color: #666; margin-top: 5px; font-style: italic;"
          )
        )
      },
      
      # Description
      if (!is.na(bcr_info_row$bcr_description)) {
        div(
          class = "bcr-description",
          style = "margin: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px; border-left: 3px solid #2E8B57;",
          p(bcr_info_row$bcr_description,
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
  # Navigation button handlers for Getting Started tab
  observeEvent(input$go_to_map_hero, {
    updateTabsetPanel(session, "main_tabs", selected = "Trend Map")
  })
  observeEvent(input$go_to_methods, {
    updateTabsetPanel(session, "main_tabs", selected = "About")
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)