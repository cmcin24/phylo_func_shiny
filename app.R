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


# Load data objects
bird_species_info <- readRDS("data/bird_species_info.rds")
bcr_info <- readRDS("data/processed/bcr_info_text_short.rds")
bcr_images <- read.csv("bcr_image.csv")
family_data <- readRDS("data/bird_family_data.rds")
regions <- readRDS("data/processed/bcr_simplified.rds")
plot_data <- readRDS("data/processed/plot_data.rds")
family_info <- readRDS("data/processed/bird_family_info_manual.rds")
order_info <- readRDS("data/processed/bird_order_info_manual.rds")
group_info <- readRDS("data/processed/grouping_info.rds")
load("data/processed/model_objects.rda")

# Load ecological data
species_ecological_data <- readRDS("data/species_ecological_avonet_pif.rds")
grouping_choices <- readRDS("data/grouping_choices_avonet_pif.rds")


# Create a list of grouping types for the UI
grouping_types <- names(grouping_choices)

# List of all species and regions available, sorted

species_list <- sort(as.character(unique(mod_data$sp_latin)))
region_list <- sort(unique(mod_data$strata_name))  

# Shared function to categorize trends consistently
categorize_trend <- function(slope, threshold_moderate = 0.02, threshold_strong = 0.05) {
  case_when(
    is.na(slope) ~ "No Data",
    abs(slope) < threshold_moderate ~ "Stable",
    slope >= threshold_strong ~ "Strong Increase",
    slope >= threshold_moderate ~ "Moderate Increase",
    slope <= -threshold_strong ~ "Strong Decrease",
    slope <= -threshold_moderate ~ "Moderate Decrease",
    TRUE ~ "Stable"
  )
}

calculate_linear_trends <- function(plot_data) {
  trends <- plot_data %>%
    group_by(species, region) %>%
    do({
      if (nrow(.) > 3) {
        model <- lm(pred ~ year, data = .)
        slope <- coef(model)[2]
        p_value <- summary(model)$coefficients[2, 4]
        r_squared <- summary(model)$r.squared
        
        data.frame(
          slope = slope,
          p_value = p_value,
          r_squared = r_squared,
          trend_category = categorize_trend(slope),
          trend_strength = abs(slope)
        )
      } else {
        data.frame(
          slope = NA,
          p_value = NA,
          r_squared = NA,
          trend_category = "Insufficient Data",
          trend_strength = NA
        )
      }
    }) %>%
    ungroup()
  
  return(trends)
}

# Calculate trends for all species-region combinations
trend_data <- calculate_linear_trends(plot_data)


# Function to calculate weighted average trends by ecological group
calculate_group_trends <- function(selected_region, group_species) {
  # Join trend data with count data
  group_count_data <- plot_data %>%
    left_join(mod_data, by = c("species"="sp_latin", "region"="strata_name","year"))
  
  # Get plot data for these species in the selected region
  group_plot_data <- group_count_data %>%
    filter(species %in% group_species, region == selected_region)
  
  if (nrow(group_plot_data) == 0) {
    return(data.frame())
  }
  
  # Calculate weighted average by year
  group_trend <- group_plot_data %>%
    group_by(year) %>%
    summarise(
      pred  = weighted.mean(pred,  w = count, na.rm = TRUE),
      lower = weighted.mean(lower, w = count, na.rm = TRUE),
      upper = weighted.mean(upper, w = count, na.rm = TRUE),
      n_species   = n_distinct(species),
      total_count = sum(count, na.rm = TRUE),
      .groups = 'drop'
    )
  
  return(group_trend)
}

# Define UI for application
ui <- fillPage(theme = shinytheme("spacelab"), navset_tab(
  nav_panel("Map", 
            tags$style(type = "text/css", 
                       "#map {height: calc(100vh - 80px) !important;}", 
                       "#controls {background-color: white; padding: 20px 20px 20px 20px; opacity: 0.8; zoom: 0.9}",
                       "#controls:hover {opacity: 0.95; transition-delay: 0}",
                       "#species_info_panel {background-color: white; padding: 15px; opacity: 0.95; zoom: 0.85; max-height: 900px; overflow-y: auto;}",
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
                            selectInput("select", "Choose species (start typing to search):", 
                                        choices = character(0),
                                        selected = character(0))
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
                                        choices = region_list,
                                        selected = character(0)),
                            tags$span(
                              title = "Bird Conservation Regions (BCRs) are ecologically distinct regions in North America with similar bird communities, habitats, and resource management issues.",
                              style = "cursor: help; color: #2E8B57; font-size: 16px; position: relative; top: 8px;",
                              "ⓘ"
                            )
                          ),
                          tags$div(style = "margin-top: 30px;",  # Add spacing above the plot
                                   plotOutput("plot", width = "300px", height = "300px")
                          )
                          # ,
                          # div(
                          #   style = "margin-top: 15px; padding: 10px; background: #e3f2fd; border-left: 4px solid #2196F3; border-radius: 4px;",
                          #   p(strong("Map Colors:"), style = "margin: 0 0 5px 0; color: #1976D2; font-size: 12px;"),
                          #   p("Color intensity shows relative trend strength for this selection. Darker colors indicate regions with stronger trends compared to other regions.", 
                          #     style = "margin: 0; font-size: 11px; line-height: 1.4;")
                          # )
            ),
            conditionalPanel(
              condition = "input.region != '' && ((input.selection_type == 'species' && input.select != '') || (input.selection_type == 'group' && input.grouping_type != '' && input.selected_group != ''))",
              absolutePanel(id = "species_info_panel", class = "panel panel-default",
                            top = 75, right = 55, width = 380, fixed = TRUE,
                            draggable = FALSE, height = "auto",
                            uiOutput("info_panel")
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
                              plotOutput("combined_trends_plot", height = "350px")
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
                # Understanding Standardized Counts Section
                div(
                  h2("Understanding Standardized Counts", class = "section-header"),
                  
                  h4("What are Standardized Counts?"),
                  p("The models in this application use standardized counts rather than raw bird counts. Standardization is a statistical transformation that makes it easier to compare trends across species with very different baseline abundances."),
                  
                  div(class = "highlight-box",
                      h5("The Standardization Process:", style = "color: #2E8B57; margin-top: 0;"),
                      p("For each species in each region, we calculate:"),
                      tags$ol(
                        tags$li("The mean count across all years"),
                        tags$li("The standard deviation of counts across all years"),
                        tags$li("Transform each count using: ", tags$code("(count - mean) / standard deviation"))
                      ),
                      p("This creates a scale where:"),
                      tags$ul(
                        tags$li(strong("0"), " represents the long-term average count for that species-region combination"),
                        tags$li(strong("+1"), " means the count is one standard deviation above the average"),
                        tags$li(strong("-1"), " means the count is one standard deviation below the average")
                      )
                  ),
                  
                  h4("Why Use Standardized Counts?"),
                  tags$ul(
                    tags$li(strong("Fair Comparisons:"), " A common species with 100 birds and a rare species with 5 birds can be compared on the same scale"),
                    tags$li(strong("Focus on Change:"), " We're interested in relative population changes, not absolute numbers"),
                    tags$li(strong("Statistical Efficiency:"), " Standardization helps the model learn patterns across species more effectively"),
                    tags$li(strong("Interpretability:"), " Changes in standard deviations provide a meaningful measure of population change magnitude")
                  ),
                  
                  h4("How to Interpret the Y-Axis"),
                  div(class = "code-box",
                      p(style = "margin: 0;", strong("Example Interpretation:")),
                      p(style = "margin: 5px 0 0 0;", "If a trend line starts at ", tags$code("-0.5"), " and increases to ", tags$code("+1.0"), ":"),
                      tags$ul(style = "margin: 5px 0;",
                              tags$li("The population started at 0.5 standard deviations below its long-term average"),
                              tags$li("It increased to 1.0 standard deviations above its long-term average"),
                              tags$li("This represents a change of 1.5 standard deviations"),
                              tags$li("This is a ", strong("substantial increase"), " in relative abundance")
                      )
                  ),
                  
                  div(class = "highlight-box",
                      h5("Practical Guidelines:", style = "color: #2E8B57; margin-top: 0;"),
                      tags$ul(
                        tags$li("Changes of ", strong("0.5 to 1.0"), " standard deviations represent moderate population shifts"),
                        tags$li("Changes greater than ", strong("1.0"), " standard deviations represent substantial population changes"),
                        tags$li("A flat trend near ", strong("0"), " indicates the population is near its long-term average"),
                        tags$li("The width of confidence ribbons indicates uncertainty in the trend estimate")
                      )
                  ),
                  
                  h4("Converting Back to Original Counts"),
                  p("While the standardized scale is useful for comparison, you can mentally convert back to approximate original counts if needed:"),
                  div(class = "code-box",
                      tags$pre("Original Count ≈ (Standardized Value × SD) + Mean"),
                      p(style = "margin: 5px 0 0 0; font-size: 11px;", 
                        "Where SD and Mean are the species-region specific values calculated during standardization")
                  ),
                  p("However, for most conservation applications, the standardized trends provide the most meaningful information about population changes over time.")
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
             Routes are surveyed once per year during peak breeding season, typically starting one-half hour before sunrise."),
                    p("Data are organized by Bird Conservation Regions (BCRs).
                    BCRs are ecologically distinct areas with similar bird communities, 
                      habitats, and resource management issues. They provide a biologically 
                      meaningful framework for analyzing population trends across 
                      North America's diverse landscapes.")
                    
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
                    
                  )
                ),
                
                # INTERPRETING RESULTS
                div(
                  h2("How to Read the Graphs", class = "section-header"),
                  p("The solid line shows the estimated population trend, which 
                represents the model's estimate of what population changes would 
                have occurred with standardized survey effort across all years.
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
                    p("Confidence ribbons around trend lines represent 95% credible intervals from the model's posterior distribution. Wider ribbons indicate greater uncertainty, typically occurring for:"),
                    tags$ul(
                      tags$li("Species or regions with limited data"),
                      tags$li("Time periods with high environmental variability"),
                      tags$li("Beginning and end of time series (edge effects)"),
                      tags$li("Species undergoing rapid population changes")
                    ),
                    h4("How to Interpret the Y-Axis"),
                    p("The models in this application use standardized counts rather than raw bird counts. Standardization is a statistical transformation that makes it easier to compare trends across species with very different baseline abundances."),
                    
                    div(class = "highlight-box",
                        h5("The Standardization Process:", style = "color: #2E8B57; margin-top: 0;"),
                        p("For each species in each region, we calculate:"),
                        tags$ol(
                          tags$li("The mean count across all years"),
                          tags$li("The standard deviation of counts across all years"),
                          tags$li("Transform each count using: ", tags$code("(count - mean) / standard deviation"))
                        ),
                        p("This creates a scale where:"),
                        tags$ul(
                          tags$li(strong("0"), " represents the long-term average count for that species-region combination"),
                          tags$li(strong("+1"), " means the count is one standard deviation above the average"),
                          tags$li(strong("-1"), " means the count is one standard deviation below the average")
                        )
                    ),
                    
                  ),
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
                  a("Access the data here.", href = "https://www.pwrc.usgs.gov/bbs/RawData/"),
                  
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
                choices = setNames(values,  labels),
                selected = NULL)
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
    
    updateSelectInput(
      session, 
      "select",
      choices = formatted_species_choices(),
      selected = current_selection
    )
  }, ignoreInit = TRUE)
  
  # Initialize species dropdown on app start
  observe({
    updateSelectInput(
      session,
      "select",
      choices = formatted_species_choices()
    )
  })
  
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
      
      d <- plot_data %>%
        filter(species == input$select, region == input$region)
      
      if (nrow(d) == 0) {
        return(data.frame())
      }
      
    } else if (input$selection_type == "group") {
      req(input$grouping_type, input$grouping_type != "")
      req(input$selected_group, input$selected_group != "")
      
      # This will trigger validation inside group_species_list()
      group_species <- tryCatch({
        group_species_list()
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(group_species) || length(group_species) == 0) {
        return(data.frame())
      }
      
      d <- calculate_group_trends(input$region, group_species)
      
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
      
      # Get trend data for selected species
      species_trends <- trend_data %>%
        filter(species == input$select)
      
      # Join species data with spatial data
      map_data <- regions %>%
        left_join(species_trends, by = c("BCR_clean" = "region"))
      
      return(map_data)
      
    } else if (input$selection_type == "group") {
      req(input$grouping_type, input$grouping_type != "")
      req(input$selected_group, input$selected_group != "")
      
      # Use tryCatch to handle potential errors
      group_species <- tryCatch({
        group_species_list()
      }, error = function(e) {
        return(NULL)
      })
      
      # Return NULL if no species found
      if (is.null(group_species) || length(group_species) == 0) {
        return(NULL)
      }
      
      # Get species count per region
      species_counts <- plot_data %>%
        filter(species %in% group_species) %>%
        group_by(region) %>%
        summarise(n_species = n_distinct(species), .groups = 'drop')
      
      # Calculate trends for each region based on weighted average
      group_trends <- lapply(unique(plot_data$region), function(reg) {
        group_data <- tryCatch({
          calculate_group_trends(reg, group_species)
        }, error = function(e) {
          return(data.frame())
        })

        if (nrow(group_data) > 3) {
          model <- lm(pred ~ year, data = group_data)
          slope <- coef(model)[2]

          data.frame(
            region = reg,
            slope = slope,
            trend_category = categorize_trend(slope) 
          )
        } else {
          data.frame(
            region = reg,
            slope = NA,
            trend_category = "Insufficient Data"
          )
        }
      }) %>%
        bind_rows() %>%
        left_join(species_counts, by = "region")
      
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
    
    # # Create custom color mapping function with relative intensity
    # get_color <- function(slope, trend_category) {
    #   # Handle missing data
    #   if (is.na(slope) || is.na(trend_category)) return("#F4F4F4")
    #   
    #   # Stable trends are always grey
    #   if (trend_category == "Stable") return("#D3D3D3")
    #   
    #   # No data cases
    #   if (trend_category %in% c("No Data", "Insufficient Data")) return("#F4F4F4")
    #   
    #   # Calculate relative intensity for this species/group
    #   if (max_abs_slope > 0) {
    #     slope_intensity <- abs(slope) / max_abs_slope
    #     
    #     # Increasing trends - green scale
    #     if (slope > 0) {
    #       if (slope_intensity <= 0.5) {
    #         return("#b6d7a8")  # Light green for moderate increase
    #       } else {
    #         return("#006600")  # Dark green for strong increase
    #       }
    #     } 
    #     # Decreasing trends - red scale
    #     else {
    #       if (slope_intensity <= 0.5) {
    #         return("#ea9999")  # Light red for moderate decrease
    #       } else {
    #         return("#CC0000")  # Dark red for strong decrease
    #       }
    #     }
    #   }
    #   
    #   return("#F4F4F4")  # Default fallback
    # }
    
    # Create custom color mapping function with ABSOLUTE intensity
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
        fillOpacity = 0.8,
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
          fillOpacity = 0.9,
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
      plot_title <- paste("Expected Trend for", display_name, "in", input$region)
    } else if (input$selection_type == "group") {
      plot_title <- paste("Expected Trend for", input$selected_group, "in", input$region)
    }
    
    ggplot(data, aes(x = year, y = pred)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5, linewidth = 0.5) +
      geom_line(linewidth = 1, alpha = 0.8, color = "darkblue") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightblue") +
      labs(title = plot_title,
           x = "Year", y = "Relative Abundance") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
        plot.margin = margin(10, 10, 5, 5),
        axis.title.x = element_text(size = 10, margin = margin(t = 5)),
        axis.title.y = element_text(size = 10, margin = margin(r = 5)),
        axis.text = element_text(size = 8)
      )
  }
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
                
                # Lifestyle
                if (!is.na(species_eco$lifestyle[1]) && species_eco$lifestyle[1] != "") {
                  p(style = "margin: 3px 0; font-size: 12px;",
                    strong("Foraging Lifestyle: "), species_eco$lifestyle[1])
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
      
      species_in_region <- plot_data %>%
        filter(species %in% group_species, region == input$region) %>%
        distinct(species) %>%
        nrow()
      
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
        
        div(
          style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          p(style = "margin: 5px 0;",
            strong("Species in ", input$region, ":"), " ", species_in_region),
          p(style = "margin: 5px 0;",
            strong("Total in group:"), " ", total_in_group)
        ),
        
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
    updateSelectInput(session, "select", selected = input$switch_to_species)
    
    # Reset the dropdown for next use
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
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
    
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
      labs(title = paste("Population Trends in", input$region2),
           x = "Year", 
           y = "Relative Abundance",
           color = "Species",
           fill = "Species") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10),  # Add margins around plot
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.7, "lines"),  # Make legend keys smaller
        legend.box.margin = margin(0, 0, 0, 0),  # Remove extra margin
        legend.margin = margin(5, 0, 0, 0)  # Small margin above legend
      ) +
      guides(
        color = guide_legend(override.aes = list(alpha = 1), nrow = 3, byrow = TRUE),  # Allow 2 rows if needed
        fill = "none"
      )
  })
  
  # Separate plots
  output$separate_plots <- renderUI({
    data <- comparisonData()
    
    if (nrow(data) == 0) {
      return(div("No data available for selected species and region"))
    }
    
    # Define colors for up to 4 species
    colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
    
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
  
  # # Display region info 
  # output$bcr_info_map_panel <- renderUI({
  #   bcr_info <- current_region_info()
  #   
  #   if (nrow(bcr_info) == 0) return(div("No region selected"))
  #   
  #   div(
  #     # Header
  #     div(
  #       style = "padding: 10px; margin-bottom: 15px; text-align: center;",
  #       h4(bcr_info$bcr_name, 
  #          style = "color: #2E8B57; margin: 0; font-size: 16px; font-weight: bold;")
  #     ),
  #     
  #     # Description
  #     if (!is.na(bcr_info$bcr_description)) {
  #       div(
  #         class = "bcr-description",
  #         style = "margin: 15px 15px; padding: 10px; background: #f8f9fa; border-radius: 5px; border-left: 3px solid #2E8B57;",
  #         p(bcr_info$bcr_description, 
  #           style = "margin: 0; color: #333; font-size: 14px;"),
  #         p(em("Source: North American Bird Conservation Initiative"), 
  #           style = "font-size: 11px; color: #666; margin-top: 8px; margin-bottom: 0;")
  #       )
  #     } else {
  #       div(
  #         style = "padding: 10px; background: #f8f9fa; border-radius: 5px; color: #666; font-style: italic;",
  #         p("Description not available", style = "margin: 0; font-size: 13px;")
  #       )
  #     }
  #   )
  # })
  
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
  
  
}


# Run the application
shinyApp(ui = ui, server = server)