# Load required libraries
library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(ggplot2)
library(dplyr)
library(viridis)

# Load data objects
plot_data <- readRDS("./tests/plot_data_allBCR.rds")

# Function to calculate trends for each species-region combination
calculate_trends <- function(plot_data) {
  trends <- plot_data %>%
    group_by(species, region) %>%
    do({
      if (nrow(.) > 3) {  # Need at least 4 points for meaningful trend
        model <- lm(pred ~ year, data = .)
        slope <- coef(model)[2]
        p_value <- summary(model)$coefficients[2, 4]
        r_squared <- summary(model)$r.squared
        
        # Classify trend based on slope and significance
        trend_category <- case_when(
          p_value > 0.05 ~ "Stable",
          slope > 0 ~ "Increasing",
          slope < 0 ~ "Decreasing"
        )
        
        data.frame(
          slope = slope,
          p_value = p_value,
          r_squared = r_squared,
          trend_category = trend_category,
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