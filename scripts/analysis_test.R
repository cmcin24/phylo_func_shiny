#### Post-processing ####
library(mgcv)
library(dplyr)
library(ggplot2)
library(marginaleffects)


# Source a few utility functions
source('Functions/utilities.R')

# Source the fitted models and data objects
load("./data/model_objects.rda")
mod <- readRDS("./models/mod.rds")

plot1 <- plot_sp_trends(model = mod,
                        data = mod_data,
                        species = 'Hirundo_rustica',
                        type = 'expected',
                        median_records = TRUE)

plot2 <- plot_sp_trends(model = mod,
                        data = mod_data,
                        species = 'Branta_canadensis',
                        type = 'expected',
                        median_records = TRUE)
plot3 <- plot_sp_trends(model = mod,
                        data = mod_data,
                        species = 'Myiarchus_crinitus',
                        type = 'expected',
                        median_records = TRUE)


plot_list <- list(plot1 = plot1, plot2 = plot2, plot3 = plot3) # Assign names for easier access

saveRDS(plot_list, file = "plots.rds")
