#
# Helper functions to load plots already computed and stored in .rds
#

library(ggplot2)
library(ggplotify)

plots <- readRDS("tests/plot_test.rds")

get_plot <- function(i) {
  return(as.ggplot(plots[[i]]))
}


