# Long-Term Trends in North American Bird Species Abundance

🔗 [Live app](https://hirundo.shinyapps.io/phylo_func_shiny/)

## Overview

This application visualises and models long-term population trends for bird species across North America, using data from the **North American Breeding Bird Survey (BBS)** — one of the largest and longest-running wildlife monitoring programs in the world, with over 50 years of standardised observations across 4,000+ routes.

The app is designed to be informative for researchers and conservationists while remaining approachable for a general audience. Users can:

- **Explore population trends** for individual species or groups of species (by family, order, habitat type, body size, and more) from 1992–2023
- **Compare trends across Bird Conservation Regions** — ecologically distinct areas with similar bird communities and habitats
- **Visualise uncertainty** through confidence ribbons derived from Bayesian posterior simulation

Population trends are estimated using a **hierarchical Generalised Additive Model (GAM)** that captures non-linear population changes over time. The model incorporates phylogenetic smoothing — meaning closely related species share statistical information, improving trend estimates for rarer species — and spatial smoothing across neighbouring regions. Model outputs were generated using University of Queensland high-performance computing infrastructure.

## Setup Instructions

### First Time Setup

1. **Pre-calculate all trends** (one-time, takes 2-5 minutes):
```bash
Rscript precalculate_trends.R
```

Or if you have `make` installed:
```bash
make precalculate
```

2. **Run the app**:
```r
shiny::runApp("app.R")
```

Or:
```bash
make run
```

### When to Re-Run Pre-Calculation

Re-run `precalculate_trends.R` when:

- You update the source data files
- You modify grouping categories
- You change the trend calculation logic

### File Structure
```
data/
  processed/
    trend_data_precalc.rds          # Pre-calculated species trends
    group_trends_precalc.rds        # Pre-calculated group trends
    group_trend_stats.rds           # Pre-calculated group statistics
    plot_data_with_counts.rds       # Pre-joined plot data
```
