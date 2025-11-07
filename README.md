# Setup Instructions

<<<<<<< HEAD
Test commit from RStudio
test2 
test3 
test4
=======
## First Time Setup

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

## When to Re-Run Pre-Calculation

Re-run `precalculate_trends.R` when:
- You update the source data files
- You modify grouping categories
- You change the trend calculation logic

## File Structure
```
data/
  processed/
    trend_data_precalc.rds          # Pre-calculated species trends
    group_trends_precalc.rds        # Pre-calculated group trends
    group_trend_stats.rds           # Pre-calculated group statistics
    plot_data_with_counts.rds       # Pre-joined plot data
```
>>>>>>> 4f8a2ea7f2bb6dc1a69b995d282805b33be11455
