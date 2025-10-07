# Script to obtain bird ecological data from AVONET and Partners in Flight
# AVONET: Tobias et al. (2022) Ecology Letters - morphological & ecological traits
# PIF: Partners in Flight Avian Conservation Assessment Database - conservation status

library(dplyr)
library(readr)
library(httr)
library(readxl)

# =============================================================================
# 1. DOWNLOAD AND LOAD AVONET DATABASE
# =============================================================================

download_avonet_data <- function() {
  cat("Downloading AVONET database...\n")
  
  # Direct download from Figshare (official repository)
  avonet_url <- "https://figshare.com/ndownloader/files/34480856"
  
  if (!file.exists("data/AVONET_Raw_Data.xlsx")) {
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    tryCatch({
      download.file(avonet_url, "data/AVONET_Raw_Data.xlsx", mode = "wb")
      cat("✓ AVONET data downloaded successfully\n")
    }, error = function(e) {
      cat("✗ Error downloading AVONET data:", e$message, "\n")
      cat("You can manually download from: https://figshare.com/articles/dataset/AVONET_morphological_ecological_and_geographical_data_for_all_birds/16586228\n")
      return(NULL)
    })
  } else {
    cat("✓ AVONET data already exists\n")
  }
  
  # Read and process AVONET data
  tryCatch({
    avonet_main <- readxl::read_excel("data/AVONET_Raw_Data.xlsx", sheet = "AVONET1_BirdLife", range = cell_cols("B:AK"))
    avonet_backup <- readxl::read_excel("data/AVONET_Raw_Data.xlsx", sheet = "AVONET2_eBird")
    cat("✓ AVONET data loaded:", nrow(avonet), "species\n")
    return(avonet)
  }, error = function(e) {
    cat("✗ Error reading AVONET data:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# 2. PARTNERS IN FLIGHT DATA ACCESS
# =============================================================================

download_pif_instructions <- function() {
  cat("\n=== Partners in Flight Data Download Instructions ===\n")
  cat("PIF provides conservation scores for North American landbirds.\n\n")
  
  cat("OPTION 1: Download via Web Interface\n")
  cat("1. Visit: https://pif.birdconservancy.org/ACAD/Database.aspx\n")
  cat("2. Click 'Query the Database'\n")
  cat("3. Select 'All Species' and your region of interest (e.g., United States-Canada)\n")
  cat("4. Click 'Export to Excel'\n")
  cat("5. Save the file as 'data/pif_species_assessment.csv'\n\n")
  
  cat("OPTION 2: Download All Data\n")
  cat("1. Visit: https://pif.birdconservancy.org/avian-conservation-assessment-database-archives/\n")
  cat("2. Download the latest version (e.g., 'ACAD_2023_US-Canada.xlsx')\n")
  cat("3. Open in Excel and save as CSV: 'data/pif_species_assessment.csv'\n\n")
  
  cat("The PIF database includes:\n")
  cat("- Population Trend scores (1-5, where 5 = steep decline)\n")
  cat("- Breeding Distribution scores\n")
  cat("- Non-breeding Distribution scores\n")
  cat("- Population Size scores\n")
  cat("- Regional Concern scores\n")
  cat("- Continental Concern scores\n\n")
}

load_pif_data <- function() {
  cat("\n=== Loading Partners in Flight Data ===\n")
  
  # Check for PIF data file
  pif_files <- c(
    "data/raw/pif_species_assessment.xlsx",
    "data/raw/PIF_Species_Assessment.xlsx",
    "data/raw/ACAD_US_Canada.xlsx"
  )
  
  pif_file <- NULL
  for (file in pif_files) {
    if (file.exists(file)) {
      pif_file <- file
      break
    }
  }
  
  if (is.null(pif_file)) {
    cat("✗ PIF data file not found\n")
    download_pif_instructions()
    return(NULL)
  }
  
  # Try to read PIF data
  tryCatch({
    pif_data <- readxl::read_excel(pif_file, col_names = TRUE)
    cat("✓ PIF data loaded:", nrow(pif_data), "species assessments\n")
    
    # Show available columns
    cat("Available PIF columns:\n")
    print(names(pif_data))
    
    return(pif_data)
  }, error = function(e) {
    cat("✗ Error reading PIF data:", e$message, "\n")
    return(NULL)
  })
}

process_pif_data <- function(pif_data) {
  cat("\n=== Processing PIF Conservation Data ===\n")
  
  if (is.null(pif_data)) return(NULL)
  
  # PIF uses different column names depending on version
  # Try to find the right columns
  possible_species_cols <- c("Scientific_name", "Scientific Name", "Scientific.name", "Species", "SPECIES")
  possible_trend_cols <- c("PTrend", "P_Trend", "Population_Trend", "PopulationTrend", "PS-g")
  possible_concern_cols <- c("RegionalConcern", "Regional_Concern", "Concern", "Continental Importance")
  
  species_col <- intersect(names(pif_data), possible_species_cols)[1]
  trend_col <- intersect(names(pif_data), possible_trend_cols)[1]
  concern_col <- intersect(names(pif_data), possible_concern_cols)[1]
  
  if (is.na(species_col)) {
    cat("⚠ Warning: Could not identify species name column\n")
    cat("Available columns:", paste(names(pif_data), collapse = ", "), "\n")
    return(NULL)
  }
  
  # Process and standardize PIF data
  pif_processed <- pif_data %>%
    mutate(
      species_pif = gsub(" ", "_", .data[[species_col]]),
      # Population trend score (1=increasing, 5=steep decline)
      pif_trend_score = if (!is.na(trend_col)) .data[[trend_col]] else NA,
      # Conservation concern
      pif_concern = if (!is.na(concern_col)) .data[[concern_col]] else NA,
      # Create simplified conservation status
      conservation_status = case_when(
        !is.na(pif_concern) & grepl("Red|High", pif_concern, ignore.case = TRUE) ~ "High Concern",
        !is.na(pif_concern) & grepl("Yellow|Moderate|Orange", pif_concern, ignore.case = TRUE) ~ "Moderate Concern",
        TRUE ~ "Low Concern"
      )
    )
  
  cat("✓ PIF data processed\n")
  cat("Conservation status breakdown:\n")
  print(table(pif_processed$conservation_status))
  
  return(pif_processed)
}

# =============================================================================
# 3. PROCESS AVONET DATA FOR YOUR SPECIES
# =============================================================================

process_avonet_ecological_data <- function(species_list, avonet, pif_data = NULL) {
  
  cat("\n=== Processing AVONET Data for Your Species ===\n")
  
  # Clean species names for matching
  species_data <- data.frame(
    species = species_list,
    species_clean = gsub("_", " ", species_list),
    stringsAsFactors = FALSE
  )
  
  # Select and rename key AVONET variables
  avonet_processed <- avonet %>%
    select(
      # Taxonomic
      Species1, Family1, Order1,
      
      # Ecological
      Habitat, `Habitat.Density`, Migration, 
      `Trophic.Level`, `Trophic.Niche`, `Primary.Lifestyle`,
      
      # Morphological  
      Mass = `Mass`,
      Wing.Length = `Wing.Length`,
      Tail.Length = `Tail.Length`,
      Beak.Length = `Beak.Length_Culmen`,
      
      # Geographic
      Range.Size,
      Centroid.Latitude = `Centroid.Latitude`,
      Centroid.Longitude = `Centroid.Longitude`
    ) %>%
    rename(
      species_avonet = Species1,
      family = Family1,
      order = Order1
    )
  
  # Match with your species list
  species_data <- species_data %>%
    left_join(avonet_processed, by = c("species_clean" = "species_avonet"))
  
  cat("✓ Matched", sum(!is.na(species_data$family)), "out of", nrow(species_data), "species with AVONET data\n")
  
  # Add PIF conservation data if available
  if (!is.null(pif_data)) {
    cat("\n=== Integrating PIF Conservation Data ===\n")
    
    pif_select <- pif_data %>%
      select(species_pif, pif_trend_score, 
             pif_concern, conservation_status)
    
    species_data <- species_data %>%
      left_join(pif_select, by = c("species" = "species_pif"))
    
    cat("✓ Matched", sum(!is.na(species_data$pif_trend_score)), "species with PIF conservation data\n")
  }
  
  # Show unmatched species
  unmatched <- species_data %>% filter(is.na(family))
  if (nrow(unmatched) > 0) {
    cat("\n⚠ Unmatched species (", nrow(unmatched), "):\n")
    print(head(unmatched$species, 10))
    if (nrow(unmatched) > 10) cat("... and", nrow(unmatched) - 10, "more\n")
  }
  
  return(species_data)
}

# =============================================================================
# 4. CREATE STANDARDIZED GROUPING CATEGORIES
# =============================================================================

create_standardized_categories <- function(species_data) {
  
  cat("\n=== Creating Standardized Ecological Categories ===\n")
  
  species_data <- species_data %>%
    mutate(
      # HABITAT CATEGORIES - Simplified from AVONET Habitat
      habitat_category = case_when(
        grepl("Forest", Habitat, ignore.case = TRUE) ~ "Forest",
        grepl("Woodland", Habitat, ignore.case = TRUE) ~ "Forest/Woodland",
        grepl("Shrubland|Scrub", Habitat, ignore.case = TRUE) ~ "Scrubland",
        grepl("Grassland|Savanna", Habitat, ignore.case = TRUE) ~ "Grassland",
        grepl("Wetland", Habitat, ignore.case = TRUE) ~ "Wetland",
        grepl("Aquatic|Marine", Habitat, ignore.case = TRUE) ~ "Aquatic",
        grepl("Desert", Habitat, ignore.case = TRUE) ~ "Desert",
        grepl("Rocky|Mountain", Habitat, ignore.case = TRUE) ~ "Rocky areas",
        grepl("Artificial|Urban", Habitat, ignore.case = TRUE) ~ "Urban",
        TRUE ~ "Other"
      ),
      
      # FORAGING STRATEGY - From AVONET Trophic.Niche
      foraging_strategy = case_when(
        grepl("Aerial", Trophic.Niche, ignore.case = TRUE) ~ "Aerial",
        grepl("Sallying", Trophic.Niche, ignore.case = TRUE) ~ "Sallying",
        grepl("Bark|Tree|Trunk", Trophic.Niche, ignore.case = TRUE) ~ "Bark gleaning",
        grepl("Foliage", Trophic.Niche, ignore.case = TRUE) ~ "Foliage gleaning",
        grepl("Ground", Trophic.Niche, ignore.case = TRUE) ~ "Ground foraging",
        grepl("Aquatic", Trophic.Niche, ignore.case = TRUE) ~ "Aquatic foraging",
        TRUE ~ as.character(Trophic.Niche)
      ),
      
      # DIET TYPE - From AVONET Trophic.Level
      diet_type = case_when(
        grepl("Omnivore", Trophic.Level, ignore.case = TRUE) ~ "Omnivore",
        grepl("Carnivore", Trophic.Level, ignore.case = TRUE) ~ "Carnivore",
        grepl("Herbivore", Trophic.Level, ignore.case = TRUE) ~ "Herbivore",
        grepl("Scavenger", Trophic.Level, ignore.case = TRUE) ~ "Scavenger",
        TRUE ~ as.character(Trophic.Level)
      ),
      
      # MIGRATION STATUS - From AVONET Migration
      migration_status = case_when(
        grepl("^1$", Migration) ~ "Sedentary",
        grepl("^2$", Migration) ~ "Partially migratory",
        grepl("^3$", Migration) ~ "Migratory",
        is.na(Migration) ~ "Unknown",
        TRUE ~ as.character(Migration)
      ),
      
      # BODY SIZE CATEGORIES - From AVONET Mass
      body_size_category = case_when(
        is.na(Mass) ~ "Unknown",
        Mass < 20 ~ "Small (<20g)",
        Mass >= 20 & Mass < 50 ~ "Medium-small (20-50g)",
        Mass >= 50 & Mass < 100 ~ "Medium (50-100g)",
        Mass >= 100 & Mass < 500 ~ "Medium-large (100-500g)",
        Mass >= 500 ~ "Large (>500g)",
        TRUE ~ "Unknown"
      ),
      
      # LIFESTYLE - From AVONET Primary.Lifestyle
      lifestyle = as.character(Primary.Lifestyle),
      
      # CONSERVATION STATUS - Use PIF if available, otherwise unknown
      conservation_category = case_when(
        !is.na(conservation_status) ~ conservation_status,
        !is.na(pif_trend_score) & pif_trend_score >= 4 ~ "High Concern - Declining",
        !is.na(pif_trend_score) & pif_trend_score == 3 ~ "Moderate Concern",
        !is.na(pif_trend_score) & pif_trend_score <= 2 ~ "Stable/Increasing",
        TRUE ~ "Unknown"
      ),
      
    )
  
  # Show summary of categories
  cat("\nHabitat categories:\n")
  print(table(species_data$habitat_category) %>% sort(decreasing = TRUE))
  
  cat("\nForaging strategies:\n")
  print(table(species_data$foraging_strategy) %>% sort(decreasing = TRUE))
  
  cat("\nMigration status:\n")
  print(table(species_data$migration_status) %>% sort(decreasing = TRUE))
  
  cat("\nBody size categories:\n")
  print(table(species_data$body_size_category) %>% sort(decreasing = TRUE))
  
  cat("\nConservation status (from PIF):\n")
  print(table(species_data$conservation_category) %>% sort(decreasing = TRUE))
  
  return(species_data)
}

# =============================================================================
# 5. CREATE GROUPING CHOICES FOR SHINY UI
# =============================================================================

create_grouping_choices <- function(species_data) {
  
  cat("\n=== Creating Grouping Choices for UI ===\n")
  
  grouping_options <- list(
    
    "Taxonomic Family" = list(
      column = "family",
      display_column = "family",
      choices = species_data %>%
        filter(!is.na(family)) %>%
        count(family, sort = TRUE) %>%
        {setNames(.$family, paste0(.$family, " (", .$n, " species)"))}
    ),
    
    "Taxonomic Order" = list(
      column = "order",
      display_column = "order",
      choices = species_data %>%
        filter(!is.na(order)) %>%
        count(order, sort = TRUE) %>%
        {setNames(.$order, paste0(.$order, " (", .$n, " species)"))}
    ),
    
    "Habitat Type" = list(
      column = "habitat_category",
      display_column = "habitat_category",
      choices = species_data %>%
        filter(!is.na(habitat_category) & habitat_category != "Other") %>%
        count(habitat_category, sort = TRUE) %>%
        {setNames(.$habitat_category, paste0(.$habitat_category, " (", .$n, " species)"))}
    ),
    
    "Foraging Strategy" = list(
      column = "foraging_strategy",
      display_column = "foraging_strategy",
      choices = species_data %>%
        filter(!is.na(foraging_strategy)) %>%
        count(foraging_strategy, sort = TRUE) %>%
        {setNames(.$foraging_strategy, paste0(.$foraging_strategy, " (", .$n, " species)"))}
    ),
    
    "Migration Pattern" = list(
      column = "migration_status",
      display_column = "migration_status",
      choices = species_data %>%
        filter(!is.na(migration_status) & migration_status != "Unknown") %>%
        count(migration_status, sort = TRUE) %>%
        {setNames(.$migration_status, paste0(.$migration_status, " (", .$n, " species)"))}
    ),
    
    "Body Size" = list(
      column = "body_size_category",
      display_column = "body_size_category",
      choices = species_data %>%
        filter(!is.na(body_size_category) & body_size_category != "Unknown") %>%
        count(body_size_category, sort = TRUE) %>%
        {setNames(.$body_size_category, .$body_size_category)}
    ),
    
    "Diet Type" = list(
      column = "diet_type",
      display_column = "diet_type",
      choices = species_data %>%
        filter(!is.na(diet_type)) %>%
        count(diet_type, sort = TRUE) %>%
        {setNames(.$diet_type, paste0(.$diet_type, " (", .$n, " species)"))}
    ),
    
    "Conservation Status" = list(
      column = "conservation_category",
      display_column = "conservation_category",
      choices = species_data %>%
        filter(!is.na(conservation_category) & conservation_category != "Unknown") %>%
        count(conservation_category, sort = TRUE) %>%
        {setNames(.$conservation_category, paste0(.$conservation_category, " (", .$n, " species)"))}
    ),
    
  )
  
  return(grouping_options)
}

# =============================================================================
# 6. CALCULATE GROUP TRENDS (for use in Shiny app)
# =============================================================================

calculate_group_trends <- function(plot_data, species_data, selected_region, 
                                   selected_group, group_column) {
  # Get species in the selected group
  group_species <- species_data %>%
    filter(!!sym(group_column) == selected_group) %>%
    pull(species)
  
  # Get plot data for these species in the selected region
  group_plot_data <- plot_data %>%
    filter(species %in% group_species, region == selected_region)
  
  if (nrow(group_plot_data) == 0) {
    return(data.frame())
  }
  
  # Calculate weighted average by year
  group_trend <- group_plot_data %>%
    group_by(year) %>%
    summarise(
      pred = mean(pred, na.rm = TRUE),
      lower = mean(lower, na.rm = TRUE),
      upper = mean(upper, na.rm = TRUE),
      n_species = n_distinct(species),
      .groups = 'drop'
    )
  
  return(group_trend)
}

# =============================================================================
# 7. MAIN EXECUTION
# =============================================================================

main <- function() {
  cat("=== Bird Ecological Data from AVONET & Partners in Flight ===\n")
  cat("AVONET: Tobias et al. (2022) Ecology Letters\n")
  cat("  https://doi.org/10.1111/ele.13898\n")
  cat("PIF: Partners in Flight Avian Conservation Assessment Database\n")
  cat("  https://pif.birdconservancy.org/ACAD/\n\n")
  
  # Create data directory
  if (!file.exists("data")) {
    dir.create("data", recursive = TRUE)
  }
  
  # Step 1: Download and load AVONET
  avonet <- download_avonet_data()
  if (is.null(avonet)) {
    stop("Failed to load AVONET data. Please download manually.")
  }
  
  # Step 2: Load PIF data (if available)
  pif_data <- load_pif_data()
  
  # Process PIF data if loaded
  if (!is.null(pif_data)) {
    pif_data <- process_pif_data(pif_data)
  } else {
    cat("\n⚠ Continuing without PIF data. Conservation status will be limited.\n")
  }
  
  # Step 3: Load your species list
  model_data <- load("data/processed/model_objects.rda")
  
  unique_species <- unique(mod_data$sp_latin)
  
  # Step 4: Match your species with AVONET and PIF data
  species_data <- process_avonet_ecological_data(unique_species, avonet, pif_data)
  
  # Step 5: Create standardized categories
  species_data <- create_standardized_categories(species_data)
  
  # Step 6: Create grouping choices for UI
  grouping_choices <- create_grouping_choices(species_data)
  
  # Step 7: Save results
  saveRDS(species_data, "data/species_ecological_avonet_pif.rds")
  saveRDS(grouping_choices, "data/grouping_choices_avonet_pif.rds")
  
  # Export to CSV for easy viewing
  write_csv(species_data, "data/species_ecological_avonet_pif.csv")
  
  cat("\n=== FILES SAVED ===\n")
  cat("✓ data/species_ecological_avonet_pif.rds (R data file)\n")
  cat("✓ data/species_ecological_avonet_pif.csv (CSV file)\n")
  cat("✓ data/grouping_choices_avonet_pif.rds (UI grouping options)\n")
  
  # Final summary
  cat("\n=== FINAL SUMMARY ===\n")
  cat("Total species in your dataset:", length(unique_species), "\n")
  cat("Species matched with AVONET:", sum(!is.na(species_data$family)), "\n")
  cat("Match rate:", round(100 * sum(!is.na(species_data$family)) / length(unique_species), 1), "%\n")
  
  if (!is.null(pif_data)) {
    cat("Species with PIF conservation data:", sum(!is.na(species_data$pif_trend_score)), "\n")
    cat("PIF match rate:", round(100 * sum(!is.na(species_data$pif_trend_score)) / length(unique_species), 1), "%\n")
  }
  
  cat("\nData includes:\n")
  cat("- Taxonomic: Family, Order\n")
  cat("- Habitat:", length(unique(species_data$habitat_category[!is.na(species_data$habitat_category)])), "categories\n")
  cat("- Foraging:", length(unique(species_data$foraging_strategy[!is.na(species_data$foraging_strategy)])), "strategies\n")
  cat("- Migration:", length(unique(species_data$migration_status[!is.na(species_data$migration_status)])), "patterns\n")
  cat("- Body size:", length(unique(species_data$body_size_category[!is.na(species_data$body_size_category)])), "categories\n")
  cat("- Diet:", length(unique(species_data$diet_type[!is.na(species_data$diet_type)])), "types\n")
  
  if (!is.null(pif_data)) {
    cat("- Conservation:", length(unique(species_data$conservation_category[!is.na(species_data$conservation_category)])), "categories\n")
  }
  
  # Show sample with PIF data
  cat("\nSample of comprehensive data:\n")
  if (!is.null(pif_data)) {
    print(head(species_data %>% 
                 select(species, family, habitat_category, foraging_strategy, 
                        migration_status, conservation_category), 10))
  } else {
    print(head(species_data %>% 
                 select(species, family, habitat_category, foraging_strategy, 
                        migration_status, body_size_category), 10))
  }
  
  return(species_data)
}

# =============================================================================
# RUN THE SCRIPT
# =============================================================================

cat("\n=== INSTRUCTIONS ===\n")
cat("1. First, download PIF data (optional but recommended):\n")
cat("   - Visit: https://pif.birdconservancy.org/ACAD/Database.aspx\n")
cat("   - Export data for US-Canada as CSV\n")
cat("   - Save as: data/pif_species_assessment.csv\n\n")
cat("2. Run: species_data <- main()\n")
cat("   This will:\n")
cat("   - Download AVONET database (if not already downloaded)\n")
cat("   - Load PIF conservation data (if available)\n")
cat("   - Match your species with ecological data\n")
cat("   - Create standardized grouping categories\n")
cat("   - Save results to data/ folder\n\n")

# Uncomment to run automatically:
species_data <- main()
