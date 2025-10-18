# Script to obtain bird ecological data from AVONET and Partners in Flight
# AVONET: Tobias et al. (2022) Ecology Letters - morphological & ecological traits
# PIF: Partners in Flight Avian Conservation Assessment Database - conservation status

library(dplyr)
library(readr)
library(httr)
library(readxl)

# =============================================================================
# 1. DOWNLOAD AND LOAD AVONET DATABASE (BOTH SHEETS)
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
  
  # Read BOTH sheets from AVONET data
  tryCatch({
    cat("Loading AVONET1_BirdLife sheet...\n")
    avonet_main <- readxl::read_excel("data/AVONET_Raw_Data.xlsx", 
                                      sheet = "AVONET1_BirdLife",
                                      range = cell_cols("B:AK"))
    cat("✓ AVONET1_BirdLife loaded:", nrow(avonet_main), "species\n")
    
    cat("Loading AVONET3_BirdTree sheet...\n")
    avonet_backup <- readxl::read_excel("data/AVONET_Raw_Data.xlsx", 
                                        sheet = "AVONET3_BirdTree")
    cat("✓ AVONET3_BirdTree loaded:", nrow(avonet_backup), "species\n")
    
    # Return both sheets in a list
    return(list(
      main = avonet_main,
      backup = avonet_backup
    ))
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
  
  pif_processed <- pif_data %>%
    mutate(
      species_pif = gsub(" ", "_", .data[[species_col]]),
      # Population trend score (1=increasing, 5=steep decline)
      pif_trend_score = if (!is.na(trend_col)) .data[[trend_col]] else NA,
      # Conservation concern
      pif_concern = if (!is.na(concern_col)) .data[[concern_col]] else NA_character_,
      # Create simplified conservation status
      conservation_status = case_when(
        !is.na(pif_concern) & grepl("Common.*Steep.*Decline", pif_concern, ignore.case = TRUE) ~ "Common Bird in Steep Decline",
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
# 3. PROCESS AVONET DATA FOR YOUR SPECIES (WITH FALLBACK TO SECOND SHEET)
# =============================================================================

process_avonet_ecological_data <- function(species_list, avonet_data, pif_data = NULL) {
  
  cat("\n=== Processing AVONET Data for Your Species ===\n")
  
  # Extract both sheets
  avonet_main <- avonet_data$main
  avonet_backup <- avonet_data$backup
  
  # Clean species names for matching
  species_data <- data.frame(
    species = species_list,
    species_clean = gsub("_", " ", species_list),
    stringsAsFactors = FALSE
  )
  
  # Function to select and rename key AVONET variables (handles both sheet formats)
  process_avonet_sheet <- function(avonet_df, sheet_number = 1) {
    # Helper function to safely convert to numeric
    safe_numeric <- function(x) {
      # Handle common non-numeric placeholders
      x <- as.character(x)
      x <- gsub("^-$|^NA$|^na$|^N/A$|^n/a$|^\\s*$", NA, x, ignore.case = TRUE)
      # Convert to numeric, suppressing warnings
      suppressWarnings(as.numeric(x))
    }
    
    # Determine column names based on sheet number
    if (sheet_number == 1) {
      species_col <- "Species1"
      family_col <- "Family1"
      order_col <- "Order1"
    } else {
      species_col <- "Species3"
      family_col <- "Family3"
      order_col <- "Order3"
    }
    
    avonet_df %>%
      select(
        # Taxonomic
        species_raw = !!species_col,
        family_raw = !!family_col,
        order_raw = !!order_col,
        
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
        species_avonet = species_raw,
        family = family_raw,
        order = order_raw
      ) %>%
      # Standardize column types using safe conversion
      mutate(
        Habitat = as.character(Habitat),
        Habitat.Density = safe_numeric(Habitat.Density),
        Migration = as.character(Migration),
        Trophic.Level = as.character(Trophic.Level),
        Trophic.Niche = as.character(Trophic.Niche),
        Primary.Lifestyle = as.character(Primary.Lifestyle),
        Mass = safe_numeric(Mass),
        Wing.Length = safe_numeric(Wing.Length),
        Tail.Length = safe_numeric(Tail.Length),
        Beak.Length = safe_numeric(Beak.Length),
        Range.Size = safe_numeric(Range.Size),
        Centroid.Latitude = safe_numeric(Centroid.Latitude),
        Centroid.Longitude = safe_numeric(Centroid.Longitude)
      )
  }
  
  # Process both AVONET sheets
  cat("Processing AVONET1_BirdLife sheet...\n")
  avonet_main_processed <- process_avonet_sheet(avonet_main, sheet_number = 1)
  
  cat("Processing AVONET3_BirdTree sheet...\n")
  avonet_backup_processed <- process_avonet_sheet(avonet_backup, sheet_number = 3)
  
  # First, match with main sheet
  species_data <- species_data %>%
    left_join(avonet_main_processed, by = c("species_clean" = "species_avonet"))
  
  matched_main <- sum(!is.na(species_data$family))
  cat("✓ Matched", matched_main, "species from AVONET1_BirdLife\n")
  
  # Find species not matched in main sheet
  unmatched_species <- species_data %>%
    filter(is.na(family)) %>%
    select(species, species_clean)
  
  if (nrow(unmatched_species) > 0) {
    cat("Checking AVONET2_eBird for", nrow(unmatched_species), "unmatched species...\n")
    
    # Get data from backup sheet for unmatched species
    backup_matches <- unmatched_species %>%
      left_join(avonet_backup_processed, by = c("species_clean" = "species_avonet"))
    
    # Count how many were found in backup
    matched_backup <- sum(!is.na(backup_matches$family))
    
    if (matched_backup > 0) {
      cat("✓ Matched", matched_backup, "additional species from AVONET2_eBird\n")
      
      # Update the main species_data with backup matches
      # Remove rows that are still unmatched
      species_data_matched <- species_data %>%
        filter(!is.na(family))
      
      # Add backup matches
      species_data <- bind_rows(
        species_data_matched,
        backup_matches %>% filter(!is.na(family))
      )
      
      # Add back any species that still weren't matched
      still_unmatched <- unmatched_species %>%
        anti_join(backup_matches %>% filter(!is.na(family)), 
                  by = c("species", "species_clean"))
      
      if (nrow(still_unmatched) > 0) {
        # Add these back with NA values
        still_unmatched_full <- species_data %>%
          filter(species %in% still_unmatched$species, is.na(family))
        
        species_data <- bind_rows(
          species_data %>% filter(!is.na(family)),
          still_unmatched_full
        )
      }
    } else {
      cat("✗ No additional matches found in AVONET2_eBird\n")
    }
  }
  
  # Final match count
  total_matched <- sum(!is.na(species_data$family))
  cat("\n✓ TOTAL: Matched", total_matched, "out of", nrow(species_data), 
      "species (", round(100 * total_matched / nrow(species_data), 1), "%)\n")
  cat("  - From AVONET1_BirdLife:", matched_main, "\n")
  if (exists("matched_backup") && matched_backup > 0) {
    cat("  - From AVONET2_eBird:", matched_backup, "\n")
  }
  
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
    cat("\n⚠ Still unmatched species (", nrow(unmatched), "):\n")
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
      habitat_category = as.character(Habitat),
      
      # # FORAGING STRATEGY - From AVONET Trophic.Niche
      # foraging_strategy = case_when(
      #   grepl("Aerial", Trophic.Niche, ignore.case = TRUE) ~ "Aerial",
      #   grepl("Sallying", Trophic.Niche, ignore.case = TRUE) ~ "Sallying",
      #   grepl("Bark|Tree|Trunk", Trophic.Niche, ignore.case = TRUE) ~ "Bark gleaning",
      #   grepl("Foliage", Trophic.Niche, ignore.case = TRUE) ~ "Foliage gleaning",
      #   grepl("Ground", Trophic.Niche, ignore.case = TRUE) ~ "Ground foraging",
      #   grepl("Aquatic", Trophic.Niche, ignore.case = TRUE) ~ "Aquatic foraging",
      #   TRUE ~ as.character(Trophic.Niche)
      # ),
      
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
      conservation_category = conservation_status
      
    )
  
  # Show summary of categories
  cat("\nHabitat categories:\n")
  print(table(species_data$habitat_category) %>% sort(decreasing = TRUE))
  
  # cat("\nForaging strategies:\n")
  # print(table(species_data$foraging_strategy) %>% sort(decreasing = TRUE))
  # 
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
  
  # Helper function to create choices, returns NULL if empty
  create_choices <- function(data, column, include_count = TRUE, exclude_values = NULL) {
    # Build filter condition
    filtered_data <- data %>%
      filter(!is.na(!!sym(column)))
    
    # Exclude specific values if provided
    if (!is.null(exclude_values)) {
      filtered_data <- filtered_data %>%
        filter(!(!!sym(column) %in% exclude_values))
    }
    
    # Count and create choices
    choice_data <- filtered_data %>%
      count(!!sym(column), sort = TRUE)
    
    if (nrow(choice_data) == 0) {
      cat("  ⚠ Warning: No data for", column, "\n")
      return(NULL)
    }
    
    if (include_count) {
      choices <- setNames(
        choice_data[[column]], 
        paste0(choice_data[[column]], " (", choice_data$n, " species)")
      )
    } else {
      choices <- setNames(choice_data[[column]], choice_data[[column]])
    }
    
    cat("  ✓", column, ":", nrow(choice_data), "categories\n")
    return(choices)
  }
  
  # Build grouping options, only including non-empty categories
  grouping_options <- list()
  
  # Taxonomic Family
  family_choices <- create_choices(species_data, "family", include_count = TRUE)
  if (!is.null(family_choices)) {
    grouping_options[["Taxonomic Family"]] <- list(
      column = "family",
      display_column = "family",
      choices = family_choices
    )
  }
  
  # Taxonomic Order
  order_choices <- create_choices(species_data, "order", include_count = TRUE)
  if (!is.null(order_choices)) {
    grouping_options[["Taxonomic Order"]] <- list(
      column = "order",
      display_column = "order",
      choices = order_choices
    )
  }
  
  # Habitat Type
  habitat_choices <- create_choices(species_data, "habitat_category", 
                                    include_count = TRUE, 
                                    exclude_values = c("Other"))
  if (!is.null(habitat_choices)) {
    grouping_options[["Habitat Type"]] <- list(
      column = "habitat_category",
      display_column = "habitat_category",
      choices = habitat_choices
    )
  }
  
  # # Foraging Strategy
  # foraging_choices <- create_choices(species_data, "foraging_strategy", include_count = TRUE)
  # if (!is.null(foraging_choices)) {
  #   grouping_options[["Foraging Strategy"]] <- list(
  #     column = "foraging_strategy",
  #     display_column = "foraging_strategy",
  #     choices = foraging_choices
  #   )
  # }
  
  # Migration Pattern
  migration_choices <- create_choices(species_data, "migration_status", 
                                      include_count = TRUE, 
                                      exclude_values = c("Unknown"))
  if (!is.null(migration_choices)) {
    grouping_options[["Migration Pattern"]] <- list(
      column = "migration_status",
      display_column = "migration_status",
      choices = migration_choices
    )
  }
  
  # Body Size
  body_size_choices <- create_choices(species_data, "body_size_category", 
                                      include_count = FALSE, 
                                      exclude_values = c("Unknown"))
  if (!is.null(body_size_choices)) {
    grouping_options[["Body Size"]] <- list(
      column = "body_size_category",
      display_column = "body_size_category",
      choices = body_size_choices
    )
  }
  
  # Diet Type
  diet_choices <- create_choices(species_data, "diet_type", include_count = TRUE)
  if (!is.null(diet_choices)) {
    grouping_options[["Diet Type"]] <- list(
      column = "diet_type",
      display_column = "diet_type",
      choices = diet_choices
    )
  }
  
  # Conservation Status
  conservation_choices <- create_choices(species_data, "conservation_category", 
                                         include_count = TRUE, 
                                         exclude_values = c("Unknown"))
  if (!is.null(conservation_choices)) {
    grouping_options[["Conservation Status"]] <- list(
      column = "conservation_category",
      display_column = "conservation_category",
      choices = conservation_choices
    )
  }
  
  cat("\n✓ Created", length(grouping_options), "grouping options\n")
  
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
  
  # Step 1: Download and load AVONET (both sheets)
  avonet_data <- download_avonet_data()
  if (is.null(avonet_data)) {
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
  load("data/processed/model_objects.rda")
  
  unique_species <- unique(mod_data$sp_latin)
  
  # Step 4: Match your species with AVONET (both sheets) and PIF data
  species_data <- process_avonet_ecological_data(unique_species, avonet_data, pif_data)
  
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
  # cat("- Foraging:", length(unique(species_data$foraging_strategy[!is.na(species_data$foraging_strategy)])), "strategies\n")
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
                 select(species, family, habitat_category, 
                        # foraging_strategy, 
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
cat("   - Load BOTH AVONET sheets (BirdLife and eBird)\n")
cat("   - Use BirdLife sheet first, then eBird as fallback\n")
cat("   - Load PIF conservation data (if available)\n")
cat("   - Match your species with ecological data\n")
cat("   - Create standardized grouping categories\n")
cat("   - Save results to data/ folder\n\n")

# Uncomment to run automatically:
# species_data <- main()