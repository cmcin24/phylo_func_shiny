# Script to generate bird family data for your Shiny app
# Run this script once to create the family data file

library(taxize)
library(dplyr)

# Family common names lookup table
family_common_names <- data.frame(
  family = c(
    "Polioptilidae", "Caprimulgidae", "Anatidae", "Columbidae", "Vireonidae", 
    "Falconidae", "Picidae", "Hirundinidae", "Icteridae", "Parulidae", 
    "Turdidae", "Paridae", "Corvidae", "Cardinalidae", "Fringillidae", 
    "Passerellidae", "Tyrannidae", "Trochilidae", "Accipitridae", "Strigidae",
    "Mimidae", "Bombycillidae", "Regulidae", "Sittidae", "Certhiidae",
    "Troglodytidae", "Cinclidae", "Muscicapidae", "Ptiliogonatidae", "Laniidae",
    "Calcariidae", "Passeridae", "Estrildidae", "Motacillidae", "Phasianidae"
  ),
  family_common_name = c(
    "Gnatcatchers", "Nightjars", "Ducks, Geese, and Swans", "Pigeons and Doves", 
    "Vireos", "Falcons and Caracaras", "Woodpeckers", "Swallows", "Blackbirds", 
    "Wood-Warblers", "Thrushes", "Chickadees and Titmice", "Crows and Jays", 
    "Cardinals and Allies", "Finches", "New World Sparrows", "Tyrant Flycatchers", 
    "Hummingbirds", "Hawks, Eagles, and Kites", "Owls", "Mockingbirds and Thrashers", 
    "Waxwings", "Kinglets", "Nuthatches", "Treecreepers", "Wrens", "Dippers", 
    "Old World Flycatchers", "Silky-flycatchers", "Shrikes", "Longspurs and Snow Buntings", 
    "Old World Sparrows", "Estrildid Finches", "Wagtails and Pipits", "Pheasants, Grouse, and Allies"
  ),
  stringsAsFactors = FALSE
)

# Function to get family data with error handling and multiple database sources
get_bird_families <- function(species_list) {
  
  # Initialize results dataframe
  family_data <- data.frame(
    species = species_list,
    family = NA_character_,
    order = NA_character_,
    source = NA_character_,
    stringsAsFactors = FALSE
  )
  
  cat("Starting family lookup for", length(species_list), "species...\n")
  
  # Try multiple databases in order of preference
  databases <- c("itis", "ncbi", "col")
  
  for (i in seq_along(species_list)) {
    species_name <- species_list[i]
    clean_name <- gsub("_", " ", species_name)
    
    cat(sprintf("Processing %d/%d: %s\n", i, length(species_list), clean_name))
    
    # Try each database until we get a result
    for (db in databases) {
      tryCatch({
        # Get classification
        classification_result <- classification(clean_name, db = db)
        
        if (!is.null(classification_result[[1]]) && 
            is.data.frame(classification_result[[1]]) && 
            nrow(classification_result[[1]]) > 0) {
          
          class_df <- classification_result[[1]]
          
          # Extract family
          family_row <- class_df[class_df$rank == "family", ]
          if (nrow(family_row) > 0) {
            family_data$family[i] <- family_row$name[1]
          }
          
          # Extract order (useful for broader groupings)
          order_row <- class_df[class_df$rank == "order", ]
          if (nrow(order_row) > 0) {
            family_data$order[i] <- order_row$name[1]
          }
          
          family_data$source[i] <- db
          
          # If we found a family, break out of database loop
          if (!is.na(family_data$family[i])) {
            cat(sprintf("  Found: %s (Family: %s, Source: %s)\n", 
                        clean_name, family_data$family[i], db))
            break
          }
        }
        
      }, error = function(e) {
        cat(sprintf("  Error with %s for %s: %s\n", db, clean_name, e$message))
      })
      
      # Add delay to be respectful to APIs
      Sys.sleep(0.5)
    }
    
    # If no family found after trying all databases
    if (is.na(family_data$family[i])) {
      cat(sprintf("  No family found for %s\n", clean_name))
    }
    
    # Save progress every 10 species
    if (i %% 10 == 0) {
      # Add family common names to temp save
      temp_data <- family_data %>%
        left_join(family_common_names, by = "family")
      saveRDS(temp_data, "data/bird_family_data_temp.rds")
      cat("Progress saved...\n")
    }
  }
  
  # Add family common names
  family_data <- family_data %>%
    left_join(family_common_names, by = "family")
  
  return(family_data)
}

# Main execution
if (!file.exists("data")) {
  dir.create("data", recursive = TRUE)
}

# Load your species list from plot_data
if (file.exists("./tests/plot_data_allBCR.rds")) {
  plot_data <- readRDS("./tests/plot_data_allBCR.rds")
  unique_species <- unique(plot_data$species)
} else {
  # Use a sample if plot_data not available
  unique_species <- c("Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", 
                      "Columba_livia", "Vireo_gilvus", "Falco_sparverius", 
                      "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", 
                      "Geothlypis_trichas")
}

cat("Choose method to get family data:\n")
cat("1. Use taxize with online databases (slower, more complete)\n")
cat("2. Use eBird taxonomy via auk package (faster, bird-specific)\n")
cat("3. Use manual assignments (fastest, limited to common species)\n")

# For automated running, uncomment one of these:

# Method 1: Online databases (most comprehensive but slow)
family_data <- get_bird_families(unique_species)

# Save the family data
saveRDS(family_data, "data/bird_family_data.rds")

