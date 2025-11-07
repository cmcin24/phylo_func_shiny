library(sf)
library(dplyr)
library(stringr)


bcr_raw <- readRDS("./data/processed/regions_sf.rds")

bcr_raw <- bcr_raw %>%
  st_make_valid()


# Clean and merge the BCR regions
bcr_clean <- bcr_raw %>%
  # Assuming your BCR column is named something like "BCR", "BCR_NAME", or "BCRNAME"
  # Adjust the column name as needed
  mutate(
    # Extract just the number part from BCR names
    BCR_number = case_when(
      
      # Handle formats like "3N", "3S" 
      str_detect(bcr_label, "^\\d+[NS]$") ~ 
        str_extract(bcr_label, "\\d+"),
      
      # If it's just a number already
      str_detect(bcr_label, "^\\d+$") ~ bcr_label,
      
      # Default: try to extract any number
      TRUE ~ str_extract(bcr_label, "\\d+")
    ),
    
    # Create clean BCR name
    BCR_clean = paste0("BCR", BCR_number)
    
  ) %>%
  # Remove any rows where we couldn't extract a BCR number
  filter(!is.na(BCR_number)) %>%
  # Group by the BCR number and merge geometries
  group_by(BCR_number, BCR_clean) %>%
  summarise(
    # Merge the geometries for split regions
    geometry = st_union(SHAPE),
    # Keep any other attributes you want (e.g., area, name)
    .groups = 'drop'
  ) %>%
  # Convert BCR_number to numeric for proper sorting
  mutate(BCR_number = as.numeric(BCR_number)) %>%
  # Sort by BCR number
  arrange(BCR_number)

bcr_simple <- st_simplify(bcr_clean, dTolerance = 1000)  # Adjust tolerance

# Save the cleaned data
saveRDS(bcr_simple, "./data/processed/bcr_simplified.rds")

    
    
