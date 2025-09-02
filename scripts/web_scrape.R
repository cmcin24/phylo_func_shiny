# Bird Data Collection Script
# Run this script once to gather and save bird information

library(httr)
library(jsonlite)
library(dplyr)
library(rvest) 
library(stringr)

data <- load("data/processed/model_objects.rda")

species_list <- unique(mod_data$sp_latin)

# Your species list
#species_list <- c("Polioptila_caerulea", "Chordeiles_minor", "Anas_platyrhynchos", 
#                  "Columba_livia", "Vireo_gilvus", "Falco_sparverius", 
#                  "Dryocopus_pileatus", "Hirundo_rustica", "Sturnella_neglecta", 
#                  "Geothlypis_trichas")

# Function to get data from iNaturalist API
get_inat_data <- function(scientific_name) {
  clean_name <- gsub("_", " ", scientific_name)
  
  cat("Fetching data for:", clean_name, "\n")
  
  tryCatch({
    # iNaturalist API call
    url <- paste0("https://api.inaturalist.org/v1/taxa?q=", 
                  URLencode(clean_name), "&rank=species")
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      if (length(data$results) > 0) {
        result <- data$results[1, ]
        
        return(data.frame(
          scientific_name = scientific_name,
          scientific_name_display = clean_name,
          common_name = ifelse(is.null(result$preferred_common_name), 
                               "Unknown", result$preferred_common_name),
          family = ifelse(is.null(result$ancestor_ids) || length(result$ancestors) == 0, 
                          "Unknown", "Unknown"), # Will try to get this from other sources
          wikipedia_url = ifelse(is.null(result$wikipedia_url), 
                                 NA, result$wikipedia_url),
          photo_url = ifelse(is.null(result$default_photo$medium_url), 
                             NA, result$default_photo$medium_url),
          conservation_status_inat = ifelse(is.null(result$conservation_status$status), 
                                            "Unknown", result$conservation_status$status),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Return empty row if no data found
    return(data.frame(
      scientific_name = scientific_name,
      scientific_name_display = clean_name,
      common_name = "Unknown",
      family = "Unknown",
      wikipedia_url = NA,
      photo_url = NA,
      conservation_status_inat = "Unknown",
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    cat("Error fetching data for", clean_name, ":", e$message, "\n")
    return(data.frame(
      scientific_name = scientific_name,
      scientific_name_display = clean_name,
      common_name = "Error",
      family = "Unknown",
      wikipedia_url = NA,
      photo_url = NA,
      conservation_status_inat = "Unknown",
      stringsAsFactors = FALSE
    ))
  })
}

# Function to get bird description from All About Birds
get_allaboutbirds_description <- function(common_name, scientific_name) {
  tryCatch({
    # Create URL-friendly version of the bird name - PRESERVE HYPHENS
    url_name <- common_name %>%
      str_replace_all("[^A-Za-z0-9\\s\\-]", "") %>%  # Keep hyphens!
      str_replace_all("\\s+", "_") %>%
      str_to_title()
    
    url <- paste0("https://www.allaboutbirds.org/guide/", url_name, "/overview")
    
    cat("Fetching:", url, "\n")
    
    # Add delay to be respectful
    Sys.sleep(1)
    
    # Try to read the page
    page <- read_html(url)
    
    # Try different selectors for the description
    description <- NULL
    
    # Method 1: Look for Basic Description heading and get the next paragraph
    basic_desc_selectors <- c(
      "h3:contains('Basic Description') + p",              # Direct next paragraph
      "h3:contains('Basic Description') ~ p:first-of-type", # First following paragraph
      "h4:contains('Basic Description') + p",              # Alternative heading level
      "h4:contains('Basic Description') ~ p:first-of-type"
    )
    
    for (selector in basic_desc_selectors) {
      tryCatch({
        desc_node <- page %>% html_node(selector)
        if (!is.null(desc_node)) {
          text <- html_text(desc_node) %>% str_trim()
          if (nchar(text) > 50) {
            description <- text
            cat("Found Basic Description using selector:", selector, "\n")
            break
          }
        }
      }, error = function(e) {
        # Continue to next selector if this one fails
      })
    }
    
    # Method 2: If no direct Basic Description found, look for section with that heading
    if (is.null(description)) {
      # Find all headings and paragraphs
      all_elements <- page %>% html_nodes("h1, h2, h3, h4, h5, h6, p")
      
      for (i in 1:length(all_elements)) {
        element_text <- html_text(all_elements[i]) %>% str_trim()
        
        # Check if this element contains "Basic Description"
        if (str_detect(element_text, "Basic Description")) {
          # Look for the next paragraph element
          for (j in (i+1):length(all_elements)) {
            next_element <- all_elements[j]
            if (html_name(next_element) == "p") {
              text <- html_text(next_element) %>% str_trim()
              if (nchar(text) > 50) {
                description <- text
                cat("Found Basic Description via heading search\n")
                break
              }
            }
            # Stop if we hit another heading
            if (html_name(next_element) %in% c("h1", "h2", "h3", "h4", "h5", "h6")) {
              break
            }
          }
          if (!is.null(description)) break
        }
      }
    }
    
    # Clean up the description - ENSURE description is never NULL
    if (!is.null(description)) {
      description <- description %>%
        str_squish() %>%                    # Remove extra whitespace
        str_replace_all("\\n", " ") %>%     # Replace newlines with spaces
        str_sub(1, 500)                     # Limit to 500 characters to avoid copyright issues
      
      if (nchar(description) == 500) {
        # If we hit the limit, try to end at a complete sentence
        last_period <- str_locate_all(description, "\\.")[[1]]
        if (nrow(last_period) > 0) {
          last_complete <- max(last_period[, "end"])
          if (last_complete > 200) {  # Only if we have a substantial amount
            description <- str_sub(description, 1, last_complete)
          }
        }
      }
    } else {
      # FIX: Ensure description is never NULL - assign default value
      description <- "No description available."
    }
    
    return(data.frame(
      url = url,
      description = description,  # Now guaranteed to be non-NULL
      success = !is.na(description) && description != "No description available.",
      scientific_name = scientific_name,
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    cat("Error for", common_name, ":", e$message, "\n")
    return(data.frame(
      url = paste0("https://www.allaboutbirds.org/guide/", 
                   str_replace_all(str_to_title(common_name), "\\s+", "_"), "/overview"),
      description = "Error retrieving description.",
      success = FALSE,
      scientific_name = scientific_name,
      stringsAsFactors = FALSE
    ))
  })
}

# Collect all data
cat("Starting data collection...\n")
cat("This may take a few minutes due to API rate limits.\n\n")

# Get iNaturalist data
inat_data_list <- list()
for (i in 1:length(species_list)) {
  inat_data_list[[i]] <- get_inat_data(species_list[i])
  
  # Be respectful to APIs - wait between requests
  Sys.sleep(1)
}
inat_data <- do.call(rbind, inat_data_list)

# Get All About Birds descriptions
abb_description_list <- list()
for (i in 1:nrow(inat_data)) {
  abb_description_list[[i]] <- get_allaboutbirds_description(
    inat_data$common_name[i], 
    inat_data$scientific_name[i]
  )
  
  # Be respectful to APIs - wait between requests
  Sys.sleep(1)
}
abb_data <- do.call(rbind, abb_description_list)

# Combine all data sources
final_bird_data <- inat_data %>%
  left_join(abb_data, by = "scientific_name") %>%
  mutate(
    # Create URLs for external resources
    allaboutbirds_url = paste0("https://www.allaboutbirds.org/guide/", 
                               gsub(" ", "_", str_to_title(common_name)), "/overview"),
    # Clean up data
    common_name = ifelse(common_name == "Unknown" | is.na(common_name), 
                         paste(toupper(substring(gsub("_", " ", scientific_name_display), 1, 1)),
                               substring(gsub("_", " ", scientific_name_display), 2), sep=""), 
                         common_name),
    # Ensure description is clean
    description = ifelse(is.na(description), 
                         "No description available.", 
                         description)
  ) %>%
  select(scientific_name, scientific_name_display, common_name, photo_url, url, description)

# Save the data
output_file <- "data/bird_species_info.rds"

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}
saveRDS(final_bird_data, output_file)

# Also save as CSV for easy viewing/editing
write.csv(final_bird_data, "data/bird_species_info.csv", row.names = FALSE)

cat("\nData collection complete!\n")
cat("Files saved:\n")
cat("- ", output_file, "\n")
cat("- data/bird_species_info.csv\n\n")

# Display summary
cat("Summary of collected data:\n")
print(final_bird_data[, c("common_name", "scientific_name_display")])

# Show a sample of descriptions
cat("\nSample descriptions:\n")
for(i in 1:min(3, nrow(final_bird_data))) {
  cat("\n", final_bird_data$common_name[i], ":\n")
  desc_preview <- if(nchar(final_bird_data$description[i]) > 100) {
    paste0(str_sub(final_bird_data$description[i], 1, 100), "...")
  } else {
    final_bird_data$description[i]
  }
  cat(desc_preview, "\n")
}

cat("\nYou can now use this data in your Shiny app!\n")