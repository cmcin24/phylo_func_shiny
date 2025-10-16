library(rvest)
library(dplyr)
library(stringr)
library(httr)

# Function to scrape NHPBS Nature Works taxonomic information (family or order)
scrape_nhpbs_taxon <- function(taxon_name, taxon_type = "family") {
  # NHPBS uses lowercase names in URLs
  taxon_lower <- tolower(taxon_name)
  url <- paste0("https://nhpbs.org/wild/", taxon_lower, ".asp")
  
  tryCatch({
    # Check if page exists
    response <- GET(url)
    if (status_code(response) != 200) {
      return(data.frame(
        taxon = taxon_name,
        taxon_type = taxon_type,
        common_name = NA,
        description = NA,
        source = "NHPBS Nature Works",
        url = url,
        page_exists = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    page <- read_html(url)
    
    # Get the common name (usually in the title or first heading)
    raw_common_name <- page %>%
      html_nodes("title") %>%
      html_text() %>%
      str_trim()
    
    if (is.na(raw_common_name) || raw_common_name == "") {
      raw_common_name <- page %>%
        html_nodes("h1, h2") %>%
        html_text() %>%
        .[1] %>%
        str_trim()
    }
    
    # Clean common name: extract only the part after " - " and before " | "
    common_name <- raw_common_name %>%
      # Remove page titles and suffixes
      gsub(" - NatureWorks.*$", "", .) %>%
      gsub(" \\| Wildlife Journal.*$", "", .) %>%
      gsub(" \\| NatureWorks.*$", "", .) %>%
      # Extract part after taxon name if present
      str_extract("(?<= - ).*") %>%
      # If no " - " found, use original but still clean it
      {if(is.na(.)) gsub(" - NatureWorks.*$|\\| Wildlife Journal.*$|\\| NatureWorks.*$", "", raw_common_name) else .} %>%
      str_trim() %>%
      # Capitalize first letter of each word
      str_to_title()
    
    # Get main content paragraphs only
    main_paragraphs <- page %>%
      html_nodes("p") %>%
      html_text() %>%
      str_trim() %>%
      # Filter out navigation, copyright, and very short text
      .[nchar(.) > 50] %>%
      # Remove common footer/header patterns
      .[!str_detect(., regex("^©|^Home|^Explore the Natural World|^This site will soon|themailbox@nhpbs\\.org", ignore_case = TRUE))] %>%
      # Remove classification text patterns
      .[!str_detect(., regex("^Classification|Kingdom:|Phylum:|Class:|Order:|Family:", ignore_case = TRUE))] %>%
      # Keep only descriptive content
      .[str_detect(., regex("\\.|species|habitat|bird|animal", ignore_case = TRUE))]
    
    # Get content from table cells (NHPBS often uses tables for main content)
    table_content <- page %>%
      html_nodes("td") %>%
      html_text() %>%
      str_trim() %>%
      .[nchar(.) > 100] %>%  # Only substantial content
      # Remove navigation and classification text
      .[!str_detect(., regex("^Classification|^Phylum:|^Class:|^Order:|^Family:|Kingdom:|Animalia|Chordata|Aves", ignore_case = TRUE))] %>%
      # Split into sentences and filter
      str_split("\\.\\s+") %>%
      unlist() %>%
      str_trim() %>%
      .[nchar(.) > 30] %>%  # Keep only substantial sentences
      # Remove sentences that contain classification terms
      .[!str_detect(., regex("Kingdom:|Phylum:|Class:|Order:|Family:|Animalia|Chordata", ignore_case = TRUE))] %>%
      paste(collapse = ". ") %>%
      paste0(., ".")
    
    # Combine and clean description
    description <- c(main_paragraphs, table_content) %>%
      paste(collapse = " ") %>%
      # Remove classification patterns that might have slipped through
      gsub("Classification\\s+Kingdom:.*?(?=\\s+[A-Z][a-z]+\\s+[a-z]+|$)", "", ., perl = TRUE) %>%
      gsub("Kingdom:\\s*Animalia\\s*Phylum:\\s*Chordata\\s*Class:\\s*Aves\\s*Order:\\s*\\w+\\s*Family:\\s*\\w+", "", ., ignore.case = TRUE) %>%
      gsub("Kingdom:\\s*\\w+\\s*Phylum:\\s*\\w+\\s*Class:\\s*\\w+\\s*Order:\\s*\\w+", "", ., ignore.case = TRUE) %>%
      # Remove multiple spaces
      gsub("\\s+", " ", .) %>%
      # Remove duplicate sentences
      {
        sentences <- str_split(., "(?<=\\.)\\s+")[[1]]
        unique_sentences <- unique(sentences)
        paste(unique_sentences, collapse = " ")
      } %>%
      str_trim() %>%
      # Trim to 500 characters maximum
      str_trunc(500, side = "right")
    
    return(data.frame(
      taxon = taxon_name,
      taxon_type = taxon_type,
      common_name = ifelse(is.null(common_name) || is.na(common_name) || common_name == "", 
                           taxon_name, common_name),
      description = description,
      source = "NHPBS Nature Works",
      url = url,
      page_exists = TRUE,
      stringsAsFactors = FALSE
    ))
    
  }, error = function(e) {
    message(paste("Error scraping", taxon_name, ":", e$message))
    return(data.frame(
      taxon = taxon_name,
      taxon_type = taxon_type,
      common_name = NA,
      description = NA,
      source = "NHPBS Nature Works",
      url = url,
      page_exists = FALSE,
      error_message = e$message,
      stringsAsFactors = FALSE
    ))
  })
}

# Helper function to extract specific sections from text
extract_text_section <- function(text, section_names) {
  for (section in section_names) {
    pattern <- paste0(section, "[:\\s]+([^\\n]{100,500})")
    match <- str_extract(text, regex(pattern, ignore_case = TRUE))
    if (!is.na(match)) {
      # Clean up the match
      clean_match <- match %>%
        gsub(paste0("^", section, "[:\\s]+"), "", ., ignore.case = TRUE) %>%
        str_trim()
      return(clean_match)
    }
  }
  return(NA)
}

# ===== MAIN EXECUTION =====

family_data <- readRDS("data/species_ecological_avonet_pif.rds")

# Get unique families and orders from your bird data
unique_families <- family_data %>%
  distinct(family) %>%
  pull(family) %>%
  sort()

# Get unique orders
unique_orders <- family_data %>%
  distinct(order) %>%
  pull(order) %>%
  sort()

# If order column doesn't exist, create a lookup table of common orders
common_orders <- c(
  "Anseriformes",      # Ducks, geese, swans
  "Galliformes",       # Chickens, turkeys, quail
  "Gaviiformes",       # Loons
  "Podicipediformes",  # Grebes
  "Procellariiformes", # Albatrosses, petrels
  "Pelecaniformes",    # Pelicans, herons
  "Suliformes",        # Cormorants, gannets
  "Accipitriformes",   # Hawks, eagles
  "Falconiformes",     # Falcons
  "Gruiformes",        # Rails, cranes
  "Charadriiformes",   # Shorebirds, gulls
  "Columbiformes",     # Pigeons, doves
  "Cuculiformes",      # Cuckoos
  "Strigiformes",      # Owls
  "Caprimulgiformes",  # Nightjars
  "Apodiformes",       # Swifts, hummingbirds
  "Coraciiformes",     # Kingfishers
  "Piciformes",        # Woodpeckers
  "Passeriformes"      # Perching birds (largest order)
)

# Use existing orders if available, otherwise use common orders
if (exists("unique_orders") && length(unique_orders) > 0) {
  orders_to_scrape <- unique_orders
} else {
  orders_to_scrape <- common_orders
}

# ===== SCRAPE FAMILIES =====
message("Starting to scrape FAMILY information from NHPBS Nature Works...")
message(paste("Processing", length(unique_families), "families...\n"))

family_info_nhpbs <- lapply(unique_families, function(fam) {
  message(paste("  Scraping family:", fam, "..."))
  Sys.sleep(1.5)  # Be polite to the server
  scrape_nhpbs_taxon(fam, "family")
}) %>%
  bind_rows()

# ===== SCRAPE ORDERS =====
message("\nStarting to scrape ORDER information from NHPBS Nature Works...")
message(paste("Processing", length(orders_to_scrape), "orders...\n"))

order_info_nhpbs <- lapply(orders_to_scrape, function(ord) {
  message(paste("  Scraping order:", ord, "..."))
  Sys.sleep(1.5)  # Be polite to the server
  scrape_nhpbs_taxon(ord, "order")
}) %>%
  bind_rows()

# ===== COMBINE AND SUMMARIZE =====
all_taxon_info <- bind_rows(family_info_nhpbs, order_info_nhpbs)

# Check success rates
successful_families <- family_info_nhpbs %>% filter(page_exists == TRUE)
successful_orders <- order_info_nhpbs %>% filter(page_exists == TRUE)

message("\n===== SCRAPING SUMMARY =====")
message(paste("Families: Successfully scraped", nrow(successful_families), 
              "out of", nrow(family_info_nhpbs)))
message(paste("Orders: Successfully scraped", nrow(successful_orders), 
              "out of", nrow(order_info_nhpbs)))

# Display what was found
message("\n===== SUCCESSFUL FAMILIES =====")
print(successful_families %>% 
        select(taxon, common_name, page_exists) %>%
        arrange(taxon))

message("\n===== SUCCESSFUL ORDERS =====")
print(successful_orders %>% 
        select(taxon, common_name, page_exists) %>%
        arrange(taxon))

# List missing taxa
missing_families <- family_info_nhpbs %>%
  filter(page_exists == FALSE) %>%
  pull(taxon)

missing_orders <- order_info_nhpbs %>%
  filter(page_exists == FALSE) %>%
  pull(taxon)

if (length(missing_families) > 0) {
  message("\n===== FAMILIES NOT FOUND ON NHPBS =====")
  message(paste(missing_families, collapse = ", "))
}

if (length(missing_orders) > 0) {
  message("\n===== ORDERS NOT FOUND ON NHPBS =====")
  message(paste(missing_orders, collapse = ", "))
}

# ===== SAVE RESULTS =====

# Save separate files for families and orders
saveRDS(family_info_nhpbs, "data/bird_family_info_nhpbs.rds")
saveRDS(order_info_nhpbs, "data/bird_order_info_nhpbs.rds")
saveRDS(all_taxon_info, "data/bird_taxon_info_nhpbs.rds")

# Save as CSV for easy inspection
write.csv(family_info_nhpbs, "data/bird_family_info_nhpbs.csv", row.names = FALSE)
write.csv(order_info_nhpbs, "data/bird_order_info_nhpbs.csv", row.names = FALSE)

# Create formatted versions for Shiny app (already trimmed to 500 in main function)
family_info_formatted <- family_info_nhpbs %>%
  filter(page_exists == TRUE) %>%
  mutate(
    # Description is already 500 chars max from scraping
    short_description = description,
    # No need for medium description since we're capping at 500
    medium_description = description
  )

order_info_formatted <- order_info_nhpbs %>%
  filter(page_exists == TRUE) %>%
  mutate(
    short_description = description,
    medium_description = description
  )

saveRDS(family_info_formatted, "data/bird_family_info_formatted.rds")
saveRDS(order_info_formatted, "data/bird_order_info_formatted.rds")

message("\n===== FILES SAVED =====")
message("Family information saved to:")
message("  - data/bird_family_info_nhpbs.rds (raw data)")
message("  - data/bird_family_info_formatted.rds (formatted for app)")
message("  - data/bird_family_info_nhpbs.csv (for inspection)")
message("\nOrder information saved to:")
message("  - data/bird_order_info_nhpbs.rds (raw data)")
message("  - data/bird_order_info_formatted.rds (formatted for app)")
message("  - data/bird_order_info_nhpbs.csv (for inspection)")
message("\nCombined data saved to:")
message("  - data/bird_taxon_info_nhpbs.rds")

# Create a summary report
summary_report <- all_taxon_info %>%
  mutate(
    has_description = !is.na(description) & nchar(description) > 100,
    description_length = nchar(description)
  ) %>%
  group_by(taxon_type, page_exists) %>%
  summarise(
    count = n(),
    avg_description_length = mean(description_length, na.rm = TRUE),
    max_description_length = max(description_length, na.rm = TRUE),
    .groups = "drop"
  )

message("\n===== DETAILED SUMMARY =====")
print(summary_report)