# Plot estimated trends for a species in a supplied set
# of BCRs
plot_sp_trends = function(model, 
                          data = mod_data,
                          species, 
                          regions,
                          type = 'response',
                          median_records = TRUE){
  
  # Take the first 9 regions in which this species was recorded
  # for default plotting
  if(missing(regions)){
    regions <- as.character((data %>%
                               dplyr::filter(sp_latin == !!species) %>%
                               dplyr::select(strata_name) %>%
                               dplyr::distinct() %>%
                               dplyr::pull(strata_name)))
    if(length(regions) > 9){
      regions <- regions[1:9]
    }
  }
  
  type <- match.arg(type, 
                    choices = c('expected', 'response'))
  
  get_offset <- function(model) {
    nm1 <- names(attributes(model$terms)$dataClasses)
    if('(offset)' %in% nm1) {
      deparse(as.list(model$call)$offset)
    } else {
      
      sub("offset\\((.*)\\)$", "\\1", grep('offset', nm1, value = TRUE))
    }
  }
  
  offset_name <- get_offset(model)
  if(!length(offset_name)){
    offset_name <- 'n_records'
  }
  
  # Use marginaleffects to create the prediction grid; if median_records = TRUE,
  # we want to ask the model what trends it would have expected if the same 
  # number of routes were taken in each region during each year. This gives a 
  # more useful 'relative' trend estimate
  if(median_records){
    newdata <- datagrid(model = model, 
                        year = unique(data$year),
                        sp_latin = species,
                        strata_name = regions) %>%
      dplyr::mutate(sp_latin_phy = species,
                    sp_latin_func = species,
                    sp_strata_name = paste0(sp_latin, '.', strata_name)) %>%
      #dplyr::select(-count_sc, -{{offset_name}}) %>%
      dplyr::select(-{{offset_name}}) %>%
      dplyr::left_join(data) %>%
      dplyr::group_by(sp_latin, strata_name) %>%
      dplyr::mutate(!!offset_name := median(!!rlang::sym(offset_name), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(strata_name, year) %>%
      dplyr::filter(!is.na(count))
  } else {
    # If median_records = FALSE, make predictions using the actual
    # recorded number of routes
    newdata <- datagrid(model = model, 
                        year = unique(data$year),
                        sp_latin = species,
                        strata_name = regions) %>%
      dplyr::mutate(sp_latin_phy = species,
                    sp_latin_func = species,
                    sp_strata_name = paste0(sp_latin, '.', strata_name)) %>%
      #dplyr::select(-count_sc, -{{offset_name}}) %>%
      dplyr::select(-{{offset_name}}) %>%
      dplyr::left_join(data) %>%
      dplyr::group_by(strata_name) %>%
      dplyr::mutate(!!offset_name := ifelse(is.na(!!rlang::sym(offset_name)),
                                            median(!!rlang::sym(offset_name), na.rm = TRUE),
                                            !!rlang::sym(offset_name))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na({{offset_name}})) %>%
      dplyr::arrange(strata_name, year) %>%
      dplyr::filter(!is.na(count))
  }
  
  # Calculate prediction intervals
  preds <- pred_intervals(model, 
                          newdata = newdata, 
                          type = type)
  newdata$pred <- preds$mean
  newdata$upper <- preds$upper
  newdata$lower <- preds$lower
  
  # # Make ggplot objects and modify accordingly
  # p <- ggplot(newdata, aes(x = year, y = pred)) + 
  #   geom_line(linewidth = 1, alpha = 0.6) +
  #   geom_ribbon(aes(ymin = lower, ymax = upper),
  #               alpha = 0.3, col = NA) +
  #   facet_wrap(~strata_name, scales = 'free') +
  #   theme_classic()
  # 
  # if(!median_records){
  #   p <- p + geom_point(aes(y = count_sc)) +
  #     labs(y = paste0('Predictions for ', 
  #                     gsub('_', ' ', species)),
  #          x = 'Year')
  # } else {
  #   p <- p + labs(y = paste0('Expected trend for ', 
  #                            gsub('_', ' ', species)),
  #                 x = 'Year')
  # }
  # p
  
  # Prepare data for parquet export
  export_data <- newdata %>%
    select(species = sp_latin, 
           region = strata_name,
           year, 
           pred, 
           lower, 
           upper,
           offset_value = !!rlang::sym(offset_name)) %>%
    # Convert to factors to save space
    mutate(species = as.factor(species),
           region = as.factor(region),
           # Add metadata about the prediction type
           prediction_type = ifelse(median_records, "standardized", "actual_effort"),
           model_type = type) %>%
    # Ensure proper ordering for faster filtering
    arrange(species, region, year)
  
  return(export_data)
  

}



# 
# 
# 
# # Prepare data for parquet export
# export_data <- newdata %>%
#   select(species = sp_latin, 
#          region = strata_name,
#          year, 
#          pred, 
#          lower, 
#          upper,
#          offset_value = !!rlang::sym(offset_name)) %>%
#   # Convert to factors to save space
#   mutate(species = as.factor(species),
#          region = as.factor(region),
#          # Add metadata about the prediction type
#          prediction_type = ifelse(median_records, "standardized", "actual_effort"),
#          model_type = type) %>%
#   # Ensure proper ordering for faster filtering
#   arrange(species, region, year)
# 
# # Create filename with species name and timestamp
# safe_species_name <- gsub("[^A-Za-z0-9_]", "_", species)
# filename <- paste0("predictions_", safe_species_name, "_", 
#                    format(Sys.time(), "%Y%m%d_%H%M%S"), ".parquet")
# 
# # Save to parquet
# if(requireNamespace("arrow", quietly = TRUE)) {
#   arrow::write_parquet(export_data, filename)
#   message("Data saved to: ", filename)
# } else {
#   warning("arrow package not available. Install with: install.packages('arrow')")
# }
# 
# # Optionally, also save a master file that accumulates all species
# master_file <- "all_species_predictions.parquet"
# if(file.exists(master_file)) {
#   # Append to existing data
#   existing_data <- arrow::read_parquet(master_file)
#   combined_data <- dplyr::bind_rows(existing_data, export_data) %>%
#     # Remove duplicates in case of re-runs
#     distinct(species, region, year, prediction_type, model_type, .keep_all = TRUE) %>%
#     arrange(species, region, year)
#   arrow::write_parquet(combined_data, master_file)
# } else {
#   # Create new master file
#   arrow::write_parquet(export_data, master_file)
# }
# 
