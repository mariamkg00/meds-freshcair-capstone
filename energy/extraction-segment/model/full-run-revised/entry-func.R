predict_new_wells <- function(entry_df) {
  # Data preparation
  entry_df <- entry_df %>%
    filter(!str_detect(doc_fieldname, "Gas") & year != 1977) %>%
    mutate(capex_imputed = as.numeric(capex_imputed),
           opex_imputed = as.numeric(opex_imputed))
  
  # Separate top 10 fields from the rest
  top_10_fields <- entry_df %>%
    group_by(doc_field_code) %>%
    summarise(total_prod = sum(total_bbls, na.rm = TRUE)) %>%
    arrange(desc(total_prod)) %>%
    dplyr::slice(1:10) %>%
    pull(doc_field_code)
  
  top_10_data <- entry_df %>%
    filter(doc_field_code %in% top_10_fields)
  
  other_data <- entry_df %>%
    filter(!doc_field_code %in% top_10_fields)
  
  # Create separate formulas for top 10 fields and other fields
  top_10_formula <- as.formula(m_new_wells_pred ~ oil_price_usd_per_bbl + capex_imputed + opex_imputed + depl)
  other_formula <- as.formula(m_new_wells_pred ~ oil_price_usd_per_bbl + capex_imputed + opex_imputed + depl)
  
  # Train separate random forest models
  top_10_model <- randomForest(top_10_formula, data = top_10_data, importance = TRUE, ntree = 500, mtry = 3)
  other_model <- randomForest(other_formula, data = other_data, importance = TRUE, ntree = 500, mtry = 3)
  
  # Make predictions using the trained models
  entry_df <- entry_df %>%
    mutate(new_wells_pred = ifelse(doc_field_code %in% top_10_fields,
                                   predict(top_10_model, newdata = entry_df %>% filter(doc_field_code %in% top_10_fields)),
                                   predict(other_model, newdata = entry_df %>% filter(!doc_field_code %in% top_10_fields))))
  
  # Round the predicted number of wells to the nearest integer
  entry_df$new_wells_pred <- round(entry_df$new_wells_pred)
  
  # Select the required columns for output
  output_df <- entry_df %>%
    select(doc_field_code, year, new_wells_pred)
  
  # Convert the output to a data table
  output_dt <- as.data.table(output_df)
  
  return(output_dt)
}