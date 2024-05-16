predict_exit_wells <- function(exit_df) {
  # Data preparation
  exit_df <- exit_df %>%
    rename(depl = depl, topfield = doc_fieldname) %>%
    mutate(capex_per_bbl_nom = as.numeric(capex_per_bbl_nom),
           opex_per_bbl_nom = as.numeric(opex_per_bbl_nom),
           capex_imputed = as.numeric(capex_imputed),
           opex_imputed = as.numeric(opex_imputed))
  
  # Separate top 10 fields from the rest
  top_10_fields <- exit_df %>%
    group_by(doc_field_code) %>%
    summarise(total_prod = sum(doc_prod)) %>%
    arrange(desc(total_prod)) %>%
    dplyr::slice(1:10) %>%
    pull(doc_field_code)
  
  top_10_data <- exit_df %>%
    filter(doc_field_code %in% top_10_fields)
  
  other_data <- exit_df %>%
    filter(!doc_field_code %in% top_10_fields)
  
  # Create separate formulas for top 10 fields and other fields
  top_10_formula <- as.formula(n_well_exit ~ brent + capex_imputed + opex_imputed + depl)
  other_formula <- as.formula(n_well_exit ~ brent + capex_imputed + opex_imputed + depl)
  
  # Train separate random forest models
  top_10_model <- randomForest(top_10_formula, data = top_10_data, importance = TRUE, ntree = 500, mtry = 3)
  other_model <- randomForest(other_formula, data = other_data, importance = TRUE, ntree = 500, mtry = 3)
  
  # Make predictions using the trained models
  exit_df <- exit_df %>%
    mutate(exit_wells_pred = ifelse(doc_field_code %in% top_10_fields,
                                    predict(top_10_model, newdata = exit_df %>% filter(doc_field_code %in% top_10_fields)),
                                    predict(other_model, newdata = exit_df %>% filter(!doc_field_code %in% top_10_fields))))
  
  # Round the predicted number of wells to the nearest integer
  exit_df$exit_wells_pred <- round(exit_df$exit_wells_pred)
  
  # Select the required columns for output
  output_df <- exit_df %>%
    select(doc_field_code, year, exit_wells_pred)
  
  # Convert the output to a data table
  output_dt <- as.data.table(output_df)
  
  return(output_dt)
}