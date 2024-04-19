# Load required packages
library(dplyr)
library(ggplot2)
library(fixest)

# Read the data (assuming the data is in a file named "entry_revised.csv")
entry_df <- read_csv(file.path("data/processed/entry_df_final_revised.csv"))

# Fit the Poisson regression model with field category and Brent interaction
model <- feglm(n_new_wells ~ field_categ:brent + capex_imputed + opex_imputed + depl,
               data = entry_df, family = poisson())

# Predict new wells using the fitted model
entry_df$new_wells_pred <- predict(model, type = "response")

# Calculate mean new wells and mean predicted new wells by field
entry_df <- entry_df %>%
  group_by(doc_field_code) %>%
  mutate(mean_new_wells = mean(n_new_wells),
         mean_new_wells_pred = mean(new_wells_pred)) %>%
  ungroup()

# Calculate the fixed effect (alpha) for each field
entry_df$fixed_effect <- ifelse(entry_df$mean_new_wells_pred > 0,
                                entry_df$mean_new_wells / entry_df$mean_new_wells_pred,
                                0)

# Adjust the predicted new wells using the fixed effect
entry_df$new_wells_pred <- entry_df$new_wells_pred * entry_df$fixed_effect

# Save coefficients
entry_df$brent_hat <- coef(model)["brent"]
entry_df$capex_hat <- coef(model)["capex_imputed"]
entry_df$opex_hat <- coef(model)["opex_imputed"]
entry_df$depl_hat <- coef(model)["depl"]

# Add field category-specific Brent coefficients
for (fc in 1:20) {
  entry_df$brent_hat[entry_df$field_categ == fc] <- entry_df$brent_hat[entry_df$field_categ == fc] +
    coef(model)[paste0(fc, ".field_categ:brent")]
}

# Save output to CSV files
write.csv(entry_df[, c("doc_field_code", "year", "n_new_wells", "new_wells_pred")],
          "data/outputs/new_wells_pred_revised.csv", row.names = FALSE)
write.csv(entry_df[entry_df$year == 2000, c("doc_fieldname", "doc_field_code", "brent_hat",
                                            "capex_hat", "opex_hat", "depl_hat", "fixed_effect")],
          "data/outputs/poisson_regression_coefficients_revised.csv", row.names = FALSE)

# Plot State-level prediction
state_plot_data <- entry_df %>%
  group_by(year) %>%
  summarise(new_wells = sum(n_new_wells),
            new_wells_pred = sum(new_wells_pred),
            brent = mean(brent),
            capex_imputed = mean(capex_imputed),
            opex_imputed = mean(opex_imputed),
            depl = mean(depl))

ggplot(state_plot_data, aes(x = year)) +
  geom_line(aes(y = new_wells), color = "black") +
  geom_line(aes(y = new_wells_pred), color = "red") +
  labs(title = "California", y = "New Wells") +
  theme_minimal()

# Plot Field-level predictions
for (tf in 0:10) {
  field_plot_data <- entry_df %>%
    filter(topfield == tf) %>%
    group_by(year) %>%
    summarise(new_wells = sum(new_wells),
              new_wells_pred = sum(new_wells_pred))
  
  fieldname <- paste0("Field", tf)
  
  ggplot(field_plot_data, aes(x = year)) +
    geom_line(aes(y = new_wells), color = "black") +
    geom_line(aes(y = new_wells_pred), color = "red") +
    labs(title = fieldname, y = "New Wells") +
    theme_minimal()
}