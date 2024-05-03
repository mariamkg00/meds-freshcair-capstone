# Load required packages
library(dplyr)
library(plm)
library(pglm)
library(ggplot2)
library(ggpubr)
library(readr)
library(stringr)

# Set working directory and paths
workDir <- "/Users/emlab/Dropbox/Research/CarbonNeutrality/STATA/"
dataDir <- file.path(workDir, "dataSTATA")
resultsTextDir <- file.path(workDir, "resultsText")
exitGoogleDriveDir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/exit"

# Load data
exit_rule <- read_csv(file.path("data/processed/well_exits_under_rule.csv"))
exit_rule <- exit_rule %>%
  rename(field_well_exits_rule = n_exits_field, year = exit_year) %>%
  filter(year != 1977, year <= 2010) %>%
  mutate(field_well_exits_rule = as.numeric(field_well_exits_rule))

exit_data <- read_csv(file.path("data/processed/well_exits.csv"))
exit_data <- exit_data %>%
  rename(year = exit_year) %>%
  mutate(well_age = year - start_year) %>%
  filter(well_age >= 0)

entry_data <- readRDS('data/processed/entry_revised.rds')
exit_data <- left_join(exit_data, entry_data, by = c("doc_field_code", "year"))
exit_data <- exit_data %>%
  mutate(well_exits = ifelse(is.na(well_exits), 0, well_exits)) %>%
  group_by(doc_field_code, year, exit_scen) %>%
  dplyr::mutate(field_vintage = group_indices(., doc_field_code, start_year)) %>%
  ungroup()

# Field-vintage level model
exit_data_fv <- exit_data %>%
  filter(!is.na(start_year))

levels <- unique(exit_data_fv$exit_scen)
results_fv <- list()

for (wellset in levels) {
  data_subset <- exit_data_fv %>% filter(exit_scen == wellset)
  
  v_noecon <- pglm(well_exits ~ depl + well_age,
                   data = data_subset, index = c("field_vintage", "year"),
                   family = poisson, model = "within")
  
  v_brentavg <- pglm(well_exits ~ brent + opex_imputed + depl + well_age,
                     data = data_subset, index = c("field_vintage", "year"),
                     family = poisson, model = "within")
  
  v_brentflex <- pglm(well_exits ~ field_categ * brent + opex_imputed + depl + well_age,
                      data = data_subset, index = c("field_vintage", "year"),
                      family = poisson, model = "within")
  
  results_fv[[wellset]] <- list(v_noecon = v_noecon, v_brentavg = v_brentavg, v_brentflex = v_brentflex)
}

# Field-level model
exit_data_f <- exit_data %>%
  group_by(doc_field_code, year, exit_scen) %>%
  summarise(field_well_exits = sum(well_exits), .groups = "drop") %>%
  filter(exit_scen %in% levels)

results_f <- list()

for (wellset in levels) {
  data_subset <- exit_data_f %>% filter(exit_scen == wellset)
  
  f_noecon <- pglm(field_well_exits ~ depl,
                   data = data_subset, index = c("doc_field_code", "year"),
                   family = poisson, model = "within")
  
  f_brentavg <- pglm(field_well_exits ~ brent + opex_imputed + depl,
                     data = data_subset, index = c("doc_field_code", "year"),
                     family = poisson, model = "within")
  
  f_brentflex <- pglm(field_well_exits ~ field_categ * brent + opex_imputed + depl,
                      data = data_subset, index = c("doc_field_code", "year"),
                      family = poisson, model = "within")
  
  results_f[[wellset]] <- list(f_noecon = f_noecon, f_brentavg = f_brentavg, f_brentflex = f_brentflex)
}

# Prediction
pred_data <- exit_data_f %>%
  filter(exit_scen == "10y", year <= 2010) %>%
  left_join(exit_rule, by = c("doc_field_code", "year")) %>%
  mutate(field_well_exits_rule = ifelse(is.na(field_well_exits_rule), 0, field_well_exits_rule))

pred_model <- pglm(field_well_exits ~ field_categ * brent + opex_imputed + depl,
                   data = pred_data, index = c("doc_field_code", "year"),
                   family = poisson, model = "within")

pred_data$well_exits_poisson_fitted <- predict(pred_model, type = "link")
pred_data <- pred_data %>%
  mutate(well_exits_poisson = exp(well_exits_poisson_fitted),
         mean_well_exits = ave(field_well_exits, doc_field_code, FUN = mean),
         mean_well_exits_poisson = ave(well_exits_poisson, doc_field_code, FUN = mean),
         well_exits_exp_alpha = ifelse(mean_well_exits_poisson > 0, mean_well_exits / mean_well_exits_poisson, NA),
         well_exits_pred = ifelse(mean_well_exits_poisson > 0, well_exits_poisson * well_exits_exp_alpha, well_exits_poisson)) %>%
  select(-well_exits_poisson_fitted, -mean_well_exits, -mean_well_exits_poisson)

field_categ_label <- c("Belridge  South", "Midway-Sunset", "Kern River", "Cymric", "Wilmington",
                       "Lost Hills", "San Ardo", "Elk Hills", "Coalinga", "Poso Creek",
                       paste0("Non-top Q", 11:20))
names(field_categ_label) <- 1:20
pred_data$field_categ <- field_categ_label[as.character(pred_data$field_categ)]

# Save coefficients
pred_data <- pred_data %>%
  mutate(brent_hat = coef(pred_model)["brent"],
         opex_hat = coef(pred_model)["opex_imputed"],
         depl_hat = coef(pred_model)["depl"],
         fixed_effect = well_exits_exp_alpha) %>%
  rename(well_exits = field_well_exits)

for (fc in 1:20) {
  pred_data$brent_hat[pred_data$field_categ == field_categ_label[fc]] <-
    pred_data$brent_hat[pred_data$field_categ == field_categ_label[fc]] +
    coef(pred_model)[paste0(fc, ".field_categ:brent")]
}

# # Save output
# write_csv(pred_data %>% select(doc_field_code, year, well_exits, well_exits_pred),
#           file.path(resultsTextDir, "well_exits_pred.csv"))

# write_csv(pred_data %>%
#             filter(year == 2000) %>%
#             select(doc_fieldname, doc_field_code, brent_hat, opex_hat, depl_hat, fixed_effect),
#           file.path(resultsTextDir, "exit_regression_coefficients.csv"))

write_csv(pred_data %>% select(doc_field_code, year, well_exits, well_exits_pred),
          file.path("data/processed/well_exits_pred.csv"))

# write_csv(pred_data %>%
#             filter(year == 2000) %>%
#             select(doc_fieldname, doc_field_code, brent_hat, opex_hat, depl_hat, fixed_effect),
#           file.path(exitGoogleDriveDir, "exit_regression_coefficients.csv"))

# Plots
pred_state <- pred_data %>%
  group_by(year) %>%
  summarise(field_well_exits = sum(well_exits),
            well_exits_pred = sum(well_exits_pred),
            field_well_exits_rule = sum(field_well_exits_rule))

ggplot(pred_state, aes(x = year)) +
  geom_line(aes(y = field_well_exits)) +
  geom_line(aes(y = well_exits_pred), color = "red") +
  labs(title = "California", y = "Well Exits") +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010)) +
  theme_minimal()

field_plots <- list()

for (fc in 1:20) {
  field_name <- field_categ_label[fc]
  
  field_data <- pred_data %>%
    filter(field_categ == field_name) %>%
    group_by(year) %>%
    summarise(field_well_exits = sum(well_exits),
              well_exits_pred = sum(well_exits_pred),
              field_well_exits_rule = sum(field_well_exits_rule))
  
  field_plots[[fc]] <- ggplot(field_data, aes(x = year)) +
    geom_line(aes(y = field_well_exits)) +
    geom_line(aes(y = well_exits_pred), color = "red") +
    labs(title = field_name, y = "Well Exits") +
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010)) +
    theme_minimal()
}

combined_plot <- ggarrange(plotlist = field_plots, ncol = 4, nrow = 5)
# ggsave(file.path(workDir, "docDir", "predictions", "exit", "pred_210701.pdf"), combined_plot)