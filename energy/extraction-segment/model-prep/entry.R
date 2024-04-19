# Calepa Carbon Neutrality
# Set up data to estimate entry model parameters
#
# Ruiwen Lee
# Created:   7 Aug 2020
# Modified: 12 Sep 2020
# Modified (for paper): 8 Jun 2021
# Translated to R 4/16/24 - MP

# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(fixest)
library(readxl)
library(stargazer)
library(broom)

# # Set working directory and paths
# workDir <- "/Users/rui/Dropbox/ca-transport-supply-decarb/STATA/" # RL's macbook
# # workDir <- "/Users/emlab/Dropbox/Research/CarbonNeutrality/STATA/" # emLab macbook
# googleDriveDir <- "/Volumes/GoogleDrive-107971834908030063679/Shared drives/emlab/projects/current-projects/calepa-cn" # on RL's macbook


setwd('/capstone/freshcair/meds-freshcair-capstone')

# Read the data
entry_df <- read_csv(file.path("data/processed/entry_df_final_revised.csv"))

# Calculating missing columns that are used in analysis ------
# m_cumsum_div_my_prod
entry_df <- entry_df %>%
  group_by(doc_field_code) %>%
  mutate(m_cumsum_div_my_prod = cumsum(doc_prod) / max(doc_prod)) %>%
  ungroup()

# totex_capex
entry_df <- entry_df %>%
  mutate(totex_capex = capex_imputed + opex_imputed)

# wm_capex_imputed, wm_opex_imputed, wm_totex
entry_df <- entry_df %>%
  group_by(doc_field_code) %>%
  mutate(wm_capex_imputed = weighted.mean(capex_imputed, doc_prod, na.rm = TRUE),
         wm_opex_imputed = weighted.mean(opex_imputed, doc_prod, na.rm = TRUE),
         wm_totex = wm_capex_imputed + wm_opex_imputed) %>%
  ungroup()

# Data preparation
entry_df <- entry_df %>%
  filter(!grepl("Gas", doc_fieldname) & year != 1977) %>%
  rename(depl = m_cumsum_div_my_prod,
         topfield = top_field) %>%
  mutate(capex_per_bbl_nom = as.numeric(capex_per_bbl_nom),
         opex_per_bbl_nom = as.numeric(opex_per_bbl_nom),
         capex_imputed = as.numeric(capex_imputed),
         opex_imputed = as.numeric(opex_imputed))

# Create field rank and categories
entry_df <- entry_df %>%
  group_by(year) %>%
  mutate(rank = dense_rank(desc(doc_prod))) %>%
  ungroup() %>%
  group_by(doc_field_code) %>%
  mutate(field_rank = max(rank)) %>%
  ungroup() %>%
  mutate(nontop_field_categ = ifelse(topfield == 0 & year == 2019, ntile(-field_rank, 10) + 10 + 1, NA)) %>%
  group_by(doc_field_code) %>%
  mutate(field_categ = max(nontop_field_categ, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(field_categ = ifelse(topfield > 0, topfield, field_categ))

# # Convert variables to real price (for revisions to paper 11/29/2022) -- not needed!
# cpi <- read_excel(file.path(googleDriveDir, "data/stocks-flows/raw/BLS-CPI-U.xlsx"), sheet = "Annual")
# entry_df <- entry_df %>%
#   left_join(cpi, by = "year") %>%
#   mutate(brent_2019 = brent / cpi * cpi2019,
#          capex_imputed_2019 = capex_imputed / cpi * cpi2019,
#          opex_imputed_2019 = opex_imputed / cpi * cpi2019)

# Plots to look at data
# Histograms
ggplot(entry_df %>% filter(n_new_wells > 0), aes(x = n_new_wells)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "steelblue", color = "white") +
  labs(title = "Histogram of New Wells (Non-zero Values)", x = "Number of New Wells", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, max(entry_df$n_new_wells), by = 5))
ggsave(file.path(docDir, "histograms/new_well.pdf"))

# Create a new variable 'n_new_wells_cat' that categorizes the number of new wells -- new
entry_df_grouped <- entry_df %>%
  mutate(n_new_wells_cat = case_when(
    n_new_wells == 0 ~ "0",
    n_new_wells >= 1 & n_new_wells <= 5 ~ "1-5",
    n_new_wells >= 6 & n_new_wells <= 10 ~ "6-10",
    n_new_wells >= 11 & n_new_wells <= 20 ~ "11-20",
    n_new_wells > 20 ~ ">20"
  ))

# Convert 'n_new_wells_cat' to a factor with the desired order of levels -- new
entry_df_grouped$n_new_wells_cat <- factor(entry_df$n_new_wells_cat, levels = c("0", "1-5", "6-10", "11-20", ">20"))

# Calculate the proportion of field-year observations in each category -- new
prop_new_wells <- entry_df_grouped %>%
  group_by(n_new_wells_cat) %>%
  summarise(prop = n() / nrow(entry_df)) %>%
  mutate(prop_pct = prop * 100)

# Create the bar plot -- new
ggplot(prop_new_wells, aes(x = n_new_wells_cat, y = prop_pct)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  labs(title = "Proportion of Field-Year Observations by Number of New Wells",
       x = "Number of New Wells", y = "Proportion (%)") +
  geom_text(aes(label = sprintf("%.1f%%", prop_pct)), vjust = -0.5, size = 3) +
  ylim(0, max(prop_new_wells$prop_pct) * 1.1)  # Set y-axis limit for better visualization

# Time series plots
entry_df_summary <- entry_df %>%
  group_by(year) %>%
  summarise(brent = first(brent),
            new_well_yearsum = sum(n_new_wells),
            new_prod_yearsum = sum(new_prod),
            doc_prod_yearsum = sum(doc_prod),
            capex_yearmean = mean(capex_imputed),
            opex_yearmean = mean(opex_imputed),
            totex_capex_yearmean = mean(totex_capex),
            # wellcost_yearmean = mean(wellcost_imputed),
            wm_capex_yearmean = mean(wm_capex_imputed),
            wm_opex_yearmean = mean(wm_opex_imputed),
            wm_totex_yearmean = mean(wm_totex))

ggplot(entry_df_summary, aes(x = year)) +
  geom_line(aes(y = new_well_yearsum), color = "steelblue") +
  geom_line(aes(y = brent), color = "black", linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Brent Price")) +
  labs(title = "Number of New Wells (sum of fields)", x = "Year", y = "Number of New Wells")
ggsave(file.path(docDir, "timeseries/new_well.pdf"))

# Poisson models
# Without field fixed effects
models_nofe <- list()
for (capvar in c("capex")) {
  for (var in c("n_new_wells", "new_prod")) {
    formula_base <- as.formula(paste0(var, " ~ brent + capex_imputed + opex_imputed + depl"))
    models_nofe[[paste0(capvar, "_", var)]] <- feglm(formula_base, data = entry_df, family = poisson)
    
    formula_tot <- as.formula(paste0(var, " ~ brent + totex_capex + depl"))
    models_nofe[[paste0(capvar, "_", var, "_tot")]] <- feglm(formula_tot, data = entry_df, family = poisson)
  }
}

# # Output results
# for (var in c("n_new_wells", "new_prod")) {
#   stargazer(models_nofe[[paste0("capex_", var)]], models_nofe[[paste0("capex_", var, "_tot")]],
#             type = "latex", out = file.path(texDir, paste0("poisson_", var, ".tex")),
#             keep = c("brent", "capex_imputed", "opex_imputed", "totex_capex", "depl"), 
#             order = c("brent", "capex_imputed", "opex_imputed", "totex_capex", "depl"),
#             covariate.labels = c("Brent", "Capex", "Opex", "Totex", "Depletion"),
#             omit.stat = c("LL", "ser", "f"),
#             add.lines = list(c("Field FEs", "N", "N", "N")),
#             star.cutoffs = c(0.1, 0.05, 0.01),
#             notes = "Standard errors clustered at field level are in parentheses.",
#             no.space = TRUE)
# }

# Output result - testing
for (var in c("n_new_wells", "new_prod")) {
  # Create the file path for the LaTeX output
  file_path <- paste0("outputs/poisson_", var, ".tex")
  
  # Create the LaTeX table using etable()
  etable(
    models_nofe[[paste0("capex_", var)]],
    models_nofe[[paste0("capex_", var, "_tot")]],
    file = file_path,
    tex = TRUE,
    keep = c("brent", "capex_imputed", "opex_imputed", "totex_capex", "depl"),
    order = c("brent", "capex_imputed", "opex_imputed", "totex_capex", "depl"),
    coefstat = "se",
    title = paste0("Poisson Models for ", var),
    notes = "Standard errors clustered at field level are in parentheses.",
    fitstat = c("n", "bic"),
    digits = 3
  )
  
  # Calculate and print pseudo R-squared separately
  cat("\nPseudo R-squared for", paste0("capex_", var), "model:", r2(models_nofe[[paste0("capex_", var)]], type = "pr2"), "\n")
  cat("Pseudo R-squared for", paste0("capex_", var, "_tot"), "model:", r2(models_nofe[[paste0("capex_", var, "_tot")]], type = "pr2"), "\n")
}

# With field fixed effects
models_fe <- list()
for (var in c("n_new_wells", "new_prod")) {
  formula_base <- as.formula(paste0(var, " ~ brent + capex_imputed + opex_imputed + depl"))
  models_fe[[paste0("fe_", var)]] <- feglm(formula_base, data = entry_df, family = poisson, 
                                           fixef = "doc_field_code")
  
  formula_tot <- as.formula(paste0(var, " ~ brent + totex_capex + depl"))
  models_fe[[paste0("fe_", var, "_tot")]] <- feglm(formula_tot, data = entry_df, family = poisson, 
                                                   fixef = "doc_field_code")
  
  formula_api <- as.formula(paste0(var, " ~ brent + wm_api_gravity + capex_imputed + opex_imputed + depl"))
  models_fe[[paste0("fe_", var, "_api")]] <- feglm(formula_api, data = entry_df, family = poisson, 
                                                   fixef = "doc_field_code")
  
  formula_apiX <- as.formula(paste0(var, " ~ brent * wm_api_gravity + capex_imputed + opex_imputed + depl"))
  models_fe[[paste0("fe_", var, "_apiX")]] <- feglm(formula_apiX, data = entry_df, family = poisson, 
                                                    fixef = "doc_field_code")
  
  formula_tot_api <- as.formula(paste0(var, " ~ brent + wm_api_gravity + totex_capex + depl"))
  models_fe[[paste0("fe_", var, "_tot_api")]] <- feglm(formula_tot_api, data = entry_df, family = poisson, 
                                                       fixef = "doc_field_code")
  
  formula_tot_apiX <- as.formula(paste0(var, " ~ brent * wm_api_gravity + totex_capex + depl"))
  models_fe[[paste0("fe_", var, "_tot_apiX")]] <- feglm(formula_tot_apiX, data = entry_df, family = poisson, 
                                                        fixef = "doc_field_code")
}

# Output results
for (var in c("new_prod", "new_wells")) {
  stargazer(models_fe[[paste0("fe_", var)]], models_fe[[paste0("fe_", var, "_tot")]],
            models_fe[[paste0("fe_", var, "_api")]], models_fe[[paste0("fe_", var, "_apiX")]],
            models_fe[[paste0("fe_", var, "_tot_api")]], models_fe[[paste0("fe_", var, "_tot_apiX")]],
            type = "latex", out = file.path(texDir, paste0("poisson_fe_", var, ".tex")),
            keep = c("brent", "capex_imputed", "opex_imputed", "totex_capex", "depl", "wm_api_gravity"), 
            order = c("brent", "capex_imputed", "opex_imputed", "totex_capex", "depl", "wm_api_gravity"),
            covariate.labels = c("Brent", "Capex", "Opex", "Totex", "Depletion", "API Gravity"),
            omit.stat = c("LL", "ser", "f"),
            add.lines = list(c("Field FEs", "Y", "Y", "Y", "Y", "Y", "Y")),
            star.cutoffs = c(0.1, 0.05, 0.01),
            notes = "Standard errors clustered at field level are in parentheses.",
            no.space = TRUE)
}