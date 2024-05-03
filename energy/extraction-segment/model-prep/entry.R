# Calepa Carbon Neutrality
# Set up data to estimate entry model parameters

# Ruiwen Lee
# Created:   7 Aug 2020
# Modified: 12 Sep 2020
# Modified (for paper): 8 Jun 2021
# Updated 4/25/24 - MP

# Initialization
rm(list = ls())
options(max.print = 99999)

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(fixest)
# library(modelsummary)

setwd('/capstone/freshcair/meds-freshcair-capstone')

# Start

# Field-asset matching uses wells then nearest asset: entry_df.csv
# Field-asset matching uses wells then asset of nearest neighbor fields: entry_df_v2
entry_df <- read_csv(file.path("data/processed/entry_df_final_revised.csv"))

# Prepare variables

# Outcome variables: new_prod new_wells doc_prod
entry_df <- entry_df %>%
  rename(new_wells = n_new_wells)

# Cost variables
entry_df <- entry_df %>%
  mutate(capex_per_bbl_nom = as.numeric(capex_per_bbl_nom),
         opex_per_bbl_nom = as.numeric(opex_per_bbl_nom),
         capex_imputed = as.numeric(capex_imputed),
         opex_imputed = as.numeric(opex_imputed))

# Field depletion variables
entry_df <- entry_df %>%
  rename(depl = m_cumsum_div_my_prod)

# Top 10 producing fields in 2019
entry_df <- entry_df %>%
  rename(topfield = top_field)

entry_df$topfield <- factor(entry_df$topfield, levels = 0:10, labels = c("Non-top fields", "Belridge South", "Midway-Sunset", "Kern River", "Cymric", "Wilmington", "Lost Hills", "San Ardo", "Elk Hills", "Coalinga", "Poso Creek"))

# Rank all fields by 2019 production
entry_df <- entry_df %>%
  group_by(year) %>%
  mutate(rank = dense_rank(desc(doc_prod))) %>%
  ungroup() %>%
  group_by(doc_field_code) %>%
  mutate(field_rank = max(rank))

# Create new categories for low-producing fields
entry_df <- entry_df %>%
  mutate(nontop_field_categ = ifelse(topfield == 0, cut(field_rank, breaks = quantile(field_rank[topfield == 0], probs = seq(0, 1, 0.1), na.rm = TRUE), labels = 10:1, include.lowest = TRUE), NA)) %>%
  group_by(doc_field_code) %>%
  mutate(field_categ = ifelse(topfield > 0, topfield, max(nontop_field_categ, na.rm = TRUE))) %>%
  ungroup()

summary(entry_df$field_categ)

write_rds(entry_df, file.path("data/processed/entry_revised.rds"))

# # Convert vars to real price (for revisions to paper 11/29/2022)
# # real prices weren't eventually used but these changes to entry.do were updated anyway
# 
# # Import CPI series
# cpi <- read_excel(file.path(googleDriveDir, "data/stocks-flows/raw/BLS-CPI-U.xlsx"), sheet = "Annual")
# cpi <- cpi %>%
#   rename_all(tolower)
# 
# entry_df <- left_join(entry_df, cpi, by = "year")
# entry_df <- entry_df %>%
#   mutate(brent_2019 = brent / cpi * cpi2019,
#          capex_imputed_2019 = capex_imputed / cpi * cpi2019,
#          opex_imputed_2019 = opex_imputed / cpi * cpi2019)
# 
# write_rds(entry_df, file.path(dataDir, "entry_revised_real.rds"))
# 
# # Plots to look at data
# entry <- read_rds(file.path(dataDir, "entry.rds"))
# 
# # Histograms
# ggplot(entry, aes(x = new_wells)) +
#   geom_histogram(binwidth = 1, center = 0, boundary = 0, closed = "left", fill = "skyblue", color = "black") +
#   labs(title = "Histogram of New Wells", x = "New Wells", y = "Frequency")
# ggsave(file.path(docDir, "histograms", "new_well.pdf"), width = 6, height = 4)
# 
# ggplot(entry, aes(x = new_prod)) +
#   geom_histogram(fill = "skyblue", color = "black") +
#   labs(title = "Histogram of New Production", x = "New Production", y = "Frequency")
# ggsave(file.path(docDir, "histograms", "new_prod.pdf"), width = 6, height = 4)
# 
# ggplot(entry, aes(x = opex_imputed)) +
#   geom_histogram(fill = "skyblue", color = "black") +
#   labs(title = "Histogram of Opex (Imputed)", x = "Opex (Imputed)", y = "Frequency")
# ggsave(file.path(docDir, "histograms", "opex_imputed.pdf"), width = 6, height = 4)
# 
# ggplot(entry, aes(x = capex_imputed)) +
#   geom_histogram(fill = "skyblue", color = "black") +
#   labs(title = "Histogram of Capex (Imputed)", x = "Capex (Imputed)", y = "Frequency")
# ggsave(file.path(docDir, "histograms", "capex_imputed.pdf"), width = 6, height = 4)
# 
# ggplot(entry, aes(x = wellcost_imputed)) +
#   geom_histogram(fill = "skyblue", color = "black") +
#   labs(title = "Histogram of Wellcost (Imputed)", x = "Wellcost (Imputed)", y = "Frequency")
# ggsave(file.path(docDir, "histograms", "wellcost_imputed.pdf"), width = 6, height = 4)
# 
# # Time series
# entry_summary <- entry %>%
#   group_by(year) %>%
#   summarize(new_well_yearsum = sum(new_wells),
#             new_prod_yearsum = sum(new_prod),
#             doc_prod_yearsum = sum(doc_prod),
#             capex_yearmean = mean(capex_imputed),
#             opex_yearmean = mean(opex_imputed),
#             totex_capex_yearmean = mean(totex_capex),
#             wellcost_yearmean = mean(wellcost_imputed),
#             wm_capex_yearmean = mean(wm_capex_imputed),
#             wm_opex_yearmean = mean(wm_opex_imputed),
#             wm_totex_yearmean = mean(wm_totex),
#             brent = first(brent))
# 
# # Outcome variables: New prod and new wells and Brent over time
# ggplot(entry_summary, aes(x = year)) +
#   geom_line(aes(y = new_well_yearsum), color = "blue") +
#   geom_line(aes(y = brent), color = "black", linetype = "dashed") +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Brent Price")) +
#   labs(title = "No. New Wells (sum of fields)", x = "Year", y = "New Wells")
# ggsave(file.path(docDir, "timeseries", "new_well.pdf"), width = 6, height = 4)
# 
# ggplot(entry_summary, aes(x = year)) +
#   geom_line(aes(y = new_prod_yearsum), color = "blue") +
#   geom_line(aes(y = brent), color = "black", linetype = "dashed") +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Brent Price")) +
#   labs(title = "New Production (sum of fields)", x = "Year", y = "New Production")
# ggsave(file.path(docDir, "timeseries", "new_prod.pdf"), width = 6, height = 4)
# 
# # Cost variables
# ggplot(entry_summary, aes(x = year)) +
#   geom_line(aes(y = wm_opex_yearmean), color = "blue") +
#   geom_line(aes(y = wm_capex_yearmean), color = "red") +
#   geom_line(aes(y = wm_totex_yearmean), color = "green") +
#   geom_line(aes(y = brent), color = "black", linetype = "dashed") +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Brent Price")) +
#   labs(title = "Capex/Opex/Totex per bbl (mean of fields)", x = "Year", y = "Cost per bbl")
# ggsave(file.path(docDir, "timeseries", "cost_imputed.pdf"), width = 6, height = 4)
# 
# ggplot(entry_summary, aes(x = year)) +
#   geom_line(aes(y = wm_opex_yearmean), color = "blue") +
#   geom_line(aes(y = wm_capex_yearmean), color = "red") +
#   geom_line(aes(y = brent), color = "black", linetype = "dashed") +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Brent Price")) +
#   labs(title = "Capex/Opex per bbl (mean of fields)", x = "Year", y = "Cost per bbl")
# 
# ggplot(entry_summary, aes(x = year)) +
#   geom_line(aes(y = wellcost_yearmean), color = "blue") +
#   geom_line(aes(y = brent), color = "black", linetype = "dashed") +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Brent Price")) +
#   labs(title = "Wellcost per EUR bbl (mean of fields)", x = "Year", y = "Wellcost per EUR bbl")
# 
# # Poisson model
# # Without field FEs
# # Regressions
# models <- list()
# 
# for (var in c("new_wells", "new_prod")) {
#   # Add depletion, separate cost vars
#   models[[paste0(var)]] <- feglm(as.formula(paste0(var, " ~ brent + capex_imputed + opex_imputed + depl")),
#                                  family = poisson(link = "log"), data = entry_df)
#   
#   # Add API, separate cost vars
#   models[[paste0(var, "_api")]] <- feglm(as.formula(paste0(var, " ~ brent + wm_api_gravity + capex_imputed + opex_imputed + depl")),
#                                          family = poisson(link = "log"), data = entry_df)
#   
#   # Add API with brent interaction, separate cost vars
#   models[[paste0(var, "_apiX")]] <- feglm(as.formula(paste0(var, " ~ brent:wm_api_gravity + capex_imputed + opex_imputed + depl")),
#                                           family = poisson(link = "log"), data = entry_df)
#   
#   # Add depletion, sum cost vars
#   models[[paste0(var, "_tot")]] <- feglm(as.formula(paste0(var, " ~ brent + totex_capex + depl")),
#                                          family = poisson(link = "log"), data = entry_df)
#   
#   # Add API, separate cost vars
#   models[[paste0(var, "_tot_api")]] <- feglm(as.formula(paste0(var, " ~ brent + wm_api_gravity + totex_capex + depl")),
#                                              family = poisson(link = "log"), data = entry_df)
#   
#   # Add API, with Brent interaction
#   models[[paste0(var, "_tot_apiX")]] <- feglm(as.formula(paste0(var, " ~ brent:wm_api_gravity + totex_capex + depl")),
#                                               family = poisson(link = "log"), data = entry_df)
# }
# 
# # Output results
# modelsummary(models, stars = c('*' = .1, '**' = .05, '***' = .01),
#              coef_map = c("brent" = "Brent Price",
#                           "capex_imputed" = "Capex (Imputed)",
#                           "opex_imputed" = "Opex (Imputed)",
#                           "totex_capex" = "Totex (Capex)",
#                           "depl" = "Depletion",
#                           "wm_api_gravity" = "API Gravity"),
#              gof_map = c("nobs" = "No. field-years", "sigma" = "No. fields"),
#              output = file.path(texDir, "poisson_noprod.tex"))
# 
# 
# # With field FEs
# # Regressions
# models_fe <- list()
# 
# for (var in c("new_prod", "new_wells")) {
#   # Add depletion, separate cost vars
#   models_fe[[paste0("fe", var)]] <- feglm(as.formula(paste0(var, " ~ brent + capex_imputed + opex_imputed + depl | doc_field_code")),
#                                           family = poisson(link = "log"), data = entry_df)
#   
#   # Add API, separate cost vars
#   models_fe[[paste0("fe", var, "_api")]] <- feglm(as.formula(paste0(var, " ~ brent + wm_api_gravity + capex_imputed + opex_imputed + depl | doc_field_code")),
#                                                   family = poisson(link = "log"), data = entry_df)
#   
#   # Add API with brent interaction, separate cost vars
#   models_fe[[paste0("fe", var, "_apiX")]] <- feglm(as.formula(paste0(var, " ~ brent:wm_api_gravity + capex_imputed + opex_imputed + depl | doc_field_code")),
#                                                    family = poisson(link = "log"), data = entry_df)
#   
#   # Add depletion, sum cost vars
#   models_fe[[paste0("fe", var, "_tot")]] <- feglm(as.formula(paste0(var, " ~ brent + totex_capex + depl | doc_field_code")),
#                                                   family = poisson(link = "log"), data = entry_df)
#   
#   # Add API, separate cost vars
#   models_fe[[paste0("fe", var, "_tot_api")]] <- feglm(as.formula(paste0(var, " ~ brent + wm_api_gravity + totex_capex + depl | doc_field_code")),
#                                                       family = poisson(link = "log"), data = entry_df)
#   
#   # Add API, with Brent interaction
#   models_fe[[paste0("fe", var, "_tot_apiX")]] <- feglm(as.formula(paste0(var, " ~ brent:wm_api_gravity + totex_capex + depl | doc_field_code")),
#                                                        family = poisson(link = "log"), data = entry_df)
# }
# 
# # Output results
# modelsummary(models_fe, stars = c('*' = .1, '**' = .05, '***' = .01),
#              coef_map = c("brent" = "Brent Price",
#                           "capex_imputed" = "Capex (Imputed)",
#                           "opex_imputed" = "Opex (Imputed)",
#                           "totex_capex" = "Totex (Capex)",
#                           "depl" = "Depletion",
#                           "wm_api_gravity" = "API Gravity"),
#              gof_map = c("nobs" = "No. field-years", "sigma" = "No. fields"),
#              output = file.path(texDir, "poisson_fe.tex"))
# 
# # Check estimated total resource against projected total production
# predicted_production <- read_csv(file.path(dataRawDir, "predicted-production_2020-2100_field.csv"))
# predicted_production <- predicted_production %>%
#   rename(doc_field_code = fieldcode) %>%
#   select(-fieldname) %>%
#   group_by(doc_field_code) %>%
#   summarize(prod_future_sum = sum(production_bbl)) %>%
#   filter(year == 2020) %>%
#   select(doc_field_code, prod_future_sum)
# 
# forecast_depl_revised <- read_rds(file.path(dataDir, "forecast_depl_revised.rds"))
# forecast_depl_merged <- left_join(forecast_depl_revised, predicted_production, by = "doc_field_code")
# 
# forecast_depl_merged <- forecast_depl_merged %>%
#   mutate(cum_prod_2019 = depl2019 * resource,
#          total_prod = cum_prod_2019 + prod_future_sum,
#          d_res_prod = resource - total_prod)
# 
# summary(forecast_depl_merged$d_res_prod)
# hist(forecast_depl_merged$d_res_prod)
# sum(forecast_depl_merged$d_res_prod < 0)
# 
# # Check estimated total resource against DOC 2009 report
# doc_reserves <- read_csv(file.path(dataRawDir, "oil_production_reserves_2009.csv"))
# doc_reserves <- doc_reserves %>%
#   mutate(doc_field_code_orig = doc_field_code,
#          doc_field_code = as.numeric(doc_field_code),
#          cum_prod = as.numeric(cumulative_oil_and_condensatembb),
#          reserves_left = as.numeric(estimated_oil_reservesmbbl)) %>%
#   replace_na(list(cum_prod = 0, reserves_left = 0)) %>%
#   filter(!is.na(doc_field_code)) %>%
#   mutate(resource_doc = cum_prod + reserves_left) %>%
#   select(doc_field_code, doc_fieldname, field_name, cum_prod, reserves_left, resource_doc)
# 
# forecast_depl_doc <- left_join(forecast_depl_revised, doc_reserves, by = "doc_field_code") %>%
#   select(doc_field_code, doc_fieldname, resource, depl2019, field_name, resource_doc) %>%
#   mutate(resource = resource / 1000)  # convert from bbl to Mbbl
# 
# ggplot(forecast_depl_doc, aes(x = resource_doc, y = resource)) +
#   geom_point() +
#   geom_text(aes(label = doc_fieldname), vjust = -0.5, size = 3) +
#   geom_smooth(method = "lm", se = FALSE) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   labs(x = "Resource (DOC)", y = "Resource (Estimated)", title = "Comparison of Estimated Resource and DOC Resource") +
#   theme_minimal()
# 
