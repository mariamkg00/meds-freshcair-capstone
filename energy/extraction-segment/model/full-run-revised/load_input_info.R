## Tracey Mangin
## February 1, 2022
## model: load input info
## Updated 6/4/24 - MP

setwd('/capstone/freshcair/meds-freshcair-capstone')


# if(zenodo_repo) {
# ## paths and file name for zenodo users ----
# main_path          = 'data'
# extract_path       = 'intermediate/extraction-model/'
# 
# model_path        = paste0(main_path, 'inputs/extraction/')
# reduct_path       = paste0(main_path, extract_path)
# scen_path         = paste0(main_path, 'inputs/scenarios')
# outputs_path      = paste0(main_path, extract_path)
# data_path         = paste0(main_path, 'inputs/extraction')
# academic_out      = paste0(main_path, extract_path)
# revision_path     = paste0(main_path, extract_path)
# forecast_path     = paste0(main_path, extract_path)
# entry_path        = paste0(main_path, extract_path)
# stocks_flows_path = paste0(main_path, extract_path)
# setback_path      = paste0(main_path, extract_path)
# decline_path      = paste0(main_path, extract_path)
# peak_path         = paste0(main_path, extract_path)
# 
# entry_file        = paste0(main_path, extract_path, 'entry_df_final_revised.csv')
# 
# } else {
# 
# ## paths and file name for emlab users -----------
# main_path          = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
# 
# model_path        = paste0(main_path, 'outputs/')
# reduct_path       = paste0(main_path, 'project-materials/scenario-inputs/')
# scen_path         = paste0(main_path, 'project-materials/scenario-inputs/')
# outputs_path      = paste0(main_path, 'outputs/')
# data_path         = paste0(main_path, 'data/stocks-flows/processed/')
# academic_out      = paste0(main_path, 'outputs/academic-out/extraction/')
# revision_path     = paste0(main_path, 'outputs/academic-out/extraction/nature-energy-rev-outputs/')
# stocks_flows_path = paste0(outputs_path, 'stocks-flows/')
# forecast_path     = paste0(outputs_path, 'stocks-flows/entry-input-df/final/')
# entry_path        = paste0(outputs_path, 'entry-model-results/')
# exist_prod_path   = paste0(outputs_path, '/predict-production/existing_production/')
# setback_path      = paste0(outputs_path, 'setback/model-inputs/')
# decline_path      = paste0(outputs_path, 'decline-historic/parameters/')
# peak_path         = paste0(outputs_path, 'decline-historic/data')
# 
# entry_file        = paste0(main_path, 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv')
# 
# 
# }

## the following are the same for emlab and zenodo users

# file names  
ccs_capture_rate  = 0.61
# forecast_file     = 'field_capex_opex_forecast_final.csv'
# ccs_ext_file      = 'ccs_extraction_scenarios_revised.csv' ## revised includes ccs cost = inf -- used revised MP
# prod_quota_file   = 'prod_quota_scenarios_with_sb.csv'
# scen_id_file      = 'scenario_id_list_targets.csv'


# source from other scripts -----

library(here)
library(dplyr)
library(openxlsx)
library(readxl)
library(purrr)
library(data.table)
library(tidyverse)
library(tidyr)
library(randomForest)
library(caret)
library(glmnet)

# source function to rank costs
# source(here::here('energy', 'extraction-segment', 'prod_quota.R'))

# source ccs emissions mean b calculation script
# source(here::here('energy', 'scenario-prep', 'ccs_parameterization.R'))

## source all functions for optimization
items <- list.files(here::here("energy", "extraction-segment", "model", "full-run-revised", "target-functions"))
walk(items, ~ here::here("energy", "extraction-segment", "model", "full-run-revised", "target-functions", .x) %>% source()) 

# # source function to create matrix of scenarios and forecasted variables
#   source(here::here('energy', 'extraction-segment', 'full-run', 'fun_input_scenarios_full.R'))

# # source function to filter scenario selection
# source(here::here('energy', 'extraction-segment', 'full-run', 'fun_filter_scenarios.R'))

## functions and info for calculating ccs info
a = 4

solve_tc <- function(a, b, q) {
  f <- (q*(a*b - a*(q^(1/a)) + b))/(a + 1)
  return(f)
}

## exit function
calc_num_well_exits <- function(fe_val, bhat, p_oil, op_hat, opex_val, dhat, depl_val) {
  
  n_well_exit = exp(bhat * p_oil + op_hat * opex_val + dhat * depl_val) * fe_val 
  
}


# load data -----

## scen id list
# scen_id_list = fread(file.path(academic_out, scen_id_file), header = T)
# add new data directory HK
emis_reduc_90 = fread('data-str/public/intermediate/health/emission_reduction_90.csv', header = T)
emis_reduc_90_val = emis_reduc_90[, ghg_emission_MtCO2e][1]

# load oil price data
# Readin in oilpx_scens with readxl instead - MP # add new data directory HK
oilpx_scens = setDT(read_excel(path = 'data-str/public/inputs/extraction/oil_price_projections_revised.xlsx', 
                               sheet = 'nominal', 
                               col_names = TRUE, 
                               col_types = 'numeric'))
# Selecting 4 columns used - MP
oilpx_scens = oilpx_scens[, c('Year', 'AEO 2021 Reference case $/b', 'AEO 2021 High oil price $/b', 'AEO 2021 Low oil price $/b')]
# oilpx_scens = setDT(read.xlsx(file = file.path('data/inputs/extraction/oil_price_projections_revised.xlsx'), sheet = 'nominal', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')

# Convert oilpx_scens to a data.table -- Added - MP
setDT(oilpx_scens)

oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

## load innovation scenarios # add new data directory HK
innovation_scens = fread(file.path('data-str/public/inputs/scenarios/innovation_scenarios.csv'), header = T)

## load carbon px scens # add new data directory HK
carbonpx_scens = fread(file.path('data-str/public/inputs/scenarios/carbon_prices_revised.csv'), header = T)
# carbonpx_scens[carbon_price_scenario == 'last CA auction price', carbon_price := 0] # assume rystard's BAU opex already embeds carbon price
carbonpx_scens[, carbon_price_usd_per_kg := carbon_price/1000] # convert from usd per metric ton to usd per kg
carbonpx_scens = carbonpx_scens[, c('year', 'carbon_price_scenario', 'carbon_price_usd_per_kg')]

## load ccs scenarios # add new data directory HK
ccs_scens = fread(file.path('data-str/private/scens/ccs_extraction_scenarios_revised.csv'), header = T)
ccs_scens[, ccs_price_usd_per_kg := ccs_price/1000] # convert from usd per metric ton to usd per kg
ccs_scens = ccs_scens[, c('year', 'ccs_scenario', 'ccs_price_usd_per_kg')]
ccs_scens[, ccs_scenario := factor(ccs_scenario, levels = c('no ccs', 'high CCS cost', 'medium CCS cost', 'low CCS cost'))]

## load price data -- Updated - MP # add new data directory HK
price_data = fread(file.path('data-str/public/intermediate/energy/production/field_capex_opex_forecast_revised.csv'), header = T)
# price_data[, doc_field_code := as.character(doc_field_code)]

# Updated - MP # add new data directory HK
resource_data = fread(file.path("data/intermediate-zenodo/intermediate/extraction-model/field_resource_revised.csv"), header = T)
# resource_data[, doc_field_code := as.character(doc_field_code)] # Added MP
resource_data = resource_data[, c('doc_field_code', 'resource')]

## oad ghg factors -- Updated - MP # add new data directory HK
ghg_factors = fread(file.path('data-str/public/intermediate/energy/production/ghg_emissions_x_field_2018-2045.csv'), header = T)
ghg_factors[, doc_field_code := as.numeric(doc_field_code)] # Added MP
ghg_factors[, doc_field_code := as.character(doc_field_code)] # Added MP
# ghg_factors = ghg_factors[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]

## adjust setback files to include setback toggle
## -----------------------------------------------------------

# load n wells in setbacks and setback coverage file 
n_wells_setbacks = fread(file.path('data-str/private/setback-cov/n_wells_area.csv'), header = T, colClasses = c('doc_field_code' = 'character'))

## setback applies to existing wells
n_wells_setbacks[, setback_existing := 1]

## setback does not apply to existing wells
n_wells_setbacks0 <- n_wells_setbacks[setback_scenario == "no_setback"]

n_wells_setbacks0[, setback_existing := 0]

adj_setback <- expand.grid(adj_setback_scen = unique(n_wells_setbacks$setback_scenario),
                           doc_field_code = unique(n_wells_setbacks0$doc_field_code))

# adj_setback$doc_field_code = as.numeric(adj_setback$doc_field_code) # Added MP

# n_wells_setbacks0$doc_field_code = as.numeric(n_wells_setbacks0$doc_field_code) # Added MP
n_wells_setbacks0 <- merge(adj_setback, n_wells_setbacks0,
                           by = c('doc_field_code'),
                           all = T)

setDT(n_wells_setbacks0)

n_wells_setbacks0[, setback_scenario := adj_setback_scen]

n_wells_setbacks0 <- n_wells_setbacks0[, .(setback_scenario, doc_field_code, doc_fieldname, 
                                           n_wells, n_wells_in_setback, adj_no_wells,
                                           setback_existing)]


n_wells_setbacks <- rbind(n_wells_setbacks, n_wells_setbacks0)


## load setback scenarios -- Updated - MP
setback_scens = fread(file.path("data-str/private/setback-buffs/setback_coverage_R.csv"), header = T, colClasses = c('doc_field_code' = 'character'))
setback_scens[, doc_field_code := as.numeric(doc_field_code)] # Added MP
setback_scens[, doc_field_code := as.character(doc_field_code)] # Added MP
setback_scens[, scen_area_m2 := orig_area_m2 *  (1 - rel_coverage)]
setback_scens <- setback_scens[, c("doc_field_code", "setback_scenario", "orig_area_m2", "scen_area_m2", "rel_coverage")]
setnames(setback_scens, 'rel_coverage', 'area_coverage')

# n_wells_setbacks[, doc_field_code := as.numeric(doc_field_code)] # Added MP
setback_scens = merge(setback_scens, n_wells_setbacks,
                      by = c('doc_field_code', 'setback_scenario'),
                      all = T)

setback_scens[, doc_fieldname := NULL]
setback_scens[, n_wells_in_setback := NULL]

setnames(setback_scens, 'n_wells', 'n_wells_start')
setnames(setback_scens, 'adj_no_wells', 'n_wells_setback')

# setback_scens[, doc_field_code := as.numeric(doc_field_code)] # Added MP
setback_scens[, setback_scenario := as.character(setback_scenario)]

setback_scens[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]


# # # # # Implementing setback starting later than 2020 --- START
# # intervention_year == 2025
# if (exists("intervention_year")) {
#   # Implementing setback starting later than 2020 --- START
#   annual_setback <- expand.grid(setback_scenario = unique(setback_scens$setback_scenario),
#                                 doc_field_code = unique(setback_scens$doc_field_code),
#                                 year = c(2020:2045))
#   
#   pre_int_setback <- setback_scens %>%
#     filter(setback_scenario == "no_setback") %>%
#     select(-setback_scenario)
#   
#   pre_int_setback2 <- annual_setback %>%
#     filter(year < intervention_year)
#   
#   pre_int_setback <- merge(pre_int_setback, pre_int_setback2, by = "doc_field_code", allow.cartesian = T)
#   
#   post_int_setback <- annual_setback %>%
#     filter(year >= intervention_year) %>%
#     full_join(., setback_scens)
#   
#   setback_scens <- rbind(pre_int_setback, post_int_setback)
#   # Implementing setback starting later than 2020 --- END
# }
# # 
# # # Implementing setback starting later than 2020 --- END

# load production quota file
prod_quota_scens = fread(file.path("data-str/public/inputs/scenarios/prod_quota_scenarios.csv"), header = T)
prod_quota_scens <- prod_quota_scens[, .(year, prod_quota_scenario, quota)]

# load excise tax file -- Updated - MP  # add new data directory HK
excise_tax_scens = fread(file.path('data-str/public/intermediate/health/excise_tax_non_target_scens.csv'), header = T)
excise_tax_scens = subset(excise_tax_scens, select = -units)

# load ccs incentives file -- Updated - MP
# Reading in with read_excel instead
incentives_scens = setDT(read_excel(path = 'data-str/public/inputs/scenarios/CCS_LCFS_45Q.xlsx',
                                    sheet = 'scenarios',
                                    col_names = TRUE,
                                    range = cell_cols(1:3)))
#incentives_scens = setDT(read.xlsx(file.path('data/inputs/scenarios/CCS_LCFS_45Q.xlsx'), sheet = 'scenarios', cols = c(1:3)))

# # pad field codes with leading zeroes ------  removing for now MP
# price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
# resource_data[, doc_field_code := sprintf("%03d", doc_field_code)]
# ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]

# create datatable of forecasted input variables -----
# vars_dt$doc_field_code = as.numeric(vars_dt$doc_field_code) # Added MP
# resource_data$doc_field_code = as.numeric(resource_data$doc_field_code) # Added MP

vars_dt = merge(price_data[year >= 2020], resource_data, by = c('doc_field_code'))
vars_dt[, doc_field_code := as.character(doc_field_code)] # Added MP

# vars_dt$doc_field_code = as.numeric(vars_dt$doc_field_code) # Added MP
# ghg_factors$doc_field_code = as.numeric(ghg_factors$doc_field_code)

vars_dt = merge(vars_dt, ghg_factors[year >= 2020], by = c('doc_field_code', 'year'))
setcolorder(vars_dt, c('doc_field_code', 'doc_fieldname', 'year', 
                       'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 'wm_capex_imputed', 'resource', 
                       'upstream_kgCO2e_bbl'))

# create adjusted ccs costs ------

ccs_scens_adj = ccs_scens[incentives_scens, on = .(year), allow.cartesian = T, nomatch = 0]
ccs_scens_adj[, ccs_scenario_adj := fcase(incentive_scenario == 'no incentives', paste0(ccs_scenario),
                                          incentive_scenario == '45Q only', paste0(ccs_scenario, ' - 45Q'),
                                          incentive_scenario == '45Q + LCFS', paste0(ccs_scenario, ' - 45Q - LCFS'))]


## remove redundant scenarios
ccs_scens_adj = ccs_scens_adj[!ccs_scenario_adj %in% c('no ccs - 45Q', 'no ccs - 45Q - LCFS')]


# adjust ccs price with incentives
ccs_scens_adj[, ccs_price_usd_per_kg_adj := ccs_price_usd_per_kg - (incentive_price/1e3)]

# create constrained version 
ccs_scens_neg = ccs_scens_adj[ccs_scenario_adj %in% unique(ccs_scens_adj[ccs_price_usd_per_kg_adj < 0, ccs_scenario_adj])]
ccs_scens_neg[, ccs_scenario_adj := paste0(ccs_scenario_adj, ' (constrained)') ]
ccs_scens_neg[, ccs_price_usd_per_kg_adj := fifelse(ccs_price_usd_per_kg_adj < 0, 0, ccs_price_usd_per_kg_adj)]

# combine ccs scenarios
ccs_scens_all = rbind(ccs_scens_adj, ccs_scens_neg)

# select columns 
ccs_scens_all = ccs_scens_all[, .(year, ccs_scenario_adj, ccs_price_usd_per_kg_adj)]
setnames(ccs_scens_all, c('ccs_scenario_adj', 'ccs_price_usd_per_kg_adj'), c('ccs_scenario', 'ccs_price_usd_per_kg'))

## ----------------------------------------
## ----------------------------------------


# load entry data # add new data directory HK -- not working so reverting to processed
entry_dt = fread(file.path("data/processed/entry_df_final_revised.csv"), header = T, colClasses = c('doc_field_code' = 'character'))
entry_dt[, doc_field_code := as.numeric(doc_field_code)] # Added MP
entry_dt[, doc_field_code := as.character(doc_field_code)] # Added MP

# # load matrix of scenarios and forecasted variables
# scenarios_dt = load_scenarios_dt(scenario_selection)

# load coefficients from poisson regression of historic data -- Updated but stole from Zenodo - MP # add new data directory HK
coefs_dt = fread(file.path('data-str/private/inputs/poisson_regression_coefficients_revised.csv'), header = T, colClasses = c('doc_field_code' = 'character'))
coefs_dt = unique(coefs_dt)
# coefs_dt$doc_field_code = as.numeric(coefs_dt$doc_field_code) # Added MP

# load decline parameters -- Updated - MP # add new data directory HK
decline_dt = fread(file.path('data-str/public/intermediate/energy/production/forecasted_decline_parameters_2020_2045.csv'), header = T, colClasses = c('doc_field_code' = 'character'))
decline_dt[, doc_field_code := as.numeric(doc_field_code)] # Added MP
decline_dt[, doc_field_code := as.character(doc_field_code)] # Added MP

# load peak production for each field # add new data directory HK
peak_dt = fread(file.path('data-str/public/intermediate/energy/production/field-year_peak-production_yearly.csv'), header = T, colClasses = c('doc_field_code' = 'character'))
peak_dt[, doc_field_code := as.numeric(doc_field_code)] # Added MP
peak_dt[, doc_field_code := as.character(doc_field_code)] # Added MP
# peak_dt[, doc_field_code := sprintf("%03d", doc_field_code)]
# peak_dt$doc_field_code = as.numeric(peak_dt$doc_field_code)

# exit file
exit_coefs = fread(file.path('data-str/private/inputs/exit_regression_coefficients.csv'), header = T, colClasses = c('doc_field_code' = 'character'))
# exit_coefs[, doc_field_code := as.numeric(doc_field_code)] # Added MP

# Removing for now MP
# exit_coefs[, doc_field_code := sprintf("%03s", doc_field_code)]
exit_coefs[, doc_fieldname := NULL]

# load forecasted production from existing (pre 2020) wells -- Updated - MP # add new data directory HK
prod_existing_vintage = fread(file.path('data-str/public/intermediate/energy/production/pred_prod_no_exit_2020-2045_field_start_year_revised.csv'), header = T, colClasses = c('doc_field_code' = 'character'))
prod_existing_vintage[, vintage := as.character(start_year)]

# exit_coefs[, doc_field_code := as.numeric(doc_field_code)] # Added MP

prod_existing_vintage[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]


# load historic modeled well entry
# hist_modeled = fread(file.path(model_path, 'entry-model-results', hist_file), header = T)

# load historic production -- Updated - MP # add new data directory HK
prod_hist = fread(file.path('data-str/public/intermediate/energy/production/crude_prod_x_field_revised.csv'), header = T, colClasses = c('doc_field_code' = 'character'))
prod_hist[, doc_field_code := as.numeric(doc_field_code)] # Added MP
prod_hist[, doc_field_code := as.character(doc_field_code)] # Added MP
# Added MP - adding new feature for predictive model
# prod_hist <- merge(prod_hist, unique(entry_dt[, .(doc_field_code, top_field)]), 
#                    by = "doc_field_code", 
#                    all.x = TRUE)

# # rename FieldCode -> doc_field_code -----
#
#   setnames(decline_dt, "FieldCode", "doc_field_code")
#   setnames(peak_dt, "FieldCode", "doc_field_code")
#   setnames(prod_hist, "FieldCode", "doc_field_code")

# # rename FieldName -> doc_fieldname -----
#   setnames(decline_dt, "FieldName", "doc_fieldname")
#   setnames(peak_dt, "FieldName", "doc_fieldname")

# pad field codes with leading zeroes ----- Removing for now - MP
# coefs_dt[, doc_field_code := sprintf("%03s", doc_field_code)]

# get peak production median of last two vintages -----

# meas-note: so this next line calculates the median of the peak production (per well) of the last few vintages to apply to new well entries.
# meas-note: i would change to use the last few start-years (maybe 2000 onwards? or something like that) instead
peak_prod_median = peak_dt[start_year >= 2000, 
                           lapply(.SD, median, na.rm = TRUE), .SDcols = c("peak_tot_prod", "peak_avg_well_prod", "peak_well_prod_rate"),
                           by = .(doc_field_code)]

# from entry data, keep: field, new wells, depletion ------

setnames(entry_dt, 'n_new_wells', 'new_wells')
setnames(entry_dt, 'm_cumsum_div_my_prod', 'depl')
entry_dt_vars = entry_dt[, .(doc_field_code, doc_fieldname, year, brent, capex_imputed, opex_imputed, depl, new_wells)]
# entry_dt = entry_dt[, .(doc_field_code, doc_fieldname, year, new_wells, depl)]

# ccs emissions scalar ---------

ccs_ghg_scalar <- 1 - ccs_capture_rate


#  --------------------------------- ML IMPLEMENTATION ---------------------------------

#  --------------------------------- ENTRY RF MODEL ---------------------------------
set.seed(650)

entry_df <- read_csv("data/processed/entry_df_final_revised.csv")
entry_df$doc_field_code <- as.numeric(entry_df$doc_field_code)
entry_df$doc_field_code <- as.character(entry_df$doc_field_code)

entry_df <- entry_df %>%
  mutate(oil_price_usd_per_bbl = brent) %>% 
  mutate(depl = m_cumsum_div_my_prod) %>% 
  na.omit()

### ADDING CUSTOM BRENT VALUES --- 

# https://www.eia.gov/outlooks/steo/realprices/real_prices.xlsx
# Using 2024 present value 

oil_prices <- c(
  "1977" = 75.05, 
  "1978" = 69.94, 
  "1979" = 93.08, 
  "1980" = 128.71, 
  "1981" = 127.77, 
  "1982" = 108.91, 
  "1983" = 92.19, 
  "1984" = 87.01, 
  "1985" = 78.56, 
  "1986" = 39.78, 
  "1987" = 50.00,
  "1988" = 38.66,
  "1989" = 45.66,
  "1990" = 52.09,
  "1991" = 43.07,
  "1992" = 40.64,
  "1993" = 34.97,
  "1994" = 32.83,
  "1995" = 35.23,
  "1996" = 41.17,
  "1997" = 36.07,
  "1998" = 23.18,
  "1999" = 32.47,
  "2000" = 50.42,
  "2001"= 38.90,
  "2002"= 41.29,
  "2003" = 47.19,
  "2004" = 59.50,
  "2005" = 78.41,
  "2006" = 91.75,
  "2007" = 101.48,
  "2008" = 134.69,
  "2009" = 86.17,
  "2010" = 108.89,
  "2011" = 142.83,
  "2012" = 137.89,
  "2013" = 131.91,
  "2014" = 118.59,
  "2015" = 61.24,
  "2016" = 50.50,
  "2017" = 62.58,
  "2018" = 76.51,
  "2019" = 70.99
)

entry_df <- entry_df %>%
  mutate(year_char = as.character(year)) %>%
  mutate(oil_price_usd_per_bbl = oil_prices[year_char]) %>%
  select(-year_char)

### ADDING CUSTOM BRENT VALUES ---

entry_df = as.data.table(entry_df)

x_train <- as.matrix(entry_df[, .(oil_price_usd_per_bbl, wm_capex_imputed, wm_opex_imputed, depl, doc_field_code)])
y_train <- entry_df$n_new_wells

rf_model_entry <- randomForest(x = x_train, y = y_train, ntree = 500, mtry = 4, importance = TRUE)

#  --------------------------------- EXIT RF MODEL ---------------------------------
exit_df <- read_csv("data/processed/well_exits_pred.csv")
setDT(exit_df)
exit_df[, doc_field_code := as.character(doc_field_code)]

entry_df <- merge(entry_df, exit_df[, .(doc_field_code, year, well_exits, well_exits_pred)], 
                  by = c("doc_field_code", "year"), 
                  all.x = TRUE)

entry_df <- entry_df %>% 
  mutate(m_opex_imputed = opex_imputed) 

entry_df <- entry_df[!is.na(well_exits)]

x_train_exit <- as.matrix(entry_df[, .(oil_price_usd_per_bbl, m_opex_imputed, depl, doc_field_code)])
y_train_exit <- entry_df$well_exits

rf_model_exit <- randomForest(x = x_train_exit, y = y_train_exit, ntree = 500, mtry = 3, importance = TRUE)

# # --------------------------------- ENTRY GRADIENT BOOSTED MODEL ---------------------------------
# library(gbm)
# library(caret)
# 
# x_train_gb <- as.matrix(entry_df[, .(oil_price_usd_per_bbl, wm_capex_imputed, wm_opex_imputed, depl)])
# y_train_gb <- entry_df$n_new_wells
# 
# gbm_grid <- expand.grid(
#   n.trees = c(800, 1000, 1200),
#   interaction.depth = c(3, 4, 5),
#   shrinkage = c(0.05, 0.1, 0.15),
#   n.minobsinnode = c(3, 5, 7)
# )
# 
# ctrl <- trainControl(
#   method = "cv",
#   number = 5,
#   verboseIter = TRUE
# )
# 
# gbm_model_entry_tuned <- train(
#   n_new_wells ~ oil_price_usd_per_bbl + wm_capex_imputed + wm_opex_imputed + depl,
#   data = entry_df,
#   method = "gbm",
#   trControl = ctrl,
#   tuneGrid = gbm_grid,
#   verbose = FALSE
# )
# 
# print(gbm_model_entry_tuned$bestTune)
# 
# best_trees_entry <- gbm_model_entry_tuned$bestTune$n.trees
# 
# 
# gbm_model_entry <- gbm(
#   n_new_wells ~ oil_price_usd_per_bbl + wm_capex_imputed + wm_opex_imputed + depl,
#   data = entry_df,
#   distribution = "gaussian",
#   n.trees = gbm_model_entry_tuned$bestTune$n.trees,
#   interaction.depth = gbm_model_entry_tuned$bestTune$interaction.depth,
#   shrinkage = gbm_model_entry_tuned$bestTune$shrinkage,
#   n.minobsinnode = gbm_model_entry_tuned$bestTune$n.minobsinnode,
#   cv.folds = 5
# )
# 
# # --------------------------------- EXIT GRADIENT BOOSTED MODEL ---------------------------------
# 
# x_train_exit <- as.matrix(entry_df[, .(oil_price_usd_per_bbl, m_opex_imputed, depl)])
# y_train_exit <- entry_df$well_exits
# 
# gbm_grid_exit <- expand.grid(
#   n.trees = c(800, 1000, 1200),
#   interaction.depth = c(3, 4, 5),
#   shrinkage = c(0.5, 0.1, 0.15),
#   n.minobsinnode = c(3, 5, 7)
# )
# 
# ctrl <- trainControl(
#   method = "cv",
#   number = 5,
#   verboseIter = TRUE
# )
# 
# gbm_model_exit_tuned <- train(
#   well_exits ~ oil_price_usd_per_bbl + m_opex_imputed + depl,
#   data = entry_df,
#   method = "gbm",
#   trControl = ctrl,
#   tuneGrid = gbm_grid_exit,
#   verbose = FALSE
# )
# 
# print(gbm_model_exit_tuned$bestTune)
# 
# best_trees_exit <- gbm_model_exit_tuned$bestTune$n.trees
# 
# gbm_model_exit <- gbm(
#   well_exits ~ oil_price_usd_per_bbl + m_opex_imputed + depl,
#   data = entry_df,
#   distribution = "gaussian",
#   n.trees = gbm_model_exit_tuned$bestTune$n.trees,
#   interaction.depth = gbm_model_exit_tuned$bestTune$interaction.depth,
#   shrinkage = gbm_model_exit_tuned$bestTune$shrinkage,
#   n.minobsinnode = gbm_model_exit_tuned$bestTune$n.minobsinnode,
#   cv.folds = 5
# )
# 
# # --------------------------------- EXIT GRADIENT BOOSTED PLOTS ---------------------------------
# 
# # Predict using the models on the entire historical data
# entry_df$new_wells_pred <- predict(gbm_model_entry, entry_df, n.trees = best_trees_entry)
# entry_df$well_exits_pred <- predict(gbm_model_exit, entry_df, n.trees = best_trees_exit)
# 
# yearly_predictions <- entry_df %>%
#   group_by(year) %>%
#   summarise(new_wells_pred = sum(new_wells_pred),
#             n_new_wells = sum(n_new_wells))
# 
# # Plot the predictions
# ggplot(yearly_predictions, aes(x = year)) +
#   geom_line(aes(y = new_wells_pred, color = "New Wells Predicted")) +
#   geom_line(aes(y = n_new_wells, color = "Actual Number of Wells")) +
#   labs(title = "Predicted Number of New Wells vs. Well Exits",
#        x = "Year",
#        y = "Number of Wells",
#        color = "Legend") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(yearly_predictions$year), max(yearly_predictions$year), by = 5)) +
#   theme(plot.title = element_text(size = 14, face = "bold"),
#         axis.title = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         legend.position = "bottom",
#         legend.box = "vertical")

# --------------------------------- ENTRY LASSO MODEL ---------------------------------
# 
# entry_df <- read_csv("data/processed/entry_df_final_revised.csv")
# entry_df_og$doc_field_code <- as.numeric(entry_df$doc_field_code)
# entry_df_og$doc_field_code <- as.character(entry_df$doc_field_code)
# 
# # Prepare data for Random Forest model
# entry_df <- entry_df %>%
#   mutate(oil_price_usd_per_bbl = brent) %>% 
#   mutate(depl = m_cumsum_div_my_prod) %>% 
#   na.omit(., cols = c("depl", "m_capex_imputed", "m_opex_imputed")) %>% 
#   mutate(m_capex_imputed = capex_imputed) %>% 
#   mutate(m_opex_imputed = opex_imputed)
# 
# entry_df = as.data.table(entry_df)
# 
# library(glmnet)
# 
# # Train Lasso model on all data
# x_train_lasso_all <- as.matrix(entry_df[, .(oil_price_usd_per_bbl, m_capex_imputed, m_opex_imputed, depl)])
# y_train_lasso_all <- entry_df$n_new_wells
# lasso_model_entry_all <- glmnet(x_train_lasso_all, y_train_lasso_all, alpha = 1, lambda = seq(0.01, 1, by = 0.01))
# cv_lasso_entry_all <- cv.glmnet(x_train_lasso_all, y_train_lasso_all, alpha = 1)
# best_lambda_entry_all <- cv_lasso_entry_all$lambda.min
# coef_lasso_entry_all <- coef(lasso_model_entry_all, s = best_lambda_entry_all)
# 
# # Training on each field -------------
# 
# # Create a data table to store field codes and their corresponding row counts
# field_counts <- entry_df[, .(count = .N), by = doc_field_code]
# 
# # Identify field codes with more than 10 rows
# large_fields <- field_counts[count > 10, doc_field_code]
# 
# # Create a list to store lasso coefficients for each field code
# entry_lasso_coefs_list <- list()
# 
# # Train entry lasso model for large fields
# for (field_code in large_fields) {
#   field_data <- entry_df[doc_field_code == field_code]
#   
#   x_train_lasso <- as.matrix(field_data[, .(oil_price_usd_per_bbl, m_capex_imputed, m_opex_imputed, depl)])
#   y_train_lasso <- field_data$n_new_wells
#   
#   # Check if capex_imputed or opex_imputed contain missing values
#   if (any(is.na(field_data$m_capex_imputed)) || any(is.na(field_data$m_opex_imputed))) {
#     # Set coefficients to zero if capex_imputed or opex_imputed contain missing values
#     entry_lasso_coefs_list[[field_code]] <- matrix(0, nrow = 1, ncol = ncol(x_train_lasso) + 1)
#     rownames(entry_lasso_coefs_list[[field_code]]) <- "(Intercept)"
#     colnames(entry_lasso_coefs_list[[field_code]]) <- c("(Intercept)", colnames(x_train_lasso))
#   } else {
#     # Check if capex_imputed or opex_imputed are 0
#     if (any(field_data$m_capex_imputed == 0) || any(field_data$m_opex_imputed == 0)) {
#       # Set coefficients to zero if capex_imputed or opex_imputed are 0
#       entry_lasso_coefs_list[[field_code]] <- matrix(0, nrow = 1, ncol = ncol(x_train_lasso) + 1)
#       rownames(entry_lasso_coefs_list[[field_code]]) <- "(Intercept)"
#       colnames(entry_lasso_coefs_list[[field_code]]) <- c("(Intercept)", colnames(x_train_lasso))
#     } else {
#       # Check if y_train_lasso is constant (either all zeros or a single unique non-zero value)
#       if (length(unique(y_train_lasso)) == 1 || (length(unique(y_train_lasso)) == 2 && 0 %in% unique(y_train_lasso))) {
#         # Assume 0 new wells for fields with constant y_train_lasso
#         entry_lasso_coefs_list[[field_code]] <- matrix(0, nrow = 1, ncol = ncol(x_train_lasso) + 1)
#         rownames(entry_lasso_coefs_list[[field_code]]) <- "(Intercept)"
#         colnames(entry_lasso_coefs_list[[field_code]]) <- c("(Intercept)", colnames(x_train_lasso))
#       } else {
#         # Train lasso model for fields with sufficient data points
#         lasso_model_entry <- glmnet(x_train_lasso, y_train_lasso, alpha = 1, lambda = seq(0.01, 1, by = 0.01))
#         cv_lasso_entry <- cv.glmnet(x_train_lasso, y_train_lasso, alpha = 1)
#         best_lambda_entry <- cv_lasso_entry$lambda.min
#         coef_lasso_entry <- coef(lasso_model_entry, s = best_lambda_entry)
#         
#         entry_lasso_coefs_list[[field_code]] <- coef_lasso_entry
#       }
#     }
#   }
# }
# 
# # Train entry lasso model for the remaining fields as a single group
# small_fields <- setdiff(unique(entry_df$doc_field_code), large_fields)
# small_fields_data <- entry_df[doc_field_code %in% small_fields]
# 
# x_train_lasso_small <- as.matrix(small_fields_data[, .(oil_price_usd_per_bbl, m_capex_imputed, m_opex_imputed, depl)])
# y_train_lasso_small <- small_fields_data$n_new_wells
# 
# lasso_model_entry_small <- glmnet(x_train_lasso_small, y_train_lasso_small, alpha = 1, lambda = seq(0.01, 1, by = 0.01))
# cv_lasso_entry_small <- cv.glmnet(x_train_lasso_small, y_train_lasso_small, alpha = 1)
# best_lambda_entry_small <- cv_lasso_entry_small$lambda.min
# coef_lasso_entry_small <- coef(lasso_model_entry_small, s = best_lambda_entry_small)
# 
# # Assign the coefficients from the small fields model to each small field code
# for (field_code in small_fields) {
#   entry_lasso_coefs_list[[field_code]] <- coef_lasso_entry_small
# }
# 
# # Convert the list of entry lasso coefficients to a data table
# entry_lasso_coefs <- rbindlist(lapply(names(entry_lasso_coefs_list), function(field_code) {
#   data.table(doc_field_code = field_code, t(as.matrix(entry_lasso_coefs_list[[field_code]])))
# }), fill = TRUE)
# 
# 
# # --------------------------------- EXIT LASSO MODEL ---------------------------------
# 
# x_train_exit_lasso <- as.matrix(entry_df[, .(oil_price_usd_per_bbl, m_opex_imputed, depl)])
# y_train_exit_lasso <- entry_df$well_exits
# 
# non_na_indices_lasso <- which(!is.na(y_train_exit_lasso))
# x_train_exit_lasso <- x_train_exit_lasso[non_na_indices_lasso, ]
# y_train_exit_lasso <- y_train_exit_lasso[non_na_indices_lasso]
# 
# lasso_model_exit <- glmnet(x_train_exit_lasso, y_train_exit_lasso, alpha = 1, lambda = seq(0.01, 1, by = 0.01))
# 
# cv_lasso_exit <- cv.glmnet(x_train_exit_lasso, y_train_exit_lasso, alpha = 1)
# best_lambda_exit <- cv_lasso_exit$lambda.min
# 
# coef_lasso_exit <- coef(lasso_model_exit, s = best_lambda_exit)
# 
# 
# 


