
## Tracey Mangin
## September 2, 2021
## Compile extraction outputs (site and county, include 2019) -- use individual rds files
## Updated 4/14/24 - MP
## Updated 4/24/24 - MP
## Updated 5/8/24 - MP


## libraries
library(data.table)
library(tidyverse)
library(purrr)
library(readxl)
library(openxlsx)
library(furrr)
library(future)
library(dplyr)
library(sf)
library(dplyr)


## model output location
save_external <- 1

## current date
cur_date              = Sys.Date()

## paths 
main_path     <- '/capstone/freshcair/meds-freshcair-capstone/'
main_path_external     <- '/capstone/freshcair/meds-freshcair-capstone/'
sp_data_path     <- paste0(main_path, "data/input/gis/")

## UPDATE THESE WITH NEW RUNS!!!!!
extraction_folder_path <- 'data/processed/extraction_2024-06-05_rf'
extraction_folder_name <- 'revision-setbacks/'
data_path  <-'data/processed/'

## health code paths
source_path   <- paste0(main_path, 'data/inputs/health/source_receptor_matrix/')
inmap_ex_path  <- paste0(main_path, "data/processed/extraction")

if(save_external == 1) {
  
  ## UPDATE THIS WITH NEW RUNS!!!!!
  extraction_path <- paste0('data/processed/extraction_2024-06-05_rf/revision-setbacks/')
  
  dir.create(paste0(main_path_external, 'data/processed/extraction_2024-06-04/academic-out/'), showWarnings = FALSE)
  compiled_save_path  <- paste0(main_path_external, 'data/processed/extraction_2024-06-05_rf/')

} else {
  
  extraction_path <- paste0(main_path, extraction_folder_path, extraction_folder_name)

  compiled_save_path  <- paste0(main_path, 'outputs/academic-out/extraction/extraction_', cur_date, '/')

}

## labor path
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'


## save path
field_save_path     = paste0(compiled_save_path, 'field-results/')
state_save_path     = paste0(compiled_save_path, 'state-results/')
## save path 
state_hs_save_path  = paste0(compiled_save_path, 'state-results/health_sens/')

county_save_path    = paste0(compiled_save_path, 'county-results/')
ct_save_path        = paste0(compiled_save_path, 'census-tract-results/')
health_county_save_path = paste0(compiled_save_path, 'health-county-results/')



## files
prod_file       <- "well_prod_m_processed.csv"
oil_price_file  <- 'oil_price_projections_revised.xlsx'
ghg_file        <- 'ghg_emissions_x_field_2018-2045.csv'
ghg_hist_file   <- 'indust_emissions_2000-2019.csv'

## create folder for outputs
dir.create(compiled_save_path, showWarnings = FALSE)
dir.create(field_save_path, showWarnings = FALSE) 
dir.create(state_save_path, showWarnings = FALSE)  
dir.create(state_hs_save_path, showWarnings = FALSE)  
dir.create(county_save_path, showWarnings = FALSE)
dir.create(ct_save_path, showWarnings = FALSE)
dir.create(health_county_save_path, showWarnings = FALSE)

## outputs should include:
## - county-level health
## - state production, health, and labor outputs


## read in files
## --------------------------------

## DAC and CES
dac_ces <- read_xlsx(paste0(main_path, 'data/inputs/health/ces3results.xlsx'))

ces_county <- dac_ces %>%
  dplyr::select("Census Tract", "California County") %>%
  dplyr::rename(census_tract = "Census Tract",
         county = "California County") %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) 

## income -- cencus tract
med_house_income <- fread(paste0(main_path, "data/inputs/gis/census-tract/ca-median-house-income.csv"), stringsAsFactors = F) 
med_house_income[, census_tract := paste0("0", GEOID)]
med_house_income <- med_house_income[, .(census_tract, estimate)]
setnames(med_house_income, "estimate", "median_hh_income")

## income -- county
county_income <- fread('data/inputs/gis/census-tract/ca-median-house-income-county.csv', stringsAsFactors = F) 
county_income <- county_income[, .(county, estimate)]
setnames(county_income, "estimate", "median_hh_income")


## monthly well production -- Updated - MP
well_prod <- fread('data/processed/well_prod_m_processed.csv', colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))
well_prod[, doc_field_code := as.numeric(doc_field_code)] # Added MP
well_prod[, doc_field_code := as.character(doc_field_code)] # Added MP

## historical GHG emissions, 2019
## --------------------------
hist_ghg <- fread('data/processed/indust_emissions_2000-2019.csv', header = T)

hist_ghg <- hist_ghg[segment %chin% c('Oil & Gas: Production & Processing') &
                       year == 2019, .(segment, unit, year, value)]

ghg_2019 <- as.numeric(hist_ghg[, value][1])



## ghg factors
ghg_factors = fread(file.path('data/processed/ghg_emissions_x_field_2018-2045.csv'), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]

## oil prices
oilpx_scens = read_excel(path = file.path('data/inputs/extraction/oil_price_projections_revised.xlsx'), 
                         sheet = "real") %>%
  dplyr::select(1:4) %>%
  setDT()
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
setDT(oilpx_scens)
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year >= 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

## multipliers

# 1. Import processed IMPLAN multipliers for the labor analysis and remove the statewide multipliers 
## NOTE: multipliers are per $1 million of output value
## NOTE: Prefix "ip." denotes a multiplier that has been replaced with the sample average because IMPLAN has no 
## data on extraction in this county 

total_multipliers_ext <- read_xlsx('data/processed/ica_multipliers_v2.xlsx', sheet = 'ica_total') %>% 
  filter((county != "Statewide" & segment == "extraction") | is.na(segment)==T) %>% 
  dplyr::rename(dire_emp_mult = direct_emp, 
         indi_emp_mult = indirect_emp, 
         indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, 
         indi_comp_mult = indirect_comp, 
         indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, 
         ip.indi_comp_mult = ip.indirect_comp, 
         ip.indu_comp_mult = ip.induced_comp)


## county information
## -------------------------------

## county lut
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

## field name look up 
fname_lut <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  unique() 

fname_lut$doc_field_code = as.character(fname_lut$doc_field_code)

## get relative county production (most recent year of nonzero production available for each field)
prod_x_county <- well_prod %>%
  left_join(county_lut) %>%
  dplyr::group_by(doc_field_code, doc_fieldname, year, adj_county_name) %>%
  dplyr::summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(doc_field_code, year) %>%
  dplyr::mutate(field_total = sum(prod, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(rel_prod = prod / field_total,
         rel_prod = ifelse(is.na(rel_prod) & prod == 0 & field_total == 0, 0, rel_prod)) %>%
  dplyr::filter(rel_prod > 0) %>%
  dplyr::group_by(doc_field_code) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::ungroup() %>%
  dplyr::select(doc_field_code, adj_county_name, rel_prod)

prod_x_county$doc_field_code = as.character(prod_x_county$doc_field_code)

## how many fields with positive prod?
# View(field_out[, c(prod = sum(total_prod_bbl, na.rm = T)), by = doc_field_code][V1 > 0])

## calculate 2019 production, emissions, revenue
init_prod <- well_prod %>%
  dplyr::filter(year == 2019) %>%
  dplyr::select(doc_field_code, doc_fieldname, year, OilorCondensateProduced) %>%
  dplyr::group_by(doc_field_code, doc_fieldname, year) %>%
  dplyr::summarise(total_prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  dplyr::ungroup()

setDT(init_prod)

# Added MP
init_prod$doc_field_code = as.character(init_prod$doc_field_code)

## merge with ghg factors
init_prod <- merge(init_prod, ghg_factors_2019,
                   by = c('doc_field_code', 'year'),
                   all.x = T)

# Added - MP
setDT(init_prod)

init_prod[, total_ghg_kgCO2e := total_prod_bbl * upstream_kgCO2e_bbl]


init_prod <- init_prod[, .(doc_field_code, year, total_prod_bbl, total_ghg_kgCO2e)]

## remove fields that do not ever produce oil and do not show up in results, as well as "Any Field)
init_prod <- init_prod[!doc_field_code %in% c("302", "502", "000")]

# init_prod_bbls_only <- init_prod[, .(doc_field_code, year, total_prod_bbl)]

## health data
## ------------------------------------------------

#  Load extraction source receptor matrix (srm) #######
n_cores <- availableCores() - 1
#plan(multisession, workers = n_cores, gc = TRUE)

#fields vector
fields_vector <- c(1:26)

#function
read_extraction <- function(buff_field){
  
  bfield <- buff_field
  
  nh3  <- read_csv(paste0("data/processed/extraction/census-tract/nh3/srm_nh3_field", bfield, ".csv", sep="")) %>% mutate(poll = "nh3")
  nox  <- read_csv(paste0("data/processed/extraction/census-tract/nox/srm_nox_field", bfield, ".csv", sep="")) %>% mutate(poll = "nox")
  pm25 <- read_csv(paste0("data/processed/extraction/census-tract/pm25/srm_pm25_field", bfield, ".csv", sep="")) %>% mutate(poll = "pm25")
  sox  <- read_csv(paste0("data/processed/extraction/census-tract/sox/srm_sox_field", bfield, ".csv", sep="")) %>% mutate(poll = "sox")
  voc  <- read_csv(paste0("data/processed/extraction/census-tract/voc/srm_voc_field", bfield, ".csv", sep="")) %>% mutate(poll = "voc")
  
  all_polls <- rbind(nh3, nox, pm25, sox, voc)
  
  all_polls$field = bfield
  
  tmp <- as.data.frame(all_polls) 
  
  return(tmp)
  
}

## build extraction srm
## SRM is concentration in microgram per meter cube (m3) for every tonne of pollutant emission

srm_all_pollutants_extraction <- future_map_dfr(fields_vector, read_extraction) %>% 
  bind_rows() %>%
  dplyr::rename(weighted_totalpm25 = totalpm25_aw)%>%
  dplyr::select(-totalpm25) %>%
  spread(poll, weighted_totalpm25) %>%
  dplyr::rename(weighted_totalpm25nh3 = nh3,
         weighted_totalpm25nox = nox,
         weighted_totalpm25pm25 = pm25,
         weighted_totalpm25sox = sox,
         weighted_totalpm25voc = voc,
         id = field)

future:::ClusterRegistry("stop")

## load county srm
## ---------------------------------------------------------------------

read_extraction_county <- function(buff_field){
  
  bfield <- buff_field
  
  nh3  <- read_csv(paste0(inmap_ex_path, "/county/nh3/srm_nh3_field", bfield, ".csv", sep="")) %>% mutate(poll = "nh3")
  nox  <- read_csv(paste0(inmap_ex_path, "/county/nox/srm_nox_field", bfield, ".csv", sep="")) %>% mutate(poll = "nox")
  pm25 <- read_csv(paste0(inmap_ex_path, "/county/pm25/srm_pm25_field", bfield, ".csv", sep="")) %>% mutate(poll = "pm25")
  sox  <- read_csv(paste0(inmap_ex_path, "/county/sox/srm_sox_field", bfield, ".csv", sep="")) %>% mutate(poll = "sox")
  voc  <- read_csv(paste0(inmap_ex_path, "/county/voc/srm_voc_field", bfield, ".csv", sep="")) %>% mutate(poll = "voc")
  
  all_polls <- rbind(nh3, nox, pm25, sox, voc)
  
  all_polls$field = bfield
  
  tmp <- as.data.frame(all_polls) 
  
  return(tmp)
  
}


srm_all_pollutants_extraction_c <- future_map_dfr(fields_vector, read_extraction_county) %>% 
  bind_rows() %>%
  dplyr::rename(weighted_totalpm25 = totalpm25_aw)%>%
  dplyr::select(-totalpm25) %>%
  spread(poll, weighted_totalpm25) %>%
  dplyr::rename(weighted_totalpm25nh3 = nh3,
         weighted_totalpm25nox = nox,
         weighted_totalpm25pm25 = pm25,
         weighted_totalpm25sox = sox,
         weighted_totalpm25voc = voc,
         id = field)



# (1.2) Calculate census tract ambient emissions for extraction  #######

#load and process cross-walk between fields and clusters -- removed sep="" 
extraction_field_clusters_10km <- read.csv('data/intermediate-zenodo/intermediate/extraction-model/extraction_fields_clusters_10km.csv') %>%
  dplyr::select(OUTPUT_FID, INPUT_FID) %>%
  dplyr::rename(id = OUTPUT_FID, input_fid = INPUT_FID)

# Removed sep=""
extraction_fields_xwalk <- foreign::read.dbf('data/intermediate-zenodo/intermediate/extraction-model/extraction_fields_xwalk_id.dbf') %>%
  dplyr::rename(input_fid = id, doc_field_code = dc_fld_)  


## Add Old Wilmington to xwalk
oldw_df <- data.table(NAME = "Old Wilmington (ABD)",
                      doc_field_code = "848",
                      input_fid = 191)

extraction_fields_xwalk <- rbind(extraction_fields_xwalk, oldw_df)

extraction_xwalk <- extraction_field_clusters_10km %>%
  left_join(extraction_fields_xwalk, by = c("input_fid")) 



## (2.1) Load demographic data
# Disadvantaged community definition
ces3 <- read.csv('data/inputs/health/ces3_data.csv', stringsAsFactors = FALSE) %>%
  dplyr::select(census_tract, population, CES3_score, disadvantaged) %>%
  dplyr::mutate(census_tract = paste0("0", census_tract, sep="")) %>%
  as.data.table()

## add counties
ces3 <- merge(ces3, ces_county, 
              by = "census_tract")

## remove population
ces3[, population := NULL]

# Population and incidence
ct_inc_pop_45 <- fread('data/processed/ct_inc_45.csv', stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  dplyr::select(ct_id, lower_age, upper_age, year, pop, incidence_2015) %>%
  as.data.table()

## census tract, population > 29
ct_pop_time <- ct_inc_pop_45 %>%
  dplyr::filter(lower_age > 29) %>%
  dplyr::group_by(ct_id, year) %>%
  dplyr::summarise(ct_pop = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  dplyr::rename(census_tract = ct_id) %>%
  as.data.table()

## dac share over time
ct_pop_time <- merge(ct_pop_time, ces3, 
                   by = c("census_tract"),
                   all.x = T)

## census-tract level population-weighted incidence rate (for age>29)
ct_inc_pop_45_weighted <- ct_inc_pop_45 %>%
  filter(lower_age > 29) %>%
  dplyr::group_by(ct_id, year) %>%
  dplyr::mutate(ct_pop = sum(pop, na.rm = T),
         share = pop/ct_pop,
         weighted_incidence = sum(share * incidence_2015, na.rm = T)) %>%
  dplyr::summarise(weighted_incidence = unique(weighted_incidence),
            pop = unique(ct_pop)) %>%
  ungroup() %>%
  as.data.table()

## county level population-weighted incidence rate (for age>29)
county_inc_pop_45_weighted <- ct_inc_pop_45 %>%
  left_join(ces_county, by = c("ct_id" = "census_tract")) %>%
  filter(lower_age > 29) %>%
  dplyr::group_by(county, lower_age, upper_age, year, incidence_2015) %>%
  dplyr::summarise(pop = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  dplyr::group_by(county, year) %>%
  dplyr::mutate(county_pop = sum(pop, na.rm = T),
         share = pop/county_pop,
         weighted_incidence = sum(share * incidence_2015, na.rm = T)) %>%
  dplyr::summarise(weighted_incidence = unique(weighted_incidence),
            pop = unique(county_pop)) %>%
  ungroup() %>%
  as.data.table()


## county pop
county_pop <- ct_inc_pop_45_weighted %>%
  left_join(ces_county, by = c('ct_id' = 'census_tract')) %>%
  dplyr::group_by(county, year) %>%
  dplyr::summarise(county_pop = sum(pop)) %>%
  ungroup() %>%
  filter(!is.na(county))

## DAC proportion
county_dac <- dcast(ct_pop_time, county + census_tract + year ~ disadvantaged, value.var = "ct_pop")
setDT(county_dac)
county_dac <- county_dac[!is.na(county)]
county_dac[, Yes := fifelse(is.na(Yes), 0, Yes)]
county_dac[, No := fifelse(is.na(No), 0, No)]
county_dac[, total_pop := No + Yes]
county_dac[, dac_share := Yes / total_pop]
county_dac <- county_dac[, .(dac_share = weighted.mean(dac_share, total_pop, na.rm = T)), by = .(county, year)]

## county geoids
county_ids <- st_read('data/inputs/gis/CA_counties_noislands/CA_Counties_TIGER2016_noislands.shp') %>%
  st_transform(crs=3310) %>%
  dplyr::select(GEOID, county = NAME) %>%
  st_drop_geometry() %>%
  unique()


## Coefficients from Krewski et al (2009) for mortality impact
beta <- 0.00582
se <- 0.0009628

## for monetary mortality impact - growth in income for use in WTP function
growth_rates <- read.csv('data/inputs/health/growth_rates.csv', stringsAsFactors = FALSE) %>%
  filter(year > 2018) %>%
  dplyr::mutate(growth = ifelse(year == 2019, 0, growth_2030),
         cum_growth = cumprod(1 + growth)) %>%
  dplyr::select(-growth_2030, -growth) %>%
  as.data.table()

#Parameters for monetary health impact
VSL_2015 <- 8705114.25462459
VSL_2019 <- VSL_2015 * 107.8645906/100 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
income_elasticity_mort <- 0.4
discount_rate <- 0.03

future_WTP <- function(elasticity, growth_rate, WTP){
  return(elasticity * growth_rate * WTP + WTP) 
}

## ------------------------------------------------------------
## process all scenarios
## ------------------------------------------------------------

## read in files
field_files_to_process <- list.files(paste0(extraction_path, 'field-out/'))


# set start time -----
start_time <- Sys.time()
print(paste("Starting extraction compiling at ", start_time))

# cores
doParallel::registerDoParallel(cores = n_cores)

for (i in 1:length(field_files_to_process)) {
  field_file_name <- field_files_to_process[i]
  
  field_scen_out <- readRDS(paste0(extraction_path, 'field-out/', field_file_name))

  # ## check fields in 2019 vs outputs
  # ## ------------------------------------------------------
  # anti_join(init_prod %>% select(doc_field_code), field_scen_out %>% select(doc_field_code) %>% unique())
  # 
  # check_projection <- anti_join(field_scen_out %>% select(doc_field_code) %>% unique(), init_prod %>% select(doc_field_code))
  # 
  # View(field_scen_out[doc_field_code %chin% check_projection[, doc_field_code] & total_prod_bbl > 0])
  # ## English Colony (ABD) has existing well production; Pacoima (ABD) has new well production
  # 
  ## prepare projection outputs
  site_out <- field_scen_out[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                            setback_scenario, setback_existing, prod_quota_scenario, excise_tax_scenario, doc_field_code,
                            year, total_prod_bbl, total_ghg_kgCO2e)]
  
  full_site_df <- expand.grid(scen_id = unique(site_out$scen_id),
                              doc_field_code = unique(c(site_out$doc_field_code, init_prod$doc_field_code)),
                              year = seq(2019, 2045, 1))
  
  setDT(full_site_df)
  
  scen_info <- distinct(site_out[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                     setback_scenario, setback_existing, prod_quota_scenario, excise_tax_scenario)])
  
  scenario_id_tmp <- scen_info[ , scen_id]
  
  full_site_df <- merge(full_site_df, scen_info,
                        by = c("scen_id"),
                        all.x = T)
  
  ## add scenario information using id
  full_site_df <- merge(full_site_df, site_out,
                        by = c("scen_id", 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 
                               'ccs_scenario', 'setback_scenario', 'setback_existing', 'prod_quota_scenario', 'excise_tax_scenario',  
                               'year', 'doc_field_code'),
                        all.x = T)
  
  # Added - MP
  full_site_df[, total_ghg_kgCO2e := site_out[match(paste(full_site_df$scen_id, full_site_df$doc_field_code, full_site_df$year), 
                                                    paste(site_out$scen_id, site_out$doc_field_code, site_out$year)), 
                                              total_ghg_kgCO2e], on = "site_out"]
  
  ## 2019
  full_site_df_2019 <- full_site_df[year == 2019]
  
  full_site_df_2019 <- merge(full_site_df_2019, fname_lut,
                             by = c("doc_field_code"),
                             all.x = T)
  
  # Added doc_fieldname in the merge - MP
  full_site_df_2019 <- merge(full_site_df_2019[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                   setback_scenario, setback_existing, prod_quota_scenario, excise_tax_scenario, doc_field_code,
                                                   doc_fieldname, year)], init_prod,
                             by = c("doc_field_code", "year"),
                             all.x = T)
  
  full_site_df_2019[,':=' (total_prod_bbl = fifelse(is.na(total_prod_bbl), 0, total_prod_bbl))]
  full_site_df_2019[,':=' (total_ghg_kgCO2e = fifelse(is.na(total_ghg_kgCO2e), 0, total_ghg_kgCO2e))] # CHECK
  
  setcolorder(full_site_df_2019, c("scen_id", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                              "setback_scenario", "setback_existing", "prod_quota_scenario", "excise_tax_scenario", "doc_field_code", 
                              "doc_fieldname", "year", "total_prod_bbl", "total_ghg_kgCO2e"))
  
  
  ## now do projection
  full_site_df_proj <- full_site_df[year > 2019]
  
  full_site_df_proj <- merge(full_site_df_proj, fname_lut,
                             by = c("doc_field_code"),
                             all.x = T)
  
  ## bind 2019 to projected
  full_site_out <- rbind(full_site_df_2019, full_site_df_proj)
  
  
  ## add oil prices (including 2019... 2020 real dollars)
  full_site_out <- merge(full_site_out, oilpx_scens,
                         by = c("oil_price_scenario", "year"),
                         all.x = T)
  
  full_site_out[, revenue := total_prod_bbl * oil_price_usd_per_bbl]
  
  full_site_out[, oil_price_usd_per_bbl := NULL]
  
  setcolorder(full_site_out, c("scen_id", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                                   "setback_scenario", "setback_existing", "prod_quota_scenario", "excise_tax_scenario", "doc_field_code", 
                                   "doc_fieldname", "year", "total_prod_bbl", "revenue", "total_ghg_kgCO2e"))
  
  setorder(full_site_out, "scen_id", "doc_field_code", "year")
  
  ## save site level output for production 
  saveRDS(full_site_out, paste0(field_save_path, scenario_id_tmp, "_field_results.rds"))
  
  t <- readRDS("/capstone/freshcair/meds-freshcair-capstone/data/processed/extraction_2024-05-14/field-results/high oil price-no_setback-no quota-carbon_target_90perc_reduction-no ccs-low innovation-no tax-0_field_results.rds")
  
  # new_t <- t%>% 
  #   group_by(year) %>% 
  #   summarise(prod = sum(total_prod_bbl,na.rm=T)) ADDED
  ## now do county-level, add labor impacts
  ## ---------------------------------------------------------
  
  county_out <- merge(full_site_out, prod_x_county,
                      by = c("doc_field_code"),
                      all.x = T,
                      allow.cartesian = T)
  
  setcolorder(county_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                            'setback_scenario', 'setback_existing', 'prod_quota_scenario', 'excise_tax_scenario', 
                            'year', 'doc_field_code', 'doc_fieldname', 'total_prod_bbl', 'adj_county_name', 'rel_prod'))
  
  ## summarise at the county level
  county_out[, county_prod_bbl := total_prod_bbl * rel_prod]
  county_out[, county_ghg_kgCO2e := total_ghg_kgCO2e * rel_prod]
  
  # Added - MP
  county_out <- county_out[, .(total_county_bbl = sum(county_prod_bbl, na.rm = TRUE),
                                   total_county_ghg_kgCO2e = sum(county_ghg_kgCO2e, na.rm = TRUE)), 
                               by = .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                      ccs_scenario, setback_scenario, setback_existing, prod_quota_scenario, excise_tax_scenario,
                                      year, adj_county_name)]
  
  # county_out <- county_out[, .(total_county_bbl = sum(county_prod_bbl),
  #                              total_county_ghg_kgCO2e = sum(county_ghg_kgCO2e)), by = .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
  #                                                                                        ccs_scenario, setback_scenario, setback_existing, prod_quota_scenario, excise_tax_scenario,
  #                                                                                        year, adj_county_name)]
  
  ## add oil price
  county_out <- merge(county_out, oilpx_scens,
                      by = c("oil_price_scenario", "year"),
                      all.x = T)
  
  county_out[, revenue := total_county_bbl * oil_price_usd_per_bbl]
  
  setcolorder(county_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                            'setback_scenario', 'setback_existing', 'prod_quota_scenario', 'excise_tax_scenario', 
                            'year', 'adj_county_name', 'total_county_bbl', 'oil_price_usd_per_bbl', 'revenue'))
  
  setorder(county_out, "scen_id", "adj_county_name", "year")
  
  setnames(county_out, "adj_county_name", "county")
  

  # Part A: Compute total impacts by county, year, and scenario 
   ## 1. Import processed IMPLAN multipliers for the labor analysis and remove the statewide multipliers 
   ## 2. Join multipliers to output from the energy modeling team by county and compute county level labor impacts
   ## 3. Save extraction output
  
  
  # Part A: file with total impacts by county, year, and scenario
  
  # 3. Join multipliers to output from the energy modeling team by county and compute county level labor impacts 
  
  ## Note: c. indicates county 
  ## c.dire_emp : Direct employment impact in either the refining or extraction segment 
  ## c.dire_comp: Direct compensation impact in either the refining or extraction segment 
  ## c.indi.emp: Indirect employment impact in either the refining or extraction segment
  ## c.indi.comp: Indirect compensation impact in either the refining or extraction segment
  ## c.indu.emp: Induced employment impact in either the refining or extraction segment
  ## c.indu.comp: Induced compensation impact in either the refining or extraction segment
  ## To get total impact for a county: add direct + indirect + induced 
  
  county_out <- merge(county_out, total_multipliers_ext,
                      by = "county",
                      all.x = T)
  
  county_out[, proj_prod := sum(total_county_bbl, na.rm=TRUE), by = .(scen_id, county)]
  county_out <- county_out[proj_prod > 0]
  county_out[, proj_prod := NULL]
  
  county_out[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult, 
                     c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult, 
                     c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
                     c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult, 
                     c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult, 
                     c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]
  
  county_out[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
                     total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]
  
  ## merge with county dac proportion
  county_out <- merge(county_out, county_dac,
                      by = c("county", "year"),
                      all.x = T)
  
  county_out <- merge(county_out, county_income,
                      by = "county",
                      all.x = T)
  
  county_out <- merge(county_out, county_pop,
                      by = c("county", "year"),
                      all.x = T)
  
  ## 
  county_out <- county_out[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                               setback_scenario, setback_existing, prod_quota_scenario, excise_tax_scenario, county, dac_share, median_hh_income,
                               year, county_pop, total_county_bbl, total_county_ghg_kgCO2e, revenue,
                               c.dire_emp, c.indi_emp, c.indu_emp, c.dire_comp, c.indi_comp, c.indu_comp, total_emp, total_comp)]
  
 
  
  
  ## save county outputs (labor, production, and revenue)
  saveRDS(county_out, paste0(county_save_path, scenario_id_tmp, "_county_results.rds"))
  
  ## HEALTH IMPACTS: calculate census level health impacts
  ## -------------------------------------------------------------
  
  # Added MP
  extraction_xwalk$doc_field_code <- as.character(extraction_xwalk$doc_field_code)
  extraction_xwalk$doc_field_code <- sub("^0+", "", extraction_xwalk$doc_field_code)
  
  ## merge extraction production scenarios with extraction cluster 
  health_site_out <- merge(full_site_out, extraction_xwalk,
                           by = "doc_field_code",
                           all.x = T)
  
  ## summarize extraction production per cluster
  total_clusters <- health_site_out[, .(total_prod_bbl = sum(total_prod_bbl, na.rm=TRUE)), by = .(id, year, scen_id, oil_price_scenario,
                                                                                      carbon_price_scenario, ccs_scenario, setback_scenario,
                                                                                      setback_existing, excise_tax_scenario)] 
  
  ## calculate air pollution using emission factors
  ## emission factors are in kg per bbl
  ## divided by 1000 gives you emissions in tonne
  
  total_clusters <- total_clusters %>%
    mutate(nh3 = total_prod_bbl * 0.00061 / 1000,
           nox = total_prod_bbl * 0.04611 / 1000,
           pm25 = total_prod_bbl * 0.00165 / 1000,
           sox = total_prod_bbl * 0.01344 / 1000,
           voc = total_prod_bbl * 0.02614 / 1000)
  
  total_clusters <- total_clusters %>%
    dplyr::right_join(srm_all_pollutants_extraction, by = c("id")) %>%
    dplyr::mutate(tot_nh3 = weighted_totalpm25nh3 * nh3,
           tot_nox = weighted_totalpm25nox * nox,
           tot_sox = weighted_totalpm25sox * sox,
           tot_pm25 = weighted_totalpm25pm25 * pm25,
           tot_voc = weighted_totalpm25voc * voc,
           total_pm25 = tot_nh3 + tot_nox + tot_pm25 + tot_sox + tot_voc,
           prim_pm25 = tot_pm25)
  
  ct_exposure <- total_clusters %>%
    ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012 
    ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
    dplyr::mutate(GEOID = ifelse(GEOID == "06037137000", "06037930401", GEOID)) %>%
    dplyr::group_by(GEOID, year, scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, setback_existing, excise_tax_scenario) %>%
    dplyr::summarise(total_pm25 = sum(total_pm25, na.rm = T), 
              prim_pm25 = sum(prim_pm25, na.rm = T)) %>%
    ungroup() %>%
    dplyr::rename(census_tract = GEOID) %>%
    dplyr::select(scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, setback_existing, excise_tax_scenario,
           census_tract, year, total_pm25)
  
  ## add ces score, income, and dac
  ct_exposure <- merge(ct_exposure, ces3[, .(census_tract, CES3_score, disadvantaged)],
                       by = c("census_tract"),
                       all.x = T)
  
  ## add income
  ct_exposure <- merge(ct_exposure, med_house_income,
                       by = c("census_tract"),
                       all.x = T) 
  
  
  setorder(ct_exposure, "census_tract", "year")
  
  setcolorder(ct_exposure, c("scen_id", "oil_price_scenario", "carbon_price_scenario", "ccs_scenario", "setback_scenario", "setback_existing",
                             "excise_tax_scenario", "census_tract", "disadvantaged", "CES3_score", "median_hh_income", "year", "total_pm25"))
  
  
  
  
  ## save census tract outputs (pm2.5 levels, mortality level, cost)
  saveRDS(ct_exposure, paste0(ct_save_path, scenario_id_tmp, "_ct_results.rds"))
  
  # remove(ct_exposure, total_clusters)
  
  ## ------------------------------------------------------------------
  ## Sensitivity analysis: create health outputs at county level
  ## ------------------------------------------------------------------
  
  ## summarize extraction production per cluster
  total_clusters_sens <- health_site_out[, .(total_prod_bbl = sum(total_prod_bbl, na.rm=TRUE)), by = .(id, year, scen_id, oil_price_scenario,
                                                                                      carbon_price_scenario, ccs_scenario, setback_scenario,
                                                                                      setback_existing, excise_tax_scenario)]

  ## calculate air pollution using emission factors
  total_clusters_sens <- total_clusters_sens %>%
    dplyr::mutate(nh3 = total_prod_bbl * 0.00061 / 1000,
           nox = total_prod_bbl * 0.04611 / 1000,
           pm25 = total_prod_bbl * 0.00165 / 1000,
           sox = total_prod_bbl * 0.01344 / 1000,
           voc = total_prod_bbl * 0.02614 / 1000)

  total_clusters_sens <- total_clusters_sens %>%
    right_join(srm_all_pollutants_extraction_c, by = c("id")) %>%
    dplyr::mutate(tot_nh3 = weighted_totalpm25nh3 * nh3,
           tot_nox = weighted_totalpm25nox * nox,
           tot_sox = weighted_totalpm25sox * sox,
           tot_pm25 = weighted_totalpm25pm25 * pm25,
           tot_voc = weighted_totalpm25voc * voc,
           total_pm25 = tot_nh3 + tot_nox + tot_pm25 + tot_sox + tot_voc,
           prim_pm25 = tot_pm25)

  ct_exposure_sens <- total_clusters_sens %>%
    dplyr::group_by(GEOID, year, scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, setback_existing, excise_tax_scenario) %>%
    dplyr::summarise(total_pm25 = sum(total_pm25, na.rm = T),
              prim_pm25 = sum(prim_pm25, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, setback_existing, excise_tax_scenario,
           GEOID, year, total_pm25)

  ## add county ids
  ct_exposure_sens <- merge(ct_exposure_sens, county_ids,
                       by = c("GEOID"),
                       all.x = T)
  
  ## add dac
  ct_exposure_sens <- merge(ct_exposure_sens, county_dac,
                            by = c("county", "year"),
                            all.x = T)


  setorder(ct_exposure_sens, "GEOID", "year")

  setcolorder(ct_exposure_sens, c("scen_id", "oil_price_scenario", "carbon_price_scenario", "ccs_scenario", "setback_scenario", "setback_existing",
                             "excise_tax_scenario", "GEOID", "county", "year", "dac_share", "total_pm25"))

  print(colnames(ct_exposure_sens))
  ## save census tract sensitivity outputs (pm2.5 levels, mortality level, cost)
  saveRDS(ct_exposure_sens, paste0(health_county_save_path, scenario_id_tmp, "_ctc_results.rds"))

  

  ## state outputs
  ## -------------------------------------
  
  state_out <- county_out[, lapply(.SD, sum, na.rm = T), .SDcols = c("county_pop", "total_county_bbl", "revenue",
                                                                     "total_county_ghg_kgCO2e",
                                                                     "c.dire_emp", "c.indi_emp", 
                                                                     "c.indu_emp", "c.dire_comp", 
                                                                     "c.indi_comp", "c.indu_comp",
                                                                     "total_emp", "total_comp"), by = .(scen_id, oil_price_scenario, innovation_scenario,
                                                                                                           carbon_price_scenario, ccs_scenario,
                                                                                                           setback_scenario, setback_existing, prod_quota_scenario, excise_tax_scenario, year)]
  
  setnames(state_out, c("county_pop", "total_county_bbl", "revenue",  "total_county_ghg_kgCO2e"), c("state_pop", "total_state_bbl", "total_state_revenue", "total_state_ghg_kgCO2"))
                                                
  
  ## save state outputs (labor, production, and revenue)
  saveRDS(state_out, paste0(state_save_path, scenario_id_tmp, "_state_results.rds"))
  
}

elapsed_time <- Sys.time() - start_time
print(elapsed_time)

## -------------------------------------------------------------
## now calculate relative health outputs
## add to ct_results
## add state values to state_results
## -------------------------------------------------------------

## get BAU vals for all three oil price scenarios

bau_scens <- c('reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_ct_results.rds',
               'low oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_ct_results.rds',
               'high oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_ct_results.rds',
               'reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax-1_ct_results.rds',
               'low oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-1_ct_results.rds',
               'high oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-1_ct_results.rds')

bau_out <- list()

for(i in 1:length(bau_scens)) {
  scen_tmp <- bau_scens[i]
  
  bau_tmp <- readRDS(paste0(ct_save_path, scen_tmp)) %>%
    dplyr::rename(bau_total_pm25 = total_pm25) %>%
    dplyr::select(oil_price_scenario, setback_existing, census_tract, year, bau_total_pm25) %>% 
    as.data.table()
  
  bau_out[[i]] <- bau_tmp
  
}

bau_out <- bind_rows(bau_out)


# ## extraction pm25 BAU
# extraction_BAU <- readRDS(paste0(ct_save_path, "reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_ct_results.rds")) %>%
#   rename(bau_total_pm25 = total_pm25) %>%
#   select(census_tract, year, bau_total_pm25) %>% 
#   as.data.table()

## scenarios
scenarios_to_process <- str_remove_all(field_files_to_process, "_field.rds")


for (i in 1:length(scenarios_to_process)) {
  print(i)
  scen_file_name <- scenarios_to_process[i]
  
  ct_scen_out <- readRDS(paste0(ct_save_path, scen_file_name, "_ct_results.rds"))
  setDT(ct_scen_out)
  
  ## calculate delta pm 2.5
  delta_extraction <- merge(ct_scen_out, bau_out,
                            by = c("oil_price_scenario", "setback_existing", "census_tract", "year"),
                            all.x = T)
  
  delta_extraction[, delta_total_pm25 := total_pm25 - bau_total_pm25]
  
  
  ## Merge demographic data to pollution scenarios
  ct_incidence <- delta_extraction %>%
    # left_join(ces3, by = c("census_tract")) %>%
    right_join(ct_inc_pop_45_weighted, by = c("census_tract" = "ct_id", "year" = "year")) %>%
    # remove census tracts that are water area only tracts (no population)
    drop_na(scen_id) %>% 
    as.data.table()
  
  ## Calculate mortality impact ######################################
  
  #Mortality impact fold adults (>=29 years old)
  ct_incidence <- ct_incidence[, mortality_delta := ((exp(beta * delta_total_pm25) - 1)) * weighted_incidence * pop]
  ct_incidence <- ct_incidence[, mortality_level := ((exp(beta * total_pm25) - 1)) * weighted_incidence * pop]

  ## Monetizing mortality ############################################
  
  ## Calculate the cost per premature mortality
  ct_incidence <- ct_incidence %>%
    dplyr::mutate(VSL_2019 = VSL_2019)%>%
    left_join(growth_rates, by = c("year" = "year"))%>%
    dplyr::mutate(VSL = future_WTP(income_elasticity_mort, 
                            (cum_growth-1),
                            VSL_2019),
           cost_2019 = mortality_delta * VSL_2019,
           cost = mortality_delta * VSL)%>%
    dplyr::group_by(year) %>%
    dplyr::mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
           cost_PV = cost/((1+discount_rate)^(year-2019))) %>%
    ungroup()
  
  ## final census tract level health outputs
  ct_incidence <- ct_incidence %>%
    dplyr::select(scen_id, census_tract, CES3_score, disadvantaged, median_hh_income, year, weighted_incidence, pop, total_pm25, bau_total_pm25, delta_total_pm25, 
           mortality_delta, mortality_level, cost_2019, cost, cost_2019_PV, cost_PV) %>%
    as.data.table()
  
  ## resave the census tract data
  saveRDS(ct_incidence, paste0(ct_save_path, scen_file_name, "_ct_results.rds"))
  
  ## ----------------------------------------------------------------------------
  ## calculate state values, read in state value, resave with mortality values
  ## ----------------------------------------------------------------------------
  
  ct_incidence_state <- ct_incidence[, lapply(.SD, sum, na.rm = T), .SDcols = c("mortality_delta", "mortality_level", 
                                                                                "cost_2019", "cost", 
                                                                                "cost_2019_PV", "cost_PV"), by = .(scen_id, year)]
  
  
  ct_pm_state <- ct_incidence[, lapply(.SD, mean, na.rm = T), .SDcols = c("total_pm25", "delta_total_pm25"), by = .(scen_id,  year)]
  setnames(ct_pm_state, c("total_pm25", "delta_total_pm25"), c("mean_total_pm25", "mean_delta_total_pm25"))
  
  ct_incidence_state <- merge(ct_incidence_state, ct_pm_state,
                              by = c("scen_id", "year"))
  
  
  
  state_out <- readRDS(paste0(state_save_path, scen_file_name, "_state_results.rds"))
  setDT(state_out)
  
  state_out <- merge(state_out, ct_incidence_state,
                     by = c("scen_id", "year"),
                     all.x = T)
  
  ## resave state outputs
  saveRDS(state_out, paste0(state_save_path, scen_file_name, "_state_results.rds"))
  
}
#########################################################
## Repeat for sensitivity
## ------------------------------------------------------
  
  ## get BAU vals for all three oil price scenarios
  
  bau_scens_sens <- c('reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_ctc_results.rds',
                      'low oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_ctc_results.rds',
                      'high oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_ctc_results.rds',
                      'reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax-1_ctc_results.rds',
                      'low oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-1_ctc_results.rds',
                      'high oil price-no_setback-no quota-price floor-no ccs-low innovation-no tax-1_ctc_results.rds')
  
  bau_out_sens <- list()
  
  for(i in 1:length(bau_scens_sens)) {
    
    scen_tmp <- bau_scens_sens[i]
    
    bau_tmp <- readRDS(paste0(health_county_save_path, scen_tmp)) %>%
      dplyr::rename(bau_total_pm25 = total_pm25) %>%
      dplyr::select(oil_price_scenario, setback_existing, GEOID, year, bau_total_pm25) %>% 
      as.data.table()
    
    bau_out_sens[[i]] <- bau_tmp
    
  }
  
  bau_out_sens <- bind_rows(bau_out_sens)
  
  ## scenarios
  # scenarios_to_process_sens <- str_remove_all(field_files_to_process, "_ctc_field.rds")
  
  for (i in 1:length(scenarios_to_process)) {
    print(i)
    
    scen_file_name_sens <- scenarios_to_process[i]
    ctc_scen_out <- readRDS(paste0(health_county_save_path, scen_file_name_sens, "_ctc_results.rds"))
    print(colnames(ctc_scen_out))
    setDT(ctc_scen_out)
    ## calculate delta pm 2.5 -- ISSUE IS HERE
    delta_extraction_sens <- merge(ctc_scen_out, bau_out_sens,
                              by = c("oil_price_scenario", "setback_existing", "GEOID", "year"),
                              all.x = T)

    delta_extraction_sens[, delta_total_pm25 := total_pm25 - bau_total_pm25]
    
    
    ## Merge demographic data to pollution scenarios
    ctc_incidence <- delta_extraction_sens %>%
      # left_join(ces3, by = c("census_tract")) %>%
      right_join(county_inc_pop_45_weighted, by = c("county", "year")) %>%
      # remove census tracts that are water area only tracts (no population)
      drop_na(scen_id) %>% 
      as.data.table()
    print("check3")
    
    ## Calculate mortality impact ######################################
    
    #Mortality impact fold adults (>=29 years old)
    ctc_incidence <- ctc_incidence[, mortality_delta := ((exp(beta * delta_total_pm25) - 1)) * weighted_incidence * pop]
    ctc_incidence <- ctc_incidence[, mortality_level := ((exp(beta * total_pm25) - 1)) * weighted_incidence * pop]
    
    ## Monetizing mortality ############################################
    
    ## Calculate the cost per premature mortality
    ctc_incidence <- ctc_incidence %>%
      dplyr::mutate(VSL_2019 = VSL_2019)%>%
      left_join(growth_rates, by = c("year" = "year"))%>%
      dplyr::mutate(VSL = future_WTP(income_elasticity_mort, 
                              (cum_growth-1),
                              VSL_2019),
             cost_2019 = mortality_delta * VSL_2019,
             cost = mortality_delta * VSL)%>%
      dplyr::group_by(year) %>%
      dplyr::mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
             cost_PV = cost/((1+discount_rate)^(year-2019))) %>%
      ungroup()
    
    ## final census tract level health outputs
    ctc_incidence <- ctc_incidence %>%
      dplyr::select(scen_id, GEOID, county, year, dac_share, weighted_incidence, pop, total_pm25, bau_total_pm25, delta_total_pm25, 
             mortality_delta, mortality_level, cost_2019, cost, cost_2019_PV, cost_PV) %>%
      as.data.table()
    
    ## resave the census tract data
    saveRDS(ctc_incidence, paste0(health_county_save_path, scen_file_name_sens, "_ctc_results.rds"))
    
    ## ----------------------------------------------------------------------------
    ## calculate state values, read in state value, resave with mortality values
    ## ----------------------------------------------------------------------------
    
    ctc_incidence_state <- ctc_incidence[, lapply(.SD, sum, na.rm = T), .SDcols = c("mortality_delta", "mortality_level", 
                                                                                  "cost_2019", "cost", 
                                                                                  "cost_2019_PV", "cost_PV"), by = .(scen_id, year)]
    
    
    ctc_pm_state <- ctc_incidence[, lapply(.SD, mean, na.rm = T), .SDcols = c("total_pm25", "delta_total_pm25"), by = .(scen_id,  year)]
    setnames(ctc_pm_state, c("total_pm25", "delta_total_pm25"), c("mean_total_pm25", "mean_delta_total_pm25"))
    
    ctc_incidence_state <- merge(ctc_incidence_state, ctc_pm_state,
                                by = c("scen_id", "year"))
    
    
    
    state_out <- readRDS(paste0(state_save_path, scen_file_name_sens, "_state_results.rds"))
    setDT(state_out)
    
    state_out <- state_out %>%
      dplyr::select(scen_id:total_comp) %>%
      as.data.table()
    
    state_out <- merge(state_out, ctc_incidence_state,
                       by = c("scen_id", "year"),
                       all.x = T)
    
    ## resave state outputs
    saveRDS(state_out, paste0(state_hs_save_path, scen_file_name_sens, "_state_results_health.rds"))
    
  
  
}













## segment, year, doc_field_code, doc_field_name, oil_price_scenario, innovation_scenario,	carbon_price_scenario,	ccs_scenario,
## setback_scenario,	prod_quota_scenario,	excise_tax_scenario,	production_bbl,	oil_price_usd_per_bbl_real, revenue,
## refining specific: demand_scenario, refining scenario, site_id, refinery_name, location, region, cluster, crude_e_total_bbl_net_exp

