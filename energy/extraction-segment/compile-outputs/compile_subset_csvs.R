## Tracey Mangin
## September 16, 2021
## create csv versions of outputs
## Updated 4/14/24 - MP

setwd('/capstone/freshcair/meds-freshcair-capstone')

## libraries
library(data.table)

external_save <- 1

## paths
main_path     <- '/capstone/freshcair/meds-freshcair-capstone/'
academic_out_path <- file.path(main_path, 'processed') 
input_path        <- file.path(main_path, 'data/processed') 

## read in saved rds files - updates as needed
extraction_folder = 'extraction_2024-05-08/'

external_path <- '/capstone/freshcair/meds-freshcair-capstone/data/processed' 

## get correct path

if(external_save == 1) {
  
  compiled_path = paste0(main_path, 'data/processed/extraction_2024-05-08/')
} else {
  
  compiled_path  = paste0(academic_out_path, extraction_folder)
  
}

## sub folders
field_path     = paste0(compiled_path, 'field-results/')
state_path     = paste0(compiled_path , 'state-results/')
state_hs_path  = paste0(compiled_path , 'state-results/health_sens/')
county_path    = paste0(compiled_path, 'county-results/')
ct_path        = paste0(compiled_path, 'census-tract-results/')
ct_hs_path     = paste0(compiled_path, 'health-county-results/')


## files
scen_file <- 'scenario_id_list_targets_finalv2.csv'

## load files
scen_list <- fread(file.path('data/processed/scenario_id_list_targets_v3.csv'), header = T) 

# Added - MP
setback_scenarios <- c("no_setback", "setback_3200ft")
subset_list <- scen_list[scen_list$setback_scenario %in% setback_scenarios]

# subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, .(scen_id, target, target_policy)]


# ---- subsetting for no setback policy MG

### MP TESTING START ---------- ---------- ---------- ---------- ----------

# Load the scen_list data table
unique_scen_ids <- unique(subset_ids$scen_id)

# Initialize a list to store missing scenarios
missing_scenarios <- list()

# Loop through the unique scen_id values and check for missing RDS files
for (scen_id in unique_scen_ids) {
  ct_file <- paste0(ct_path, scen_id, '_ct_results.rds')
  ct_hs_file <- paste0(ct_hs_path, scen_id, '_ctc_results.rds')
  county_file <- paste0(county_path, scen_id, '_county_results.rds')
  state_file <- paste0(state_path, scen_id, '_state_results.rds')
  state_hs_file <- paste0(state_hs_path, scen_id, '_state_results_health.rds')
  
  if (!file.exists(ct_file) || !file.exists(ct_hs_file) || !file.exists(county_file) ||
      !file.exists(state_file) || !file.exists(state_hs_file)) {
    missing_scenarios <- append(missing_scenarios, scen_id)
  }
}

# Print the missing scenarios
if (length(missing_scenarios) > 0) {
  print("Missing scenarios:")
  print(missing_scenarios)
} else {
  print("No missing scenarios found.")
}

### MP TESTING END ---------- ---------- ---------- ---------- ----------

## start function
## 1) read in rds for subset ids; 2) save to drive; 3) compile field level outputs for health (for now)

county_out_list   <- list()
state_out_list    <- list()
state_hs_out_list <- list()
ct_out_list       <- list()
ct_hs_out_list    <- list()

for (i in 1:nrow(subset_ids)) {
  print(i)
  
  id_name_tmp <- subset_ids[i, scen_id]
  
  ## census tract (main results)
  ct_out_tmp <- readRDS(paste0(ct_path, id_name_tmp, '_ct_results.rds'))
  
  ct_out_tmp <- merge(ct_out_tmp, subset_ids,
                      by = "scen_id")
  
  
  ## census tract (health sensitivity, county means)
  ct_hs_out_tmp <- readRDS(paste0(ct_hs_path, id_name_tmp, '_ctc_results.rds'))
  
  ct_hs_out_tmp <- merge(ct_hs_out_tmp, subset_ids,
                      by = "scen_id")
  
  
  ## county out
  county_out_tmp <- readRDS(paste0(county_path, id_name_tmp, '_county_results.rds'))
  
  county_out_tmp <- merge(county_out_tmp, subset_ids,
                      by = "scen_id")
  
  print("county main")
  
  ## state out (main results)
  state_out_tmp <- readRDS(paste0(state_path, id_name_tmp, '_state_results.rds'))
  
  state_out_tmp <- merge(state_out_tmp, subset_ids,
                          by = "scen_id")

  
  ## state out (main results) 
  state_hs_out_tmp <- readRDS(paste0(state_hs_path, id_name_tmp, '_state_results_health.rds'))

  state_hs_out_tmp <- merge(state_hs_out_tmp, subset_ids,
                         by = "scen_id")
  
  
  ct_out_list[[i]]        <- ct_out_tmp
  ct_hs_out_list[[i]]     <- ct_hs_out_tmp
  county_out_list[[i]]    <- county_out_tmp
  state_out_list[[i]]     <- state_out_tmp
  state_hs_out_list[[i]]  <- state_hs_out_tmp 
  
}

ct_subset_all        <- rbindlist(ct_out_list)
ct_hs_subset_all     <- rbindlist(ct_hs_out_list) 
county_subset_all    <- rbindlist(county_out_list)
state_subset_all     <- rbindlist(state_out_list)
state_hs_subset_all  <- rbindlist(state_hs_out_list)

fwrite(ct_subset_all, paste0(ct_path, "subset_census_tract_results.csv"))
fwrite(ct_hs_subset_all, paste0(ct_hs_path, "subset_county_hs_results.csv")) # include health census 
fwrite(county_subset_all, paste0(county_path, "subset_county_results.csv"))
fwrite(state_subset_all, paste0(state_path, "subset_state_results.csv"))
fwrite(state_hs_subset_all, paste0(state_hs_path, "subset_state_hs_results.csv")) # include health census 

