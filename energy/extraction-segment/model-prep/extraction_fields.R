## Tracey Mangin
## July 21, 2021
## Make list of fields included in the analysis
# Updated 2/28/24 - MP

## libraries
library(data.table)
library(tidyverse)
library(sf)

## set paths and file names
# model_path          = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
# model_out_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/'
# run_folder          = 'extraction_2021-07-22/revised-ccs-correct-new-setback/'
# field_fname         = "diagnostic-field-level-results.csv"

## files
entry_file        = 'data-str/public/intermediate/energy/production/entry_df_final_revised.csv'
prod_vintage_file = 'data-str/public/intermediate/energy/production/pred_prod_no_exit_2020-2045_field_start_year_revised.csv'

## save path
save_path           = 'data-str/public/outputs/results-out/'

## field boundaries
boundaries <- st_read(file.path("data-str/public/inputs/gis/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(3488)


## read in output file
entry_dt = fread(file.path(entry_file), header = T, colClasses = c('doc_field_code' = 'character'))
entry_dt_fields = unique(entry_dt[, .(doc_field_code)])

prod_existing_vintage = fread(file.path(prod_vintage_file), header = T, colClasses = c('doc_field_code' = 'character'))
prod_fields = unique(prod_existing_vintage[, .(doc_field_code)])

# field_out <- fread(paste0(model_out_path, run_folder, field_fname), header = T, colClasses = c('doc_field_code' = 'character'))
# 
# field_out[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
#                                  fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]
# 
# 
# length(unique(field_out[, doc_field_code]))
# 
# pos_field <- field_out %>%
#   group_by(scen_name, doc_field_code, doc_fieldname) %>%
#   summarise(sum_prod = sum(total_prod_bbl)) %>%
#   ungroup() %>%
#   filter(sum_prod > 0)
# 
# field_out_zero <- field_out %>%
#   filter(!doc_field_code %in% pos_field$doc_field_code)

field_boundaries <- boundaries %>%
  filter(FIELD_CODE %in% c(entry_dt_fields$doc_field_code, prod_fields$doc_field_code)) %>%
  dplyr::select(NAME, doc_field_code = FIELD_CODE)

## save for health/labor team
st_write(field_boundaries, dsn = paste0(save_path, "extraction_fields.shp"), update = TRUE)


