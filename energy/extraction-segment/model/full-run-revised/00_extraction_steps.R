
## the user must define run_name (which will be used when saving your results) and 
## the save path, which should direct to a folder where the outputs will be saved

## Updated 2/28/24 - MP
## Updated 4/14/24 - MP

## define if you are using zenodo repo for inputs (if yes, set to TRUE)
zenodo_repo <- FALSE

if(zenodo_repo) {
  ## zenodo users define run name save path here ----
  zenodo_user_path <- '~/Desktop'
  save_path  = ''
  run_name = 'xxx'
  
  
} else {
  # args = commandArgs(trailingOnly = TRUE)
  run_name        = "revision-setbacks"
  # save_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  save_path       = 'data/processed'  # add new data directory HK
  
}

# ## set path if zenodo user
# if(zenodo_repo) {
#   
#   main_path          = paste0(zenodo_user_path, '/ca-transport-supply-decarb-files/')
#   extract_path       = 'intermediate/extraction-model/'
#   
# }




# create save path that is based on the specified path and the run date ------ 

cur_date              = Sys.Date()
save_path             = file.path(save_path, paste0('extraction_', cur_date))
dir.create(save_path, showWarnings = TRUE)

# create directories for individual outputs

save_info_path = file.path(save_path, run_name)
dir.create(save_info_path)

dir.create(file.path(save_path, run_name, 'vintage-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'field-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'state-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'density-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'depl-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'exit-out'), showWarnings = FALSE)

# set seed
set.seed(228)

# load libraries ------

library(data.table)
library(openxlsx)
library(tidyverse)
library(tidyr)
# Multiprocessing
library(doParallel)
library(foreach)

# source from other scripts -------

# source load_input_info.R to load input info
source(here::here('energy', 'extraction-segment', 'model', 'full-run-revised', 'load_input_info_fc.R'))

# source function to predict extraction
source(here::here('energy', 'extraction-segment', 'model', 'full-run-revised', 'fun_extraction_model_targets.R'))
# source(here::here('energy', 'extraction-segment', 'model', 'full-run-revised', 'fun_extraction_model_targets_fc.R'))

## step 0: load the inputs

scen_id_list_final = fread(file.path('data/processed/scenario_id_list_targets_finalv2.csv'), header = T)

selected_scens_final <- scen_id_list_final[subset_scens == 1]

# Select model: 1 for Poisson, 2 for Random Forest, 3 for Gradient Boosted
model_choice = 3

# step 1: run extraction model and get outputs -------

# set start time -----
start_time <- Sys.time()
print(paste("Starting extraction model at ", start_time))

# cores
n_cores <- future::availableCores() - 1
doParallel::registerDoParallel(cores = n_cores)

run_extraction_model(selected_scens_final)

elapsed_time <- Sys.time() - start_time
print(elapsed_time)

