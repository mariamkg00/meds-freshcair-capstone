## Ruiwen Lee
## June 29, 2021
## Create well exit variable based on original production threshold rule
# Updated 2/28/24 - MP
# Updated 4/7/24 - MP

library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(zoo)

setwd('/capstone/freshcair/meds-freshcair-capstone') # Sets directory based on Taylor structure
getwd()


# ## set directory
# vintage_prod_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/decline-historic/data/"
# exit_rule_directory    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/'
# save_directory         <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/exit/'

## files
exit_rule_file          <- 'well_exit_volume_x_field_v1_revised.csv'

## read in files
vintage_prod <- fread("data-str/private/production/production_field-year_yearly_entry.csv", colClasses = c('doc_field_code' = 'character'))
exit_threshold <- fread("data-str/private/entry-exit/well_exit_volume_x_field_v1_revised.csv", colClasses = c('doc_field_code' = 'character'))

## construct main dataset that contains exit threshold and annual production for each field-vintage
dt_vintage_exit <- merge(vintage_prod[,c('doc_field_code','doc_fieldname', 'start_year','year_no','well_prod','no_wells')],
                         exit_threshold[,c('doc_field_code','mean_final_yr_prod_adj')],
                         by = 'doc_field_code',
                         all.x = T)

# track no. of wells that exit from each field in each year - RL
dt_vintage_exit[, n_exits := ifelse(well_prod < mean_final_yr_prod_adj,
                                    no_wells,
                                    0)]

dt_vintage_exit[, exit_year := start_year + year_no - 1]

# sum no. of exits across start years
dt_field_exits <- dt_vintage_exit[, .(n_exits_field = sum(n_exits)),  by = .(doc_field_code, doc_fieldname, exit_year)]

## Save field-year-level well exit data

write.csv(dt_field_exits, "data-str/private/entry-exit/well_exits_under_rule.csv")



