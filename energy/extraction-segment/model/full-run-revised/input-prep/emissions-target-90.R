## Tracey Mangin
## January 31, 2022
## 90% emissions reduction target
# Updated 2/28/24 - MP

## libraries
library(tidyverse)
library(data.table)

setwd('/capstone/freshcair/meds-freshcair-capstone') # Sets directory based on Taylor structure
getwd()

## paths -- Updated - MP # add new data directory HK
data_path <- 'data-str/public/outputs/labor-out/' 

## file
ghg_file <- 'indust_emissions_2000-2019.csv'

## 2019 GHG emissions
## --------------------------
hist_ghg <- fread(paste0(data_path, ghg_file), header = T)

hist_ghg <- hist_ghg[segment %chin% c('Oil & Gas: Production & Processing') &
                       year == 2019, .(segment, unit, year, value)]

ghg_2019 <- as.numeric(hist_ghg[, value][1])
ghg_target_90 <- 0.1 * ghg_2019

ghg_target_df <- tibble(emission_reduction = "90perc_reduction",
                        ghg_emission_MtCO2e = ghg_target_90)

## save -- Updated - MP # add new data directory HK
fwrite(ghg_target_df, 'data-str/public/intermediate/health/emission_reduction_90.csv')


