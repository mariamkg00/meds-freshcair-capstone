## meas meng
## september 6, 2020
## calculate proportion of yearly oil production from each county in each field
# Updated 2/20/24 - MP

# inputs ------

setwd('/capstone/freshcair/meds-freshcair-capstone') # Sets directory based on Taylor structure
getwd()


# data_directory  = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/'
# prod_file       = 'well_prod_m_processed.csv'
# save_dir        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/'

# load libraries -------- 

library(data.table)  
# library(lubridate)
# library(zoo)
# library(stringr)

# read in data ------

## well production
well_prod <- fread("data-str/public/outputs/results-out/well_prod_m_processed.csv", colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character')) # add new data directory HK

# aggregate production annually by field-county ------

prod_field_county = well_prod[, .(oil_prod = sum(OilorCondensateProduced, na.rm = TRUE)), by = .(year, doc_field_code, doc_fieldname, county_name)]

# get annual field-level production  ----

prod_field = well_prod[, .(oil_prod = sum(OilorCondensateProduced, na.rm = TRUE)), by = .(year, doc_field_code, doc_fieldname)]


# calculate proportions -----

prod_field_county[, prop_production := oil_prod / sum(oil_prod, na.rm = TRUE), by = .(year, doc_field_code)]
prod_field_county[is.na(prop_production), prop_production := 0]
setorderv(prod_field_county, c('year', 'doc_field_code', 'county_name'))
setcolorder(prod_field_county, c('year', 'doc_field_code', 'doc_fieldname', 'county_name', 'oil_prod', 'prop_production'))

# get max year of non zero production for each field ----

field_last_year = prod_field[oil_prod > 0, .SD[which.max(year)], by = .(doc_field_code)]
field_last_year[, oil_prod := NULL]
field_county_last_year = field_last_year[prod_field_county, on = c('doc_field_code', 'year'), nomatch = 0]
setcolorder(field_county_last_year, c('doc_field_code', 'doc_fieldname', 'year', 'county_name', 'oil_prod', 'prop_production'))
setorderv(field_county_last_year, c('doc_field_code'))

# export to csv ------

fwrite(prod_field_county, 'data-str/private/production/annual_field_county_production_proportion_revised.csv', row.names = F) # add new data directory HK
fwrite(field_county_last_year, 'data-str/private/well-fields/annual_final_year_field_county_production_proportion_revised.csv', row.names = F) # add new data directory HK
