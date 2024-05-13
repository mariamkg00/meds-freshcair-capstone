## Tracey Mangin
## June 9, 2020
## well vintage and production
# Updated 2/20/24 - MP

library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(readxl)
library(openxlsx)
library(data.table)

# ## set directory
# data_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/"
# save_directory <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/outputs/"

## monthly well production
well_prod <- fread("data-str/public/outputs/results-out/well_prod_m_processed.csv", colClasses = c('api_ten_digit' = 'character',
                                                                     'doc_field_code' = 'character')) # add new data directory HK
## field codes and field names
field_codes <- unique(well_prod[, c("doc_field_code", "doc_fieldname")])

## prod by field
field_prod <- well_prod[, .(total_bbls = sum(OilorCondensateProduced, na.rm = T)), by = .(doc_field_code, year)]

field_prod2 <- expand.grid(doc_field_code = unique(field_prod$doc_field_code),
                           year = unique(field_prod$year)) %>%
  left_join(field_prod) %>%
  mutate(total_bbls = ifelse(is.na(total_bbls), 0, total_bbls)) %>%
  arrange(doc_field_code, year) %>%
  left_join(field_codes) %>%
  select(doc_field_code, doc_fieldname, year, total_bbls)

field_zero_prod <- field_prod2 %>%
  group_by(doc_field_code, doc_fieldname) %>%
  summarise(prod = sum(total_bbls)) %>%
  ungroup() %>%
  filter(prod == 0)

field_prod3 <- field_prod2 %>%
  filter(!doc_field_code %in% field_zero_prod$doc_field_code)

write_csv(field_prod3, path = "data-str/public/intermediate/energy/production/crude_prod_x_field_revised.csv") # add new data directory HK




