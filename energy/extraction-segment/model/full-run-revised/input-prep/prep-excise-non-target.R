## Tracey Mangin
## January 31, 2022
## prep excise tax scenarios
# Updated 2/28/24 - MP

library(tidyverse)
library(data.table)
library(rebus)

## df of non-target excise taxes
excise_tax_df <- expand.grid(year = c(2020:2045),
                             tax_rate = c(0, 0.05, 0.10, 0.50, 0.90, 1.00))

excise_tax_df2 <- excise_tax_df %>%
  mutate(excise_tax_scenario = paste0("tax_", tax_rate)) %>%
  mutate(excise_tax_scenario = ifelse(excise_tax_scenario == "tax_0", "no tax", excise_tax_scenario)) %>%
  mutate(units = "fraction of oil price")

fwrite(excise_tax_df2, 'data/processed/excise_tax_non_target_scens.csv')

