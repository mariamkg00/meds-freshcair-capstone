# calepa-cn: Processing direct, indirect, and induced multipliers by county 
# Chris Malloy (cmalloy@ucsb.edu)
# created: 07/28/2021
# updated: 08/17/2021
# updated 4/5/24 - MP

############################################################################################
# Set up environment 
############################################################################################

# Clearing previous 
rm(list=ls())


library(cowplot)
library(rstudioapi)
library(magrittr)
library(readr)
library(stringr)
library(readxl)
library(quantmod)
library(lubridate)
library(writexl)
library(tigris)
library(sf)
library(tidyverse)

#Set wd 
setwd('/capstone/freshcair/meds-freshcair-capstone') # Sets directory based on Taylor structure
getwd()

############################################################################################ 

#Read in spreadsheet for FTE conversion 


# UPDATED - MG 2/19/2024
fte_convert <- read_xlsx('data/inputs/labor/fte-convert.xlsx')%>% 
  dplyr::rename(fte_per_job = FTEperTotalEmp, ind_code = Implan546Index) %>% 
  dplyr::select(ind_code,fte_per_job)


#1. Import ICA files, clean up names, and add county & segment identifiers 

##1a. Industry Contribution Analysis (ICA)

### NOTE: all multipliers are for $1 million of output value in an industry 

setwd('/capstone/freshcair/meds-freshcair-capstone/data/inputs/labor/ica')

### Kern

#### extraction 


# UPDATED - MG - 2/19/2024
# UPDATE - not sure where ICA data is located
ica_emp_ext_kern <- read_csv('ica-emp-ext-kern.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_kern <- read_csv('ica-va-ext-kern.csv',skip = 1, col_names = TRUE) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "extraction") %>% 
    dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                  direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                  direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                  indirect_comp = `Employee Compensation...8`, 
                  indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                  indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                  induced_comp = `Employee Compensation...13`, 
                  induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                  induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`)%>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### drilling 

# UPDATED - MG - 2/19/2024
ica_emp_drill_kern <- read_csv('ica-emp-drill-kern.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UPDATED - MG - 2/19/2024
ica_comp_drill_kern <- read_csv('ica-va-drill-kern.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
         direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
         direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
         indirect_comp = `Employee Compensation...8`, 
         indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
         induced_comp = `Employee Compensation...13`, 
         induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
         induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


# UPDATED - MG - 2/19/2024

#### refining 

ica_emp_ref_kern <- read_csv('ica-emp-ref-kern.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UPDATED - MG - 2/19/2024
ica_comp_ref_kern <- read_csv('ica-va-ref-kern.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kern", segment = "refining") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################

## Los Angeles

#### extraction 

ica_emp_ext_la <- read_csv('ica-emp-ext-la.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Los Angeles", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UDPATED - MG - 2/19/2024
ica_comp_ext_la <- read_csv('ica-va-ext-la.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Los Angeles", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
         direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
         direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
         indirect_comp = `Employee Compensation...8`, 
         indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
         indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
         induced_comp = `Employee Compensation...13`, 
         induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
         induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### drilling 

# UPDATED - MG - 2/19/2024
ica_emp_drill_la <- read_csv('ica-emp-drill-la.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Los Angeles", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


# UPDATED - MG - 2/19/2024
ica_comp_drill_la <- read_csv('ica-va-drill-la.csv', skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Los Angeles", segment = "drilling") %>% 
    dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                  direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                  direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                  indirect_comp = `Employee Compensation...8`, 
                  indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                  indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                  induced_comp = `Employee Compensation...13`, 
                  induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                  induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
    dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                  -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)
 


#### refining 

# UPDATED - MG - 2/19/2024
ica_emp_ref_la <- read_csv('ica-emp-ref-la.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Los Angeles", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


# UPDATED - MG - 2/19/2024
ica_comp_ref_la <- read_csv('ica-va-ref-la.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Los Angeles", segment = "refining") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################

## Santa Barbara

#### extraction 

# UPDATED - MG - 2/19/2024
ica_emp_ext_sb <- read_csv('ica-emp-ext-sb.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


# UPDATED - MG - 2/19/2024
ica_comp_ext_sb <- read_csv('ica-va-ext-sb.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "extraction") %>%
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)
 


#### drilling 

# UPDATED 2/19/2024
ica_emp_drill_sb <- read_csv('ica-emp-drill-sb.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


# UPDATED - MG - 2/19/2024
ica_comp_drill_sb <- read_csv('ica-va-drill-sb.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### refining 

# UPDATED - MG - 2/19/2024
ica_emp_ref_sb <- read_csv('ica-emp-ref-sb.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UPDATED - MG - 2/19/2024
ica_comp_ref_sb <- read_csv('ica-va-ref-sb.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Barbara", segment = "refining") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################

## Monterey

#### extraction 

# UPDATED - MG - 2/19/2024
ica_emp_ext_monterey <- read_csv('ica-emp-ext-monterey.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Monterey", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UPDATED - MG - 2/19/2024
ica_comp_ext_monterey <- read_csv('ica-va-ext-monterey.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Monterey", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### drilling 

# UPDATED - MG - 2/19/2024
ica_emp_drill_monterey <- read_csv('ica-emp-drill-monterey.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Monterey", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UPDATED - MG - 2/19/2024
ica_comp_drill_monterey <- read_csv('ica-va-drill-monterey.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Monterey", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)



####################################################################################################################

## Ventura

#### extraction 

# UPDATED - MG - 2/19/2024
ica_emp_ext_ventura <- read_csv('ica-emp-ext-ventura.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Ventura", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


# UPDATED - MG - 2/19/2024
ica_comp_ext_ventura <- read_csv('ica-va-ext-ventura.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Ventura", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### drilling 

# UPDATED - MG - 2/19/2024
ica_emp_drill_ventura <- read_csv('ica-emp-drill-ventura.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Ventura", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UDPATED - MG - 2/19/2024
ica_comp_drill_ventura <- read_csv('ica-va-drill-ventura.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Ventura", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)



####################################################################################################################

## Orange

#### extraction 

# UPDATED - MG - 2/19/2024
ica_emp_ext_orange <- read_csv('ica-emp-ext-orange.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Orange", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UDPATED - MG - 2/19/2024
ica_comp_ext_orange <- read_csv('ica-va-ext-orange.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Orange", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### drilling 

# UPDATED - MG - 2/19/2024
ica_emp_drill_orange <- read_csv('ica-emp-drill-orange.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Orange", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 

# UPDATED - MG - 2/19/2024
ica_comp_drill_orange <- read_csv('ica-va-drill-orange.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Orange", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################
# UPDATED - MG - 2/19/2024

## Fresno

#### extraction 

ica_emp_ext_fresno <- read_csv('ica-emp-ext-fresno.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Fresno", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_fresno <- read_csv('ica-va-ext-fresno.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Fresno", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### drilling 

ica_emp_drill_fresno <- read_csv('ica-emp-drill-fresno.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Fresno", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_drill_fresno <- read_csv('ica-va-drill-fresno.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Fresno", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################
# UPDATED - MG - 2/19/2024

## Contra Costa

#### extraction 

ica_emp_ext_cc <- read_csv('ica-emp-ext-cc.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Contra Costa", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_cc <- read_csv('ica-va-ext-cc.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Contra Costa", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### refining 

ica_emp_ref_cc <- read_csv('ica-emp-ref-cc.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Contra Costa", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ref_cc <- read_csv('ica-va-ref-cc.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Contra Costa", segment = "refining") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)




####################################################################################################################
# UPDATED - MG - 2/19/2024

## Solano

#### refining 

ica_emp_ref_solano <- read_csv('ica-emp-ref-solano.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Solano", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ref_solano <- read_csv('ica-va-ref-solano.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Solano", segment = "refining") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

####################################################################################################################
# UPDATED - MG - 2/19/2024

## San Luis Obispo

#### extraction 

ica_emp_ext_slo <- read_csv('ica-emp-ext-slo.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_slo <- read_csv('ica-va-ext-slo.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


#### refining 

ica_emp_ref_slo <- read_csv('ica-emp-ref-slo.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ref_slo <- read_csv('ica-va-ref-slo.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Luis Obispo", segment = "refining") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

####################################################################################################################
# UPDATED - MG - 2/19/2024

## San Benito

#### extraction 

ica_emp_ext_sanbenito <- read_csv('ica-emp-ext-sanbenito.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Benito", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_sanbenito <- read_csv('ica-va-ext-sanbenito.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Benito", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

####################################################################################################################
# UPDATED - MG - 2/19/2024

## San Bernardino

#### extraction 

ica_emp_ext_sanbernardino <- read_csv('ica-emp-ext-sanbernardino.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Bernardino", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_sanbernardino <- read_csv('ica-va-ext-sanbernardino.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Bernardino", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################
# UPDATED - MG - 2/19/2024

## Tulare

#### extraction 

ica_emp_ext_tulare <- read_csv('ica-emp-ext-tulare.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Tulare", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_tulare <- read_csv('ica-va-ext-tulare.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Tulare", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################
# UPDATED - MG - 2/19/2024

## San Mateo

#### extraction 

ica_emp_ext_sanmateo <- read_csv('ica-emp-ext-sanmateo.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Mateo", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_sanmateo <- read_csv('ica-va-ext-sanmateo.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "San Mateo", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)


####################################################################################################################
# UPDATED - MG - 2/19/2024

## Kings

#### extraction 

ica_emp_ext_kings <- read_csv('ica-emp-ext-kings.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kings", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_kings <- read_csv('ica-va-ext-kings.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Kings", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

####################################################################################################################
# UPDATED - MG - 2/19/2024

## Alameda

#### extraction 

ica_emp_ext_alameda <- read_csv('ica-emp-ext-alameda.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Alameda", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_alameda <- read_csv('ica-va-ext-alameda.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Alameda", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

####################################################################################################################
# UPDATED - MG - 2/19/2024

## Riverside

#### extraction 

ica_emp_ext_riverside <- read_csv('ica-emp-ext-riverside.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Riverside", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_riverside <- read_csv('ica-va-ext-riverside.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Riverside", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

####################################################################################################################
# UPDATED - MG - 2/19/2024

## Santa Clara

#### extraction 

ica_emp_ext_santaclara <- read_csv('ica-emp-ext-santaclara.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Clara", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_santaclara <- read_csv('ica-va-ext-santaclara.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Santa Clara", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)



####################################################################################################################
# UDPATED - MG - 2/19/2024

## Statewide 

#### extraction 

ica_emp_ext_statewide_test <- read_csv('ica-emp-ext-statewide.csv')

ica_emp_ext_statewide <- read_csv('ica-emp-ext-statewide.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Statewide", segment = "extraction") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ext_statewide <- read_csv('ica-va-ext-statewide.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Statewide", segment = "extraction") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

#### drilling 

ica_emp_drill_statewide <- read_csv('ica-emp-drill-statewide.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Statewide", segment = "drilling") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_drill_statewide <- read_csv('ica-va-drill-statewide.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Statewide", segment = "drilling") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)



#### refining 

ica_emp_ref_statewide <- read_csv('ica-emp-ref-statewide.csv') %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Statewide", segment = "refining") %>% 
  dplyr::rename(industry = Impact) %>% 
  dplyr::select(-...1,-...6) 


ica_comp_ref_statewide <- read_csv('ica-va-ref-statewide.csv',skip = 1) %>% 
  filter(is.na(...1)==F) %>% 
  mutate(county = "Statewide", segment = "refining") %>% 
  dplyr::rename(industry = `Industry Display`, direct_comp = `Employee Compensation...3`, 
                direct_proprietor_income = `Proprietor Income...4`, direct_other_property_income = `Other Property Income...5`,
                direct_taxes_prod_imports = `Taxes on Production & Imports...6`, direct_va = `Value Added...7`,
                indirect_comp = `Employee Compensation...8`, 
                indirect_proprietor_income = `Proprietor Income...9`, indirect_other_property_income = `Other Property Income...10`,
                indirect_taxes_prod_imports = `Taxes on Production & Imports...11`, indirect_va = `Value Added...12`,
                induced_comp = `Employee Compensation...13`, 
                induced_proprietor_income = `Proprietor Income...14`, induced_other_property_income = `Other Property Income...15`,
                induced_taxes_prod_imports = `Taxes on Production & Imports...16`, induced_va = `Value Added...17`) %>% 
  dplyr::select(-`Employee Compensation...18`, -`Proprietor Income...19`, -`Other Property Income...20`,
                -`Taxes on Production & Imports...21`, -`Value Added...22`,-...1)

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################


# UPDATED - MG - 2/19/2024


#2. Bind data frames together for ICA and impact

## ica 

ica_comp <- bind_rows(ica_comp_ref_cc,ica_comp_ref_solano,ica_comp_ref_la,ica_comp_ref_kern,ica_comp_ref_sb,ica_comp_ref_slo,ica_comp_ref_statewide,
                      ica_comp_drill_fresno,ica_comp_drill_kern,ica_comp_drill_la,ica_comp_drill_monterey,
                      ica_comp_drill_orange,ica_comp_drill_sb,ica_comp_drill_ventura,ica_comp_drill_statewide,
                      ica_comp_ext_fresno,ica_comp_ext_kern,ica_comp_ext_la,ica_comp_ext_monterey,
                      ica_comp_ext_orange,ica_comp_ext_sb,ica_comp_ext_ventura,ica_comp_ext_slo,ica_comp_ext_sanbenito,ica_comp_ext_cc,
                      ica_comp_ext_sanbernardino,ica_comp_ext_tulare,ica_comp_ext_sanmateo,ica_comp_ext_kings,ica_comp_ext_alameda,
                      ica_comp_ext_riverside,ica_comp_ext_santaclara,ica_comp_ext_statewide)


ica_emp <- bind_rows(ica_emp_ref_cc,ica_emp_ref_solano,ica_emp_ref_la,ica_emp_ref_kern,ica_emp_ref_sb,ica_emp_ref_slo,ica_emp_ref_statewide,
                     ica_emp_drill_fresno,ica_emp_drill_kern,ica_emp_drill_la,ica_emp_drill_monterey,
                     ica_emp_drill_orange,ica_emp_drill_sb,ica_emp_drill_ventura,ica_emp_drill_statewide,
                     ica_emp_ext_fresno,ica_emp_ext_kern,ica_emp_ext_la,ica_emp_ext_monterey,
                     ica_emp_ext_orange,ica_emp_ext_sb,ica_emp_ext_ventura,ica_emp_ext_slo,ica_emp_ext_sanbenito,ica_emp_ext_cc,
                     ica_emp_ext_sanbernardino,ica_emp_ext_tulare,ica_emp_ext_sanmateo,ica_emp_ext_kings,ica_emp_ext_alameda,
                     ica_emp_ext_riverside,ica_emp_ext_santaclara,ica_emp_ext_statewide) %>% 
  dplyr::rename(direct_emp = `1 - Direct`,
         indirect_emp = `2 - Indirect`, induced_emp = `3 - Induced`) %>% 
  mutate(direct_emp = as.numeric(direct_emp), indirect_emp = as.numeric(indirect_emp), 
         induced_emp = as.numeric(induced_emp)) %>% 
  separate(industry, into = c("ind_code","ind"), sep=" - ",remove = FALSE) %>% 
  mutate(ind_code = as.numeric(ind_code)) %>% 
  left_join(fte_convert, by = "ind_code") %>% 
  mutate(direct_emp = direct_emp*fte_per_job, indirect_emp = indirect_emp*fte_per_job, induced_emp = induced_emp*fte_per_job) %>% 
  dplyr::select(-ind_code,-ind,-fte_per_job)


ica <- inner_join(ica_emp,ica_comp,by=c("county","segment","industry")) 



####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# UPDATED 2/20/2024


#3. Separate the direct, indirect, and induced multipliers into different dfs, reshape each to wide format

## ICA 

ica_emp_wide <- ica_emp %>% 
  pivot_wider(id_cols = c("county","segment"), names_from = industry, 
              values_from = c("direct_emp","indirect_emp","induced_emp"))

ica_emp_wide_direct <- ica_emp %>% 
  dplyr::select(-indirect_emp,-induced_emp) %>% 
  filter(direct_emp != 0) %>% 
  dplyr::arrange(county,segment,-direct_emp) %>% 
pivot_wider( names_from = "industry", 
            values_from = c("direct_emp")) 

# UPDATED - MG - 2/21/2024
ica_emp_wide_indirect <- ica_emp %>% 
  dplyr::select(-direct_emp,-induced_emp) %>% 
  filter(indirect_emp != 0) %>% 
  group_by(county,segment)  %>% 
  arrange(county,segment,-indirect_emp) %>% 
  pivot_wider(names_from = "industry", 
              values_from = c("indirect_emp"))

# UPDATED - MG - 2/21/2024
ica_emp_wide_induced <- ica_emp %>% 
  dplyr::select(-direct_emp,-indirect_emp) %>% 
  filter(induced_emp != 0) %>% 
  group_by(county,segment)  %>% 
  arrange(county,segment,-induced_emp) %>% 
  pivot_wider(names_from = "industry", 
              values_from = c("induced_emp")) 


### Compaensation 

ica_comp_wide <- ica_comp %>% 
  pivot_wider(id_cols = c("county","segment"), names_from = "industry", 
              values_from = c("direct_comp","indirect_comp","induced_comp"))

ica_comp_wide_direct <- ica_comp %>% 
  dplyr::select(county,segment,industry,direct_comp) %>% 
  filter(direct_comp != 0) %>% 
  arrange(county,segment,-direct_comp) %>% 
  pivot_wider(id_cols = c("county","segment"), names_from = "industry", 
              values_from = c("direct_comp")) 

ica_comp_wide_indirect <- ica_comp %>% 
  dplyr::select(county,segment,industry,indirect_comp) %>% 
  filter(indirect_comp != 0) %>% 
  group_by(county,segment)%>% 
  arrange(county,segment,-indirect_comp) %>% 
  pivot_wider(id_cols = c("county","segment"), names_from = "industry", 
              values_from = c("indirect_comp")) 


ica_comp_wide_induced <- ica_comp %>% 
  dplyr::select(county,segment,industry,induced_comp) %>% 
  filter(induced_comp != 0) %>% 
  group_by(county,segment) %>% 
  arrange(county,segment,-induced_comp) %>% 
  pivot_wider(id_cols = c("county","segment"), names_from = "industry", 
              values_from = c("induced_comp")) 


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#3. Create dfs with total direct, indirect, induced multipliers by county and segment 
# UPDATED - MG - 2/21/2024



#detach(package:plyr)

ica_total <- ica %>% 
  group_by(segment,county) %>% 
  summarise(direct_emp = sum(direct_emp),
            indirect_emp = sum(indirect_emp, na.rm = TRUE),
            induced_emp = sum(induced_emp, na.rm = TRUE),
            direct_comp = sum(direct_comp, na.rm = TRUE),
            indirect_comp = sum(indirect_comp, na.rm = TRUE),
            induced_comp = sum(induced_comp, na.rm = TRUE)) 

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

#4. Add in counties without O&G sector activity 

# Removed filter since there is only one row - MP
ica_total_state <- filter(ica_total,county=="Statewide")
ica_emp_wide_direct_state <- filter(ica_emp_wide_direct,county=="Statewide")
ica_emp_wide_indirect_state <- filter(ica_emp_wide_indirect,county=="Statewide")
ica_emp_wide_induced_state <- filter(ica_emp_wide_induced,county=="Statewide")
ica_comp_wide_direct_state <- filter(ica_comp_wide_direct,county=="Statewide")
ica_comp_wide_indirect_state <- filter(ica_comp_wide_indirect,county=="Statewide")
ica_comp_wide_induced_state <- filter(ica_comp_wide_induced,county=="Statewide")

county_shape <- counties(year = 2010,state = "California")

county_df <- dplyr::select(county_shape,NAME10) 

st_geometry(county_df) <- NULL

ica_total_all_counties <- left_join(county_df,ica_total,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_total_state)

ica_total_all_counties[is.na(ica_total_all_counties)] <- 0 
ica_total_all_counties <- mutate(ica_total_all_counties, segment = ifelse(segment=="0",NA,segment))


####################################################################################################################
# run on 2/25/2024 -MG 

ica_emp_direct_all_counties <- left_join(county_df,ica_emp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment)%>% 
  bind_rows(ica_emp_wide_direct_state)

ica_emp_direct_all_counties[is.na(ica_emp_direct_all_counties)] <- 0 
ica_emp_direct_all_counties <- mutate(ica_emp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_emp_indirect_all_counties <- left_join(county_df,ica_emp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_emp_wide_indirect_state)

ica_emp_indirect_all_counties[is.na(ica_emp_indirect_all_counties)] <- 0 
ica_emp_indirect_all_counties <- mutate(ica_emp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_emp_induced_all_counties <- left_join(county_df,ica_emp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_emp_wide_induced_state)

ica_emp_induced_all_counties[is.na(ica_emp_induced_all_counties)] <- 0 
ica_emp_induced_all_counties <- mutate(ica_emp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))

####################################################################################################################
# run on 2/25/2024 - MG 

ica_comp_direct_all_counties <- left_join(county_df,ica_comp_wide_direct,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_comp_wide_direct_state)

ica_comp_direct_all_counties[is.na(ica_comp_direct_all_counties)] <- 0 
ica_comp_direct_all_counties <- mutate(ica_comp_direct_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_comp_indirect_all_counties <- left_join(county_df,ica_comp_wide_indirect,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_comp_wide_indirect_state)

ica_comp_indirect_all_counties[is.na(ica_comp_indirect_all_counties)] <- 0 
ica_comp_indirect_all_counties <- mutate(ica_comp_indirect_all_counties, segment = ifelse(segment=="0",NA,segment))


ica_comp_induced_all_counties <- left_join(county_df,ica_comp_wide_induced,by=c("NAME10" = "county")) %>% 
  rename(county = NAME10) %>% 
  arrange(county,segment) %>% 
  bind_rows(ica_comp_wide_induced_state)

ica_comp_induced_all_counties[is.na(ica_comp_induced_all_counties)] <- 0 
ica_comp_induced_all_counties <- mutate(ica_comp_induced_all_counties, segment = ifelse(segment=="0",NA,segment))

ica_comp_direct_all_counties %>% 
  group_by(county)

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# 2/25/2024 - MG 
#5. replace direct compensation multipliers with the sample average for counties where it is 0 

ica_total_positive <- ica_total_all_counties %>% 
  filter(direct_comp != 0 & indirect_comp != 0 & induced_comp != 0) %>% 
  group_by(segment) %>% 
  summarize(avg_direct_comp = mean(direct_comp),avg_indirect_comp = mean(indirect_comp),avg_induced_comp = mean(induced_comp))

ica_total_all_counties_interpolated <- ica_total_all_counties %>% 
  inner_join(ica_total_positive,by=c("segment")) %>% 
  mutate(ip.direct_comp = ifelse((direct_comp==0 & is.na(segment) == F),avg_direct_comp,direct_comp),
         ip.indirect_comp = ifelse((indirect_comp==0 & is.na(segment) == F),avg_indirect_comp,indirect_comp),
         ip.induced_comp = ifelse((induced_comp==0 & is.na(segment) == F),avg_induced_comp,induced_comp)) %>% 
  dplyr::select(-avg_direct_comp,-avg_indirect_comp,-avg_induced_comp)


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

# UPDATED - 2/25/2024 - MG 
#5. export as xlsx 


# updating working directory to processed data folder in meds-freshcair
setwd('/capstone/freshcair/meds-freshcair-capstone/data/processed/')

## ICA file 

ica_list = list(ica_total=ica_total_all_counties_interpolated,ica_indirect_emp = ica_emp_indirect_all_counties,
                ica_induced_emp = ica_emp_induced_all_counties, ica_indirect_comp = ica_comp_indirect_all_counties,
                ica_induced_comp = ica_comp_induced_all_counties)
write_xlsx(x=ica_list,"ica_multipliers_v2.xlsx")


####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################

# UDPATED - 2/25/2024 - MG 
#6. save ICA industry multipliers as a csv for use in compiling results by industry in long format

# updating working directory to processed data folder in meds-freshcair

setwd('/capstone/freshcair/meds-freshcair-capstone/data/processed/')

ica_ind_output <- ica %>% 
  dplyr::select(county,segment,industry,direct_emp,indirect_emp,induced_emp,direct_comp,indirect_comp,induced_comp)

write_csv(ica_ind_output,'ica_multipliers_by_industry_long.csv')

library(sysfonts)
library(scales)
font_add_google(name = 'Inter', family = 'inter')
show_text_auto()
### producing visual 
ica_top_ten_counties <- ica_total_all_counties %>% 
  filter(segment == 'drilling') %>% 
  filter(county != 'Statewide') %>% 
  arrange(desc(direct_comp))
options(scipen = 999)
sub("0{3}$", "K", ica_top_ten_counties$direct_comp)
ggplot(data = ica_top_ten_counties)+
  geom_col(aes(x = reorder(county, -direct_comp), y = direct_comp), fill = '#006CD1')+
  geom_text(aes(x = reorder(county, -direct_comp), y = direct_comp, label = scales::comma(round(direct_comp,2)), vjust = -0.5))+
  scale_y_continuous(labels = label_dollar(scale = .001, suffix = "K"))+
  labs(x = 'California County',
       y = 'Direct Compensation ($USD)',
       title = 'Top California Counties with Highest Direct Compensation Multipliers')+
  annotate('text' , x = 5.75, y = 300000, label = 'Direct impacts were computed by inputting the level of revenue observed\n for the drilling industries separately for each county with active operations. ',
           fontface = 'italic')+
  theme_bw()+
  theme(axis.text.x = element_text(family = 'inter', size = 12),
        axis.text.y = element_text(family = 'inter', size = 12),
        axis.title.x = element_text(family = 'inter', size = 14),
        axis.title.y = element_text(family = 'inter', size = 14),
        plot.title = element_text(family = 'inter', size = 15, hjust = 0.5),
        panel.grid = element_blank(),
        panel.border = element_blank())

