## Tracey Mangin
## January 31, 2022
## scenario list - revise model to generate excise tax/carbon tax stream for targets
## Updated 2/28/24 - MP

## libraries
library(data.table)
library(tidyverse)
library(readxl)
library(openxlsx)
library(rebus)


# # paths -----
# outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
# data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
# scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
# academic_out      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/'
# save_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/nature-energy-rev-outputs'
# 

# load data -----

## load oil price data -- Updated - MP
# Reading in with readxl due to errors
oilpx_scens = setDT(read_excel(path = 'data/inputs/extraction/oil_price_projections_revised.xlsx',
                               sheet = 'nominal',
                               col_names = TRUE))

oilpx_scens = oilpx_scens[, c('Year', 'AEO 2021 Reference case $/b', 'AEO 2021 High oil price $/b', 'AEO 2021 Low oil price $/b')]

# oilpx_scens = setDT(read.xlsx(file.path('data/inputs/extraction/oil_price_projections_revised.xlsx'), sheet = 'nominal', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')

# Convert oilpx_scens to a data.table -- Added - MP
setDT(oilpx_scens)

oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

## unique oil px scens
oilpx_scens_names <- distinct(oilpx_scens[, .(oil_price_scenario)])

## load innovation scenarios -- Updated - MP
innovation_scens = fread(file.path("data/inputs/scenarios/innovation_scenarios.csv"), header = T)

innovation_scens_name <- distinct(innovation_scens[, .(innovation_scenario)])

## load carbon px scens -- Updated - MP
carbonpx_scens = fread(file.path("data/inputs/scenarios/carbon_prices_revised.csv"), header = T)

carbonpx_scens_name <- distinct(carbonpx_scens[, .(carbon_price_scenario)])

## load ccs scenarios -- Updated - MP
ccs_scens = fread(file.path("data/processed/ccs_extraction_scenarios_revised.csv"), header = T)
ccs_scens[, ccs_price_usd_per_kg := ccs_price/1000] # convert from usd per metric ton to usd per kg
ccs_scens = ccs_scens[, c('year', 'ccs_scenario', 'ccs_price_usd_per_kg')]
ccs_scens[, ccs_scenario := factor(ccs_scenario, levels = c('no ccs', 'high CCS cost', 'medium CCS cost', 'low CCS cost'))]


## load setback scenarios -- Updated - MP
setback_scens = fread(file.path('data/processed/setback_coverage_R.csv'), header = T, colClasses = c('doc_field_code' = 'character'))

setback_scens[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]

setback_scens_name <- distinct(setback_scens[, .(setback_scenario)])


# load production quota file -- Updated - MP
prod_quota_scens = fread(file.path("data/inputs/scenarios/prod_quota_scenarios.csv"), header = T)

prod_quota_scens_names <- distinct(prod_quota_scens[, .(prod_quota_scenario)])

# load excise tax file
excise_tax_scens = fread(file.path("data/processed/excise_tax_non_target_scens.csv"), header = T)

excise_tax_scens_name <- distinct(excise_tax_scens[, .(excise_tax_scenario)])

# load ccs incentives file 
# Reading in with read_excel due to errors
incentives_scens = setDT(read_excel(path = "data/inputs/scenarios/CCS_LCFS_45Q.xlsx",
                                    sheet = "scenarios",
                                    col_names = TRUE))

incentives_scens = incentives_scens[, 1:3]
# incentives_scens = setDT(read.xlsx(file.path("data/inputs/scenarios/CCS_LCFS_45Q.xlsx"), sheet = 'scenarios', cols = c(1:3)))

# create adjusted ccs costs ------

ccs_scens_adj = ccs_scens[incentives_scens, on = .(year), allow.cartesian = T, nomatch = 0]
ccs_scens_adj[, ccs_scenario_adj := fcase(incentive_scenario == 'no incentives', paste0(ccs_scenario),
                                          incentive_scenario == '45Q only', paste0(ccs_scenario, ' - 45Q'),
                                          incentive_scenario == '45Q + LCFS', paste0(ccs_scenario, ' - 45Q - LCFS'))]


# adjust ccs price with incentives
ccs_scens_adj[, ccs_price_usd_per_kg_adj := ccs_price_usd_per_kg - (incentive_price/1e3)]

# create constrained version 
ccs_scens_neg = ccs_scens_adj[ccs_scenario_adj %in% unique(ccs_scens_adj[ccs_price_usd_per_kg_adj < 0, ccs_scenario_adj])]
ccs_scens_neg[, ccs_scenario_adj := paste0(ccs_scenario_adj, ' (constrained)') ]
ccs_scens_neg[, ccs_price_usd_per_kg_adj := fifelse(ccs_price_usd_per_kg_adj < 0, 0, ccs_price_usd_per_kg_adj)]

# combine ccs scenarios
ccs_scens_all = rbind(ccs_scens_adj, ccs_scens_neg)

# select columns 
ccs_scens_all = ccs_scens_all[, .(year, ccs_scenario_adj, ccs_price_usd_per_kg_adj)]
setnames(ccs_scens_all, c('ccs_scenario_adj', 'ccs_price_usd_per_kg_adj'), c('ccs_scenario', 'ccs_price_usd_per_kg'))

ccs_scens_names <- distinct(ccs_scens_all[, .(ccs_scenario)])

## remove no ccs with incentives
ccs_scens_names = ccs_scens_names[!ccs_scenario %in% c('no ccs - 45Q', 'no ccs - 45Q - LCFS')]


## ----------------------------------------
## target scenarios
## ----------------------------------------

## carbon px - targets
carbon_px_vec <- c("price floor", "price ceiling", "central SCC")

carbon_target_vec <- c(paste0("carbon_target_", setback_scens_name$setback_scenario[1:3]), "carbon_target_90perc_reduction")

## target excise taxes
tax_target_vec <- c(paste0("tax_", setback_scens_name$setback_scenario[1:3]), "tax_90perc_reduction")

## scenario combinations ----------

## first only non-targets
scen_sel <- expand.grid(oil_price_scenario = unique(oilpx_scens_names[, oil_price_scenario]),
                        setback_scenario = unique(setback_scens_name[, setback_scenario]), 
                        prod_quota_scenario = unique(prod_quota_scens_names[, prod_quota_scenario]),
                        carbon_price_scenario = unique(carbonpx_scens_name[, carbon_price_scenario]),
                        ccs_scenario = unique(ccs_scens_names[, ccs_scenario]), 
                        innovation_scenario = unique(innovation_scens_name[, innovation_scenario]), 
                        excise_tax_scenario = unique(excise_tax_scens_name[, excise_tax_scenario]))

setDT(scen_sel)

## add ID column 
# scen_sel[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
#                             carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "-")]

# Testing above MP
scen_sel[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                            carbon_price_scenario, ifelse(ccs_scenario %like% "- 45Q", "no ccs", ccs_scenario),
                            innovation_scenario, excise_tax_scenario, sep = "-")]

setcolorder(scen_sel, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                        'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario'))

# ## remove production quota
# scen_sel = scen_sel[prod_quota_scenario == 'no quota']


## bau scen ids
scen_sel[, BAU_scen := fifelse((oil_price_scenario == 'reference case' & 
                                  innovation_scenario == 'low innovation' & 
                                  carbon_price_scenario == 'price floor' & 
                                  ccs_scenario == 'no ccs' &
                                  excise_tax_scenario == 'no tax' &
                                  setback_scenario == 'no_setback' &
                                  prod_quota_scenario == 'no quota'), 1, 0)]

# Updated - MP
scen_sel[, target := fifelse(prod_quota_scenario %in% c('setback_1000_quota', 'setback_2500_quota', 'setback_3200_quota', 'setback_5280_quota'),
                             str_remove(prod_quota_scenario, "_quota"), "no_target")]

# Updated - MP
scen_sel[, target := fifelse(target %in% c('setback_1000', 'setback_2500', 'setback_3200', 'setback_5280'), paste0(target, "ft"), target)]


scen_sel[, target_policy := fifelse(target == "no_target", "no_target_policy", "prod_quota")]

## create dfs for target scenarios, bind to main df
## -------------------------------------------------

## excise targets
## -----------------------
excise_target_df <- expand.grid(oil_price_scenario = unique(oilpx_scens_names[, oil_price_scenario]),
                                setback_scenario = unique(setback_scens_name[, setback_scenario]),
                                prod_quota_scenario = "no quota",
                                carbon_price_scenario = unique(carbonpx_scens_name[, carbon_price_scenario]),
                                ccs_scenario = unique(ccs_scens_names[, ccs_scenario]),
                                innovation_scenario = unique(innovation_scens_name[, innovation_scenario]),
                                excise_tax_scenario = tax_target_vec,
                                BAU_scen = 0)

setDT(excise_target_df)

excise_target_df[, target := str_remove(excise_tax_scenario, pattern = "tax_")]

excise_target_df[, target_policy := "excise_tax"]

## filter for: setback_1000ft, setback_2500ft, setback_5280ft with no_setback - Updated MP
excise_target_df[, keep := fifelse(setback_scenario == "no_setback" & target %in% c('setback_1000ft',
                                                                                    'setback_2500ft',
                                                                                    'setback_3200ft',
                                                                                    'setback_5280ft'), 1,
                                   fifelse(target == "90perc_reduction", 1, 0))]

excise_target_df <- excise_target_df[keep == 1]
excise_target_df[, keep := NULL]


# excise_target_df[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
#                                     carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "-")]
# Testing above MP
excise_target_df[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                    carbon_price_scenario, ifelse(ccs_scenario %like% "- 45Q", "no ccs", ccs_scenario),
                                    innovation_scenario, excise_tax_scenario, sep = "-")]

setcolorder(excise_target_df, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                                'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario', 'BAU_scen', 'target', 'target_policy'))

## carbon tax targets
## ---------------------------------


carbon_target_df <- expand.grid(oil_price_scenario = unique(oilpx_scens_names[, oil_price_scenario]),
                                setback_scenario = unique(setback_scens_name[, setback_scenario]),
                                prod_quota_scenario = "no quota",
                                carbon_price_scenario = unique(carbon_target_vec),
                                ccs_scenario = unique(ccs_scens_names[, ccs_scenario]),
                                innovation_scenario = unique(innovation_scens_name[, innovation_scenario]),
                                excise_tax_scenario = unique(excise_tax_scens_name[, excise_tax_scenario]),
                                BAU_scen = 0)

setDT(carbon_target_df)

## add target
carbon_target_df[, target := str_remove(carbon_price_scenario, pattern = "carbon_target_")]

carbon_target_df[, target_policy := "carbon_tax"]

## filter: setback_1000ft, setback_2500ft, setback_5280ft with no_setback -- Updated - MP
carbon_target_df[, keep := fifelse(setback_scenario == "no_setback" & target %in% c('setback_1000ft',
                                                                                    'setback_2500ft',
                                                                                    'setback_3200ft',
                                                                                    'setback_5280ft'), 1,
                                   fifelse(target == "90perc_reduction", 1, 0))]

carbon_target_df <- carbon_target_df[keep == 1]
carbon_target_df[, keep := NULL]

# ## add scen id
# carbon_target_df[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
#                                     carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "-")]
# Testing above MP
carbon_target_df[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                    carbon_price_scenario, ifelse(ccs_scenario %like% "- 45Q", "no ccs", ccs_scenario),
                                    innovation_scenario, excise_tax_scenario, sep = "-")]

setcolorder(carbon_target_df, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                                'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario', 'BAU_scen', 'target', 'target_policy'))


## bind
target_scens <- rbind(excise_target_df, carbon_target_df)

## add target scens to scen sel
## --------------------------------------------------
scen_sel <- rbind(scen_sel, target_scens)


## add toggle that determines if setbacks apply to existing and new wells or just new wells
setback_toggle <- expand.grid(scen_id = unique(scen_sel$scen_id),
                              setback_existing = c(1, 0))

scen_sel_toggle <- left_join(setback_toggle, scen_sel) %>%
  mutate(scen_id = paste(scen_id, setback_existing, sep = "-")) %>%
  as.data.table()


## find scen selection
## ------------------------------

## Updated - MP
subset_dt = unique(## non-taget (all oil, all setback, all carbon px, no tax, low inno, no ccs, no quota)
  scen_sel_toggle[(innovation_scenario == 'low innovation' &
                     carbon_price_scenario == "price floor" &
                     ccs_scenario %in% c("no ccs") & 
                     excise_tax_scenario == 'no tax' &
                     prod_quota_scenario == 'no quota') |
                    ## targets
                    (innovation_scenario == 'low innovation' &
                       !carbon_price_scenario %in% c("price ceiling", "central SCC") & 
                       prod_quota_scenario == 'no quota' &
                       ccs_scenario %in% c("no ccs") &
                       excise_tax_scenario %in% c("no tax", "tax_setback_1000ft", "tax_setback_2500ft", "tax_setback_3200ft", "tax_setback_5280ft", "tax_90perc_reduction") &   
                       target != "no_target")])


## indicate scenarios
scen_sel_toggle[, subset_scens := fifelse(scen_id %in% subset_dt[, scen_id], 1, 0)]

## set col order
setcolorder(scen_sel_toggle, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                               'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario', 'target', 'target_policy', 'subset_scens', 'BAU_scen'))


# Updated - MP
fwrite(scen_sel_toggle, file.path('data/processed/scenario_id_list_targets_v5.csv'), row.names = F)