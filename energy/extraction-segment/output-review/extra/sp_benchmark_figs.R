## Tracey Mangin
## June 23, 2021
## spatial figure for diagnostics

library(tidyverse)
library(data.table)
library(sf)
library(maps)
library(cowplot)
library(scales)

## path
proj_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/"

## output folder
output_folder <- 'outputs/predict-production/extraction_2021-06-25/revised-new-entry-model/'

## save path
save_path <- paste0(proj_dir, output_folder, 'benchmark-figs/')

## files
prod_file    <- "well_prod_m_processed.csv"

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## read in outputs from most recent run
field_outputs <- fread(paste0(proj_dir, output_folder, "benchmark-field-level-results.csv"), colClasses = c('doc_field_code' = 'character'))

## boundaries
boundaries <- st_read(file.path(proj_dir, "data/GIS/raw/field-boundaries/DOGGR_Admin_Boundaries_Master.shp")) %>% st_transform(ca_crs)

## counties boundaries
county_boundaries <- st_read(file.path(proj_dir, "data/GIS/raw/CA_Counties/CA_Counties_TIGER2016.shp")) %>% st_transform(ca_crs) %>%
  dplyr::select(adj_county_name = NAME)

## monthly well production
well_prod <- fread(paste0(proj_dir, "data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                               'doc_field_code' = 'character'))

## county
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

## get 2020 and 2045 field-level production - BAU
pred_prod <- field_outputs %>%
  filter(year %in% c(2020, 2045) &
         oil_price_scenario == 'iea oil price' & 
         innovation_scenario == 'low innovation' & 
         carbon_price_scenario == 'price floor' & 
         ccs_scenario == 'medium CCS cost' &
         excise_tax_scenario == 'no tax' &
         setback_scenario == 'no_setback' &
         prod_quota_scenario == 'no quota') %>%
  group_by(year, doc_field_code, doc_fieldname) %>%
  summarise(prod = sum(total_prod_bbl, na.rm = T)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_prod = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  mutate(rel_prod = prod / total_prod) %>%
  dplyr::select(-total_prod) 

## prod by county 
prod_x_county <- well_prod %>%
  left_join(county_lut) %>%
  group_by(doc_field_code, doc_fieldname, adj_county_name) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code) %>%
  mutate(field_total = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  mutate(rel_prod = prod / field_total) %>%
  dplyr::select(doc_field_code, adj_county_name, rel_prod)

## get 2019 field-level production, join to pred prod
# prod_comp <- well_prod %>%
#   filter(year == 2019) %>%
#   group_by(year, doc_field_code, doc_fieldname) %>%
#   summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
#   ungroup() %>%
#   group_by(year) %>%
#   mutate(total_prod = sum(prod, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(rel_prod = prod / total_prod) %>%
#   dplyr::select(-total_prod) %>%
#   rbind(pred_prod)

# prod_comp2 <- prod_comp %>%
#   pivot_longer(prod:rel_prod, names_to = "measure", values_to = "value") %>%
#   mutate(year = paste0('x', year)) %>%
#   pivot_wider(names_from = year, values_from = value)
# 
# prod_comp2[is.na(prod_comp2)] <- 0
# 
# prod_comp3 <- prod_comp2 %>%
#   mutate(diff_2020_2019 = x2020 - x2019,
#          diff_2045_2019 = x2045 - x2019)

## 2019 vs 2020 comparison, map
# boundary_geom <- boundaries %>%
#   dplyr::select(doc_field_code = FIELD_CODE)


# prod_comp_map <- prod_comp3 %>%
#   left_join(boundary_geom) %>%
#   filter(measure == 'prod') %>% 
#   left_join(county_lut) 
  
## ggplot plot, production
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

california <- states %>% filter(ID == "california") %>%
  st_transform(ca_crs)


# california <- st_make_valid(california)
# ca_cropped <- st_crop(california, xmin = -123, xmax = -117,
#                       ymin = 33, ymax = 38)

# ggplot() +
#   geom_sf(data = california, mapping = aes(fill = NULL), show.legend = FALSE) +
#   geom_sf(data = prod_comp_map, mapping = aes(geometry = geometry, fill = prod), lwd = 0, show.legend = TRUE) +
#   facet_wrap( ~ year, ncol = 3) +
#   scale_fill_viridis_c(option = "plasma",
#                        name = 'production (bbls)') + 
#   theme_bw() +
#   theme(legend.position = "bottom")
  
## county-level
## ---------------------------------------

## county prod for 2019
county_prod_2019 <- well_prod %>%
  filter(year == 2019) %>%
  group_by(doc_field_code, year) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(prod_x_county) %>%
  mutate(county_prod = prod * rel_prod) %>%
  group_by(year, adj_county_name) %>%
  summarise(county_prod = sum(county_prod, na.rm = T)) %>%
  ungroup() 

sum(county_prod_2019$county_prod)

## 
pred_prod_county <- field_outputs %>%
  filter(year %in% c(2020, 2045) &
           oil_price_scenario == 'iea oil price' & 
           innovation_scenario == 'low innovation' & 
           carbon_price_scenario == 'price floor' & 
           ccs_scenario == 'medium CCS cost' &
           excise_tax_scenario == 'no tax' &
           setback_scenario == 'no_setback' &
           prod_quota_scenario == 'no quota') %>%
  group_by(year, doc_field_code, doc_fieldname) %>%
  summarise(prod = sum(total_prod_bbl, na.rm = T)) %>%
  ungroup() %>%
  left_join(prod_x_county) %>%
  mutate(county_prod = prod * rel_prod) %>%
  group_by(year, adj_county_name) %>%
  summarise(county_prod = sum(county_prod, na.rm = T)) %>%
  ungroup()

## all county
all_county_prod <- rbind(pred_prod_county, county_prod_2019) %>%
  mutate(year = paste0('x', year)) %>%
  pivot_wider(names_from = year, values_from = county_prod) %>%
  mutate(x2019 = ifelse(is.na(x2019), 0, x2019),
         x2020 = ifelse(is.na(x2020), 0, x2020),
         x2045 = ifelse(is.na(x2045), 0, x2045)) %>%
  mutate(diff_2020_2019 = x2020 - x2019,
         rel_2020_2019= diff_2020_2019 / x2019,
         diff_2045_2019 = x2045 - x2019,
         rel_2045_2019 = diff_2045_2019 / x2019) %>%
  filter(!is.na(rel_2020_2019)) %>%
  left_join(county_boundaries)

## plot
all_county_prod_df <- all_county_prod %>%
  dplyr::select(adj_county_name, diff_2020_2019, rel_2020_2019) %>%
  pivot_longer(diff_2020_2019:rel_2020_2019, names_to = 'metric', values_to = 'values') %>%
  mutate(metric = ifelse(metric == 'diff_2020_2019', 'difference (bbls)', '% difference'),
         adj_val = ifelse(metric == 'difference (bbls)', values / 1e6, values * 100),
         adj_val = ifelse(metric == '% difference' & adj_val > 300, 300, adj_val)) %>%
  left_join(county_boundaries)

comp_2019_2020_bbls <- ggplot() +
  geom_sf(data = california, mapping = aes(fill = NULL), show.legend = FALSE) +
  geom_sf(data = all_county_prod_df %>% filter(metric == 'difference (bbls)'), mapping = aes(geometry = geometry, fill = adj_val), lwd = 0.25, show.legend = TRUE) +
  scale_fill_gradient2(midpoint = 0, low = "red", mid = "white",
                        high = "blue") +
  labs(title = 'Change in production: 2020 vs 2019',
       fill = 'million bbls',
       x = NULL,
       y = NULL) +
  geom_sf_text(data = all_county_prod_df %>% filter(metric == 'difference (bbls)'), aes(geometry = geometry, label = paste0(adj_county_name, '\n ', round(adj_val, digits = 2), ' mbbls')), colour = "black", size = 2) +
  theme_bw() +
  theme(legend.position = "bottom") 

# ggsave(comp_2019_2020_bbls, 
#        filename = file.path(save_path, 'spatial-figs/delta_county_prod_2019_2020.pdf'), 
#        width = 6, 
#        height = 8)
 
## perc

comp_2019_2020_perc <- ggplot() +
  geom_sf(data = california, mapping = aes(fill = NULL), show.legend = FALSE) +
  geom_sf(data = all_county_prod_df %>% filter(metric != 'difference (bbls)'), mapping = aes(geometry = geometry, fill = adj_val), lwd = 0.25, show.legend = TRUE) +
  scale_fill_gradient2(midpoint = 0, low = "red", mid = "white",
                       high = "blue") +
  labs(title = 'Change in production: 2020 vs 2019',
       fill = '% change',
       x = NULL,
       y = NULL,
       subtitle = 'San Mateo = 1211%') +
  geom_sf_text(data = all_county_prod_df %>% filter(metric != 'difference (bbls)'), aes(geometry = geometry, label = paste0(adj_county_name, '\n', round(adj_val), '%')), colour = "black", size = 2) +
  theme_bw() +
  theme(legend.position = "bottom") 

# ggsave(comp_2019_2020_perc, 
#        filename = file.path(save_path, 'spatial-figs/delta_county_perc_prod_2019_2020.pdf'), 
#        width = 6, 
#        height = 8)

fig_2019_2020_comp <- plot_grid(comp_2019_2020_bbls, comp_2019_2020_perc, nrow = 1)

ggsave(fig_2019_2020_comp, 
       filename = file.path(save_path, 'spatial-figs/comparison_2019_2020_production.pdf'), 
       width = 12, 
       height = 8)

## which fiels are driving large changes?
# field_county_df <- prod_comp3 %>%
#   left_join(county_lut)

