# Created 4/9 - MP

# Load some libraries
library(dplyr)
library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
library(stringr)
library(ggplot2)
library(ggridges)
library(sf)
library(rmapshaper)
library(mapview)
library(tidycensus)

### Well location data  --------------------------------------------

ca_crs <- 3488

well_colors <- c("Active" = "#0747f7", "Plugged" = "yellow", "Abeyance" = "#f7cf07", "Abandoned" = "red",
                 "Idle" = "#f7cf07", "New" = "#0747f7", "PluggedOnly" = "yellow", "Unknown" = "grey")

# Define CA
ca <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID == "california") %>%
  st_transform(ca_crs)

# Get data for each unique well
wells <- sf::st_read("data/proprietery-data/AllWells_gis/Wells_All.shp") %>% 
  st_transform(ca_crs) %>%
  dplyr::select(API, WellStatus, FieldName) %>%
  unique()

# Count number of wells for each status
status_groups <- wells %>% 
  group_by(WellStatus) %>% 
  summarise(count = n())

# Regroup wells 
wells <- wells %>%
  mutate(WellStatus = case_when(
    WellStatus %in% c("Active", "New") ~ "Active",
    WellStatus %in% c("PluggedOnly") ~ "Plugged",
    WellStatus %in% c("Abeyance") ~ "Idle",
    TRUE ~ WellStatus
  )) 

# Separating well status into separate dfs
active_wells <- wells %>% filter(WellStatus == "Active")
canceled_wells <- wells %>% filter(WellStatus == "Canceled")
idle_wells <- wells %>% filter(WellStatus == c("Idle", "Abeyance"))
plugged_wells <- wells %>% filter(WellStatus == c("Plugged", "PluggedOnly"))
unknown_wells <- wells %>% filter(WellStatus == "Unknown")

wells_loc_map <- mapview(ca, layer.name = "California", alpha.regions = 0.5, homebutton = TRUE, layersControl = TRUE) +
  mapview(active_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Active Wells", legend = TRUE,
          col.regions = well_colors["Active"], pointShape = 21, pointSize = 0.03, pointFill = "black") +
  mapview(plugged_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Plugged Wells", legend = TRUE,
          col.regions = well_colors["Plugged"], pointShape = 21, pointSize = 0.03, pointFill = "black") +
  mapview(unknown_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Unknown Wells", legend = TRUE,
          col.regions = well_colors["Unknown"], pointShape = 21, pointSize = 0.03, pointFill = "black") +
  mapview(canceled_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Abandoned Wells", legend = TRUE,
          col.regions = well_colors["Abandoned"], pointShape = 21, pointSize = 0.03, pointFill = "black")

wells_loc_map
