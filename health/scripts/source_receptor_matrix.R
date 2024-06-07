## Directory
# updated: 2/12/24
# 

library(dplyr)
library(tidyverse)
library(sf)
library(purrr)


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('/capstone/freshcair/meds-freshcair-capstone') # Sets directory based on Taylor structure
getwd()


# ## paths 
# main_path        <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
# sp_data_path     <- paste0(main_path, "data/GIS/raw/")

# updated health_data_path 4/8/24 - MG 
health_data_path <- "data-str/public/intermediate/health/inmap_processed_srm/"

# not used - HK
# srm_save_path    <- paste0("data/health/source_receptor_matrix/inmap_processed_srm/")

## Read census tract shp file - UPDATED - MP
census_tract <- read_sf('data-str/public/inputs/gis/census-tract/tl_2019_06_tract.shp') %>%
  st_transform(crs = 3310) %>%
  # sf::st_drop_geometry() %>%
  dplyr::select(-STATEFP:-TRACTCE, -NAME:-INTPTLON)

## counties, no islands - UPDATED - MP
CA_counties <- st_read('data-str/public/inputs/gis/CA_counties_noislands/CA_Counties_TIGER2016_noislands.shp') %>%
  st_transform(crs=3310) %>%
  dplyr::select(OBJECTID, GEOID)

## remove islands
CA_counties_noisl <- CA_counties %>%
  filter(!OBJECTID %in% c(3, 49)) %>%
  dplyr::select(- OBJECTID) %>% 
  st_transform(crs=3310) # added - MP
  

# county_shp <- read_sf("./data/inmap/census-tract/tl_2019_06_tract.shp")%>%
#   select(-STATEFP:-TRACTCE,-NAME:-INTPTLON)%>%
#   st_transform(crs=3310)



#Select sector
sector <- "extraction/" 
#sector <- "refining/"

## set spatial resolutoin for SRM
sp_res <- "county"
# sp_res <- "census-tract-results"
# sp_res <- "cesnsus-tract"

if(sp_res == "county") {
  
  sp_res_path <- "county-results/"
  shp_int <- CA_counties_noisl
  
} else {
  
  sp_res_path <- "census-tract-results/"
  shp_int <- census_tract
  
}

## create directories for each pollutant
pollutants_vec <- c("nh3", "nox", "pm25", "sox", "voc")

## read in files
inmap_files_raw <- list.files(paste0(health_data_path))
inmap_files <- ifelse(stringr::str_sub(inmap_files_raw,-3,-1)=="shp",inmap_files_raw, 0)
inmap_files <- inmap_files[!inmap_files %in% c(0)]

dir.create(paste0("data-str/private/inputs/", sector), showWarnings = FALSE)

# Create the spatial resolution directory inside the sector-specific directory
dir.create(paste0("data-str/private/inputs/", sector, sp_res_path), showWarnings = FALSE)

# Create directories for each pollutant inside the spatial resolution directory
for(i in 1:length(pollutants_vec)) {
  dir.create(paste0("data-str/private/inputs/", sp_res_path, pollutants_vec[i], "/"), recursive = TRUE, showWarnings = FALSE)
}

pattern <- paste0(c("nh3", "nox", "pm25", "sox", "voc"), collapse = "|")

inmap_process_func <- function(x) {
  pol_tmp <- str_extract(x, pattern)

  read_sf(paste0(health_data_path, x)) %>%
    st_transform(crs=3310) %>%
    dplyr::select(-BasePM25:-SOx,-TotalPop, -WindSpeed) %>%
    st_intersection(shp_int) %>%
    dplyr::mutate(area = as.numeric(st_area(.)))%>%
    group_by(GEOID)%>%
    dplyr::mutate(weight = area/sum(area))%>%
    summarize(totalpm25 = mean(TotalPM25, na.rm = T),
              totalpm25_aw = sum(weight * TotalPM25, na.rm = T))%>%
    data.frame()%>%
    dplyr::select(-geometry) %>%
    write.csv(paste0("data-str/public/intermediate/health/inmap_processed_srm/", pol_tmp, "/", substr(x, 1, nchar(x) - 4), ".csv", sep = ""), row.names = FALSE)
}

# inmap_process_func <- function(x) {
#   pol_tmp <- str_extract(x, pattern)
#   
#   shp_data <- read_sf(paste0(health_data_path, x)) %>%
#     st_transform(crs = 3310) %>%
#     dplyr::select(-BasePM25:-SOx, -TotalPop, -WindSpeed)
#   
#   if (!identical(st_crs(shp_data), st_crs(shp_int))) {
#     shp_int_transformed <- st_transform(shp_int, st_crs(shp_data))
#   } else {
#     shp_int_transformed <- shp_int
#   }
#   
#   shp_data %>%
#     st_intersection(shp_int_transformed) %>%
#     mutate(area = as.numeric(st_area(.))) %>%
#     group_by(GEOID) %>%
#     mutate(weight = area / sum(area)) %>%
#     summarize(
#       totalpm25 = mean(TotalPM25, na.rm = T),
#       totalpm25_aw = sum(weight * TotalPM25, na.rm = T)
#     ) %>%
#     data.frame() %>%
#     dplyr::select(-geometry) %>%
#     write.csv(paste0("data-str/public/intermediate/health", pol_tmp, "/", substr(x, 1, nchar(x) - 4), ".csv", sep = ""), row.names = FALSE)
# } 

## run function
purrr::map(as.list(inmap_files), inmap_process_func)


# purrr::map(inmap_files[[1]], inmap_process_func)





# inmap_files <- subset(inmap_files, inmap_files != 0);inmap_fileslapply(unique(inmap_files), function(x))
# 
#   pol_tmp <- str_extract(x, pattern)
# 
#   read_sf(paste0(health_data_path, sector, x)) %>%
#     st_transform(crs=3310) %>%
#     select(-BasePM25:-SOx,-TotalPop, -WindSpeed) %>%
#     st_intersection(shp_int) %>%
#     mutate(area = as.numeric(st_area(.)))%>%
#     group_by(GEOID)%>%
#     mutate(weight = area/sum(area))%>%
#     summarize(totalpm25 = mean(TotalPM25, na.rm = T),
#               totalpm25_aw = sum(weight * TotalPM25, na.rm = T))%>%
#     data.frame()%>%
#     select(-geometry) %>%
#     write.csv(paste0(srm_save_path, sector, sp_res_path, pol_tmp, "/", substr(x,1,nchar(x)-4),".csv", sep=""), row.names = FALSE)
# )

library(mapview)

plot_pm25_fields <- function() {
  shapefile_directory <- "data-str/public/intermediate/health/inmap_processed_srm"
  
  shapefile_names <- paste0("srm_pm25_field", 1:26, ".shp")
  
  combined_sf <- list()
  
  # Iterate over each shapefile
  for (shapefile_name in shapefile_names) {
    shapefile_path <- file.path(shapefile_directory, shapefile_name)
    if (file.exists(shapefile_path)) {
      shapefile <- st_read(shapefile_path)
      print(paste("Shapefile:", shapefile_name))
      print(colnames(shapefile))
      if ("totalpm25_aw" %in% colnames(shapefile)) {
        combined_sf <- append(combined_sf, list(shapefile))
      } else {
        warning(paste("Shapefile", shapefile_name, "does not contain 'totalpm25_aw'"))
      }
    } else {
      warning(paste("Shapefile not found:", shapefile_name))
    }
  }
  
  # Combine all shapefiles into a single sf object
  combined_sf <- do.call(rbind, combined_sf)
  
  # Define a custom color palette
  color_palette <- colorRampPalette(c("white", "yellow", "orange"))(100)
  
  map <- mapview(combined_sf, zcol = "totalpm25_aw", layer.name = "PM2.5 Fields",
                 map.types = "OpenStreetMap", legend = TRUE, homebutton = FALSE,
                 col.regions = color_palette)
  
  return(map)
}

# Plot the PM2.5 fields
plot_pm25_fields()

