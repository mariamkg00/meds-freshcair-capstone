## Tracey Mangin
## May 7, 2021
## prep FrackTracker data for analysis
##revised : Feb 14, 2024 by Haejin / Feb 25, 2024 Haejin 
## Updated 3/13/24 MP

# comment out and add your own machine's file path
home <- "/capstone/freshcair/meds-freshcair-capstone" #### revise filepath
ft_path <- "/data/proprietery-data/FracTrackerSetbackdata.gdb" #### revise filepath
save_path <- paste0(home, "/data/processed/") #### revise filepath
setwd(home)

# load packages
library(ggplot2) # MP added
library(plotly) # MP added
library(dplyr) # MP added
library(leaflet) # MP added
library(tmap)
library(sf)
library(tidyverse)
library(purrr)
library(rgdal)
library(gdalUtilities)
library(maps)
library(mapview)

# transform to NAD83(NSRS2007) / California Albers
# units will be in meters
ca_crs <- 3488

## quick CA
ca <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID == "california")

################################# READ DATA AND TRANSFORM

# to get the names of layers in the shapefile
layers <- sf::st_layers(dsn = file.path(home, "/data/proprietery-data/FracTrackerSetbackdata.gdb")) 

## read in the SR layers
layer_vec <- c("SetbackOutlines_SR_Dwellings_082220", "PlaygroundsinCities", "DayCareCenters", "reselderlyCare",
               "CHHS_adultdayhealthcare_csv_Events", "CHHS_altbirthing_csv_Events", "CHHS_Dialysis_csv_Events", 
               "CHHS_healthcare_facility_locations_csv_Events", "CHHS_intermedcarefac_csv_Events", 
               "CHHS_PrimaryCareClinic_csv_Events", "CHHS_psychclinics_csv_Events",
               "CHHS_rehabclinic_csv_Events", "CHHS_skillednursingfacs_csv_Events", "CHHS_surgicalclinic_csv_Events",
               "CHHS_acutecarehospital_csv_Events_1", "CAAcuteCAreHostpitalslatlon_1", "PrivSchoolsCA_1", "SchoolPropCA_1",
               "SchoolsCA_Sabins_1")


## dwellings
sr_dwellings <- sf::st_read(dsn = file.path(home, ft_path), layer = "SetbackOutlines_SR_Dwellings_082220")

# Remove MULTISURFACE types (just first row) - MP
sr_dwellings <- sr_dwellings %>%
  filter(st_geometry_type(.) != "MULTISURFACE")

sr_dwellings <- sr_dwellings %>% 
  st_transform(ca_crs) 
sr_dwellings <- sf::st_cast(sr_dwellings, "MULTIPOLYGON") 
sr_dwellings <- st_union(sr_dwellings)  

## playgrounds
sr_pg <- sf::st_read(dsn = file.path(home, ft_path), layer = "PlaygroundsinCities") %>%
  st_transform(ca_crs) %>%
  dplyr::select(fac_type = FAC_TYPE)

## day care centers
sr_dc <- sf::st_read(dsn = file.path(home, ft_path), layer = "DayCareCenters") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "child day care center") %>%
  dplyr::select(fac_type)

## res elderly care
sr_ec <- sf::st_read(dsn = file.path(home, ft_path), layer = "reselderlyCare") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "retirement community") %>%
  dplyr::select(fac_type)

## chhs adult day health care
sr_adhc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_adultdayhealthcare_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "adult day health care") %>%
  dplyr::select(fac_type)

## alt birthing
sr_ab <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_altbirthing_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "alternative birthing center") %>%
  dplyr::select(fac_type)

## dialysis
sr_d <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_Dialysis_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "dialysis clinic") %>%
  dplyr::select(fac_type)

## health care
sr_hc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_healthcare_facility_locations_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "skilled nursing facility") %>%
  dplyr::select(fac_type)


## health care
sr_pcc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_PrimaryCareClinic_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "primary care clinic") %>%
  dplyr::select(fac_type)


## med care
sr_imc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_intermedcarefac_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "hospice") %>%
  dplyr::select(fac_type)

## psych clinics
sr_pc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_psychclinics_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "psychology clinic") %>%
  dplyr::select(fac_type)

## rehab clinics
sr_rc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_rehabclinic_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "rehab") %>%
  dplyr::select(fac_type)

## skilled nursing facs
sr_snf <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_skillednursingfacs_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "skilled nursing facility") %>%
  dplyr::select(fac_type)

## surgical clinic
sr_sc <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_surgicalclinic_csv_Events") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "surgical clinic") %>%
  dplyr::select(fac_type)

## acute care hospital
sr_ach <- sf::st_read(dsn = file.path(home, ft_path), layer = "CHHS_acutecarehospital_csv_Events_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "acute care hospital") %>%
  dplyr::select(fac_type)

## ca acute care hospital
sr_caach <- sf::st_read(dsn = file.path(home, ft_path), layer = "CAAcuteCAreHostpitalslatlon_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "general acute care hospital") %>%
  dplyr::select(fac_type)

## private schools 
sr_ps <- sf::st_read(dsn = file.path(home, ft_path), layer = "PrivSchoolsCA_1") %>%
  st_transform(ca_crs) %>%
  mutate(fac_type = "private school") %>%
  dplyr::select(fac_type)

# ## schools (polygons) - removing for now MP
# sr_s <- sf::st_read(dsn = file.path(home, ft_path), layer = "SchoolPropCA_1") %>%
#   st_transform(ca_crs) %>%
#   dplyr::filter(st_geometry_type(.) != "MULTISURFACE") %>%  # MP added
#   mutate(fac_type = "school") %>%
#   dplyr::select(fac_type) 

# ##### MP TESTING
# 
# # Step 1: Check for invalid geometries
# invalid_geoms <- st_is_valid(sr_s, NA_on_error = FALSE) == FALSE
# 
# # Print the number of invalid geometries
# cat("Number of invalid geometries:", sum(invalid_geoms), "\n")
# 
# # Step 2: Repair invalid geometries
# # Only proceed if there are invalid geometries
# if (any(invalid_geoms)) {
#   sr_s$Shape <- st_make_valid(sr_s$Shape)
#   
#   # Alternatively, if you want to replace the entire sf object considering all geometries:
#   # sr_s <- st_make_valid(sr_s)
# }
# 
# # Check again to ensure all geometries are now valid
# all_valid_post_repair <- all(st_is_valid(sr_s, NA_on_error = FALSE))
# 
# # Print the status of geometries after repair
# cat("All geometries valid after repair:", all_valid_post_repair, "\n")
# 
# #####
  
# Removing for now, need to figure out how to read polygons since the rest of the data are points- MP ?????
# sr_s <- sf::st_cast(sr_s, "MULTIPOLYGON") # we can see any false geometries format

# ### MP TESTING
# 
# invalid <- st_is_valid(sr_s, NA_on_error = FALSE) == FALSE
# if (any(invalid)) {
#   print(paste("Invalid geometries at positions:", which(invalid)))
# }
# 
# if (any(invalid)) {
#   sr_s <- st_make_valid(sr_s)
# }
# 
# ### END MP TESTING 
# sr_s <- st_union(sr_s) # st_union <- convert multipolygon to polygon

# ## SchoolsCA_Sabins_1 -- having an issue reading these in so commenting out for now - MP
# sr_sca <- sf::st_read(dsn = file.path(home, ft_path), layer = "SchoolsCA_Sabins_1") %>%
#   st_transform(ca_crs) %>%
#   dplyr::filter(st_geometry_type(.) != "MULTISURFACE") %>%  # MP added
#   mutate(fac_type = "school") %>%
#   dplyr::select(fac_type)

# sandy's checks
# ensure that everything is read in: looks ok 19 SR objects
length(layer_vec)
ls(pattern = "sr") %>% length()

# put everything into a list
all <- lapply(ls(pattern= "sr"), get)

all %>% purrr::map(~( .x %>% summary()))

# sr_r and ar_dwellings are multipolygons

# all are points except for 7 and 16 --> multipolygons

## combine points, union
# everything above except the multipolygons
sr_pts <- rbind(sr_pg,
                sr_dc,
                sr_ec,
                sr_adhc,
                sr_ab,
                sr_d,
                sr_hc,
                sr_imc,
                sr_pc,
                sr_rc,
                sr_snf,
                sr_sc,
                sr_ach,
                sr_caach,
                sr_ps,
                #sr_sca,
                sr_pcc)

# st_union()'s help file
# Unioning a set of overlapping polygons has the effect of merging the areas 
# (i.e. the same effect as iteratively unioning all individual polygons together). 
# Unioning a set of LineStrings has the effect of fully noding and dissolving the input linework. 
# In this context "fully noded" means that there will be a node or endpoint in the output for every endpoint or 
# line segment crossing in the input. "Dissolved" means that any duplicate (e.g. coincident) line segments or portions 
# of line segments will be reduced to a single line segment in the output. Unioning a set of Points has the effect of merging 
# all identical points (producing a set with no duplicates).

sr_pts <- st_union(sr_pts)

## simplify dwellings

simp_sr_dwell <- rmapshaper::ms_simplify(sr_dwellings, keep = 0.3, keep_shapes = TRUE, explode = TRUE) # not working = come back soon
length(st_geometry(simp_sr_dwell))

# sandy's checks
# the reduction in size by 20% could probably go further since look so similar
(object.size(sr_dwellings)-object.size(simp_sr_dwell))/object.size(sr_dwellings)

# plot close up to see what is lost
# looks very similar

# define random bounding box to check
xcheck <- c(200000, 230000)
ycheck <- c(-500000,-480000)

par(mfrow = c(1, 2))

plot(sr_dwellings,
     xlim = xcheck ,
     ylim = ycheck,
     border = 1,
     axes = TRUE)

plot(simp_sr_dwell,
     xlim = xcheck ,
     ylim = ycheck,
     border = 1,
     axes = TRUE)

par(mfrow = c(1, 1))
## mapview
mapviewOptions(fgb = FALSE) # -- if map not rendering, run this
mapview(sr_dwellings, layer.name = "dwellings") 
# sandy: this function is not running for me

## save simplified version to view in QGIS and compare
# st_write(simp_sr_dwell, dsn = paste0(save_path, "simplified_dwellings.shp"))

# ### MP TESTING
# 
# # Assuming each object is a data frame with longitude and latitude columns named 'lon' and 'lat'
# objects_to_convert <- list(sr_dwellings, sr_pts, sr_s)
# names(objects_to_convert) <- c("sr_dwellings", "sr_pts", "sr_s")
# 
# converted_objects <- lapply(objects_to_convert, function(df) {
#   if ("lon" %in% names(df) && "lat" %in% names(df)) {
#     st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
#   } else {
#     warning("Data frame does not have 'lon' and 'lat' columns")
#     return(NULL)
#   }
# })
# 
# # Assign converted objects back to your environment
# list2env(converted_objects, envir = .GlobalEnv)
# 
# ### MP TESTING 

## create an sf object for each buffer
## ----------------------------------------

buffer_dist_ft <- c(3200)
ft_meter_val <- 0.3048

create_buffer <- function(dist_ft) {
  
buff_dist_ft_name <- paste0(buffer_dist_ft, "ft")
  
  dist_m <- dist_ft * ft_meter_val
  
  pt_buff_tmp <- sr_pts %>%
    st_buffer(dist = dist_m) %>%
    st_union() 
  
  # looking good!!!
  plot(pt_buff_tmp, xlim = xcheck, ylim = ycheck)
  plot(sr_pts, xlim = xcheck, ylim = ycheck, add = TRUE, pch = 16, cex = .5)
  
  # Commenting out for now - MP
  # schl_buff_tmp <- sr_s %>% 
  #   st_buffer(dist = dist_m) %>%
  #   st_union()
  
  dwelling_buff_tmp <- simp_sr_dwell %>%
    st_buffer(dist = dist_m) %>%
    st_union()
  
  # Commenting out for now - MP
  #out_tmp1 <- st_union(pt_buff_tmp, schl_buff_tmp)
  out_tmp1 <- st_union(pt_buff_tmp)
  
  out_tmp2 <- st_union(dwelling_buff_tmp, out_tmp1)
  
  # uncomment to check
  plot(out_tmp2, xlim = xcheck, ylim = ycheck)
  plot(sr_pts, xlim = xcheck, ylim = ycheck, add = TRUE, pch = 16, cex = .5)
  plot(simp_sr_dwell, xlim = xcheck, ylim = ycheck, add = TRUE, col = "red")
  # plot(sr_s, xlim = xcheck, ylim = ycheck, add = TRUE, col = "blue")
  
  # Write dsn path - MP
  dsn_path <- paste0("data/processed/buffer_", buff_dist_ft_name, ".shp")
  print(dsn_path)
  print(length(dsn_path))
  
  ## save output
  st_write(out_tmp2, dsn = dsn_path) # MP updated
  
}

purrr::map(buffer_dist_ft, create_buffer) 


### MP testing ------------

# Get names of objects starting with "sr"
object_names <- ls(pattern = "^sr")

# Retrieve the objects 
objects <- mget(object_names, envir = .GlobalEnv)

# Extract and display geometry types correctly
geometry_types <- sapply(objects, function(obj) {
  if ("sf" %in% class(obj)) { # Check if the object is an sf object
    geom_types <- st_geometry_type(obj, by_geometry = FALSE) # Get geometry types
    return(unique(as.character(geom_types))) # Ensure unique types are returned as character strings
  } else {
    return("Not an sf object") 
  }
})

# Print the results
print(geometry_types)

setback3200 <- st_read('data/processed/buffer_5280ft.shp')

setback3200_crs <- st_transform(setback3200, st_crs(ca))

ggplot() +
  geom_sf(data = ca, fill = "beige", color = "black") + # Plot CA shapefile
  geom_sf(data = setback3200_crs, color = "blue", size = 3) + # Add points
  theme_minimal() +
  labs(title = "Sensitive Receptors in California")

tm_shape(ca) +
  tm_polygons(col = "beige", border.col = "black") + # Plot CA shapefile
  tm_shape(setback5280_crs) +
  tm_borders(col = "blue", lwd = 3)  # Assuming setback5280_crs are borders; adjust according to actual geometry









