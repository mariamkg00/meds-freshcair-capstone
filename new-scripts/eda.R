# Updated 4/8/24 - MP

# Load necessary libraries
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

# Read in well processed data
well_prod_m_processed <- read.csv("data/processed/well_prod_m_processed.csv")

# Inspect the structure of the well_prod_m_processed dataset
str(well_prod_m_processed)

# Calculate and print the number of unique wells
num_wells <- length(unique(well_prod_m_processed$api_ten_digit))
paste0("Number of unique wells: ", num_wells)

# Calculate and print the number of counties
num_counties <- length(unique(well_prod_m_processed$county))
paste0("Number of counties: ", num_counties)

# Calculate and print the number of unique field codes
num_fieldcodes <- length(unique(well_prod_m_processed$doc_field_code))
paste0("Number of unique field codes: ", num_fieldcodes)

# Extract and print all unique well production types
well_productions <- unique(well_prod_m_processed$well_type_name)
paste0(well_productions)

# Filter wells with BTU of gas produced greater than 0, indicating production
producing_wells <- well_prod_m_processed %>% 
  filter(BTUofGasProduced > 0)

# Calculate and print the percentage of each production status across all wells
percentage_production_status <- well_prod_m_processed %>%
  count(ProductionStatus) %>%
  mutate(Percentage = n / sum(n) * 100)

# Calculate and print the total BTU of gas produced for each well
total_btu_per_well <- well_prod_m_processed %>%
  group_by(api_ten_digit) %>%
  summarise(TotalBTUofGasProduced = sum(BTUofGasProduced, na.rm = TRUE))
print(total_btu_per_well)

# Make sure ProductionReportDate is date-time class
well_prod_m_processed$ProductionReportDate <- as.Date(well_prod_m_processed$ProductionReportDate)

# Calculate the first production date for each well and ungroup
well_start_dates <- well_prod_m_processed %>%
  filter(ProductionStatus == "Active") %>%
  group_by(api_ten_digit) %>%
  summarise(FirstProductionDate = min(ProductionReportDate, na.rm = TRUE)) %>%
  ungroup()

# Join the first production date back to the original dataset
well_prod_m_processed_updated <- well_prod_m_processed %>%
  left_join(well_start_dates, by = "api_ten_digit")

# Calculate the number of active years for each well
active_months_per_well <- well_prod_m_processed_updated %>%
  filter(ProductionStatus == "Active") %>%
  group_by(api_ten_digit) %>%
  summarise(ActiveYears = n()/12) %>%
  ungroup()

# Join the active years data back to the dataset
processed <- well_prod_m_processed_updated %>%
  left_join(active_months_per_well, by = "api_ten_digit")

# Filter for wells with an active production status
processed_active <- processed %>% 
  filter(ProductionStatus == "Active") %>% 
  arrange(api_ten_digit, ProductionReportDate) %>%
  group_by(api_ten_digit) %>%
  mutate(MonthInProduction = row_number()) %>% # Assigns a sequential number to each active month
  ungroup()

# Calculate the last production date for each well
well_last_dates <- well_prod_m_processed %>%
  filter(ProductionStatus == "Active") %>%
  group_by(api_ten_digit) %>%
  summarise(LastProductionDate = max(ProductionReportDate, na.rm = TRUE)) %>%
  ungroup()

# Join the last production date to the active wells dataset
processed_active <- processed_active %>%
  left_join(well_last_dates, by = "api_ten_digit")

# Group the processed_active data and calculate summary statistics
processed_active_grouped <- processed_active %>%
  group_by(api_ten_digit) %>%
  summarize(
    TotalProduction = sum(BTUofGasProduced, na.rm = TRUE),
    ActiveYears = ActiveYears,
    FirstProductionDate = min(ProductionReportDate, na.rm = TRUE), # First production date
    LastProductionDate = max(ProductionReportDate, na.rm = TRUE) # Last production date
  ) %>%
  distinct(api_ten_digit, .keep_all = TRUE) %>%
  filter(TotalProduction > 0)

# Plot histogram of well active years
active_years_plot <- ggplot(processed_active_grouped, aes(x = ActiveYears)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  labs(x = "Active Years", y = "Count of Wells", title = "Histogram of Well Active Years") +
  theme_bw() 

# Calculate the number of active wells per month
active_wells_per_month <- processed_active %>%
  group_by(month_year = floor_date(ProductionReportDate, "month")) %>%
  summarise(ActiveWells = n_distinct(api_ten_digit)) %>%
  ungroup()

# Plot the number of active wells over time
ggplot(active_wells_per_month, aes(x = month_year, y = ActiveWells)) +
  geom_line() + 
  geom_point() + 
  labs(x = "Year", y = "Number of Active Wells", title = "Active Wells Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### BTU ----------------------------------------

# Calculate the total BTU produced per month
total_production_per_month <- processed_active %>%
  group_by(month_year = floor_date(ProductionReportDate, "month")) %>%
  summarise(TotalBTUProduced = sum(BTUofGasProduced, na.rm = TRUE)) %>%
  ungroup()

# Plot the total BTU of gas produced over time
ggplot(total_production_per_month, aes(x = month_year, y = TotalBTUProduced)) +
  geom_line(color = "blue") +
  geom_point(color = "red") + 
  labs(x = "Year", y = "Total BTU of Gas Produced", title = "Total Gas Production Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### GAS  ----------------------------------------

# Calculate and print the total Gas Produced for each well
total_gas_per_well <- well_prod_m_processed %>%
  group_by(api_ten_digit) %>%
  summarise(TotalGasProduced = sum(GasProduced, na.rm = TRUE))
print(total_gas_per_well)

# Calculate the total Gas Produced per month
total_gas_production_per_month <- processed_active %>%
  group_by(month_year = floor_date(ProductionReportDate, "month")) %>%
  summarise(TotalProduced = sum(OilorCondensateProduced + GasProduced + WaterProduced, na.rm = TRUE)) %>%
  ungroup()

# Make sure data is ordered
total_gas_production_per_year <- total_gas_production_per_month %>%
  arrange(month_year)

# Calculate the 1 year rolling average for TotalGasProduced
total_gas_production_per_year$TotalGasProducedRollingAvg <- rollmean(total_gas_production_per_year$TotalProduced, 12, fill = NA, align = 'right')

# Plot the total Gas Produced over time with the 1 year rolling average
ggplot(total_gas_production_per_year, aes(x = month_year)) +
  geom_line(aes(y = TotalGasProducedRollingAvg), color = "red", linetype = "solid") +
  labs(x = "Month", y = "Total Gas Produced (units)", title = "Total Gas Production Over Time (One Year Rolling Average)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculating number of active wells by field code over time --------------------
# Filter for active wells. Define 'active' based on your criteria, here assuming BTUofGasProduced > 0
active_wells <- well_prod_m_processed_updated %>%
  filter(ProductionStatus == "Active")

# Calculate the cumulative count of active wells for each field code
active_wells_summary <- active_wells %>%
  group_by(doc_field_code) %>%
  summarise(TotalActiveWells = n_distinct(api_ten_digit)) %>%
  ungroup()

# Identify the top 5 field codes with the highest number of active wells
top_field_codes <- active_wells_summary %>%
  top_n(5, TotalActiveWells) %>%
  pull(doc_field_code)

# Filter the dataset to include only the top 5 field codes
top_active_wells_by_field_and_time <- active_wells %>%
  filter(doc_field_code %in% top_field_codes) %>%
  group_by(doc_field_code, month_year = floor_date(ProductionReportDate, "month")) %>%
  summarise(ActiveWells = n_distinct(api_ten_digit)) %>%
  ungroup()

# Plot the number of active wells over time for the top 5 field codes
ggplot(top_active_wells_by_field_and_time, aes(x = month_year, y = ActiveWells, color = as.factor(doc_field_code))) +
  geom_line() +
  labs(x = "Month", y = "Number of Active Wells", title = "Top 5 Field Codes: Active Wells Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_text("Field Code"))

well_prod_m_processed_grouped_fields <- active_wells %>%
  mutate(
    doc_field_first_digit = substr(as.character(doc_field_code), 1, 1),
    month_year = floor_date(ProductionReportDate, "month")
  )

# Group by the first digit of doc_field_code and month_year, then calculate average active wells
average_active_wells_by_group_and_time <- well_prod_m_processed_grouped_fields %>%
  group_by(doc_field_first_digit, month_year) %>%
  summarise(AverageActiveWells = mean(n_distinct(api_ten_digit))) %>%
  ungroup()

# Plot the trends over time for each first digit group
ggplot(average_active_wells_by_group_and_time, aes(x = month_year, y = AverageActiveWells, color = doc_field_first_digit)) +
  geom_line() +
  labs(x = "Month", y = "Average Number of Active Wells", title = "Average Active Wells by First Digit of Field Code Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_text("First Digit of Field Code"))

# Visualizing the number of active wells in each county over time -----------
well_prod_m_processed_updated <- well_prod_m_processed_updated %>%
  mutate(year = year(ProductionReportDate))

# Filter for Active wells and calculate the total active wells per county
total_active_wells_per_county <- well_prod_m_processed_updated %>%
  filter(ProductionStatus == "Active") %>%
  group_by(county_name) %>%
  summarise(TotalActiveWells = n_distinct(api_ten_digit), .groups = 'drop') %>%
  top_n(10, TotalActiveWells)

top_counties_active_wells_over_time <- well_prod_m_processed_updated %>%
  filter(ProductionStatus == "Active", county_name %in% total_active_wells_per_county$county_name) %>%
  mutate(year = year(ProductionReportDate)) %>%
  group_by(county_name, year) %>%
  summarise(ActiveWells = n_distinct(api_ten_digit), .groups = 'drop') %>%
  ungroup()

# Plotting with log scale
ggplot(top_counties_active_wells_over_time, aes(x = year, y = ActiveWells, color = county_name)) +
  geom_line() +
  geom_point() +
  scale_y_log10() + # Apply log scale to Y axis
  labs(title = "Number of Active Wells by Top 10 Counties Over Time (Log Scale)",
       x = "Year",
       y = "Number of Active Wells (Log Scale)") +
  theme_bw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "County Name"))

### Setback coverage by buffer ---------------------- ----------------------

# Read in setback coverage data
coverage <- read_csv('data/processed/setback_coverage_R.csv') %>% 
  janitor::clean_names()

# Adding sq miles covered in each field
coverage <- coverage %>% 
  mutate(area_sq_mi = round(area_sq_mi * rel_coverage, digits = 6)) %>% 
  mutate(covered_sq_mi = area_sq_mi * rel_coverage)

# Group the data by setback_scenario and calculate the total area_sq_mi for each scenario
setback_areas <- coverage %>%
  group_by(setback_scenario) %>%
  summarise(total_area_sq_mi = sum(area_sq_mi)) 

# Pivot the data to create columns for each setback scenario
setback_areas_wide <- setback_areas %>%
  pivot_wider(names_from = setback_scenario, values_from = total_area_sq_mi)

# Add a row with the setback distance for each scenario
setback_distances <- c(0, 1000, 2500, 3200, 5280)
setback_areas_wide <- setback_areas_wide %>%
  mutate(setback_distance = setback_distances)

# Reshape the data to long format for plotting
setback_areas_long <- setback_areas_wide %>%
  pivot_longer(
    cols = -setback_distance,
    names_to = "setback_scenario",
    values_to = "total_area_sq_mi"
  )

# Plot the relationship between setback distance and total area
library(ggplot2)

ggplot(setback_areas_long, aes(x = setback_distance, y = total_area_sq_mi, color = setback_scenario)) +
  geom_line() +
  geom_point() +
  labs(x = "Setback Distance (ft)", y = "Total Area (sq mi)", color = "Setback Scenario") +
  theme_minimal()

# Print the results
print(setback_areas_wide)
  

# Quartile stats for average coverage for each setback distance
buffer_summary <- coverage %>% 
  group_by(setback_scenario) %>% 
  summarise(
    n = n(),
    min = min(rel_coverage),
    q1 = quantile(rel_coverage, 0.25),
    median = median(rel_coverage),
    mean = mean(rel_coverage),
    q3 = quantile(rel_coverage, 0.75),
    max = max(rel_coverage),
    sd = sd(rel_coverage)
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), round, 3))

print(buffer_summary)

# Box plot of relative coverage by setback
ggplot(coverage, aes(x = setback_scenario, y = rel_coverage)) +
  geom_boxplot() +
  labs(x = "Setback Scenario", y = "Relative Coverage") +
  theme_minimal()

# Violin plot of relative coverage by setback 
ggplot(coverage, aes(x = setback_scenario, y = rel_coverage)) +
  geom_violin() +
  labs(x = "Setback Scenario", y = "Relative Coverage") +
  theme_minimal()

# Ridge plot of relative coverage by setback
ggplot(coverage, aes(x = rel_coverage, y = setback_scenario, fill = setback_scenario)) +
  geom_density_ridges(alpha = 0.6) +
  labs(x = "Relative Coverage", y = "Setback Scenario") +
  theme_minimal() +
  theme(legend.position = "none")

# Box plot of relative coverage 
ggplot(coverage, aes(x = rel_coverage, fill = setback_scenario)) +
  geom_histogram(binwidth = 0.1, alpha = 0.7, position = "identity") +
  labs(x = "Relative Coverage", y = "Count", fill = "Setback Scenario") +
  theme_minimal() +
  facet_wrap(~setback_scenario, ncol = 1) +
  scale_fill_discrete(name = "Setback Scenario") +
  theme(legend.position = "none")

### Checking well locations  --------------------------------------------
ca_crs <- 3488

wells <- sf::st_read("data/proprietery-data/AllWells_gis/Wells_All.shp") %>% 
  st_transform(ca_crs) %>%
  dplyr::select(API, WellStatus, FieldName) %>%
  unique()

# Regroup wells 
wells <- wells %>%
  mutate(WellStatus = case_when(
    WellStatus %in% c("Active", "New") ~ "Active",
    WellStatus %in% c("PluggedOnly") ~ "Plugged",
    WellStatus %in% c("Abeyance", "Canceled", "Idle") ~ "Abandoned",
    TRUE ~ WellStatus
  )) %>%
  mutate(api_num = as.numeric(API)) %>%  # Remove first 3 digits
  select(-API)

# Getting separate well status
active_wells <- wells %>% filter(WellStatus == "Active")
abandoned_wells <- wells %>% filter(WellStatus == "Abandoned")
plugged_wells <- wells %>% filter(WellStatus == "Plugged")
unknown_wells <- wells %>% filter(WellStatus == "Unknown")

well_colors <- c("Active" = "#0747f7", "Plugged" = "yellow", "Abeyance" = "#f7cf07", "Abandoned" = "red",
                 "Idle" = "#f7cf07", "New" = "#0747f7", "PluggedOnly" = "yellow", "Unknown" = "grey")

ca <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  filter(ID == "california") %>%
  st_transform(ca_crs)

interactive_map <- mapview(ca, layer.name = "California", alpha.regions = 0.5, homebutton = TRUE, layersControl = TRUE) +
  mapview(active_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Active Wells", legend = TRUE,
          col.regions = well_colors["Active"], pointShape = 21, pointSize = 0.03, pointFill = "black") +
  mapview(plugged_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Plugged Wells", legend = TRUE,
          col.regions = well_colors["Plugged"], pointShape = 21, pointSize = 0.03, pointFill = "black") +
  mapview(unknown_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Unknown Wells", legend = TRUE,
          col.regions = well_colors["Unknown"], pointShape = 21, pointSize = 0.03, pointFill = "black") +
  mapview(abandoned_wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Abandoned Wells", legend = TRUE,
          col.regions = well_colors["Abandoned"], pointShape = 21, pointSize = 0.03, pointFill = "black")


# Display the map
interactive_map

# # Heat map
# well_buffers <- st_buffer(wells, dist = 10000 * 0.3048)
# 
# interactive_map <- mapview(ca, layer.name = "California", alpha.regions = 0.5, homebutton = TRUE, layersControl = TRUE) +
#   mapview(well_buffers, zcol = "WellStatus", crs = ca_crs, layer.name = "Well Buffers", legend = TRUE,
#           col.regions = c("purple", "yellow"), alpha.regions = c(1, 0.5)) +
#   mapview(wells, zcol = "WellStatus", crs = ca_crs, layer.name = "Wells", legend = TRUE,
#           col.regions = well_colors, pointShape = 21, pointSize = 1.5, pointFill = "black")
# 
# interactive_map


### Census tract analysis   ----------------------------------------------------------------------------------------
# Get census tract boundaries
library(tigris)
options(tigris_use_cache = TRUE)

census_tracts <- get_acs(
  geography = "tract",
  variables = c(total_pop = "B01003_001"),
  state = "CA",
  geometry = TRUE,
  year = 2022
) %>%
  st_transform(ca_crs)

# Checking out census variables
v22 <- load_variables(2022, "acs5", cache = TRUE)

filtered_v22 <- v22 %>%
  filter(grepl("minority", label, ignore.case = TRUE))

census_tract_groups <- census_tracts %>% 
  group_by(NAME)

# Join well counts by census info
wells_by_tract <- st_join(wells, census_tracts, join = st_within)

# Count wells in each census tract
wells_count <- wells_by_tract %>%
  group_by(NAME) %>%
  summarise(total_wells = n())

interactive_map <- mapview(wells_by_tract, zcol = "WellStatus", layer.name = "Wells and Census Tracts")

interactive_map

library(RColorBrewer)

reds <- colorRampPalette(brewer.pal(5, "Reds"))(5)

wells_count_sf <- st_as_sf(wells_count, coords = NULL, crs = st_crs(census_tracts))

wells_count_df <- st_drop_geometry(wells_count)

# Join total_wells from wells_count to census_tracts
census_tracts_with_wells <- census_tracts %>%
  left_join(wells_count_df %>% 
              select(NAME, total_wells), by = "NAME") 

# # Check the updated structure
# str(census_tracts_with_wells)

interactive_map_tracts <- mapview(ca, layer.name = "California", alpha.regions = 0.5) +
  mapview(census_tracts_with_wells, zcol = "total_wells", layer.name = "Num of wells", at = seq(0, 40000, 5000, na.rm = TRUE), length.out = 9)

interactive_map_tracts

# Kern county -----
kern_county_tracts <- census_tracts_with_wells %>%
  filter(grepl("Kern County", NAME)) %>% 
  filter(!is.na(total_wells))

kern_county_wells <- census_tracts_with_wells %>%
  filter(grepl("Kern County", NAME))

kern_county_wells <- kern_county_wells %>%
  mutate(total_wells_cat = case_when(
    total_wells == 0 ~ "0",
    total_wells > 0 & total_wells <= 100 ~ "1-100",
    total_wells > 100 & total_wells <= 1000 ~ "101-1000",
    total_wells > 1000 & total_wells <= 10000 ~ "1001-10000",
    total_wells > 10000 ~ "> 10000"
  )) %>%
  mutate(total_wells_cat = factor(total_wells_cat, levels = c("0", "1-100", "101-1000", "1001-10000", "> 10000")))

# # Now plotting
# ggplot(kern_county_wells, aes(x = NAME, fill = total_wells_cat)) +
#   geom_point(stat = "count") +
#   scale_fill_manual(values = c("0" = "#1f77b4", "1-100" = "green", "101-1000" = "yellow", "1001-10000" = "#d62728", "> 10000" = "#9467bd"),
#                     labels = c("0", "1-100", "101-1000", "1001-10000", "> 10000"),
#                     breaks = c("0", "1-100", "101-1000", "1001-10000", "> 10000")) +
#   labs(fill = "Number of Wells", x = total_wells, y = "Frequency") +
#   theme_minimal() +
#   labs(title = NULL, # Correctly removing the title
#        subtitle = NULL, # Correctly removing the subtitle
#        caption = NULL, # Correctly removing the caption
#        x = "X-axis Label",
#        y = "Y-axis Label",
#        fill = NULL) + # Correctly removing the legend title for fill
#   theme(legend.title.align = 0.5)

kern_county_map <- mapview(kern_county_tracts, zcol = "total_wells", layer.name = "Wells Count", legend = TRUE)

kern_county_map

# !Kern county ------
non_kern_county_tracts <- census_tracts_with_wells %>%
  filter(!grepl("Kern County", NAME))

non_kern_county_wells <- non_kern_county_tracts %>%
  select(NAME, total_wells, geometry) %>%
  filter(!is.na(total_wells)) %>% 
  mutate(well_count_category = case_when(
    total_wells >= 0 & total_wells <= 25 ~ "1-25",
    total_wells >= 26 & total_wells <= 250 ~ "26-250",
    total_wells >= 251 & total_wells <= 2500 ~ "251-2500",
    total_wells >= 2501 & total_wells <= 11000 ~ "2501-11000",
    total_wells > 11000 ~ "> 11000"
  ), well_count_category = factor(well_count_category, 
                                  levels = c("1-25", "26-250", "251-2500", "2501-11000", "> 11000"),
                                  ordered = TRUE)) 

ggplot(non_kern_county_wells, aes(x = NAME, fill = well_count_category)) +
  geom_bar(stat = "count") +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  theme_minimal() +
  labs(fill = "Well Count Category", x = NULL, y = "Frequency") +
  theme(legend.title.align = 0.5)

well_count_bins <- cut(non_kern_county_wells$total_wells, breaks = c(0, 25, 250, 2500, 9000, Inf), include.lowest = TRUE, labels = c("1-25", "26-250", "251-2500", "2501-11000", "> 11000"))

# Create an interactive map for non-Kern County areas
non_kern_county_map <- mapview(non_kern_county_wells, zcol = "well_count_category", layer.name = "Wells Count", legend = TRUE)

# Display the non-Kern County map
non_kern_county_map

mapview(non_kern_county_wells, zcol = as.character(well_count_bins), col.regions = reds, layer.name = "Wells Count", legend = TRUE)

