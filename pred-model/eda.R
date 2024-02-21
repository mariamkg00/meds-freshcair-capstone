# Updated 2/20/24 - MP

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(zoo)
library(stringr)

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
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = "County Name"))
