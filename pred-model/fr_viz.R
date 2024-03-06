# 3/5/24 - MP

setwd("/capstone/freshcair/meds-freshcair-capstone")

library(ggplot2)
library(here)
library(janitor)
library(sf)
library(tigris)
library(forecast)

# Read in data
field_prod <- read.csv('data/processed/annual_field_county_production_proportion_revised.csv')

# Count the number of unique counties
num_counties <- length(unique(field_prod$county_name))
paste0("Number of counties: ", num_counties)

# Count the number of unique fields 
num_fields <- length(unique(field_prod$doc_field_code))
paste0("Number of fields: ", num_fields)

# Plotting total production in each county per year
prod_county_year <- field_prod %>% 
  group_by(year, county_name) %>% 
  summarize(total_prod = sum(oil_prod, na.rm = TRUE))

prod_year <- field_prod %>% 
  group_by(year) %>% 
  summarize(year_prod = sum(oil_prod, na.rm = TRUE))


ggplot(data = california_counties) +
  geom_sf(aes(fill = TotalNumberOfActiveWells), color = "white") +
  scale_fill_gradient(name = "Total Number of Active Wells", 
                      low = "lightblue", high = "darkblue") +
  labs(title = "Map of California with Total Number of Active Wells by County, 2019") +
  theme_minimal()



## --------------------------------------------
setback_coverage <- read.csv('data/processed/setback_coverage_R.csv')

well_prod_m_processed <- read_csv('data/processed/well_prod_m_processed.csv')

active_wells_per_field_per_year <- well_prod_m_processed %>%
  filter(ProductionStatus == "Active") %>%  # Adjust this condition based on your definition of active wells
  filter(month == 1) %>% 
  group_by(doc_field_code, year) %>%
  summarise(NumberOfActiveWells = n(), .groups = 'drop')

# Make sure types match
field_prod$doc_field_code <- as.character(field_prod$doc_field_code)
field_prod$year <- as.numeric(field_prod$year)

# Merge field code and active wells per field per year data 
field_data_per_year <- merge(field_prod, active_wells_per_field_per_year, by = c("doc_field_code", "year"))

field_data_2019 <- filter(field_data_per_year, year == 2019)

# Group by 'county_name' and summarize
county_2019_summary <- field_data_2019 %>%
  group_by(county_name) %>%
  summarise(
    TotalNumberOfActiveWells = sum(NumberOfActiveWells),
    TotalOilProd = sum(oil_prod)
  )

field_prod_2019_check <- field_prod %>% 
  filter(year == 2019) 
# --------------------------------------------

ces3 <- read_excel('data/inputs/health/ces3results.xlsx')

write.csv(ces3, 'data/inputs/health/ces3results.csv', row.names = FALSE)

dash_cols <- ces3 %>% 
  janitor::clean_names()

# dash_cols <- dash_cols %>% 
#  select(census_tract, total_population, california_county, sb_535_disadvantaged_community, pm2_5_pctl, )

sum_pm <- dash_cols %>%
  group_by(pm2_5) %>%
  summarise(county_count = n_distinct(census_tract))

viz1 <- ggplot(data = dash_cols) +
  geom_histogram(aes(x = pm2_5, fill = sb_535_disadvantaged_community), 
                 bins = 20, position = "identity", alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue"),
                    name = "Disadvantaged\nCommunity") +
  labs(x = "PM2.5 Values", y = "Count of Census Tract", 
       title = "Distribution of Census Tracts by PM2.5 Exposure") +
  theme_bw(base_size = 14) +
  theme(legend.title.align = 0.5, 
        legend.position = "right",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  geom_vline(xintercept = 12, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 16.2, y = 1200, label = "CA air quality standard threshold", 
           size = 5, angle = 0, vjust = 2, hjust = 0.5, color = "black") +
  geom_segment(aes(x = 13, y = 1050, xend = 16, yend = 1050), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
               color = "black", size = 0.5) + # Add this line for the arrow
  annotate("text", x = 14.5, y = 1050, label = "Poor quality", 
           size = 5, angle = 0, vjust = 2, hjust = 0.5, color = "black")

ggsave("data/processed/viz1.png", viz1, height = 8, width = 12)

# --------------------------------------------
# Testing some map viz
library(scales)

california_counties <- counties(state = "CA", cb = TRUE) %>% 
  clean_names()

ca_counties <- ggplot(data = california_counties) +
  geom_sf() +
  geom_sf(data = california_counties[california_counties$NAME == "Kern", ], fill = "red") +  # Highlight Kern County
  ggtitle("Map of California with Kern County Highlighted")

ca <- st_as_sf(map("state", plot = FALSE, fill = TRUE, cb = TRUE)) %>%
  filter(ID == "california")

county_prod_2019 <- field_prod %>%
  filter(year == 2019) %>%
  group_by(county_name) %>%
  summarise(
    oil_prod = sum(oil_prod),
    prop_production = sum(prop_production)
  )

california_counties <- california_counties %>%
  left_join(county_2019_summary, by = c("name" = "county_name"))

buffer_5280 <- st_read(here("data/processed/buffer_5280ft.shp")) 

viz2 <- ggplot() +
  geom_sf(data = california_counties, aes(fill = TotalNumberOfActiveWells), color = "white") +
  geom_sf(data = buffer_5280, color = "red", fill = 'red', alpha = 0.3) + 
  scale_fill_gradient(name = "Number of active wells", 
                      low = "lightblue", high = "darkblue",
                      labels = label_comma()) +
  labs(title = "Number of Active Wells by County in 2019 with 5,280 Foot Buffer") +
  coord_sf(xlim = c(-122, -116), ylim = c(32, 38), expand = FALSE) +
  theme_bw()

ggsave("data/processed/viz2.png", viz2, width = 12, height = 8)

# # --------------------------------------------
# Testing regression
prod_county_year_wide <- prod_county_year %>%
  pivot_wider(
    names_from = year,
    values_from = total_prod
  ) %>% 
  filter(rowSums(select(., -county_name) != 0) > 0)

X <- as.matrix(prod_county_year_wide[, -1])

alameda_data <- as.numeric(prod_county_year_wide[1, -1])
alameda_data[is.na(alameda_data)] <- 0
ts_alameda <- ts(alameda_data, start = 1977, frequency = 1)
fit_alameda <- auto.arima(ts_alameda)
forecast_alameda <- forecast(fit_alameda, h = 26)
plot(forecast_alameda)

forecasts_list <- list()

for (i in 1:nrow(prod_county_year_wide)) {
  county_data <- as.numeric(prod_county_year_wide[i, -1])
  
  # Replace NAs with 0s
  county_data[is.na(county_data)] <- 0
  
  # Convert to a time series object
  ts_county <- ts(county_data, start = 1977, frequency = 1)
  
  # Forecast 
  fit <- auto.arima(ts_county)
  forecast_result <- forecast(fit, h = 26)  # Forecasting up to 2045
  
  forecasts_list[[i]] <- forecast_result
}

num_years <- length(2020:2045)  
num_counties <- nrow(prod_county_year_wide)  

plot_data <- data.frame(
  Year = rep(2020:2045, each = num_counties),
  Forecast = NA,  
  County = rep(prod_county_year_wide$county_name, times = num_years)
)

for (i in 1:length(forecasts_list)) {
  plot_data$Forecast[plot_data$County == prod_county_year_wide$county_name[i]] <- forecasts_list[[i]]$mean
}
ggplot(plot_data, aes(x = Year, y = Forecast, color = County)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Oil Production Forecast by County (2020-2045)", x = "Year", y = "Forecasted Production") +
  theme(legend.position = "bottom")
