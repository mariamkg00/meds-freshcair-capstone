# Calepa Carbon Neutrality
# Estimate implied technically recoverable resource in each field

# Ruiwen Lee
# Created:  10 Sep 2020
# Modified: 21 Oct 2020
# Modified (for paper): 12 May 2021

# Initialization
rm(list = ls())
options(max.print = 99999)

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

workDir <- "/Dropbox/Research/CarbonNeutrality/R/"

codeDir <- "codeR"
dataDir <- "dataR"
dataRawDir <- "dataRAW"
tempDir <- "temp"
resultsDir <- "resultsR"
resultsTextDir <- "resultsText"
logDir <- "logs"
docDir <- "docs"
texDir <- "tex"

resultsGoogleDriveDir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/entry-model-results"


# Start

entry_revised <- read_rds(file.path("data/processed/entry_revised.rds"))  # from entry.do

# Estimate total resource by field

# Calculate cumulative prod by field
entry_revised <- entry_revised %>%
  group_by(doc_field_code) %>%
  arrange(year) %>%
  mutate(cum_doc_prod = cumsum(doc_prod)) %>%
  group_by(doc_field_code) %>%
  mutate(sum_doc_prod = max(cum_doc_prod))

entry_revised$resource <- NA  # estimated total resource
entry_revised$cum_prod_1977 <- NA
entry_revised$cum_prod_all <- NA
entry_revised$check <- NA  # flag if resource is larger than latest cumulative prod

entry_revised$brent_inverse <- 1 / entry_revised$brent

# Original method
f_start <- min(entry_revised$doc_field_code)
f_end <- max(entry_revised$doc_field_code)

for (f in f_start:f_end) {
  tryCatch({
    model <- lm(depl ~ brent_inverse + cum_doc_prod, data = subset(entry_revised, doc_field_code == f))
    entry_revised$resource[entry_revised$doc_field_code == f] <- 1 / coef(model)["cum_doc_prod"]
    entry_revised$cum_prod_1977[entry_revised$doc_field_code == f] <- coef(model)["(Intercept)"] * entry_revised$resource[entry_revised$doc_field_code == f]
    entry_revised$cum_prod_all[entry_revised$doc_field_code == f] <- entry_revised$cum_prod_1977[entry_revised$doc_field_code == f] + entry_revised$sum_doc_prod[entry_revised$doc_field_code == f]
    entry_revised$check[entry_revised$doc_field_code == f] <- entry_revised$resource[entry_revised$doc_field_code == f] > entry_revised$cum_prod_all[entry_revised$doc_field_code == f]
  }, error = function(e) {})
}

sum(entry_revised$year == 2019 & entry_revised$cum_prod_1977 < 0)
sum(entry_revised$year == 2019 & entry_revised$check == 0)
entry_revised$resource_left <- entry_revised$resource - entry_revised$cum_prod_all
summary(entry_revised$resource_left[entry_revised$year == 2019 & entry_revised$check == 1])

forecast_depl_revised <- entry_revised %>%
  filter(year == 2019) %>%
  select(doc_field_code, resource, depl) %>%
  rename(depl2019 = depl)

write_csv(select(forecast_depl_revised, doc_field_code, resource), file.path("data/processed/field_resource_revised.csv"))

# Plot for topfields
entry <- read_rds(file.path("/capstone/freshcair/meds-freshcair-capstone/data/processed/entry_revised.rds"))

entry_topfields <- entry %>%
  select(doc_field_code, doc_fieldname, topfield) %>%
  distinct()

forecast_vars <- left_join(entry_topfields, forecast_depl_revised, by = "doc_field_code")

forecast_vars_summary <- forecast_vars %>%
  group_by(topfield) %>%
  summarize(capex_forecast = mean(capex_forecast),
            opex_forecast = mean(opex_forecast))

forecast_vars_summary_wide <- forecast_vars_summary %>%
  pivot_wider(names_from = topfield, values_from = c(capex_forecast, opex_forecast))

capex_plot <- ggplot(forecast_vars_summary_wide, aes(x = year)) +
  geom_line(aes(y = capex_forecast_0, color = "Non-top fields")) +
  geom_line(aes(y = capex_forecast_1, color = "Top fields")) +
  labs(title = "Capex Forecast by Topfield") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave(file.path(docDir, "timeseries", "topfield_forecast_capex.pdf"), plot = capex_plot)

opex_plot <- ggplot(forecast_vars_summary_wide, aes(x = year)) +
  geom_line(aes(y = opex_forecast_0, color = "Non-top fields")) +
  geom_line(aes(y = opex_forecast_1, color = "Top fields")) +
  labs(title = "Opex Forecast by Topfield") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggsave(file.path(docDir, "timeseries", "topfield_forecast_opex.pdf"), plot = opex_plot)

# Check estimated total resource against projected total production
predicted_production <- read_csv(file.path(dataRawDir, "predicted-production_2020-2100_field.csv"))
predicted_production <- predicted_production %>%
  rename(doc_field_code = fieldcode) %>%
  select(-fieldname) %>%
  group_by(doc_field_code) %>%
  summarize(prod_future_sum = sum(production_bbl)) %>%
  filter(year == 2020) %>%
  select(doc_field_code, prod_future_sum)

forecast_depl_merged <- left_join(forecast_depl_revised, predicted_production, by = "doc_field_code")

forecast_depl_merged$cum_prod_2019 <- forecast_depl_merged$depl2019 * forecast_depl_merged$resource
forecast_depl_merged$total_prod <- forecast_depl_merged$cum_prod_2019 + forecast_depl_merged$prod_future_sum
forecast_depl_merged$d_res_prod <- forecast_depl_merged$resource - forecast_depl_merged$total_prod

summary(forecast_depl_merged$d_res_prod)
hist(forecast_depl_merged$d_res_prod)
sum(forecast_depl_merged$d_res_prod < 0)

# Check estimated total resource against DOC 2009 report
doc_reserves <- read_csv(file.path(dataRawDir, "oil_production_reserves_2009.csv"))
doc_reserves <- doc_reserves %>%
  mutate(doc_field_code_orig = doc_field_code,
         doc_field_code = as.numeric(doc_field_code),
         cum_prod = as.numeric(cumulative_oil_and_condensatembb),
         reserves_left = as.numeric(estimated_oil_reservesmbbl)) %>%
  replace_na(list(cum_prod = 0, reserves_left = 0)) %>%
  filter(!is.na(doc_field_code)) %>%
  mutate(resource_doc = cum_prod + reserves_left) %>%
  select(doc_field_code, doc_fieldname, field_name, cum_prod, reserves_left, resource_doc)

forecast_depl_doc <- left_join(forecast_depl_revised, doc_reserves, by = "doc_field_code") %>%
  select(doc_field_code, doc_fieldname, resource, depl2019, field_name, resource_doc) %>%
  mutate(resource = resource / 1000)  # convert from bbl to Mbbl

ggplot(forecast_depl_doc, aes(x = resource_doc, y = resource)) +
  geom_point() +
  geom_text(aes(label = doc_fieldname), vjust = -0.5, size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "Resource (DOC)", y = "Resource (Estimated)", title = "Comparison of Estimated Resource and DOC Resource") +
  theme_minimal()