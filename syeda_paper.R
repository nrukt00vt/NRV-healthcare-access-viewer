# -----------------------------
# Libraries
# -----------------------------
library(sf)          # for spatial data (CBG polygons)
library(tidyverse)   # for data wrangling + ggplot2
library(lubridate)   # needed for year()/month() used below (load early!)
library(plyr)

# -----------------------------
# Inputs (CSV + Shapefile)
# -----------------------------
# all_POIs: metadata for POIs (must include safegraph_place_id, city, naics_code or naics_code_2, etc.)
all_POIs   = read.csv('AllPOIs_Montgomery_VA.csv')

# NRHD_data: CBG-level covariates (must include 'cbg2019' and 'prop_urban' in [0,1])
NRHD_data  = read.csv("NRHDcovariate.csv")

# time_data: visit counts to POIs over time
time_data  = read.csv("allvisits_VA_new.csv")

# Shapefile of 2019 VA Census Block Groups (51 = Virginia)
shapefile  = read_sf(dsn = "base_files", layer = "tl_2019_51_bg")

# -----------------------------
# Basic cleaning / harmonization
# -----------------------------

# Rename visit count column from 'number' -> 'total_visitors' in time_data
names(time_data)[which(names(time_data) == "number")] = "total_visitors"

# Merge POI metadata to visits by POI ID
# WARNING: Ensure the right-hand key matches your time_data: 'safegraph_place' vs 'safegraph_place_id'.
# If your time_data column is 'safegraph_place_id', change by.y accordingly.
overall_trips_all = merge(
  all_POIs, time_data,
  by.x = "safegraph_place_id",
  by.y = "safegraph_place"
)

# Restrict geography by removing out-of-area cities (example: not in NRHD)
overall_trips_all = subset(overall_trips_all, !is.element(city, c("Leesburg", "Lansdowne")))

# Create a "month" identifier as the first day of each month.
# NOTE: Requires lubridate::year() and lubridate::month().
# If time_data$date is a character, convert first: time_data$date <- as.Date(time_data$date)
overall_trips_all$month = as.Date(
  paste0(year(overall_trips_all$date), "-", month(overall_trips_all$date), "-01")
)

# -----------------------------
# Subset to healthcare POIs (NAICS codes)
# -----------------------------
# NAICS used here:
#   621210 = Dentists
#   621340 = Outpatient PT/OT/Speech
#   621111 = Offices of Physicians
#   621493 = Freestanding Urgent Care
#   622110 = General Medical & Surgical Hospitals
hf_trips_all = subset(
  overall_trips_all,
  is.element(naics_code, c(621210, 621340, 621111, 621493, 622110))
)

# -----------------------------
# Aggregate visits by home CBG and day, then build Feb 2020 baseline
# -----------------------------
cbg_number_visits_all = aggregate(
  overall_trips_all$total_visitors,
  by = list(overall_trips_all$visitor_home_cbg, overall_trips_all$date),
  FUN = sum
)
names(cbg_number_visits_all) = c("home_cbg", "month", "total_visitors")  # 'month' currently holds daily dates
cbg_number_visits_all = subset(cbg_number_visits_all, total_visitors > 0)

# Pull Feb 2020 rows to be used as the pre-pandemic baseline
cbg_feb_2020_all = subset(
  cbg_number_visits_all,
  month > as.Date("2020-01-31") & month < as.Date("2020-03-01")
)

# Keep only the Feb 2020 total and rename for later merge
names(cbg_feb_2020_all)[which(names(cbg_feb_2020_all) == "total_visitors")] = "feb_2020_visitors"
cbg_feb_2020_all = subset(cbg_feb_2020_all, select = -c(month))

# Join Feb 2020 baseline back to full time series by home CBG
cbg_number_visits_all = merge(cbg_number_visits_all, cbg_feb_2020_all, by = c("home_cbg"))

# Normalize each CBG-day by its own Feb 2020 value.
# NOTE: If a CBG has 0 visits in Feb 2020, this will produce Inf; consider guarding against division by zero.
cbg_number_visits_all$ratio_to_feb_2020 =
  cbg_number_visits_all$total_visitors / cbg_number_visits_all$feb_2020_visitors
# e.g., to guard:
# cbg_number_visits_all$ratio_to_feb_2020[!is.finite(cbg_number_visits_all$ratio_to_feb_2020)] <- NA

# -----------------------------
# Repeat aggregation for healthcare-only trips
# -----------------------------
hf_number_visits_all = aggregate(
  hf_trips_all$total_visitors,
  by = list(hf_trips_all$visitor_home_cbg, hf_trips_all$date),
  FUN = sum
)
names(hf_number_visits_all) = c("home_cbg", "month", "total_visitors")
hf_number_visits_all = subset(hf_number_visits_all, total_visitors > 0)

hf_feb_2020_all = subset(hf_number_visits_all, month > as.Date("2020-01-31") & month < as.Date("2020-03-01"))
names(hf_feb_2020_all)[which(names(hf_feb_2020_all) == "total_visitors")] = "feb_2020_visitors"
hf_feb_2020_all = subset(hf_feb_2020_all, select = -c(month))

hf_number_visits_all = merge(hf_number_visits_all, hf_feb_2020_all, by = c("home_cbg"))
hf_number_visits_all$ratio_to_feb_2020 =
  hf_number_visits_all$total_visitors / hf_number_visits_all$feb_2020_visitors
# (Consider the same division-by-zero guard as above.)

# -----------------------------
# Merge CBG covariates and group by urbanicity bins
# -----------------------------
# Join covariates (prop_urban in [0,1]) to the all-trips series
cbg_number_visits_all = subset(cbg_number_visits_all, total_visitors > 0)
cbg_number_visits_all = merge(cbg_number_visits_all, NRHD_data, by.x = "home_cbg", by.y = "cbg2019")

# Bin CBGs by prop_urban. Breaks chosen to create "very rural" / "rural" / "urban" / "very urban".
# The upper bound 1.1 ensures values exactly equal to 1 fall inside the last bin.
cbg_number_visits_all$prop_urban_cut = cut(cbg_number_visits_all$prop_urban, c(0, 0.05, 0.18, 0.95, 1.1))

# For each urbanicity bin and date, compute median visitors (robust to outliers)
cbg_number_visits_agg_all = aggregate(
  cbg_number_visits_all$total_visitors,
  by = list(cbg_number_visits_all$prop_urban_cut, cbg_number_visits_all$month),
  FUN = median
)

# Repeat for healthcare-only
hf_number_visits_all = subset(hf_number_visits_all, total_visitors > 0)
hf_number_visits_all = merge(hf_number_visits_all, NRHD_data, by.x = "home_cbg", by.y = "cbg2019")
hf_number_visits_all$prop_urban_cut = cut(hf_number_visits_all$prop_urban, c(0, 0.05, 0.18, 0.95, 1.1))
hf_number_visits_agg_all = aggregate(
  hf_number_visits_all$total_visitors,
  by = list(hf_number_visits_all$prop_urban_cut, hf_number_visits_all$month),
  FUN = median
)

# -----------------------------
# Normalize series within each urbanicity bin by each bin's own median
# -----------------------------
names(cbg_number_visits_agg_all) = c("prop_urb", "month", "trips")
unique_codes = unique(cbg_number_visits_agg_all$prop_urb)
cbg_data_all = data.frame()
for (code in unique_codes) {
  sub_data = subset(cbg_number_visits_agg_all, prop_urb == code)
  sub_data$num_normalized = sub_data$trips / median(sub_data$trips)
  cbg_data_all = rbind(cbg_data_all, sub_data)
}

names(hf_number_visits_agg_all) = c("prop_urb", "month", "trips")
unique_codes = unique(hf_number_visits_agg_all$prop_urb)
hf_data_all = data.frame()
for (code in unique_codes) {
  sub_data = subset(hf_number_visits_agg_all, prop_urb == code)
  sub_data$num_normalized = sub_data$trips / median(sub_data$trips)
  hf_data_all = rbind(hf_data_all, sub_data)
}

# -----------------------------
# Plots: normalized trends by urbanicity
# -----------------------------
ggplot() +
  geom_line(data = cbg_data_all,
            mapping = aes(x = month, y = num_normalized, colour = prop_urb, group = prop_urb)) +
  ggtitle("All trips, by urban/rural")

ggplot() +
  geom_line(data = hf_data_all,
            mapping = aes(x = month, y = num_normalized, colour = prop_urb, group = prop_urb)) +
  ggtitle("Healthcare trips, by urban/rural")

# -----------------------------
# Choropleths by month (NRHD only)
# -----------------------------

# Convert daily dates to the first of month for mapping
hf_number_visits_all$month_agg = as.Date(paste0(
  year(hf_number_visits_all$month), "-", month(hf_number_visits_all$month), "-01"
))

# Aggregate median by month_agg and urbanicity (consistent with previous approach)
hf_number_visits_agg_all = aggregate(
  hf_number_visits_all$total_visitors,
  by = list(hf_number_visits_all$prop_urban_cut, hf_number_visits_all$month_agg),
  FUN = median
)

# Convert daily dates to the first of month for mapping
cbg_number_visits_all$month_agg = as.Date(paste0(
  year(cbg_number_visits_all$month), "-", month(cbg_number_visits_all$month), "-01"
))

# Aggregate median by month_agg and urbanicity (consistent with previous approach)
cbg_number_visits_agg_all = aggregate(
  cbg_number_visits_all$total_visitors,
  by = list(cbg_number_visits_all$prop_urban_cut, cbg_number_visits_all$month_agg),
  FUN = median
)
# Helper: draw a filled CBG choropleth for a chosen month,
# filling by ratio_to_feb_2020 (blue <1, white ~1, red >1)
plot_shapefile_with_visitors = function(month_chosen, shapefile, cbg_number_visits, hf_visits) {
  
  # Filter to the chosen month’s CBG metrics
  cbg_number_visits_subset = subset(cbg_number_visits, month_agg == month_chosen)
  # Filter to the chosen month’s CBG metrics
  hf_number_visits_subset = subset(hf_visits, month_agg == month_chosen)
  
  # Merge metrics onto CBG polygons (ensure both GEOID and home_cbg are same class, e.g., character)
  shapefile_with_cbg_visits = merge(shapefile, cbg_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")
  shapefile_with_cbg_visits$id = "cbg"
  
  shapefile_with_hf_visits = merge(shapefile, hf_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")
  shapefile_with_hf_visits$id = "hf"
  
  shapefile_all_visits = rbind(shapefile_with_hf_visits,shapefile_with_cbg_visits)
  plot_with_visitors =
    ggplot() +
    geom_sf(data = shapefile_all_visits, mapping = aes(fill = log(ratio_to_feb_2020)), colour = NA) +
    scale_fill_gradient2(midpoint = 0, high = "#a50026", mid = "#ffffff", low = "#313695") +
    theme_minimal(base_size = 8) + 
    theme(legend.position = "bottom") +
    facet_grid(~id)+
    ggtitle(format(month_chosen, "%Y-%m-%d"))
  
  return(plot_with_visitors)
}

# Unique months available from merged dataset
unique_months = unique(overall_trips_all$month)

# Pick one example month to preview
month_select_2 = unique_months[1]

# Filter shapefile to NRHD (FIPS codes):
#   063 = Floyd, 071 = Giles, 121 = Montgomery, 155 = Pulaski, 750 = Radford City
shapefile = subset(shapefile, is.element(COUNTYFP, c("063", "071", "121", "155", "750")))


# Save a PNG for each available month
for (month_select in 1:length(unique_months)) {
  print(month_select)
  plot_obj = plot_shapefile_with_visitors(unique_months[month_select], shapefile, cbg_number_visits_all, hf_number_visits_all)
  
  ggsave(filename = paste0("D:/Downloads/shapefile_with_visitors_", unique_months[month_select], ".png"),
         plot = plot_obj, height = 5, width = 9)
}

# -----------------------------
# Simple regression example: urbanicity vs. reduction in trips (Aug 2020)
# -----------------------------
create_shapefile_with_visitors = function(month_chosen, shapefile, cbg_number_visits) {
  
  cbg_number_visits_subset = subset(cbg_number_visits_all, month_agg == month_chosen)
  shapefile_with_visits = merge(shapefile, cbg_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")
  return(shapefile_with_visits)
}

shapefile_aug_2020_cbg = create_shapefile_with_visitors(
  month_chosen = as.Date("2020-09-01"), shapefile, cbg_number_visits_all
)

# Regress ratio_to_feb_2020 on prop_urban for a single month (illustrative, not causal)
summary(lm(cbg_number_visits_all$ratio_to_feb_2020 ~ cbg_number_visits_all$prop_urban))

# -----------------------------
# Sectoral time series by NAICS 2-digit code
# -----------------------------

# Aggregate visits by date and NAICS 2-digit sector
# IMPORTANT:
# - Earlier we renamed 'number' -> 'total_visitors'. The merged table therefore has 'total_visitors'.

overall_trips_all$naics_code_2 = substr(overall_trips_all$naics_code,1,2)
NAICS_aggregate = aggregate(
  overall_trips_all$total_visitors,                              # CHANGED from $number -> $total_visitors
  by = list(overall_trips_all$date, overall_trips_all$naics_code_2),
  FUN = sum, na.rm=T
)

# Label the columns
names(NAICS_aggregate) = c("date", "NAICS", "num")

# Normalize each NAICS series by its own median (robust scaler)
unique_codes = unique(NAICS_aggregate$NAICS)
NAICS_data = data.frame()
for (code in unique_codes) {
  sub_data = subset(NAICS_aggregate, NAICS == code)
  sub_data$num_normalized = sub_data$num / median(sub_data$num, na.rm = T)
  NAICS_data = rbind(NAICS_data, sub_data)
}

# Prepare for plotting
NAICS_data$date  = as.Date(NAICS_data$date)
NAICS_data$NAICS = as.factor(NAICS_data$NAICS)

# Map 2-digit NAICS to human-readable sector names.
# Tip: If avoiding plyr, you can use:
# NAICS_data$NAICS_name <- dplyr::recode(NAICS_data$NAICS, `44`="Grocery", `45`="Department", ...)
# Using plyr::revalue below if you prefer:
 library(plyr)
 NAICS_data$NAICS_name = revalue(NAICS_data$NAICS, c(
   "44" = "Grocery", "45" = "Department", "61" = "Schools", "62" = "Healthcare",
   "71" = "Recreation", "72" = "Restaurants"
 ))


# Plot normalized sector trends
ggplot() +
  geom_line(data = NAICS_data,
            mapping = aes(x = date, y = num_normalized, colour = NAICS_name, group = NAICS_name)) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("POI visits by sector (normalized by sector median)")
