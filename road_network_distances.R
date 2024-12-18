library(corrplot)
library(spdep)
library(tidyverse)
library(dplyr)
library(ggridges)
library(data.table)
library(sf)
library(osrm)
library(tmap)
library(tmaptools)


# Loading or processing POI and trips data
if (!file.exists("trips_with_CBG_centroids.csv")) {
  # Loading raw POI and trips data
  POI_names = fread("HealthPOIs_Montgomery_VA 2.csv") %>% 
    rename(safegraph_place = safegraph_place_id)
  
  POI_trips_weekly = read.csv("healthcarevisits_VA_new.csv") %>%
    inner_join(POI_names, by = join_by(safegraph_place))
  
  # Loading and processing shapefile
  shapefile = read_sf(dsn = "tl_2019_51_bg") %>%
    mutate(GEOID = as.numeric(GEOID)) %>%
    filter(COUNTYFP %in% c(121, 71, 63, 750, 155))
  
  # Computing centroids
  centroids = st_coordinates(st_centroid(shapefile)$geometry)
  shapefile$lon = centroids[, 1]
  shapefile$lat = centroids[, 2]
  
  # Subsetting and processing POI trips
  POI_trips_sub = subset(
    POI_trips_weekly, 
    !is.na(visitor_home_cbg) & 
      is.element(city, c("Blacksburg", "Christiansburg", "Radford", "Shawsville", "Merrimac", "Dublin", "Fairlawn"))
  )
  POI_trips_sub = subset(POI_trips_sub, is.element(visitor_home_cbg, shapefile$GEOID))
  POI_trips_sub = POI_trips_sub %>%
    mutate(
      cbg_lon = map_dbl(visitor_home_cbg, ~ shapefile$lon[shapefile$GEOID == .]),
      cbg_lat = map_dbl(visitor_home_cbg, ~ shapefile$lat[shapefile$GEOID == .])
    )
  
  names(POI_trips_sub)[names(POI_trips_sub) == "latitude"] = "POI_lat"
  names(POI_trips_sub)[names(POI_trips_sub) == "longitude"] = "POI_lon"
  
  # Saving processed data for future use
  write.csv(POI_trips_sub, "trips_with_CBG_centroids.csv", row.names = FALSE)
} else {
  # Loading preprocessed data
  POI_trips_sub = read.csv("trips_with_CBG_centroids.csv") %>%
    mutate(
      cbg_lon = as.numeric(cbg_lon),
      cbg_lat = as.numeric(cbg_lat),
      POI_lon = as.numeric(POI_lon),
      POI_lat = as.numeric(POI_lat)
    )
}

# Calculating distances
if (!file.exists("distances_results.csv")) {
  distances = data.frame(
    CBG_Lon = POI_trips_sub$cbg_lon,
    CBG_Lat = POI_trips_sub$cbg_lat,
    POI_Lon = POI_trips_sub$POI_lon,
    POI_Lat = POI_trips_sub$POI_lat,
    Distance = NA
  )
  
  for (i in 1:nrow(POI_trips_sub)) {
    cbgs = c(POI_trips_sub$cbg_lon[i], POI_trips_sub$cbg_lat[i])
    pois = c(POI_trips_sub$POI_lon[i], POI_trips_sub$POI_lat[i])
    route = tryCatch(
      osrmRoute(src = cbgs, dst = pois, osrm.profile = "car"),
      error = function(e) NULL
    )
    distances$Distance[i] = if (!is.null(route)) route$distance else NA
  }
  
# Saving distances to a file
  write.csv(distances, "distances_results.csv", row.names = FALSE)
} else {
  distances = read.csv("distances_results.csv")
}

View(distances)

# Saving final data for visualization or analysis
write.csv(distances, "final_results_with_distances.csv", row.names = FALSE)



# Removing duplicates in the original data frames
POI_trips_sub = POI_trips_sub %>% distinct(cbg_lon, cbg_lat, POI_lon, POI_lat, .keep_all = TRUE)
distances = distances %>% distinct(cbg_lon, cbg_lat, POI_lon, POI_lat, .keep_all = TRUE)

# Merging data frames
new_merged_data = POI_trips_sub %>%
  inner_join(distances, by = c("cbg_lon", "cbg_lat", "POI_lon", "POI_lat"))

# Removing any remaining duplicates after merging
newly_merged_data = new_merged_data %>% distinct()

# Viewing merged data
View(new_merged_data)


library(ggplot2)
# Creating the histogram
ggplot(new_merged_data, aes(x = Distance, weight = number)) +
  geom_histogram(binwidth = 1, fill = "green", color = "orange", alpha = 0.5) +
  labs(
    title = "Histogram of Distances",
    x = "Distance",
    y = "Visitor Numbers"
  ) +
  theme_minimal()


library(dplyr)
library(lubridate)

# Ensuring the 'date' column is in Date format
new_merged_data$date = as.Date(new_merged_data$date, format = "%Y-%m-%d")

# Filtering data for the specified date range
filtered_data = new_merged_data %>%
  filter(date >= as.Date("2019-01-07") & date <= as.Date("2020-11-30"))

# Extracting the year-month combination
filtered_data = filtered_data %>%
  mutate(month = floor_date(date, "month")) # Adds a 'month' column

# Summarizing data by month
monthly_summary = filtered_data %>%
  group_by(month) %>%
  summarize(
    avg_distance = mean(Distance, na.rm = TRUE),  # Average distance traveled
    total_visitors = sum(number, na.rm = TRUE),   # Total visitors
    count = n()                                   # Total records
  )


# Plotting average distance over months
ggplot(monthly_summary, aes(x = month)) +
  geom_line(aes(y = avg_distance), color = "blue", size = 1) +
  geom_line(aes(y = total_visitors / 1000), color = "red", size = 1, linetype = "dashed") +
  labs(title = "Monthly Analysis of Visitor Numbers and Distances",
       x = "Months",
       y = "Values",
       subtitle = "Blue = Avg Distance | Red = Total Visitors (in 1000s)") +
  theme_minimal()



