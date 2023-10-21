
library(sf)
library(tidyverse)

#Read in the shapefile as an "sf" object
shapefile = read_sf(dsn ="base_files", layer= "tl_2022_51_bg")
#Choose HealthPOIs_Montgomery_VA.csv
health_POIs = read.csv('HealthPOIs_Montgomery_VA 2.csv')
montgomery_health_POIs = subset(health_POIs, city == "Blacksburg" | city == "Christiansburg")%>% distinct(street_address, .keep_all = TRUE)


#Read in data
all_data = read.csv("overall_trips_VA.csv")
overall_trips = merge(montgomery_health_POIs,all_data, by.x="safegraph_place_id",by.y="safegraph_place")

uniqueLocations <- overall_trips %>% distinct(safegraph_place_id, .keep_all = TRUE)
uniqueNAICS = unique(uniqueLocations$naics_code)
maximum_value = max(aggregate(overall_trips[,c("total_visitors")],by = list(overall_trips$visitor_home_cbg, overall_trips$naics_code),FUN = sum)$x)
print("done prep")

