
library(sf)
library(tidyverse)
library(lubridate)

#Read in the shapefile as an "sf" object
shapefile = read_sf(dsn ="base_files", layer= "tl_2019_51_bg")
#Choose HealthPOIs_Montgomery_VA.csv
health_POIs = read.csv('HealthPOIs_Montgomery_VA 2.csv')


#Read in data
all_data = read.csv("NRV_monthly_data.csv")[,-1]
all_data = subset(all_data,!is.na(all_data$visitor_home_cbg) & num > 0)
overall_trips = merge(health_POIs,all_data, by.x="safegraph_place_id",by.y="safegraph_place")

uniqueLocations <- overall_trips %>% distinct(safegraph_place_id, .keep_all = TRUE)
uniqueNAICS = unique(uniqueLocations$naics_code)
maximum_value = max(aggregate(overall_trips[,c("num")],by = list(overall_trips$visitor_home_cbg, overall_trips$naics_code, overall_trips$month),FUN = sum)$x)
print("done prep")