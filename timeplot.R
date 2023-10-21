library(sf)
library(tidyverse)

#Read in the shapefile as an "sf" object
shapefile = read_sf(dsn ="base_files", layer= "tl_2022_51_bg")
#Choose HealthPOIs_Montgomery_VA.csv
health_POIs = read.csv('base_files/HealthPOIs_Montgomery_VA 2.csv')
#montgomery_health_POIs = subset(health_POIs, city == "Blacksburg" | city == "Christiansburg")%>% distinct(street_address, .keep_all = TRUE)

