library(sf)
library(tidyverse)

#Read in the shapefile as an "sf" object
shapefile = read_sf(dsn ="base_files", layer= "tl_2019_51_bg")
#New River Health District only
shapefile = subset(shapefile,is.element(COUNTYFP,c("063","121","155","750","071")))

#We won't worry about isolating to Blacksburg and Christiansburg
health_POIs = read.csv('HealthPOIs_Montgomery_VA 2.csv')


#Read in data
all_data = read.csv("NRV_monthly_data.csv")
names(all_data)[which(names(all_data) == "num")] = "total_visitors"
overall_trips = merge(health_POIs,all_data, by.x="safegraph_place_id",by.y="safegraph_place")

#We will only use the CBGs that actually appear in the dataset, just to save processing time
#shapefile_subset = subset(shapefile, is.element(GEOID, unique(overall_trips$visitor_home_cbg)))

#calculate the centroid for each CBG
centroids_cbgs = st_centroid(shapefile) %>% st_set_crs(4326)

#Get all unique healthcare IDs; we will start to fill in a dataframe where we also fill in the average travel distance to there
unique_healthcare_ids = unique(overall_trips$safegraph_place_id)
unique_months = unique(overall_trips$month)
healthcare_dist = expand.grid(ID = unique_healthcare_ids, distance = 0,month = unique_months)

#### Healthcare facility based distance calculation
#Iterates row by row; doesn't actually need nested for loops


for (i in 1:length(healthcare_dist$ID)){
  working_month = healthcare_dist$month[i]
  working_id = healthcare_dist$ID[i]
  print(working_id)
  
  #subset all trips to the healthcare facility
  trips_to_healthcare = subset(overall_trips,safegraph_place_id == working_id & month == working_month)
  #associate each row with the corresponding CBG centroid
  trips_to_healthcare_w_centroids = merge(trips_to_healthcare,centroids_cbgs,by.x="visitor_home_cbg",by.y="GEOID")
  trips_to_healthcare_w_centroids = st_as_sf(trips_to_healthcare_w_centroids) %>%   st_set_crs(4326)
  
  #create a point for the healthcare facility
  healthcare_point = st_as_sf(subset(health_POIs, safegraph_place_id == working_id), coords = c("longitude","latitude"))%>% 
    st_set_crs(4326)

  #calculate distance between healthcare facility and all the CBG centroids
  trips_to_healthcare_w_centroids$distances = as.numeric(st_distance(healthcare_point,trips_to_healthcare_w_centroids))
  #create a weighted average for trips to the healthcare facility
  weighted_average_distance = sum(trips_to_healthcare_w_centroids$distances * trips_to_healthcare_w_centroids$total_visitors / sum(trips_to_healthcare_w_centroids$total_visitors))
  
  #add this distance to the healthcare with distances traveled dataframe -- note that this is putting it in the first row because it involves the first healthcare facility;
  #as you run through the for loop, you will want to make sure it's placed in the corresponding row
  healthcare_dist$distance[i] = weighted_average_distance
  
  
}



#plotting
healthcare_dist = subset(healthcare_dist, distance > 0)

hist(healthcare_dist$distance)
health_POIs_sf = st_as_sf(health_POIs, coords = c("longitude","latitude"))%>% 
  st_set_crs(4326)

health_POIs_sf_dist = merge(health_POIs_sf,healthcare_dist, by.x="safegraph_place_id", by.y = "ID")

ggplot() + geom_sf(data = health_POIs_sf_dist, mapping = aes(colour = distance)) + 
  scale_colour_distiller(palette = "YlOrRd", trans ="log10") + ylim(c(37,37.3)) + xlim(c(-81,-80))


#### Calculating distance traveled by people in each cbg
#create dataset with all cbgs, which will be filled in with distances traveled by people in each CBG
#This will use broadly the same logic, just will be focused on CBGs rather than healthcare facilities
unique_cbg_ids = unique(overall_trips$visitor_home_cbg)

cbg_dist = expand.grid(ID = unique_cbg_ids, distance = 0,month = unique_months)

# start with the first one
working_id = cbg_dist$ID[1]
#Iterates row by row; doesn't actually need nested for loops

for (i in 1:length(cbg_dist$ID)){
  working_id = cbg_dist$ID[i]
  working_month = cbg_dist$month[i]
  print(i) 

  trips_fr_cbg = subset(overall_trips,visitor_home_cbg == working_id& month == working_month)
  
  trips_fr_cbg =  st_as_sf(trips_fr_cbg, coords = c("longitude","latitude"))%>% 
    st_set_crs(4326)
  
  cbg_centroid = subset(centroids_cbgs,GEOID == working_id)
  if (nrow(cbg_centroid)>0){
    trips_fr_cbg$distances = as.numeric(st_distance(trips_fr_cbg,cbg_centroid))
    weighted_average_distance = sum(trips_fr_cbg$distances * trips_fr_cbg$total_visitors / sum(trips_fr_cbg$total_visitors))
  
    cbg_dist$distance[i] = weighted_average_distance
  }
}


#plotting
cbg_dist = subset(cbg_dist, distance > 0)

hist(cbg_dist$distance)
centroids_cbg_sf = st_as_sf(centroids_cbgs, coords = c("longitude","latitude"))%>% 
  st_set_crs(4326)
centroids_cbg_sf_dist = merge(centroids_cbg_sf,cbg_dist, by.x="GEOID", by.y = "ID")


ggplot() + geom_sf(data = centroids_cbg_sf_dist, mapping = aes(colour = distance)) + 
  scale_colour_distiller(palette = "YlOrRd", trans ="log10") 


