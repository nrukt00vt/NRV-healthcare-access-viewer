library(sf)
library(tidyverse)

#Read in the shapefile as an "sf" object

shapefile = read_sf(dsn ="D:/Downloads/tl_2019_51_bg", layer= "tl_2019_51_bg")
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
healthcare_dist = subset(healthcare_dist, month == "2019-07-01")
health_POIs_sf_dist = merge(health_POIs_sf,healthcare_dist, by.x="safegraph_place_id", by.y = "ID")
health_POIs_sf_dist = health_POIs_sf_dist[order(health_POIs_sf_dist$distance,decreasing=T),]
health_POIs_sf_dist = subset(health_POIs_sf_dist,distance < 40000)
ggplot() + geom_sf(data = shapefile) +
  geom_sf(data = health_POIs_sf_dist, mapping = aes(colour = distance)) + 
  scale_colour_distiller(palette = "YlOrRd",trans="log10",direction = 1) + ylim(c(37,37.3)) + xlim(c(-81,-80))


#### Calculating distance traveled by people in each cbg
#create dataset with all cbgs, which will be filled in with distances traveled by people in each CBG
#This will use broadly the same logic, just will be focused on CBGs rather than healthcare facilities
unique_cbg_ids = unique(overall_trips$visitor_home_cbg)

cbg_dist = expand.grid(ID = unique_cbg_ids, distance = 0,avg_visitors = 0,month = unique_months)

# start with the first one
working_id = cbg_dist$ID[1]
#Iterates row by row; doesn't actually need nested for loops
all_trips = aggregate(overall_trips$total_visitors, by = list(overall_trips$visitor_home_cbg, overall_trips$month), FUN = sum)
names(all_trips) = c("cbg","month","trips")
#all_trips = subset(all_trips, trips > 0)
all_trips2 = aggregate(all_trips$trips, by = list(all_trips$cbg), FUN = mean)
names(all_trips2) = c("cbg","trips")

for (i in 1:length(cbg_dist$ID)){
  working_id = cbg_dist$ID[i]
  working_month = cbg_dist$month[i]

  
  trips_fr_cbg = subset(overall_trips,visitor_home_cbg == working_id & month == working_month)
  
  trips_fr_cbg =  st_as_sf(trips_fr_cbg, coords = c("longitude","latitude"))%>% 
    st_set_crs(4326)
  
  cbg_centroid = subset(centroids_cbgs,GEOID == working_id)
  if (nrow(cbg_centroid)>0){
    print(i)
    trips_fr_cbg$distances = as.numeric(st_distance(trips_fr_cbg,cbg_centroid))
    weighted_average_distance = sum(trips_fr_cbg$distances * trips_fr_cbg$total_visitors / sum(trips_fr_cbg$total_visitors))
    if (sum( trips_fr_cbg$total_visitors )>0){
      cbg_dist$avg_visitors[i] = sum( trips_fr_cbg$total_visitors ) /all_trips2[which(all_trips2$cbg == working_id),]$trips
    }
    if (sum( trips_fr_cbg$total_visitors ) == 0) {
    cbg_dist$avg_visitors[i] = sum( trips_fr_cbg$total_visitors )
    }
    cbg_dist$distance[i] = weighted_average_distance
  }
}


#plotting
cbg_dist = subset(cbg_dist, distance > 0)

hist(cbg_dist$distance)
centroids_cbg_sf = st_as_sf(centroids_cbgs, coords = c("longitude","latitude"))%>% 
  st_set_crs(4326)
centroids_cbg_sf_dist = merge(centroids_cbg_sf,cbg_dist, by.x="GEOID", by.y = "ID")
months = as.Date(unique(centroids_cbg_sf_dist$month))
library(lubridate)
months = months[year(months) == 2020]
centroids_cbg_sf_dist$month = as.Date(centroids_cbg_sf_dist$month)
for (i in 1:length(months)){
  
centroids_cbg_sf_dist_sub = subset(centroids_cbg_sf_dist, month == months[i])

num_visitors = aggregate(centroids_cbg_sf_dist$num_visitors,
                                  by = list(centroids_cbg_sf_dist$GEOID),
                                  FUN = sum)
names(num_visitors) = c("GEOID","num_visitors")
average_dist_traveled = aggregate(centroids_cbg_sf_dist$distance,
                                  by = list(centroids_cbg_sf_dist$GEOID),
                                  FUN = mean)
names(average_dist_traveled) = c("GEOID","avg_dist")

centroids_cbg_sf_dist = merge(centroids_cbg_sf_dist,average_dist_traveled)
centroids_cbg_sf_dist= merge(centroids_cbg_sf_dist,num_visitors)
centroids_cbg_sf_dist$distance_prop = centroids_cbg_sf_dist$distance / centroids_cbg_sf_dist$avg_dist
centroids_cbg_sf_dist_sub = subset(centroids_cbg_sf_dist, month == months[i])
centroids_cbg_sf_dist_sub = centroids_cbg_sf_dist_sub[order(centroids_cbg_sf_dist_sub$distance_prop, decreasing = F),]
shapefile_with_dist = merge(shapefile, st_drop_geometry(centroids_cbg_sf_dist_sub),by="GEOID")
dist_plot = ggplot() +geom_sf(data = shapefile_with_dist, colour = "light grey",mapping = aes(fill = distance_prop)) + 
  scale_fill_gradient2(low="blue",high ="red",mid = "white",midpoint = 1) + ggtitle(months[i])
ggsave(dist_plot, filename = paste0("distance_plot",months[i],".png"))
}

months_pre = months[which(months < as.Date("2020-03-01"))]
months_during = months[-which(months < as.Date("2020-03-01"))]
centroids_cbg_sf_dist_sub_pre = subset(centroids_cbg_sf_dist, is.element(month,months_pre)) %>% st_drop_geometry()
centroids_cbg_sf_dist_sub_pre = aggregate(centroids_cbg_sf_dist_sub_pre$avg_dist, by = list(centroids_cbg_sf_dist_sub_pre$GEOID), FUN = median)
names(centroids_cbg_sf_dist_sub_pre) = c("GEOID", "dist")
shapefile_with_dist_pre = merge(shapefile, st_drop_geometry(centroids_cbg_sf_dist_sub_pre),by="GEOID")
ggplot() +geom_sf(data = shapefile_with_dist_pre, colour = "light grey",mapping = aes(fill = dist)) + 
  scale_fill_distiller(palette = "Spectral")

centroids_cbg_sf_dist_sub_during = subset(centroids_cbg_sf_dist, is.element(month,months_during)) %>% st_drop_geometry()
centroids_cbg_sf_dist_sub_during = aggregate(centroids_cbg_sf_dist_sub_during$avg_dist, by = list(centroids_cbg_sf_dist_sub_during$GEOID), FUN = median)
names(centroids_cbg_sf_dist_sub_during) = c("GEOID", "dist")
shapefile_with_dist_during = merge(shapefile, st_drop_geometry(centroids_cbg_sf_dist_sub_during),by="GEOID")
ggplot() +geom_sf(data = shapefile_with_dist_during, colour = "light grey",mapping = aes(fill = dist)) + 
  scale_fill_distiller(palette = "Spectral")


input_data <- read.csv("~/Documents/GitHub/NRV-healthcare-access-viewer/NRHDcovariate.csv")
is.element(cbg_number_visits$home_cbg)

cbg_number_visits = aggregate(overall_trips$total_visitors, by = list(overall_trips$visitor_home_cbg,
                                                                      overall_trips$month),
                              FUN = sum)
names(cbg_number_visits) = c("home_cbg", "month", "total_visitors")

cbg_number_visits = subset(cbg_number_visits, total_visitors > 0)
cbg_number_visits = merge(cbg_number_visits, input_data, by.x= "home_cbg", by.y = "cbg2019")
cbg_number_visits$prop_urban_cut = cut(cbg_number_visits$prop_urban,c(0, 0.05, 0.18, .95,1.1))
cbg_number_visits_agg = aggregate(cbg_number_visits$total_visitors, by = list(cbg_number_visits$prop_urban_cut,
                                                                              cbg_number_visits$month),
                                  FUN = median)

names(cbg_number_visits_agg) = c("prop_urb", "month", "trips")
unique_codes = unique(cbg_number_visits_agg$prop_urb)
cbg_data = data.frame()
for (code in unique_codes){
  sub_data = subset(cbg_number_visits_agg, prop_urb == code)
  sub_data$num_normalized = sub_data$trips / median(sub_data$trips)
  cbg_data = rbind(cbg_data, sub_data)
}

ggplot() + geom_line(data = cbg_data, mapping = aes(x = month, y = num_normalized, colour = prop_urb, group = prop_urb)) +
  ggtitle("HF trips, by urban/rural")
cbg_feb_2020 = subset(cbg_number_visits, month == "2020-02-01")

# Merge Feb 2020 data, using home cbg, back into cbg_number_visits, so that we have the feb 2020 total visitor numbers to compare each month's numbers against
# The ratio we will plot is the total visitors / feb 2020 visitors 
# This will require renaming the feb 2020 data (rename total_visitors in this dataset), and using the "merge" function

names(cbg_feb_2020)[which(names(cbg_feb_2020) == "total_visitors")] = "feb_2020_visitors"
cbg_feb_2020 = subset(cbg_feb_2020, select = -c(month))
cbg_number_visits = merge(cbg_number_visits, cbg_feb_2020, by = c("home_cbg"))

cbg_number_visits$ratio_to_feb_2020 = cbg_number_visits$total_visitors / cbg_number_visits$feb_2020_visitors

`#Then, subset each month using subset function, merge it with the shapefile, and plot the shapefile (similar to the above code) for each month

unique_months = unique(cbg_number_visits$month)

month_select = unique_months[2]

cbg_number_visits_subset = subset(cbg_number_visits, month == month_select)
shapefile_with_visits = merge(shapefile, cbg_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")

ggplot() + geom_sf(shapefile_with_visits, mapping = aes(fill = ratio_to_feb_2020))+
  scale_fill_gradient2(midpoint = 1, high ="#d73027", mid = "#ffffff", low = "#4575b4") +
  ggtitle(month_select)

cbg_number_visits_wdat =merge(cbg_number_visits, input_data, by.x = "home_cbg", by.y = "cbg2019")
cbg_number_visits_wdat$urban_cat = cut(cbg_number_visits_wdat$prop_urban, breaks = c(0,.05, .2, .8,1.1))
cbg_number_visits_wdat$income_cat = cut(cbg_number_visits_wdat$Income.below..25.000, breaks = c(0,16, 21, 30,85))

cbg_number_visits_urban = aggregate(cbg_number_visits_wdat$ratio_to_feb_2020, FUN = mean,by = list(cbg_number_visits_wdat$urban_cat,
                                                                                                   cbg_number_visits_wdat$month))
names(cbg_number_visits_urban) = c("urban", "month","ratio_to_feb_2020")
ggplot() + geom_line(data = cbg_number_visits_urban, mapping = aes(x = month,y = ratio_to_feb_2020,group = urban, colour = urban))


cbg_number_visits_income = aggregate(cbg_number_visits_wdat$ratio_to_feb_2020, FUN = mean,by = list(cbg_number_visits_wdat$income_cat,
                                                                                                    cbg_number_visits_wdat$month))
names(cbg_number_visits_income) = c("income", "month","ratio_to_feb_2020")
ggplot() + geom_line(data = cbg_number_visits_income, mapping = aes(x = month,y = ratio_to_feb_2020,group = income, colour = income))
cbg_number_visits_wdat$month))



#Read in the shapefile as an "sf" object
#shapefile = read_sf(dsn ="base_files", layer= "tl_2022_51_bg")
all_POIs = read.csv('AllPOIs_Montgomery_VA.csv')

#Read in the time series data
time_data = read.csv("allvisits_VA_new.csv")

names(time_data)[which(names(time_data) == "number")] = "total_visitors"
overall_trips_all = merge(all_POIs,time_data, by.x="safegraph_place_id",by.y="safegraph_place")

hf_trips_all = subset( overall_trips_all, is.element(naics_code, c(621210, 621340, 621111, 621493, 622110)))
cbg_number_visits_all = aggregate(overall_trips_all$total_visitors, by = list(overall_trips_all$visitor_home_cbg,
                                                                          overall_trips_all$date),
                              FUN = sum)

hf_number_visits_all = aggregate(hf_trips_all$total_visitors, by = list(hf_trips_all$visitor_home_cbg,
                                                                        hf_trips_all$date),
                                  FUN = sum)

names(cbg_number_visits_all) = c("home_cbg", "month", "total_visitors")
names(hf_number_visits_all) = c("home_cbg", "month", "total_visitors")

cbg_number_visits_all = subset(cbg_number_visits_all, total_visitors > 0)
cbg_number_visits_all = merge(cbg_number_visits_all, input_data, by.x= "home_cbg", by.y = "cbg2019")
cbg_number_visits_all$prop_urban_cut = cut(cbg_number_visits_all$prop_urban,c(0, 0.05, 0.18, .95,1.1))
cbg_number_visits_agg_all = aggregate(cbg_number_visits_all$total_visitors, by = list(cbg_number_visits_all$prop_urban_cut,
                                                                                  cbg_number_visits_all$month),
                                  FUN = median)


hf_number_visits_all = subset(hf_number_visits_all, total_visitors > 0)
hf_number_visits_all = merge(hf_number_visits_all, input_data, by.x= "home_cbg", by.y = "cbg2019")
hf_number_visits_all$prop_urban_cut = cut(hf_number_visits_all$prop_urban,c(0, 0.05, 0.18, .95,1.1))
hf_number_visits_agg_all = aggregate(hf_number_visits_all$total_visitors, by = list(hf_number_visits_all$prop_urban_cut,
                                                                                      hf_number_visits_all$month),
                                      FUN = mean)

names(cbg_number_visits_agg_all) = c("prop_urb", "month", "trips")
unique_codes = unique(cbg_number_visits_agg_all$prop_urb)
cbg_data_all = data.frame()
for (code in unique_codes){
  sub_data = subset(cbg_number_visits_agg_all, prop_urb == code)
  sub_data$num_normalized = sub_data$trips / median(sub_data$trips)
  cbg_data_all = rbind(cbg_data_all, sub_data)
}

names(hf_number_visits_agg_all) = c("prop_urb", "month", "trips")
unique_codes = unique(hf_number_visits_agg_all$prop_urb)
hf_data_all = data.frame()
for (code in unique_codes){
  sub_data = subset(hf_number_visits_agg_all, prop_urb == code)
  sub_data$num_normalized = sub_data$trips / median(sub_data$trips)
  hf_data_all = rbind(hf_data_all, sub_data)
}

ggplot() + geom_line(data = cbg_data_all, mapping = aes(x = month, y = num_normalized, colour = prop_urb, group = prop_urb)) +
  ggtitle("All trips, by urban/rural")

ggplot() + geom_line(data = hf_data_all, mapping = aes(x = month, y = num_normalized, colour = prop_urb, group = prop_urb)) +
  ggtitle("Healthcare trips, by urban/rural")
# plotting shapefile with visitor data for each month

plot_shapefile_with_visitors = function(month_chosen, shapefile, cbg_number_visits) {
 
  cbg_number_visits_subset = subset(cbg_number_visits, month == month_chosen)
  shapefile_with_visits = merge(shapefile, cbg_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")
  
   plot_with_visitors = ggplot() + geom_sf(data = shapefile_with_visits, mapping = aes(fill = ratio_to_feb_2020)) +
    scale_fill_gradient2(midpoint = 1, high = "#d73027", mid = "#ffffff", low = "#4575b4") + theme_minimal(base_size = 8)+ theme(legend.position="bottom")+
    ggtitle(month_chosen)
  
  return(plot_with_visitors)
}
month_select_2 = unique_months[1]

plot_shapefile_with_visitors(month_select_2, shapefile, cbg_number_visits)
for (month_select in unique_months) {
  print(month_select)
  plot = plot_shapefile_with_visitors(month_select, shapefile, cbg_number_visits)
  ggsave(plot, filename = paste0("shapefile_with_visitors_", month_select, ".png"), height = 6, width = 5)
}


create_shapefile_with_visitors = function(month_chosen, shapefile, cbg_number_visits) {
  
  cbg_number_visits_subset = subset(cbg_number_visits, month == month_chosen)
  shapefile_with_visits = merge(shapefile, cbg_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")
return(shapefile_with_visits)
}
shapefile_aug_2020 = create_shapefile_with_visitors("2020-08-01",shapefile,cbg_number_visits)
#Merge the NRHD_covariates with the shapefile

merged_data <- merge(shapefile_aug_2020, NRHDcovariate, by.x = "GEOID", by.y = "ct2021", all = TRUE)

merged_data_no_na = subset(merged_data, !is.na(merged_data$ratio_to_feb_2020))
#change out predictors with predcitors from NRHD_covariates

lm(merged_data$ratio_to_feb_2020 ~ merged_data$prop_urban)
