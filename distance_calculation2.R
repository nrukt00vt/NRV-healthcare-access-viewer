library(sf)
library(tidyverse)

##### All data from here; not HF specific
#Read in the shapefile as an "sf" object
#shapefile = read_sf(dsn ="base_files", layer= "tl_2022_51_bg")
all_POIs = read.csv('AllPOIs_Montgomery_VA.csv')
input_data = read.csv("NRHDcovariate.csv")
#Read in the time series data
time_data = read.csv("allvisits_VA_new.csv")

names(time_data)[which(names(time_data) == "number")] = "total_visitors"
overall_trips_all = merge(all_POIs,time_data, by.x="safegraph_place_id",by.y="safegraph_place")
overall_trips_all$month = as.Date(paste0(year(overall_trips_all$date),"-",month(overall_trips_all$date),"-01"))

hf_trips_all = subset( overall_trips_all, is.element(naics_code, c(621210, 621340, 621111, 621493, 622110)))
cbg_number_visits_all = aggregate(overall_trips_all$total_visitors, by = list(overall_trips_all$visitor_home_cbg,
                                                                          overall_trips_all$date),
                              FUN = sum)

names(cbg_number_visits_all) = c("home_cbg", "month", "total_visitors")
cbg_number_visits_all = subset(cbg_number_visits_all, total_visitors > 0)

cbg_feb_2020_all = subset(cbg_number_visits_all,  month > as.Date("2020-01-31") & month < as.Date("2020-03-01"))

names(cbg_feb_2020_all)[which(names(cbg_feb_2020_all) == "total_visitors")] = "feb_2020_visitors"
cbg_feb_2020_all = subset(cbg_feb_2020_all, select = -c(month))
cbg_number_visits_all = merge(cbg_number_visits_all, cbg_feb_2020_all, by = c("home_cbg"))

cbg_number_visits_all$ratio_to_feb_2020 = cbg_number_visits_all$total_visitors / cbg_number_visits_all$feb_2020_visitors

hf_number_visits_all = aggregate(hf_trips_all$total_visitors, by = list(hf_trips_all$visitor_home_cbg,
                                                                        hf_trips_all$date),
                                  FUN = sum)

names(hf_number_visits_all) = c("home_cbg", "month", "total_visitors")
hf_number_visits_all = subset(hf_number_visits_all, total_visitors > 0)

hf_feb_2020_all = subset(hf_number_visits_all, month > as.Date("2020-01-31") & month < as.Date("2020-03-01"))

names(hf_feb_2020_all)[which(names(hf_feb_2020_all) == "total_visitors")] = "feb_2020_visitors"
hf_feb_2020_all = subset(hf_feb_2020_all, select = -c(month))
hf_number_visits_all = merge(hf_number_visits_all, hf_feb_2020_all, by = c("home_cbg"))

hf_number_visits_all$ratio_to_feb_2020 = hf_number_visits_all$total_visitors / hf_number_visits_all$feb_2020_visitors


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
                                      FUN = median)

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

cbg_number_visits_all$month_agg = as.Date(paste0(year(cbg_number_visits_all$month),"-",
                                                 month(cbg_number_visits_all$month), "-01"
                                                 ))
cbg_number_visits_agg_all = aggregate(cbg_number_visits_all$total_visitors, by = list(cbg_number_visits_all$prop_urban_cut,
                                                                                      cbg_number_visits_all$month_agg),
                                      FUN = median)


plot_shapefile_with_visitors = function(month_chosen, shapefile, cbg_number_visits) {
 
  cbg_number_visits_subset = subset(cbg_number_visits, month_agg == month_chosen)
  shapefile_with_visits = merge(shapefile, cbg_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")
  
   plot_with_visitors = ggplot() + geom_sf(data = shapefile_with_visits, mapping = aes(fill = ratio_to_feb_2020)) +
    scale_fill_gradient2(midpoint = 1, high = "#d73027", mid = "#ffffff", low = "#4575b4") + theme_minimal(base_size = 8)+ theme(legend.position="bottom")+
    ggtitle(month_chosen)
  
  return(plot_with_visitors)
}
unique_months = unique(overall_trips_all$month)

month_select_2 = unique_months[1]

shapefile = read_sf(dsn ="base_files", layer= "tl_2019_51_bg")
#New River Health District only
shapefile = subset(shapefile,is.element(COUNTYFP,c("063","121","155","750","071")))
plot_shapefile_with_visitors(month_select_2, shapefile, cbg_number_visits_all)
for (month_select in unique_months) {
  print(month_select)
  plot = plot_shapefile_with_visitors(month_select, shapefile, cbg_number_visits_all)
  ggsave(plot, filename = paste0("shapefile_with_visitors_", month_select, ".png"), height = 6, width = 5)
}

NRHDcovariate = read.csv("NRHDcovariate.csv")
create_shapefile_with_visitors = function(month_chosen, shapefile, cbg_number_visits) {
  
  cbg_number_visits_subset = subset(cbg_number_visits, month == month_chosen)
  shapefile_with_visits = merge(shapefile, cbg_number_visits_subset, by.x = "GEOID", by.y = "home_cbg")
return(shapefile_with_visits)
}
shapefile_aug_2020 = create_shapefile_with_visitors("2020-08-01",shapefile,cbg_number_visits)
#Merge the NRHD_covariates with the shapefile

merged_data <- merge(shapefile_aug_2020, NRHDcovariate, by.x = "GEOID", by.y = "cbg2019", all = TRUE)

merged_data_no_na = subset(merged_data, !is.na(merged_data$ratio_to_feb_2020))
#change out predictors with predcitors from NRHD_covariates

lm(merged_data$ratio_to_feb_2020 ~ merged_data$prop_urban)


#### Trips by location type
#read.csv: will read a certain file, data input
health_POIs = read.csv('AllPOIs_Montgomery_VA.csv')

#Read in the time series data
time_data = read.csv("allvisits_VA_new.csv")
#remove records where nobody visited the corresponding healthcare facility
#subset: will indicate which rows to keep
#is.na: not available (since ! is before it would be available)
time_data = subset(time_data, !is.na(time_data$visitor_home_cbg))
#merge the dataset with the locations & types of the healthcare facilities
time_data_merged = merge(time_data,health_POIs,
                         by.x="safegraph_place",by.y="safegraph_place_id")

time_data_merged=subset(time_data_merged, !is.element(city,c("Leesburg","Lansdowne")))
#You'll want to make two plots, one with every NAICS code, and then one with the "grouped" NAICS codes, which we will create below:

#Aggregate by NAICS code; this means we will sum up all the different doctors for each NAICS code to get one result per NAICS code
# $ in between is the data set's specified field.
# FUN: function will be a sum.
time_data_merged$naics_code_2 = substr(time_data_merged$naics_code,1,2)
#Aggregate: get the summary stats of the data group
NAICS_aggregate = aggregate(time_data_merged$number , 
                            by=list(time_data_merged$date,time_data_merged$naics_code_2), FUN = sum)

# these declare the names of each data set into vectors
names(NAICS_aggregate) = c("date","NAICS","num")

#The way we are normalizing is by dividing each number by the median number of trips to that NAICS code; therefore the output number can be represented as "% of trips compared to median"
#2 means 2x as many trips as median, .5 means half as many, etc.
unique_codes = unique(NAICS_aggregate$NAICS)
NAICS_data = data.frame()
for (code in unique_codes){
  sub_data = subset(NAICS_aggregate, NAICS == code)
  sub_data$num_normalized = sub_data$num / median(sub_data$num)
  NAICS_data = rbind(NAICS_data, sub_data)
}

#Plotting
NAICS_data$date = as.Date(NAICS_data$date)

NAICS_data$NAICS = as.factor(NAICS_data$NAICS)
library(plyr)
NAICS_data$NAICS_name = revalue(NAICS_data$NAICS, c(
  "44" = "Grocery", "45" = "Department", "61" = "Schools", "62" = "Healthcare",
  "71" = "Recreation", "72" = "Restaurants"))
ggplot() + geom_line(data=NAICS_data, mapping = aes(x=date,
                                                    y = num_normalized, colour = NAICS_name , group = NAICS_name)) + scale_colour_brewer(palette="Set1")


