library(osrm)
library(dplyr)

library(tmap)

library(tmaptools)
remotes::install_github("riatelab/osrm")



route = osrmRoute(src = #this should be the lat and then lon of the cbg
                    #in poi_trips_sub, this will look something like poi_trips_sub$cbg_lat[1] and then poi_trips_Sub$cbg_lon[1]
                    c(cbgs[1], cbgs[2]) ,  
                  #poi_trips_sub$poi_lat[1] poi_trips_sub$poi_lon[1]
                  dst = c(pois[1], pois[2]), osrm.profile = "car")


cbgs = c(POI_trips_sub$cbg_lon[1], POI_trips_sub$cbg_lat[1])  
pois = c(POI_trips_sub$POI_lon[1], POI_trips_sub$POI_lat[1]) 

print(cbgs) 
print(pois)


route = osrmRoute(src = cbgs, dst = pois, osrm.profile = "car")

print(route$distance)

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
  route = osrmRoute(src = cbgs, dst = pois, osrm.profile = "car")
  distances$Distance[i] = route$distance
}

View(distances)

write.csv(distances, "distances_results.csv")

distances = read.csv("distances_results.csv")
