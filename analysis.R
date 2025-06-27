#Librarys
#########
library(sf)
library(tidyverse)
library(leaflet)
##########################
#POI graphing Data Process
##########################

#Read in the shapefile as an "sf" object
shapefile = read_sf(dsn =".", layer= "tl_2019_51_bg")
#Choose HealthPOIs_Montgomery_VA.csv
health_POIs = read.csv('HealthPOIs_Montgomery_VA 2.csv')
montgomery_health_POIs1 = subset(health_POIs, city == "Blacksburg" | city == "Christiansburg")
montgomery_health_POIs2 <- montgomery_health_POIs1 %>% distinct(street_address, .keep_all = TRUE)


#Read in data
all_data = read.csv("overall_trips_VA.csv")
locations_with_data = merge(montgomery_health_POIs2,all_data, by.x="safegraph_place_id",by.y="safegraph_place")



uniqueNAICS = unique(uniqueLocations$naics_code)

overall_trips = locations_with_data
overall_trips$NAICS_code <- overall_trips$naics_code
overall_trips$safegraph_place <- overall_trips$safegraph_place_id
uniqueLocations <- overall_trips %>% distinct(safegraph_place, .keep_all = TRUE)

getUniqueNAICSFrames <- function(ids){
  
  listofcodes <- list()
  
  for (i in 1:length(ids)){
    sub_dat3 <- subset(overall_trips, NAICS_code == ids[i])
    sub_dat4 <- sub_dat3 %>% distinct(location_name, .keep_all = TRUE)
    sub_dat3$location_name <- 0
    sub_dat3$latitude <- 0
    sub_dat3$longitude <- 0
    for (j in 1:length(sub_dat4$location_name)){
      sub_dat3$location_name[j] <- sub_dat4$location_name[j]
      sub_dat3$latitude[j] <- sub_dat4$latitude[j]
      sub_dat3$longitude[j] <- sub_dat4$longitude[j]
    }
    listofcodes[[sub_dat3$location_name[i]]] <- sub_dat3
  }
  
  return(listofcodes)
  
}
individual_NAICS_movement <- list()
uniqueNAICS <- overall_trips %>% distinct(NAICS_code, .keep_all = TRUE)
individual_NAICS_movement <- getUniqueNAICSFrames(uniqueNAICS$NAICS_code)

shapefile$total_visitors = 0
shapefile$location_name = 0
shapefile$latitude = 0
shapefile$longitude = 0
getUniqueNAICSmaps <- function(individual_NAICS_movement){
  listofmaps <- list()
  for (i in 1:length(individual_NAICS_movement)){
    new_map <- shapefile
    for (j in 1:length(new_map$GEOID)){
      for (k in 1:length(individual_NAICS_movement[[i]]$visitor_home_cbg )){
        if ((length(new_map$GEOID[j]) == length(individual_NAICS_movement[[i]]$visitor_home_cbg [k])) && 
            (new_map$GEOID[j] == individual_NAICS_movement[[i]]$visitor_home_cbg [k])){
          new_map$total_visitors[j] <- (new_map$total_visitors[j] + individual_NAICS_movement[[i]]$total_visitors[k])
        }else{}
      }
      print(paste(j, "/", length(new_map$GEOID)))
    }
    sub_dat <- individual_NAICS_movement[[i]] %>% distinct(location_name, .keep_all = TRUE)
    for (j in 1:length(sub_dat$location_name)){
      new_map$location_name[j] <- sub_dat$location_name[j]
      new_map$latitude[j] <- sub_dat$latitude[j]
      new_map$longitude[j] <- sub_dat$longitude[j]
    }
    listofmaps[[i]] <- new_map
    print(paste(i,"/", length(individual_NAICS_movement)))
  }
  return(listofmaps)
}
NAICS_movement_maps <- list()
NAICS_movement_maps <- getUniqueNAICSmaps(individual_NAICS_movement)

#Testing one map
domain <- subset(NAICS_movement_maps[[1]], total_visitors > 1)
locations <- individual_NAICS_movement[[1]] %>% distinct(location_name, .keep_all = TRUE)
print(domain$total_visitors)
mypallet <- colorNumeric( palette="Spectral", domain=log10(domain$total_visitors), na.color='black')
icons <- awesomeIcons(
  icon = "ios-medkit",
  iconColor = 'blue',
  library = 'ion',
  markerColor = "red"
)
choro <- leaflet(NAICS_movement_maps[[1]]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~mypallet(log10(total_visitors)),
    weight = 2,
    opacity = 1,
    color = 'lightblue',
    highlightOptions = highlightOptions(
      weight = 5,
      color = 'purple',
      bringToFront = TRUE),
    label = ~GEOID,
    popup = as.character(NAICS_movement_maps[[1]]$total_visitors)) %>%
  addAwesomeMarkers(locations$longitude, locations$latitude, icon = icons,
                    popup = locations$location_name) %>%
  setView(lng = median(locations$longitude), 
          lat = median(locations$latitude), 
          zoom = 10) %>%
  leaflet::addLegend(data = NAICS_movement_maps[[1]],
                     position = "bottomright",
                     pal = mypallet, values = ~log10(domain$total_visitors),
                     title = paste("Total Visitors (log10)", locations$NAICS_code[1]),
                     opacity = 0.5)
choro

