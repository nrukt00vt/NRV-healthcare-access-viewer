library(tmap)


library(tmaptools)
remotes::install_github("riatelab/osrm")
remotes::install_github("riatelab/osrm", dependencies = TRUE, upgrade = "never")


q1 = geocode_OSM("6301 Silver Dart Dr, Mississauga, ON L5P 1B2")
q2 = geocode_OSM("290 Bremner Blvd, Toronto, ON M5V 3L9")

q1 = as.numeric(q1$coords)
q2 = as.numeric(q2$coords)

q1_lat = q1[1]
q1_long = q1[2]
q2_lat = q2[1]
q2_long = q2[2]

route = osrmRoute(src = c(q1[1], q1[2]) ,  dst = c(q2[1], q2[2]), osrm.profile = "car")
route$distance
