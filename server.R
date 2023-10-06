
function(input, output, session) {
  naics_id <- reactive({input$NAICS_selection})
  print(naics_id)
  subset_trips <- subset(overall_trips, naics_code == naics_id)#%>% distinct(location_name, .keep_all = TRUE)
  
  uniqueLocations <- aggregate(subset_trips[,c("total_visitors")], by=list(subset_trips$location_name), FUN=sum)
  names(uniqueLocations) = c("location_name","num_visitors")
  uniqueLocations = merge(x=uniqueLocations,y=unique(subset_trips[,c("location_name","safegraph_place_id","latitude","longitude")]))
  
  
  visitor_data = aggregate(subset_trips[,c("total_visitors")],by = list(subset_trips$visitor_home_cbg),FUN = sum)
  names(visitor_data) = c("home_cbg","num_visitors")
  shapefile_with_data = merge(shapefile,visitor_data,by.x = "GEOID",by.y="home_cbg")
  
  icons <- awesomeIcons(
    icon = "ios-medkit",
    iconColor = 'blue',
    library = 'ion',
    markerColor = "red"
  )
  
  mypallet <- colorNumeric( palette="Spectral", domain=log10(shapefile_with_data$num_visitors), na.color='black')

  output$outputmap <- renderLeaflet({
    leaflet(shapefile_with_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~mypallet(log10(num_visitors)),
        weight = 2,
        opacity = 1,
        color = 'lightgrey',
        highlightOptions = highlightOptions(
          weight = 5,
          color = 'purple',
          bringToFront = TRUE),
        label = ~paste0("GEOID: ",GEOID,"; # visitors: ", num_visitors),
        popup = as.character(shapefile_with_data$num_visitors)) %>%
      addAwesomeMarkers(uniqueLocations$longitude, uniqueLocations$latitude, icon = icons,
                        popup = uniqueLocations$location_name) %>%
      setView(lng = median(uniqueLocations$longitude), 
              lat = median(uniqueLocations$latitude), 
              zoom = 10) %>%
      leaflet::addLegend(data = shapefile_with_data,
                         position = "bottomright",
                         pal = mypallet, values = ~log10(shapefile_with_data$num_visitors),
                         title = paste("Total Visitors (log10); NAICS ", naics_id),
                         opacity = 0.5)
  })
}
