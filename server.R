
function(input, output, session) {

  subset_trips <- reactive({subset(overall_trips, naics_code == input$NAICS_selection)})#%>% distinct(location_name, .keep_all = TRUE)
  output$uniqueLocations <- renderDataTable({
    
    uniqueLocations <- aggregate(subset_trips()[,c("total_visitors")], by=list(subset_trips()$location_name), FUN=sum)
    names(uniqueLocations) = c("location_name","num_visitors")
    merge(x=uniqueLocations,y=unique(subset_trips()[,c("location_name","safegraph_place_id","latitude","longitude")]))
    uniqueLocations
  })
  
  output$visitor_data <- renderDataTable({
    visitor_data = aggregate(subset_trips()[,c("total_visitors")],by = list(subset_trips()$visitor_home_cbg),FUN = sum)
    names(visitor_data) = c("home_cbg","num_visitors")
    merge(shapefile,visitor_data,by.x = "GEOID",by.y="home_cbg")
    shapefile
  })
  icons <- awesomeIcons(
    icon = "ios-medkit",
    iconColor = 'blue',
    library = 'ion',
    markerColor = "green"
  )
  

  output$outputmap <- renderLeaflet({
    visitor_data = aggregate(subset_trips()[,c("total_visitors")],by = list(subset_trips()$visitor_home_cbg),FUN = sum)
    names(visitor_data) = c("home_cbg","num_visitors")
    shapefile_visited = merge(shapefile,visitor_data,by.x = "GEOID",by.y="home_cbg")

    
    uniqueLocations <- aggregate(subset_trips()[,c("total_visitors")], by=list(subset_trips()$location_name), FUN=sum)
    names(uniqueLocations) = c("location_name","num_visitors")
    uniqueLocations = merge(x=uniqueLocations,y=unique(subset_trips()[,c("location_name","safegraph_place_id","latitude","longitude")]))
    
    mypallet <- colorNumeric( palette="YlOrRd", domain=c(0,log10(maximum_value)),na.color='black')#shapefile_visited$num_visitors), na.color='black')
    leaflet(shapefile_visited) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~mypallet(log10(num_visitors)),
        weight = 2,
        opacity = 1,
        fillOpacity = .8,
        color = 'lightgrey',
        highlightOptions = highlightOptions(
          weight = 5,
          color = 'purple',
          bringToFront = TRUE),
        label = ~paste0("GEOID: ",GEOID,"; # visitors: ", num_visitors),
        popup = as.character(shapefile_visited$num_visitors)) %>%
      addAwesomeMarkers(uniqueLocations$longitude, uniqueLocations$latitude, icon = icons,
                        popup = uniqueLocations$location_name) %>%
      setView(lng = median(uniqueLocations$longitude), 
              lat = median(uniqueLocations$latitude), 
              zoom = 10) %>%
      leaflet::addLegend(data = shapefile_visited,
                         position = "bottomright",
                         pal = mypallet, values = ~log10(shapefile_visited$num_visitors),
                         title = paste("Total Visitors (log10); NAICS ", unique(subset_trips()$naics_code)),
                         opacity = 0.5)
  })
}
