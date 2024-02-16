
function(input, output, session) {

  #subset_trips <- reactive({subset(overall_trips, naics_code == input$NAICS_selection)})#%>% distinct(location_name, .keep_all = TRUE)
  subset_trips <- reactive({
    if (input$POI_selection == "All POIs"){
      subset(overall_trips, naics_code == NAICSTranslator %>%
                                          filter(Name == input$NAICS_selection) %>%
                                          pull(Code))
    } else {
      subset(overall_trips, naics_code == NAICSTranslator %>%
                                          filter(Name == input$NAICS_selection) %>% 
                                          pull(Code) &
                                          location_name == input$POI_selection)
    }
  })
  subset_trips_month <- reactive({
    #print(subset_trips())
    month_select = floor_date(input$Month_selection,unit = "month")
    if (input$POI_selection == "All POIs"){
      subset(subset_trips(), month == month_select)
    } else {
      subset(subset_trips(), month == month_select)
    }
  })
  output$uniqueLocations <- renderDataTable({
    uniqueLocations <- aggregate(subset_trips_month()[,c("num")], by=list(subset_trips_month()$location_name), FUN=sum)
    names(uniqueLocations) = c("location_name","num_visitors")
    merge(x=uniqueLocations,y=unique(subset_trips_month()[,c("location_name","safegraph_place_id","latitude","longitude")]))
    uniqueLocations
  })
  
  output$visitor_data <- renderDataTable({
    visitor_data = aggregate(subset_trips_month()[,c("num")],by = list(subset_trips_month()$visitor_home_cbg),FUN = sum)
    names(visitor_data) = c("home_cbg","num_visitors")
    merge(shapefile,visitor_data,by.x = "GEOID",by.y="home_cbg")
    shapefile
  })
  
  plot_dat = reactive({ 
    plot_data = subset_trips()
    plot_data_agg = aggregate(plot_data$num,by=list(plot_data$month), FUN = sum)
    names(plot_data_agg) = c("month","num")
    plot_data_agg})
  
  output$plot_months<-renderPlot({
    print(plot_dat())
    ggplot()+geom_line(data = plot_dat() , mapping = aes(x=month,y=num, group = 1)) + 
      ggtitle(paste0(input$NAICS_selection, ", ", input$POI_selection))+ scale_x_continuous(name = "")+
      scale_y_continuous(name = "# Visitors")+
            theme_bw(base_size=20)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))},height=500,width=500)

  output$visitor_data <- renderDataTable({
    visitor_data = aggregate(subset_trips_month()[,c("num")],by = list(subset_trips_month()$visitor_home_cbg),FUN = sum)
    names(visitor_data) = c("home_cbg","num_visitors")
    merge(shapefile,visitor_data,by.x = "GEOID",by.y="home_cbg")
    shapefile
  })
  #POI marker icons
  icons <- awesomeIcons(
    icon = "ios-medkit",
    iconColor = 'red',
    library = 'ion',
    markerColor = "blue"
  )
  #sets up Health POI selection box to react to NAICS choice
  observe({
    x <- NAICSTranslator %>%
      filter(Name == input$NAICS_selection) %>% 
      pull(Code)
    list <- subset(uniqueLocations, naics_code == x)
    list <- with(list, list[order(location_name),])
    list <- list$location_name
    list[length(list)+1] <- "All POIs"
    updateSelectInput(session, "POI_selection",
      choices = list,
      selected = list[length(list)]
    )
  })
  
  output$outputmap <- renderLeaflet({
    
    print(subset_trips_month())
    visitor_data = aggregate(subset_trips_month()[,c("num")],by = list(subset_trips_month()$visitor_home_cbg),FUN = sum)
    names(visitor_data) = c("home_cbg","num_visitors")
    shapefile_visited = merge(shapefile,visitor_data,by.x = "GEOID",by.y="home_cbg")

    
    uniqueLocations <- aggregate(subset_trips_month()[,c("num")], by=list(subset_trips_month()$location_name), FUN=sum)
    names(uniqueLocations) = c("location_name","num_visitors")
    uniqueLocations = merge(x=uniqueLocations,y=unique(subset_trips_month()[,c("location_name","safegraph_place_id","latitude","longitude")]))
    
    mypallet <- colorNumeric( palette="YlOrRd", domain=c(0,log10(maximum_value)),na.color='black')#shapefile_visited$num_visitors), na.color='black')
    leaflet(shapefile_visited) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~mypallet(log10(round(num_visitors))),
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
                         pal = mypallet, values = ~log10(round(shapefile_visited$num_visitors)),
                         title = paste("Total Visitors (log10); NAICS ", unique(subset_trips_month()$naics_code)),
                         opacity = 0.5)
  })
}
