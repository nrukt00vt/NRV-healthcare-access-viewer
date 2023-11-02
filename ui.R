library(leaflet)
fluidPage(
  # Application title
  titlePanel("NAICS Viewer"),
  
sidebarLayout(
  # Sidebar with a slider and selection inputs
  sidebarPanel(
    selectInput("NAICS_selection", "Choose NAICS code:",
                choices = uniqueNAICS,
                selected = 0),
    selectInput("POI_selection", "Select Individual POI: ",
                choices = NULL),
   
  ),
  # Show Word Cloud
  mainPanel(
    leafletOutput("outputmap",height = 1000)
  )
)
)
