library(leaflet)
fluidPage(
  # Application title
  titlePanel("NAICS Viewer"),
  
sidebarLayout(
  # Sidebar with a slider and selection inputs
  sidebarPanel(
    selectInput("NAICS_selection", "Choose code:",
                choices = uniqueNAICS,
                selected = 0),
   
  ),
  # Show Word Cloud
  mainPanel(
    leafletOutput("outputmap")
  )
)
)
