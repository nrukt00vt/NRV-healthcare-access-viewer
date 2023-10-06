library(leaflet)
fluidPage(
  # Application title
  titlePanel("NAICS Viewer"),
  
sidebarLayout(
  # Sidebar with a slider and selection inputs
  sidebarPanel(
    selectInput("NAICS_selection", "Choose code:",
                choices = uniqueNAICS,
                selected = uniqueNAICS[1]),
    actionButton("update", "Update map"),
   
  ),
  # Show Word Cloud
  mainPanel(
    leafletOutput("outputmap")
  )
)
)
