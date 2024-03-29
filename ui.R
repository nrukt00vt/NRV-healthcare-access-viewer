library(leaflet)
fluidPage(
  # Application title
  titlePanel("NAICS Viewer"),
  
sidebarLayout(
  # Sidebar with a slider and selection inputs
  sidebarPanel(
    selectInput("NAICS_selection", "Choose NAICS code:",
                choices = NAICSTranslator$Name,
                selected = 0),
    selectInput("POI_selection", "Select Individual POI: ",
                choices = NULL),
    sliderInput("Month_selection",
                "Dates:",
                min = min(as.Date(overall_trips$month)),
                max = max(as.Date(overall_trips$month)),
                value=as.Date("2019-12-01"),
                timeFormat="%Y-%m"),
    plotOutput("plot_months", inline = TRUE),
  ),

  # Show Word Cloud
  mainPanel(
    leafletOutput("outputmap",height = 950)
  )
),

)
