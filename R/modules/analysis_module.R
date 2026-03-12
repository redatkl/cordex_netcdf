# teh analysis page module
analysis_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/analysis.css"),
      tags$script(src = "js/analysis.js")
    ),
    
    tags$div(
      class = "analysis-container",
      
      tags$div(
        class = "config-content",
        tags$p("config place")
      ), 
      
      tags$div(
        class = "map-content",
        leafletOutput(ns("map"), width = "100%", height = "100%")
      ),
      
      
      tags$div(
        class = "chart-content",
        tags$p("chart place"),
      )
      
    )
   
  )
}

analysis_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # leaflet map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 2,
        maxZoom = 18,
        attributionControl = FALSE,
        zoomControl = FALSE
      )) %>%
        #addTiles() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)%>%
        htmlwidgets::onRender("
          function(el, x) {
            var map = this;
            
            // Create zoom control at bottom right
            var zoomControl = L.control.zoom({
              position: 'bottomright'
            });
            
            // Add it to the map
            map.addControl(zoomControl);
          }
        ")
      
    })
    
  })
}