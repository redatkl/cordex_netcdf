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
        tags$p("config place"),
      ), 
      
      tags$div(
        class = "map-content",
        tags$p("map place")
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
    
  })
}