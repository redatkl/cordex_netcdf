# teh analysis page module
analysis_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "analysis-content",
      tags$h1("Working on this page ⏳"),
    )
  )
}

analysis_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}