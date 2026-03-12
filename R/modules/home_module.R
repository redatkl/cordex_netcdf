# teh home page module
home_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "home-content",
      tags$h1("Working on this page ⏳"),
    )
  )
}

home_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}