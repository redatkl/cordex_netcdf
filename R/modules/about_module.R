# teh about page module
about_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "about-content",
      tags$h1("Working on the 'About' page ⏳"),
    )
  )
}

about_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}