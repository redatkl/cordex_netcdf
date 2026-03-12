# Source global file
source("global.R")


# Define UI
ui <- tagList(
  
  # Custom CSS styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/navigation.css"),
    
    # Add favicon in the head section
    tags$link(rel = "shortcut icon", type = "image/png", href = "favicon/favicon.ico"),
    # JavaScript for navigation button styling
    tags$script(src = "js/navigation.js")
  ),
  fluidPage(
    
    # Custom navigation bar using HTML tags
    tags$div(
      class = "navbar",
      
      # add first logo 
      tags$div(
        class= "tittle-left",
        tags$p("CORDEX Analysis")
      ),
      
    
      #center navigation buttons
      tags$div(
        class = "navbar-buttons",
        tags$button(
          id = "nav_accueil",
          class = "nav-btn active",
          `data-page` = "Home",
          onclick = "Shiny.setInputValue('current_page', 'Home')",
          "Home"
        ),
        tags$button(
          id = "nav_data", 
          class = "nav-btn",
          `data-page` = "analysis",
          onclick = "Shiny.setInputValue('current_page', 'analysis')",
          "Analysis"
        ),
        
        tags$button(
          id = "nav_reporting", 
          class = "nav-btn",
          `data-page` = "about",
          onclick = "Shiny.setInputValue('current_page', 'about')",
          "About"
        )
      ),
      # first logo on the right 
      tags$div(
        class= "logo-right",
        tags$a(href = "https://github.com/waterhy", target = "_blank", tags$img(src = "logos/github.svg", height = "50px",style = "background-color: white; border-radius: 50%; padding: 2px;"))
      ),
      # second logo on the right
      tags$div(
        class= "logo-right",
        tags$a(href = "https://www.linkedin.com/in/aikaterini-lyra-434143103", target = "_blank", tags$img(src = "logos/linkedin.svg", height = "50px"))
      )
    ),
    
    # Page content container
    tags$div(
      class = "page-content",
      conditionalPanel(
        condition = "input.current_page == 'Home' || typeof input.current_page === 'undefined'",
        home_module_ui("Home")
      ),
      conditionalPanel(
        condition = "input.current_page == 'analysis'",
        analysis_module_ui("analysis")
      ),
      conditionalPanel(
        condition = "input.current_page == 'about'",
        about_module_ui("about")
      )
    )
  ),
  # Footer
  tags$footer(
    class = "footer",
    tags$div(
      class = "footer-content",
      # Left section - Copyright
      tags$div(
        class = "footer-left",
        tags$p("")
      ),
      # Center section - Logo and text
      tags$div(
        class = "footer-center",
        #tags$img(src = "logos/logo_footer.png", height = "20px", class = "footer-logo"),
        tags$p(class = "footer-text", "CORDEX")
      ),
      # Right section - Can be empty or add content
      tags$div(
        class = "footer-right",
        tags$p(paste0("All rights reserved ©", year_footer))
      )
    )
  )
)


# Define server
server <- function(input, output, session) {
  observe(
    # Initialize current page
    if (is.null(input$current_page)) {
      updateSelectInput(session, "current_page", selected = "Home")
    })
  

}

# Run the application
shinyApp(ui = ui, server = server)