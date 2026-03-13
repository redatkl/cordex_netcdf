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
        
        tags$div(class = "config-title",
          tags$p("CORDEX Analysis")),
        
        tags$div(class = "config-subtitle",
                 tags$p("NetCDF Times-Series Tool")),
        
        tags$div(
          class = "upload-section",
          tags$p("Upload NetCDF Files", class = "upload-title"),
          # Hidden real Shiny fileInput
          div(style = "display: none;",
              fileInput(ns("nc_files"), label = NULL, accept = c(".nc", ".nc4"))
          ),
          tags$label(
            `for` = ns("nc_files"),
            class = "upload-dropzone",
            tags$div(
              class = "upload-icon", "☁️"
            ),
            tags$p("Drag & drop files or ", tags$span(class = "browse-link", "browse")),
            tags$p("Supports .nc, .nc4", class = "upload-subtext")
          )
        ),
        
        
        tags$div(
          class = "analysis-settings",
          tags$div(
            class = "variable-item",
            tags$label("Select Variable"),
            uiOutput(ns("variable_selector_container")) %>%
              withSpinner(type = 8, color = "#3b82f6", size = 0.5)
          ),
          tags$div(
            class = "extraction-method-item",
           radioButtons(
              inputId = ns("extraction_method"),
              label = "Extraction Method",
              choices = c("Nearest Neighbor" = "near_neighbor", "IDW Interpolation" = "idw"),
              selected = "near_neighbor",
              inline = TRUE
            )
          )
        ),
        
         tags$button(
            id = ns("config_button"),
            class = "config-btn", "Config")
        
      ), 
      
      tags$div(
        class = "map-content",
        leafletOutput(ns("map"), width = "100%", height = "100%")
      ),
      
      
      tags$div(
        class = "chart-content",
        tags$p("chart place")
      )
      
    )
   
  )
}

analysis_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
    
    # ---- Variable Selection ----
    file_vars <- reactive({
      req(input$nc_files)
      nc <- nc_open(input$nc_files$datapath[1])
      vars <- setdiff(names(nc$var), c("lat", "lon"))
      nc_close(nc)
      return(vars)
    })
    
    # Render the Select Input dynamically
    output$variable_selector_container <- renderUI({
      # If no file is uploaded yet, show a placeholder
      if (is.null(input$nc_files)) {
        return(tags$select(class = "setting-select", tags$option("Upload a file first")))
      }
      # req() here makes renderUI suspend → triggers spinner
      vars <- file_vars()
      req(vars)
      
      # Once file is uploaded, create the real dropdown
      selectInput(
        inputId = ns("variable_select"), 
        label = NULL, 
        choices = vars,
        selectize = FALSE 
      ) %>% tagAppendAttributes(class = "setting-select")
    })
    
  })
}