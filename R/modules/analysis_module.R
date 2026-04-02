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
          div(style = "position: absolute; width: 0; height: 0; overflow: hidden; opacity: 0;",
              fileInput(ns("nc_files"), label = NULL, accept = c(".nc", ".nc4"))
          ),
          tags$label(
            `data-target` = ns("nc_files"),
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
            uiOutput(ns("variable_selector_container"))
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
        
        tags$div(
          class = "coordinates-section",
          tags$p("Select Point", class = "upload-title"),
          numericInput(ns("input_lat"), "Latitude:", value = NA, step = 0.01),
          numericInput(ns("input_lon"), "Longitude:", value = NA, step = 0.01)
        ),
        
        actionButton(
          ns("config_button"),
          "Config",
          class = "config-btn"
        )
        
      ), 
      
      tags$div(
        class = "map-content",
        leafletOutput(ns("map"), width = "100%", height = "100%")
      ),
      
      
      tags$div(
        class = "chart-content",
        tableOutput(ns("preview")),
        plotOutput(ns("tsplot"))
      )
      
    )
   
  )
}

analysis_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Longitude Wrapping Helper ----
    wrap_lon <- function(lon_vals) {
      if (max(lon_vals, na.rm = TRUE) > 180) {
        lon_vals[lon_vals > 180] <- lon_vals[lon_vals > 180] - 360
      }
      return(lon_vals)
    }
    
    # leaflet map
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 2,
        maxZoom = 18,
        attributionControl = FALSE,
        zoomControl = FALSE
      )) %>%
        addTiles() %>%
        #addProviderTiles(providers$Esri.WorldImagery) %>%
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
    
    # ---- Extraction ----
    idw_points <- reactiveVal(NULL)
    
    extracted_data <- eventReactive(input$config_button, {
      req(input$nc_files, input$variable_select, selected_point())
      
      results <- list()
      
      for (i in seq_len(nrow(input$nc_files))) {
        nc <- nc_open(input$nc_files$datapath[i])
        
        var_vals <- ncvar_get(nc, input$variable_select)
        lat_vals <- as.vector(ncvar_get(nc, "lat"))
        lon_vals <- as.vector(ncvar_get(nc, "lon"))
        lon_vals <- wrap_lon(lon_vals)
        time_var <- ncvar_get(nc, "time")
        time_units <- ncatt_get(nc, "time", "units")$value
        time_dates <- as.Date(time_var, origin = sub(".*since ", "", time_units))
        
        nc_close(nc)
        
        dims <- dim(var_vals)
        var_mat <- matrix(var_vals, nrow = dims[1] * dims[2], ncol = dims[3])
        
        dist <- (lat_vals - selected_point()[2])^2 +
          (lon_vals - selected_point()[1])^2
        
        if (input$extraction_method == "near_neighbor") {
          idx <- which.min(dist)
          ts_vals <- var_mat[idx, ]
          idw_points(data.frame(lon = lon_vals[idx], lat = lat_vals[idx]))
          
        } else {
          # IDW - 4 neighbors, power=2
          neighbors <- order(dist)[1:4]
          d <- sqrt(dist[neighbors])
          d[d == 0] <- 1e-10
          weights <- 1 / (d^2)
          weights <- weights / sum(weights)
          ts_vals <- colSums(var_mat[neighbors, ] * weights)
          idw_points(data.frame(lon = lon_vals[neighbors], lat = lat_vals[neighbors]))
        }
        
        period_label <- paste0(min(time_dates), " to ", max(time_dates))
        results[[i]] <- data.frame(
          Time = time_dates,
          FileLabel = period_label,
          Value = as.numeric(ts_vals)
        )
      }
      
      bind_rows(results)
    })
    
    # ---- Highlight Neighbor Points on Map ----
    observe({
      req(idw_points())
      pts <- idw_points()
      
      leafletProxy(ns("map")) %>%
        clearGroup("idw") %>%
        addCircleMarkers(
          data = pts,
          lng = ~lon,
          lat = ~lat,
          radius = 6,
          color = ifelse(input$extraction_method == "near_neighbor", "green", "red"),
          fillOpacity = 0.8,
          group = "idw"
        )
    })
    
    # ---- Selected Point ----
    selected_point <- reactiveVal(NULL)
    
    # ---- Map Click ----
    observeEvent(input$map_click, {
      click <- input$map_click
      selected_point(c(click$lng, click$lat))
      
      updateNumericInput(session, "input_lat", value = round(click$lat, 4))
      updateNumericInput(session, "input_lon", value = round(click$lng, 4))
      
      leafletProxy(ns("map")) %>%
        clearGroup("selected") %>%
        addMarkers(
          lng = click$lng,
          lat = click$lat,
          group = "selected"
        )
    })
    
    # ---- Manual Coordinate Input ----
    observeEvent({
      input$input_lat
      input$input_lon
    }, {
      req(!is.na(input$input_lat), !is.na(input$input_lon))
      
      selected_point(c(input$input_lon, input$input_lat))
      
      leafletProxy(ns("map")) %>%
        clearGroup("selected") %>%
        addMarkers(
          lng = input$input_lon,
          lat = input$input_lat,
          group = "selected"
        )
    }, ignoreInit = TRUE)
    
    
    # ---- Map Extent ----
    map_extent <- reactive({
      req(input$nc_files)
      nc <- nc_open(input$nc_files$datapath[1])
      lat_vals <- as.vector(ncvar_get(nc, "lat"))
      lon_vals <- as.vector(ncvar_get(nc, "lon"))
      nc_close(nc)
      lon_vals <- wrap_lon(lon_vals)
      list(lat_vals = lat_vals, lon_vals = lon_vals)
    })
    
    # ---- Show Grid Points on Map ----
    observe({
      req(map_extent())
      m <- map_extent()
      leafletProxy(ns("map")) %>%
        clearGroup("grid") %>%
        addCircleMarkers(
          lng = m$lon_vals,
          lat = m$lat_vals,
          radius = 2,
          color = "blue",
          fillOpacity = 0.4,
          group = "grid"
        )
    })
    
    # ---- Variable Selection ----
    file_vars <- reactive({
      message(">>> file_vars triggered")
      message(">>> input$nc_files is NULL: ", is.null(input$nc_files))
      req(input$nc_files)
      message(">>> File path: ", input$nc_files$datapath[1])
      nc <- nc_open(input$nc_files$datapath[1])
      vars <- setdiff(names(nc$var), c("lat", "lon"))
      message(">>> Variables found: ", paste(vars, collapse = ", "))
      nc_close(nc)
      return(vars)
    })
    
    # Render the Select Input dynamically
    output$variable_selector_container <- renderUI({
      message(">>> renderUI triggered, nc_files NULL: ", is.null(input$nc_files))
      # If no file is uploaded yet, show a placeholder
      if (is.null(input$nc_files)) {
        return(tags$select(class = "setting-select", tags$option("Upload a file first")))
      }
      # req() here makes renderUI suspend → triggers spinner
      vars <- file_vars()
      message(">>> vars received in renderUI: ", paste(vars, collapse = ", "))
      req(vars)
      
      # Once file is uploaded, create the real dropdown
      selectInput(
        inputId = ns("variable_select"), 
        label = NULL, 
        choices = vars,
        selectize = FALSE 
      ) %>% tagAppendAttributes(class = "setting-select")
    })
    
    # ---- Preview ----
    output$preview <- renderTable({
      df <- extracted_data()
      
      # Make sure df exists and has rows
      validate(
        need(!is.null(df) && nrow(df) > 0, "No data available. Select files and click 'Run'.")
      )
      
      head(df)
    })
    
    # --- Plot with legend showing periods ---
    output$tsplot <- renderPlot({
      df <- extracted_data()
      # validate(
      #   need(nrow(df) > 0, "No data to display")
      # )
      ggplot(df, aes(x=Time, y=Value, color=FileLabel)) +
        geom_line() +
        theme_minimal() +
        labs(y=input$varname, color="Period") +
        theme(
          axis.text.x = element_text(size = 11),  # x-axis labels
          axis.text.y = element_text(size = 11),  # y-axis labels
          axis.title.x = element_text(size = 12, face = "bold"),  # x-axis title
          axis.title.y = element_text(size = 12, face = "bold"),  # y-axis title
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12, face = "bold")
        )
    })
    
  })
}