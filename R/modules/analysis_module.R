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
      
      # -- Left config panel --
      tags$div(
        class = "config-content",
        
        # -- tittle panel --
        tags$div(class = "config-title", "CORDEX Analysis"),
        
        tags$div(class = "config-subtitle", "NetCDF Times-Series Tool"),
        
        # -- Upload Section --
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
        
        # -- Analysis block --
        tags$div(
          class = "settings-block",
          tags$p("Analysis Settings", class = "section-label"),
          
          # Variable selection
          tags$div(
            class = "field-group",
            tags$label("Variable Selection", class = "field-label"),
            uiOutput(ns("variable_selector_container"))
          ),
          
          # Extraction method
          tags$div(
            class = "field-group",
            tags$label("Extraction Method", class = "field-label"),
            radioButtons(
              inputId  = ns("extraction_method"),
              label    = NULL,
              choices  = c("Nearest Neighbor" = "near_neighbor",
                           "IDW Interpolation" = "idw"),
              selected = "near_neighbor"
            )
          )
        ),
        
        # -- Point selection --
        tags$div(
          class = "settings-block",
          tags$p("Point Selection", class = "section-label"),
          tags$div(
            class = "coord-row",
            tags$div(
              class = "coord-field",
              tags$label("LATITUDE",  class = "coord-label"),
              numericInput(ns("input_lat"), label = NULL,
                           value = NA, step = 0.01,
                           width = "100%")
            ),
            tags$div(
              class = "coord-field",
              tags$label("LONGITUDE", class = "coord-label"),
              numericInput(ns("input_lon"), label = NULL,
                           value = NA, step = 0.01,
                           width = "100%")
            )
          )
        ),
        
        # -- Run Button --
        actionButton(
          ns("config_button"),
          "Run Extraction",
          class = "run-btn",
          width  = "100%"
        ),
        
        # File Metadata Summary
        tags$div(
          class = "metadata-block",
          tags$p("FILE METADATA SUMMARY", class = "section-label"),
          uiOutput(ns("metadata_summary"))
        ),
        
        # Footer credit
        tags$div(
          class = "config-footer",
          tags$p("Created by Aikaterini Lyra")
        )
      ),
      
      # ── MAP ────────────────────────────────────────────────────
      tags$div(
        class = "map-content",
        leafletOutput(ns("map"), width = "100%", height = "100%")
      ),
      
      # ── BOTTOM: CHART + DATA PREVIEW ───────────────────────────
      tags$div(
        class = "chart-content",
        
        # Chart panel
        tags$div(
          class = "chart-panel",
          tags$div(
            class = "chart-header",
            tags$div(
              tags$p("Time-Series Analysis",  class = "chart-title"),
              uiOutput(ns("chart_subtitle"))
            )
          ),
          plotOutput(ns("tsplot"), height = "100%")
        ),
        
        # Data preview panel
        tags$div(
          class = "preview-panel",
          tags$div(
            class = "preview-header",
            tags$div(
              tags$p("DATA PREVIEW",  class = "preview-title"),
              tags$p("Showing top 50 rows", class = "preview-subtitle")
            ),
            downloadButton(ns("download_csv"), "Export as CSV",
                           class = "export-btn")
          ),
          div(
            class = "preview-table-wrap",
            tableOutput(ns("preview"))
          )
        )
        
      )
      
    )
  )
}

analysis_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Helpers ─────────────────────────────────────────────────
    wrap_lon <- function(lon_vals) {
      if (max(lon_vals, na.rm = TRUE) > 180)
        lon_vals[lon_vals > 180] <- lon_vals[lon_vals > 180] - 360
      lon_vals
    }
    
    # ── Map ──────────────────────────────────────────────────────
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(
        worldCopyJump   = FALSE,
        minZoom         = 2,
        maxZoom         = 18,
        attributionControl = FALSE,
        zoomControl     = FALSE
      )) %>%
        addTiles() %>%
        setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var map = this;
            map.addControl(L.control.zoom({ position: 'bottomright' }));
          }
        ")
    })
    
    # ── Reactive: selected point ─────────────────────────────────
    selected_point <- reactiveVal(NULL)
    idw_points     <- reactiveVal(NULL)
    
    # Map click → update inputs & marker
    observeEvent(input$map_click, {
      click <- input$map_click
      selected_point(c(click$lng, click$lat))
      updateNumericInput(session, "input_lat", value = round(click$lat, 4))
      updateNumericInput(session, "input_lon", value = round(click$lng, 4))
      leafletProxy(ns("map")) %>%
        clearGroup("selected") %>%
        addMarkers(lng = click$lng, lat = click$lat, group = "selected")
    })
    
    # Manual coords → update marker
    observeEvent(list(input$input_lat, input$input_lon), {
      req(!is.na(input$input_lat), !is.na(input$input_lon))
      selected_point(c(input$input_lon, input$input_lat))
      leafletProxy(ns("map")) %>%
        clearGroup("selected") %>%
        addMarkers(lng = input$input_lon, lat = input$input_lat, group = "selected")
    }, ignoreInit = TRUE)
    
    # ── Map extent / grid points ─────────────────────────────────
    map_extent <- reactive({
      req(input$nc_files)
      nc <- nc_open(input$nc_files$datapath[1])
      lat_vals <- as.vector(ncvar_get(nc, "lat"))
      lon_vals <- wrap_lon(as.vector(ncvar_get(nc, "lon")))
      nc_close(nc)
      list(lat_vals = lat_vals, lon_vals = lon_vals)
    })
    
    observe({
      req(map_extent())
      m <- map_extent()
      leafletProxy(ns("map")) %>%
        clearGroup("grid") %>%
        addCircleMarkers(lng = m$lon_vals, lat = m$lat_vals,
                         radius = 2, color = "blue",
                         fillOpacity = 0.4, group = "grid")
    })
    
    # ── Neighbor highlight ───────────────────────────────────────
    observe({
      req(idw_points())
      pts <- idw_points()
      leafletProxy(ns("map")) %>%
        clearGroup("idw") %>%
        addCircleMarkers(
          data = pts, lng = ~lon, lat = ~lat,
          radius = 6,
          color  = ifelse(input$extraction_method == "near_neighbor", "green", "red"),
          fillOpacity = 0.8,
          group = "idw"
        )
    })
    
    # ── Variable selector ────────────────────────────────────────
    file_vars <- reactive({
      req(input$nc_files)
      nc   <- nc_open(input$nc_files$datapath[1])
      vars <- setdiff(names(nc$var), c("lat", "lon"))
      nc_close(nc)
      vars
    })
    
    output$variable_selector_container <- renderUI({
      if (is.null(input$nc_files))
        return(tags$select(class = "setting-select",
                           tags$option("Upload a file first")))
      vars <- file_vars()
      req(vars)
      selectInput(ns("variable_select"), label = NULL,
                  choices = vars, selectize = FALSE) %>%
        tagAppendAttributes(class = "setting-select")
    })
    
    # ── File metadata summary ────────────────────────────────────
    output$metadata_summary <- renderUI({
      req(input$nc_files)
      nc         <- nc_open(input$nc_files$datapath[1])
      lat_vals   <- as.vector(ncvar_get(nc, "lat"))
      lon_vals   <- as.vector(ncvar_get(nc, "lon"))
      time_var   <- ncvar_get(nc, "time")
      time_units <- ncatt_get(nc, "time", "units")$value
      time_dates <- as.Date(time_var,
                            origin = sub(".*since ", "", time_units))
      
      # Try to get domain global attribute
      domain <- ncatt_get(nc, 0, "CORDEX_domain")$value
      if (isFALSE(domain)) domain <- "N/A"
      
      res    <- round(abs(diff(sort(unique(lat_vals))))[1], 2)
      nc_close(nc)
      
      rows <- list(
        c("Basin",      domain),
        c("Resolution", paste0(res, " deg")),
        c("Time", paste0(min(time_dates), " to ", max(time_dates))),
        c("Freq",       "daily")
      )
      
      tags$div(
        class = "metadata-table",
        lapply(rows, function(r) {
          tags$div(
            class = "meta-row",
            tags$span(r[1], class = "meta-key"),
            tags$span(r[2], class = "meta-val")
          )
        })
      )
    })
    
    # ── Extraction ───────────────────────────────────────────────
    extracted_data <- eventReactive(input$config_button, {
      req(input$nc_files, input$variable_select, selected_point())
      
      results <- list()
      for (i in seq_len(nrow(input$nc_files))) {
        nc       <- nc_open(input$nc_files$datapath[i])
        var_vals <- ncvar_get(nc, input$variable_select)
        lat_vals <- as.vector(ncvar_get(nc, "lat"))
        lon_vals <- wrap_lon(as.vector(ncvar_get(nc, "lon")))
        time_var <- ncvar_get(nc, "time")
        time_units <- ncatt_get(nc, "time", "units")$value
        time_dates <- as.Date(time_var,
                              origin = sub(".*since ", "", time_units))
        nc_close(nc)
        
        dims    <- dim(var_vals)
        var_mat <- matrix(var_vals, nrow = dims[1] * dims[2], ncol = dims[3])
        dist    <- (lat_vals - selected_point()[2])^2 +
          (lon_vals - selected_point()[1])^2
        
        if (input$extraction_method == "near_neighbor") {
          idx     <- which.min(dist)
          ts_vals <- var_mat[idx, ]
          idw_points(data.frame(lon = lon_vals[idx], lat = lat_vals[idx]))
        } else {
          neighbors <- order(dist)[1:4]
          d         <- sqrt(dist[neighbors]); d[d == 0] <- 1e-10
          weights   <- (1 / d^2); weights <- weights / sum(weights)
          ts_vals   <- colSums(var_mat[neighbors, ] * weights)
          idw_points(data.frame(lon = lon_vals[neighbors],
                                lat = lat_vals[neighbors]))
        }
        
        period_label <- paste0(min(time_dates), " to ", max(time_dates))
        results[[i]] <- data.frame(Time      = time_dates,
                                   FileLabel = period_label,
                                   Value     = as.numeric(ts_vals))
      }
      dplyr::bind_rows(results)
    })
    
    # ── Chart subtitle ───────────────────────────────────────────
    output$chart_subtitle <- renderUI({
      req(input$variable_select)
      tags$p(paste("Extracted data for Variable:", input$variable_select),
             class = "chart-subtitle")
    })
    
    # ── Time-series plot ─────────────────────────────────────────
    output$tsplot <- renderPlot({
      df <- extracted_data()
      validate(need(nrow(df) > 0, "Run extraction to see the chart."))
      ggplot(df, aes(x = Time, y = Value, color = FileLabel)) +
        geom_line(linewidth = 0.8) +
        theme_minimal(base_size = 12) +
        labs(x = NULL, y = input$variable_select, color = "Period") +
        theme(
          panel.grid.minor  = element_blank(),
          legend.position   = "bottom",
          axis.text         = element_text(size = 10),
          axis.title.y      = element_text(size = 11, face = "bold"),
          legend.text       = element_text(size = 10),
          legend.title      = element_text(size = 11, face = "bold"),
          plot.background   = element_rect(fill = "transparent", colour = NA),
          panel.background  = element_rect(fill = "transparent", colour = NA)
        )
    }, bg = "transparent")
    
    # ── Data preview ─────────────────────────────────────────────
    output$preview <- renderTable({
      df <- extracted_data()
      validate(need(!is.null(df) && nrow(df) > 0,
                    "No data yet – run extraction first."))
      head(df[, c("Time", "Value")], 50)
    }, striped = TRUE, hover = TRUE, bordered = FALSE,
    colnames = TRUE, digits = 2)
    
    # ── CSV download ─────────────────────────────────────────────
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("timeseries_", input$variable_select, "_",
               format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write.csv(extracted_data(), file, row.names = FALSE)
      }
    )
    
  })
}