library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(geojsonio)
library(spatialEco)
library(rmarkdown)
library(knitr)
library(DT)

# ------------------ UI ------------------
ui <- page_sidebar(
  title = "🌍 GeoJSON Viewer",
  
  theme = bs_theme(bootswatch = "flatly", version = 5),  # Apply theme
  
  sidebar = sidebar(
    title = "Upload & Messages",
    
    fileInput("upload", "📂 Upload GeoJSON", accept = c(".geojson", ".json")),  # Upload input
    
    downloadButton("download_geojson", "Download GeoJSON"),  # Download cleaned GeoJSON
    
    br(), br(),
    textOutput("dupMsg"),         # Message for duplicates
    textOutput("geomErrorMsg")    # Message for MultiPolygon issues
  ),
  
  layout_columns(
    col_widths = c(12),
    
    card(
      title = "🗺️ Map Viewer",
      class = "mb-4",
      leafletOutput("map", height = "700px")  # Leaflet map output
    ),
    
    card(
      title = "📋 Attribute Table",
      class = "mb-4",
      DTOutput("table"),  # Table for non-spatial attributes
      style = "max-height: 300px; overflow-y: auto;" 
    ),
    
    card(
      title = "Generate Report",
      downloadButton("download_report", "Download Report")  # RMarkdown report download
    )
  )
)

# ------------------ Server ------------------
server <- function(input, output, session) {
  
  # Reactive values to track counts
  multi_geom_count <- reactiveVal(0)  # Tracks number of MultiPolygon conversions
  dup_count <- reactiveVal(0)         # Tracks number of duplicates removed
  geo_data <- reactiveVal(NULL)       # Holds processed spatial data
  
  # Function to handle MultiPolygon geometries
  handleMultiGeometries <- function(data) {
    multi_count <- sum(st_geometry_type(data) %in% c("MULTIPOLYGON"))
    multi_geom_count(multi_count)
    data <- st_cast(data, "POLYGON")
    multipolygon_sf <- st_make_valid(data)
    polygons_sf <- st_cast(multipolygon_sf, "POLYGON")
    
    # Assign original row IDs for traceability
    polygons_sf$original_id <- rep(
      1:nrow(multipolygon_sf),
      sapply(st_geometry(multipolygon_sf), function(geom) {
        if (inherits(geom, "MULTIPOLYGON")) length(geom) else 1
      })
    )
    
    # Add feature IDs for UI referencing
    polygons_sf$fid <- seq_len(nrow(polygons_sf))
    polygons_sf <- polygons_sf[, c("fid", setdiff(names(polygons_sf), "fid"))]
    
    return(polygons_sf)
  }
  
  # Dummy duplicate removal (replace with real logic if needed)
  remove_duplicates <- function(data) {
    data[!duplicated(data$geometry), ]
  }
  
  # Reactive expression to read, clean, and process uploaded GeoJSON
  geoData <- reactive({
    req(input$upload)
    tryCatch({
      data <- st_read(input$upload$datapath, quiet = TRUE)  # Read file
      data <- st_zm(data)  # Remove Z/M dimensions if present
      data_multi <- handleMultiGeometries(data)  # Convert MultiPolygons
      data_cleaned <- remove_duplicates(data_multi)  # Remove duplicates
      
      multi_geom_count(nrow(data_multi) - nrow(data))
      dup_count(nrow(data_multi) - nrow(data_cleaned))
      geo_data(data_cleaned)  # Store cleaned data
      
      return(data_cleaned)
    }, error = function(e) {
      showNotification("❌ Invalid GeoJSON or unsupported geometries.", type = "error")
      NULL
    })
  })
  
  # Display message about removed duplicates
  output$dupMsg <- renderText({
    if (dup_count() > 0) {
      paste("⚠️ Removed", dup_count(), "duplicate geometries.")
    } else {
      "✅ No duplicate geometries found."
    }
  })
  
  # Display message about converted MultiPolygons
  output$geomErrorMsg <- renderText({
    if (multi_geom_count() > 0) {
      paste("⚠️ Converted", multi_geom_count(), "MultiPolygon geometries.")
    } else {
      "✅ No MultiPolygon geometries found."
    }
  })
  
  # Initial map setup (before file is uploaded)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Update map once data is processed and ready
  observeEvent(geoData(), {
    data <- geoData()
    req(data)
    
    if (!is.null(data$geometry) && nrow(data) > 0) {
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(
          data = data,
          color = "deepskyblue",
          weight = 2,
          fillOpacity = 0.5,
          popup = ~as.character(st_as_text(geometry)),
          layerId = ~fid
        )
    } else {
      showNotification("❌ No valid geometries found.", type = "error")
    }
  })
  
  # Display attribute table without geometry
  output$table <- renderDT({
    data <- geoData()
    req(data)
    datatable(
      as.data.frame(st_drop_geometry(data)),
      selection = "single",
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  # Zoom and highlight selected feature when clicked in table
  observeEvent(input$table_rows_selected, {
    row <- input$table_rows_selected
    data <- geoData()
    req(row)
    
    selected_fid <- data$fid[row]
    selected_geom <- data[data$fid == selected_fid, ]
    center <- st_centroid(selected_geom)
    coords <- st_coordinates(center)
    
    leafletProxy("map") %>%
      setView(lng = coords[1], lat = coords[2], zoom = 10) %>%
      clearShapes() %>%
      addPolygons(
        data = selected_geom,
        color = "red",
        weight = 2,
        fillOpacity = 0.7
      )
  })
  
  # Download handler for report in PDF format
  output$download_report <- downloadHandler(
    filename = function() {
      paste("GeoJSON_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      data <- geo_data()
      rmarkdown::render(
        input = "report_template.Rmd",  # RMarkdown template
        output_file = file,
        params = list(
          dup_count = dup_count(),
          multi_geom_count = multi_geom_count()
        )
      )
    }
  )
  
  # Download handler for cleaned GeoJSON
  output$download_geojson <- downloadHandler(
    filename = function() {
      paste0("Processed_GeoJSON_", Sys.Date(), ".geojson")
    },
    content = function(file) {
      data <- geo_data()
      req(data)
      st_write(data, file, driver = "GeoJSON", quiet = TRUE)
    }
  )
}

# ------------------ Run the app ------------------
shinyApp(ui, server)
