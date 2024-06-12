packages <- c("shiny", "leaflet", "raster", "DT", "shinyWidgets", "sf", "leafem", "mapview", "gdistance", "dplyr")

# Install packages if they are not already installed
try(install.packages(packages[!packages %in% installed.packages()]))

# Load packages 
try(invisible(lapply(packages, library, character.only = TRUE)))

# Load the raster file directly from GitHub
rst <- raster("/vsicurl/https://raw.githubusercontent.com/hyugoshirai/LeastCostPath/main/BD_LeastCostTool/landuse_simplified.tif")


# Read the shapefile directly from GitHub
origin_sf <- st_read("/vsicurl/https://raw.githubusercontent.com/hyugoshirai/LeastCostPath/main/BD_LeastCostTool/origin.shp")
goal_sf <- st_read("/vsicurl/https://raw.githubusercontent.com/hyugoshirai/LeastCostPath/main/BD_LeastCostTool/goal.shp")

# Extract coordinates of the first point
origin_cd <- st_coordinates(origin_sf)[1, 1:2]
goal_cd <- st_coordinates(goal_sf)[1, 1:2]

# Define UI
ui <- fluidPage(
  titlePanel("Land Use Reclassification"),
  sidebarLayout(
    sidebarPanel(
      h4("Reclassification Table"),
      DTOutput("landuse_table"),
      actionButton("apply_changes", "Apply Changes"),
      actionButton("execute_shortest_path", "Execute Shortest Path") # Add button for executing shortest path
    ),
    mainPanel(
      h4("Original Raster"),
      leafletOutput("original_map"),
      h4("Reclassified Raster"),
      leafletOutput("reclassified_map"),
      # Add progress bar
      div(id = "progress_container",
          tags$div(id = "progress_bar", style = "width: 0%;", class = "progress-bar")
      ),
      # Add plot for reclassified raster and shortest path
      plotOutput("raster_plot")
    )
  )
)

server <- function(input, output, session) {
  # Define land use labels and colors
  land_use_labels <- c("Forest", "Non Forest Natural Formation", "Farming", "Non vegetated area", "Water", "Forest Plantation")
  
  # Define land use colors in the specified order
  land_use_colors <- c("Forest" = "#32a65e", 
                       "Non Forest Natural Formation" = "#02d659", 
                       "Farming" = "#FFFFB2", 
                       "Non vegetated area" = "#d4271e", 
                       "Water" = "#0000FF", 
                       "Forest Plantation" = "#7a5900")
  
  # Load the raster file
  rst <- raster("BD_LeastCostTool/landuse_simplified.tif")
  
  # Get unique raster values and sort them
  unique_values <- sort(unique(values(rst)))
  
  # Create landuse_data data frame with sorted values and corresponding labels
  landuse_data <- data.frame(
    raster_value = unique_values,
    land_use = factor(unique_values, levels = unique_values, labels = land_use_labels),
    new_value = unique_values,
    stringsAsFactors = FALSE
  )
  
  # Reactive value to store reclassified raster
  reclassified_r <- reactiveVal(rst)
  
  output$landuse_table <- renderDT({
    datatable(landuse_data, editable = "cell")
  })
  
  observeEvent(input$apply_changes, {
    new_data <- input$landuse_table_cell_edit
    if (!is.null(new_data)) {
      new_data <- as.data.frame(new_data)
      for (i in seq_len(nrow(new_data))) {
        row <- new_data[i,]
        landuse_data[row$row, 3] <- as.numeric(row$value)
        print(landuse_data)
      }
    }
    
    reclassified_values <- values(rst)
    for (i in 1:nrow(landuse_data)) {
      reclassified_values[values(rst) == landuse_data$raster_value[i]] <- landuse_data$new_value[i]
    }
    reclassified_r_temp <- rst
    values(reclassified_r_temp) <- reclassified_values
    
    reclassified_r(reclassified_r_temp)
    
    output$reclassified_map <- renderLeaflet({
      # Define a gradient palette from hot to cold colors
      color_palette <- colorNumeric(
        palette = c("#FF0000", "#FFFF00", "#00FFFF", "#0000FF"),
        domain = values(reclassified_r())
      )
      leaflet() %>%
        setView(lng = -48.9660322, lat = -22.8864848, zoom = 10) %>%
        addTiles() %>%
        addRasterImage(reclassified_r(), colors = color_palette) %>%
        addLegend(pal = color_palette, values = values(reclassified_r()),
                  title = "Reclassified Land Use", position = "bottomright")
    })
  })
  
  observeEvent(input$execute_shortest_path, {
    # Execute shortest path analysis asynchronously
    progress <- Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    
    # Perform the analysis and update the progress bar
    Sys.sleep(2) # Placeholder for actual analysis time
    for (i in 1:100) {
      progress$set(value = i)
      Sys.sleep(0.1) # Simulate processing time
    }
    
    tr <- transition(x = reclassified_r(), transitionFunction = mean, directions = 8)
    AtoB <- shortestPath(x = tr, origin = origin_cd, goal = goal_cd, output = "SpatialLines")
    
    # Prompt user for file path to save shortest path
    path <- shiny::textInput("save_path", "Enter file path to save the shortest path:", value = "")
    
    # Clear existing layers from the map
    leafletProxy("reclassified_map", session) %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearPopups()
    
    output$raster_plot <- renderPlot({
      plot(reclassified_r(), xlab = "x coordinate (m)", ylab = "y coordinate (m)")
      lines(AtoB)
    })
    
    # Save the shortest path as a shapefile
    observeEvent(input$save_path, {
      if (!is.null(input$save_path) && input$save_path != "") {
        st_write(AtoB, dsn = paste0(input$save_path, ".shp"))
      }
    })
  })
  
  output$original_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -48.9660322, lat = -22.8864848, zoom = 10) %>%
      addTiles() %>%
      addRasterImage(rst, colors = land_use_colors) %>%
      addLegend(colors = land_use_colors, labels = names(land_use_colors),
                title = "Original Land Use", position = "bottomright")
  })
}

# Run the application
shinyApp(ui = ui, server = server)