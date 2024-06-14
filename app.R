packages <- c("shiny", "leaflet", "raster", "DT", "shinyWidgets", "sf", "leafem", "mapview", "gdistance", "dplyr")

# Install packages if they are not already installed
try(install.packages(packages[!packages %in% installed.packages()]))

# Load packages 
try(invisible(lapply(packages, library, character.only = TRUE)))

# Define UI
ui <- fluidPage(
  titlePanel("Land Use Reclassification"),
  sidebarLayout(
    sidebarPanel(
      h4("Reclassification Table"),
      DTOutput("landuse_table"),
      actionButton("apply_changes", "Apply Changes"),
      actionButton("execute_shortest_path", "Execute Shortest Path")
    ),
    mainPanel(
      h4("Map"),
      leafletOutput("map"),
      plotOutput("raster_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Load the raster file directly from GitHub
  rst <- raster("/vsicurl/https://raw.githubusercontent.com/hyugoshirai/LeastCostPath/main/BD_LeastCostTool/landuse_simplified.tif")
  
  # Read the shapefile directly from GitHub
  origin_sf <- st_read("/vsicurl/https://raw.githubusercontent.com/hyugoshirai/LeastCostPath/main/BD_LeastCostTool/origin.shp")
  goal_sf <- st_read("/vsicurl/https://raw.githubusercontent.com/hyugoshirai/LeastCostPath/main/BD_LeastCostTool/goal.shp")
  
  # Extract coordinates of the first point
  origin_cd <- st_coordinates(origin_sf)[1, 1:2]
  goal_cd <- st_coordinates(goal_sf)[1, 1:2]
  
  # Define land use labels and colors
  land_use_labels <- c("Forest", "Non Forest Natural Formation", "Farming", "Non vegetated area", "Water", "Forest Plantation")
  land_use_colors <- c("Forest" = "#32a65e", 
                       "Non Forest Natural Formation" = "#02d659", 
                       "Farming" = "#FFFFB2", 
                       "Non vegetated area" = "#d4271e", 
                       "Water" = "#0000FF", 
                       "Forest Plantation" = "#7a5900")
  
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
      }
    }
    
    reclassified_values <- values(rst)
    for (i in 1:nrow(landuse_data)) {
      reclassified_values[values(rst) == landuse_data$raster_value[i]] <- landuse_data$new_value[i]
    }
    reclassified_r_temp <- rst
    values(reclassified_r_temp) <- reclassified_values
    
    reclassified_r(reclassified_r_temp)
    
    output$map <- renderLeaflet({
      color_palette <- colorNumeric(
        palette = c("#FF0000", "#FFFF00", "#00FFFF", "#0000FF"),
        domain = values(reclassified_r())
      )
      leaflet() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        setView(lng = -48.9660322, lat = -22.8864848, zoom = 10) %>%
        addRasterImage(rst, colors = land_use_colors, group = "Original Raster") %>%
        addRasterImage(reclassified_r(), colors = color_palette, group = "Reclassified Raster") %>%
        addLegend(pal = color_palette, values = values(reclassified_r()),
                  title = "Reclassified Land Use", position = "bottomright", group = "Reclassified Raster") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite"),
          overlayGroups = c("Original Raster", "Reclassified Raster"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
  })
  
  observeEvent(input$execute_shortest_path, {
    tr <- transition(x = reclassified_r(), transitionFunction = mean, directions = 8)
    AtoB <- shortestPath(x = tr, origin = origin_cd, goal = goal_cd, output = "SpatialLines")
    
    # Convert to sf object
    AtoB_sf <- st_as_sf(AtoB)
    
    output$raster_plot <- renderPlot({
      plot(rst, xlab = "x coordinate (m)", ylab = "y coordinate (m)", col = land_use_colors)
      lines(AtoB)
    })
    
    # # Use shinyFiles to let user navigate to save path
    # shinyFileSave(input, "save_path", roots = c(wd = '.'), session = session)
    # 
    # observeEvent(input$save_path, {
    #   req(input$save_path)
    #   save_path <- parseSavePath(roots = c(wd = '.'), input$save_path)
    #   if (nrow(save_path) > 0) {
    #     st_write(AtoB_sf, dsn = save_path$datapath, driver = "ESRI Shapefile")
    #   }
    # })
    
    leafletProxy("map") %>%
      addPolylines(data = AtoB_sf, color = "blue", weight = 2, group = "Shortest Path") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        overlayGroups = c("Original Raster", "Reclassified Raster", "Shortest Path"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      setView(lng = -48.9660322, lat = -22.8864848, zoom = 10) %>%
      addRasterImage(rst, colors = land_use_colors, group = "Original Raster") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        overlayGroups = c("Original Raster"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
