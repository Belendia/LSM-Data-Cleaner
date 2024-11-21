library(geosphere)

# saves the dataframe to a csv file
write_dataset_to_csv <- function (df, file_name) {
  write.csv(df, file=file_name, row.names=TRUE)
}

# Function to calculate distance between two coordinates (Haversine formula)
calculate_distance <- function(lat1, long1, lat2, long2) {
  distVincentySphere(cbind(long1, lat1), cbind(long2, lat2))

}

# Calculate distances between all households and store in a matrix
compute_distance_matrix <- function(df, total_households = nrow(df)) {
  dist_matrix <- matrix(NA, nrow = total_households, ncol = total_households)
  
  # Set row and column names to X_uuid for easy reference
  rownames(dist_matrix) <- df$X_uuid
  colnames(dist_matrix) <- df$X_uuid
  
  # Calculate pairwise distances and store them in the matrix
  for (i in 1:total_households) {
    for (j in i:total_households) {
      dist_matrix[i, j] <- calculate_distance(df$X_Capture_GPS_latitude[i], df$X_Capture_GPS_longitude[i], 
                                              df$X_Capture_GPS_latitude[j], df$X_Capture_GPS_longitude[j])
      # Since distance matrix is symmetric, copy the value to the mirrored element
      dist_matrix[j, i] <- dist_matrix[i, j]
    }
  }
  
  return(dist_matrix)
}


# Function to check and reassign Kebele, Transect and Clustered for mis-categorized households
update_categories <- function(household_data, distance_matrix) {
  
  # Initialize columns for outliers and corrected kebeles
  households_data <- household_data %>%
    mutate(Is_Kebele_Outlier = FALSE, Corrected_Kebele = Kebele,
           Is_Transect_Outlier = FALSE, Corrected_Transect = Transect,
           Is_Cluster_Outlier = FALSE, Corrected_Cluster = Cluster)
  
  # Correct the Kebele column based on neighbors
  for (i in 1:nrow(households_data)) {
    # Get the UUID of the current household
    current_uuid <- households_data$X_uuid[i]
    
    # Find the indices of neighbors within the distance threshold (excluding the current household itself)
    neighbor_indices <- order(distance_matrix[current_uuid, ])[2:6]
    
    # Extract neighboring Kebeles (using X_uuid to reference the row)
    neighboring_kebeles <- households_data$Corrected_Kebele[neighbor_indices]
    
    # Determine the most common Kebele among neighbors
    most_common_kebele <- names(sort(table(neighboring_kebeles), decreasing = TRUE))[1]
    
    # If the current household's Kebele is not the same as the most common Kebele
    if (!is.na(most_common_kebele) && households_data$Kebele[i] != most_common_kebele) {
      households_data$Is_Kebele_Outlier[i] <- TRUE
      households_data$Corrected_Kebele[i] <- most_common_kebele
    }
    
    # Extract neighboring Transect (using X_uuid to reference the row)
    neighboring_transect <- households_data$Corrected_Transect[neighbor_indices]
    
    # Determine the most common Transect among neighbors
    most_common_transect <- names(sort(table(neighboring_transect), decreasing = TRUE))[1]
    
    # If the current household's Transect is not the same as the most common Kebele
    if (!is.na(most_common_transect) && households_data$Transect[i] != most_common_transect) {
      households_data$Is_Transect_Outlier[i] <- TRUE
      households_data$Corrected_Transect[i] <- most_common_transect
    }
    
    # Extract neighboring Cluster (using X_uuid to reference the row)
    neighboring_cluster <- households_data$Corrected_Cluster[neighbor_indices]
    
    # Determine the most common Cluster among neighbors
    neighboring_cluster <- names(sort(table(neighboring_cluster), decreasing = TRUE))[1]
    
    # If the current household's Cluster is not the same as the most common Kebele
    if (!is.na(neighboring_cluster) && households_data$Cluster[i] != neighboring_cluster) {
      households_data$Is_Cluster_Outlier[i] <- TRUE
      households_data$Corrected_Cluster[i] <- neighboring_cluster
    }
  }
  
  households_data$Corrected_Cluster <- as.integer(households_data$Corrected_Cluster)
  
  return(households_data)
}

plot_kebele_map <- function(households_data, kebele_boundary) {
  
  kebele_palette <- colorFactor(c("red", "blue", "green", "orange", "purple", "brown"), domain = households_data$Kebele)
  
  leaflet(households_data) %>%
    addTiles() %>%
    addPolygons(
      data = kebele_boundary,  # The GeoJSON or shapefile data for the Kebele boundary
      fill = FALSE,            
      color = "black",        
      weight = 2,              
      opacity = 1,
      popup = ~paste("Kebele:", Kebele)
    ) %>%
    addCircleMarkers(
      lng = ~X_Capture_GPS_longitude,
      lat = ~X_Capture_GPS_latitude,
      color = ~kebele_palette(Kebele),
      radius = 7,               
      stroke = FALSE,
      fillOpacity = 0.6,
      popup = ~paste(
        "Kebele:", Kebele, "<br>",
        "Transect:", Transect, "<br>",
        "Cluster:", Cluster, "<br>",
        "Type:", type, "<br>",
        "Is Kebele Outlier:", Is_Kebele_Outlier, "<br>",
        "Corrected Kebele:", Corrected_Kebele, "<br>",
        "ID:", unique_id
      ),
    ) %>%
    setView(lng = mean(households_data$X_Capture_GPS_longitude), lat = mean(households_data$X_Capture_GPS_latitude), zoom = 12) %>%
    addLegend("bottomright",
              colors = kebele_palette(unique(households_data$Kebele)),
              labels = unique(households_data$Kebele),
              title = "Kebele") 
}

plot_map_with_filter <- function(households_data) {
  kebele_palette <- colorFactor(c("red", "blue", "darkgreen", "purple"), domain = households_data$Corrected_Transect)
  dash_styles <- c("1", "5,5", "10,5", "10,10", "1,10")
  radii <- c(7, 8, 9, 10, 11)
  opacities <- c(0.3, 0.4, 0.5, 0.6, 0.7)
  
  # Initialize the Leaflet map
  map <- leaflet(households_data) %>%
    addTiles()
  
  # Add CircleMarkers for each Corrected Kebele as a separate group
  for (kebele in unique(households_data$Corrected_Kebele)) {
    kebele_data <- households_data %>% filter(Corrected_Kebele == kebele)
    
    map <- map %>%
      addCircleMarkers(
        data = kebele_data,
        lng = ~X_Capture_GPS_longitude,
        lat = ~X_Capture_GPS_latitude,
        color = ~kebele_palette(Corrected_Transect),
        radius = ~radii[Corrected_Cluster],
        stroke = TRUE,
        weight = 2,
        fillOpacity =  ~opacities[Corrected_Cluster],
        dashArray = ~dash_styles[Corrected_Cluster],
        popup = ~paste(
          "Kebele:", Kebele, "<br>",
          "Transect:", Transect, "<br>",
          "Cluster:", Cluster, "<br>",
          "Type:", type, "<br>",
          "Is Kebele Outlier:", Is_Kebele_Outlier, "<br>",
          "Corrected Kebele:", Corrected_Kebele, "<br>",
          "Is Transect Outlier:", Is_Transect_Outlier, "<br>",
          "Corrected Transect:", Corrected_Transect, "<br>",
          "Is Cluster Outlier:", Is_Cluster_Outlier, "<br>",
          "Corrected Cluster:", Corrected_Cluster, "<br>",
          "ODK ID:", X_uuid, "<br>",
          "ID:", unique_id
        ),
        group = kebele # Group by Corrected_Kebele for filtering
      )
  }
  
  # Add Layers Control
  map <- map %>%
    addLayersControl(
      overlayGroups = unique(households_data$Corrected_Kebele), # Filter by Corrected_Kebele
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Add Legend
  map <- map %>%
    addLegend(
      "bottomright",
      pal = kebele_palette,
      values = ~Corrected_Transect,
      title = "Transect",
      opacity = 1
    )
  
  return(map)
}

