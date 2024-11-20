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

# This function iterates through pairs of duplicate households and updates a column to link each household 
# to its duplicate. For each pair, it assigns the row number of the second household to the first and 
# vice versa. If a household already has duplicate references, it appends the new reference to the existing 
# ones as a comma-separated string.

link_duplicate_household_rows <- function(df, total_households) {
  # Create an empty vector to store the row number of the duplicate pair
  duplicate_column <- rep(NA, total_households)
  
  for (i in 1:nrow(df)) {
    hh1 <- df[i, 1]
    hh2 <- df[i, 2]
    
    # Assign the duplicate pair row number to the new column (for both rows)
    duplicate_column[hh1] <- ifelse(is.na(duplicate_column[hh1]), hh2, paste(duplicate_column[hh1], hh2, sep = ","))
    duplicate_column[hh2] <- ifelse(is.na(duplicate_column[hh2]), hh1, paste(duplicate_column[hh2], hh1, sep = ","))
  }
  
  return(duplicate_column)
}


# # Function to find outliers in household clusters
# find_outliers <- function(households_data) {
#   households_data %>%
#     group_by(Kebele) %>%
#     mutate(
#       # Calculate the centroid for each Kebele
#       centroid_longitude = mean(X_Capture_GPS_longitude, na.rm = TRUE),
#       centroid_latitude = mean(X_Capture_GPS_latitude, na.rm = TRUE),
#       
#       # Calculate the distance of each household from the centroid (in meters)
#       distance_from_centroid = distGeo(
#         cbind(X_Capture_GPS_longitude, X_Capture_GPS_latitude),
#         cbind(centroid_longitude, centroid_latitude)
#       )
#     ) %>%
#     ungroup() %>%
#     group_by(Kebele) %>%
#     mutate(
#       # Identify outliers using the IQR method
#       Q1 = quantile(distance_from_centroid, 0.20, na.rm = TRUE),
#       Q3 = quantile(distance_from_centroid, 0.80, na.rm = TRUE),
#       IQR = Q3 - Q1,
#       lower_bound = Q1 - 1.5 * IQR,
#       upper_bound = Q3 + 1.5 * IQR,
#       is_outlier = distance_from_centroid < lower_bound | distance_from_centroid > upper_bound
#     ) %>%
#     ungroup() 
# }

# # Function to find outliers based on convex hull intersection
# find_outliers_by_intersection <- function(households_data) {
#   # Convert data to sf (Simple Features) for spatial operations
#   households_sf <- households_data %>%
#     st_as_sf(coords = c("X_Capture_GPS_longitude", "X_Capture_GPS_latitude"), crs = 4326)
#   
#   # Compute convex hull for each Kebele
#   convex_hulls <- households_sf %>%
#     group_by(Kebele) %>%
#     summarise(geometry = st_convex_hull(st_union(geometry))) %>%
#     ungroup()
#   
#   # Find intersections between convex hulls of neighboring Kebeles
#   intersections <- st_intersects(convex_hulls, convex_hulls, sparse = FALSE)
#   intersection_points <- list()
#   
#   for (i in 1:nrow(intersections)) {
#     for (j in 1:ncol(intersections)) {
#       if (i < j && intersections[i, j]) {
#         intersect_geom <- st_intersection(convex_hulls[i, ], convex_hulls[j, ])
#         intersection_points[[paste(i, j, sep = "_")]] <- intersect_geom
#       }
#     }
#   }
#   
#   # Combine intersection points into a single sf object
#   intersection_points_sf <- do.call(rbind, intersection_points) %>%
#     st_as_sf() %>%
#     st_cast("POINT")
#   
#   # Identify households lying on intersection points
#   households_data$is_outlier <- st_intersects(households_sf, intersection_points_sf, sparse = FALSE) %>%
#     rowSums() > 0
#   
#   households_data
# }


# Function to check and reassign Kebele for miscategorized households
correct_kebele <- function(household_data, distance_matrix) {
  
  # Initialize columns for outliers and corrected kebeles
  households_data <- household_data %>%
    mutate(Outlier_Kebele = FALSE, Corrected_Kebele = Kebele,
           Outlier_Transect = FALSE, Corrected_Transect = Transect,
           Outlier_Cluster = FALSE, Corrected_Cluster = Cluster)
  
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
      households_data$Outlier_Kebele[i] <- TRUE
      households_data$Corrected_Kebele[i] <- most_common_kebele
    }
  }
  
  return(households_data)
}

correct_transect <- function(households_data, distance_matrix) {
  
  # Correct the Kebele column based on neighbors
  for (i in 1:nrow(households_data)) {
    # Get the UUID of the current household
    current_uuid <- households_data$X_uuid[i]
    
    # Find the indices of neighbors within the distance threshold (excluding the current household itself)
    neighbor_indices <- order(distance_matrix[current_uuid, ])[2:6]
    
    # Extract neighboring Transect (using X_uuid to reference the row)
    neighboring_transect <- households_data$Corrected_Transect[neighbor_indices]
    
    # Determine the most common Transect among neighbors
    most_common_transect <- names(sort(table(neighboring_transect), decreasing = TRUE))[1]
    
    # If the current household's Transect is not the same as the most common Kebele
    if (!is.na(most_common_transect) && households_data$Transect[i] != most_common_transect) {
      households_data$Outlier_Transect[i] <- TRUE
      households_data$Corrected_Transect[i] <- most_common_transect
    }
  }
  
  return(households_data)
}

correct_cluster <- function(households_data, distance_matrix) {
  
  # Correct the Kebele column based on neighbors
  for (i in 1:nrow(households_data)) {
    # Get the UUID of the current household
    current_uuid <- households_data$X_uuid[i]
    
    # Find the indices of neighbors within the distance threshold (excluding the current household itself)
    neighbor_indices <- order(distance_matrix[current_uuid, ])[2:6]
    
    # Extract neighboring Cluster (using X_uuid to reference the row)
    neighboring_cluster <- households_data$Corrected_Cluster[neighbor_indices]
    
    # Determine the most common Cluster among neighbors
    neighboring_cluster <- names(sort(table(neighboring_cluster), decreasing = TRUE))[1]
    
    # If the current household's Cluster is not the same as the most common Kebele
    if (!is.na(neighboring_cluster) && households_data$Cluster[i] != neighboring_cluster) {
      households_data$Outlier_Cluster[i] <- TRUE
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
      fill = FALSE,            # Don't fill the polygon, just show the border
      color = "black",         # Border color
      weight = 2,              # Border weight (thickness)
      opacity = 1,
      # highlightOptions = highlightOptions(
      #   weight = 5,
      #   color = "red",
      #   bringToFront = TRUE
      # ),# Border opacity
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
        "Outlier Kebele:", Outlier_Kebele, "<br>",
        "Corrected Kebele:", Corrected_Kebele, "<br>",
        "ID:", unique_id
      ),# Customize popup as needed
      #clusterOptions = markerClusterOptions()
    ) %>%
    setView(lng = mean(households_data$X_Capture_GPS_longitude), lat = mean(households_data$X_Capture_GPS_latitude), zoom = 12) %>%
    addLegend("bottomright",
              colors = kebele_palette(unique(households_data$Kebele)),
              labels = unique(households_data$Kebele),
              title = "Kebele") 
}

plot_map_with_filter <- function(households_data) {
  kebele_palette <- colorFactor(c("red", "blue", "green", "purple"), domain = households_data$Corrected_Transect)
  dash_styles <- c("1", "5,5", "10,5", "10,10", "1,10")
  radii <- c(7, 8, 9, 10, 11)
  opacities <- c(0.3, 0.4, 0.5, 0.6, 0.7)
  
  # Initialize the Leaflet map
  map <- leaflet(households_data) %>%
    addTiles()
  
  # Add CircleMarkers for each Corrected_Kebele as a separate group
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
          "Outlier Kebele:", Outlier_Kebele, "<br>",
          "Corrected Kebele:", Corrected_Kebele, "<br>",
          "Outlier Transect:", Outlier_Transect, "<br>",
          "Corrected Transect:", Corrected_Transect, "<br>",
          "Outlier Cluster:", Outlier_Cluster, "<br>",
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

