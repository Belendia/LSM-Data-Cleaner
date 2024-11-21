library(tidyverse)
library(leaflet)

source('libs/utils.R')


households_df <- read_csv("data/household.csv")

# Filter households that have a GPS coordinate during enumeration
hhs_with_gps_df <- households_df %>% 
  filter(!is.na(`_Capture_GPS_longitude`))

# Number of households to check for duplication. For testing purposes, you can replace 
# this with a constant value to focus on a smaller set of households and verify if the algorithm 
# works correctly.
households_to_check <- nrow(hhs_with_gps_df)

# Calculate the distance between each household
dist_matrix <- compute_distance_matrix(hhs_with_gps_df, households_to_check)

# Set the diagonal to NA (We don’t need the distance of a household to itself. It will always be 0)
diag(dist_matrix) <- NA

# Find the rows where distance <= 5 meters
duplicate_households <- which(dist_matrix <= 5, arr.ind = TRUE)

# For each pair of duplicates, assign the row number of the duplicate to the corresponding rows
hhs_with_gps_df$duplicate_by_distance <- link_duplicate_household_rows(duplicate_households, households_to_check)

# Save the processed data frame
write_dataset_to_csv(hhs_with_gps_df, 'data/processed.csv')


# Map
color_scale <- distinct(hhs_with_gps_df$unique_id)
leaflet(df) %>%
  addTiles() %>%
  # Add markers for each household
  addCircleMarkers(
    lng = df$`_Capture_GPS_longitude`, lat = df$`_Capture_GPS_latitude`,
    color = ~factor(df$color_group, levels = color_scale),
    radius = 5, 
    popup = ~paste("ID: ", df$id, "<br>TV: ", df$has_tv, "<br>Car: ", df$has_car, "<br>Radio: ", df$has_radio),
    fillOpacity = 0.7,
    stroke = FALSE
  )

