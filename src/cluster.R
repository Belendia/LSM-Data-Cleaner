# Load necessary libraries
library(dplyr)
library(leaflet)
library(sf)
library(htmltools)
source('libs/utils.R')

# Load the dataset
# Replace "image.png" with the correct dataset file path
household_data <- read.csv("data/household.csv") # Replace with actual path if not CSV

# Remove rows with missing GPS coordinates
household_data <- household_data %>%
  filter(!is.na(X_Capture_GPS_latitude) & !is.na(X_Capture_GPS_longitude)) %>%
  filter(X_Capture_GPS_precision<=100)

# Calculate the distance matrix
distance_matrix <- compute_distance_matrix(household_data)


#kebele_boundaries <- st_read(dsn="shapefiles", layer="Ethiopia_AdminBoundaries")
kebele_boundaries <- st_read("shapefiles/Ethiopia_Kebeles_Boundaries.geojson")

diredawa_boundary <- kebele_boundaries %>%
                    filter(R_NAME=="Dredewa" & nchar(RK_CODE)==9) %>%
                    mutate(Kebele = paste0("K",substr(RK_CODE, nchar(RK_CODE) - 1, nchar(RK_CODE)))) %>%
                    filter(Kebele %in% c("K03", "K05", "K06", "K07", "K08", "K09"))

# Fix Kebele and Map
kebele_corrected_data <- correct_kebele(household_data, distance_matrix)
plot_kebele_map(kebele_corrected_data, kebele_boundary = diredawa_boundary)

# Fix Transect and Map
# Before correcting transect
plot_map_with_filter(kebele_corrected_data)
transect_corrected_data <- correct_transect(kebele_corrected_data, distance_matrix)
# After correcting transect
plot_map_with_filter(transect_corrected_data)

# Fix Cluster and Map
cluster_corrected_data <- correct_cluster(transect_corrected_data, distance_matrix)
plot_map_with_filter(cluster_corrected_data)













write.csv(household_data, "data/household_with_groups.csv", row.names = FALSE)



