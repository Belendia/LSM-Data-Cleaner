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
compute_distance_matrix <- function(df, row = nrow(df), col=nrow(df)) {
  dist_matrix <- matrix(NA, nrow = row, ncol = col)
  
  for (i in 1:row) {
    for (j in i:col) {
      dist_matrix[i, j] <- calculate_distance(df$`_Capture_GPS_latitude`[i], df$`_Capture_GPS_longitude`[i], 
                                              df$`_Capture_GPS_latitude`[j], df$`_Capture_GPS_longitude`[j])
    }
  }
  return (dist_matrix)
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
