library(stringdist)

# Identify similar location names in which the households are enrolled
unique_names <- unique(household_ds$Unique_name_of_local_household_is_located)
dist_matrix <- stringdistmatrix(unique_names, unique_names, method = "jw")  # Jaro-Winkler distance

threshold <- 0.2
similar_groups <- list()

for (i in 1:nrow(dist_matrix)) {
  for (j in i:ncol(dist_matrix)) {
    if (dist_matrix[i, j] < threshold) {
      # If names are similar, add them to the same group
      similar_groups[[length(similar_groups) + 1]] <- c(unique_names[i], unique_names[j])
    }
  }
}

# Remove duplicate groups (if any)
similar_groups <- unique(similar_groups)

# Create a mapping of unique names to standard names
# We assign the first name of each group to be the standard name
name_mapping <- unlist(lapply(similar_groups, function(group) {
  setNames(rep(group[1], length(group)), group)
}), use.names = TRUE)

# Add a new column with the standardized names
household_ds$Standardized_unique_name <- name_mapping[household_ds$Unique_name_of_local_household_is_located]



writeDatasetToCSV(household_ds, "processed.csv")