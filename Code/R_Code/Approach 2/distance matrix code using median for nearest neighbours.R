# Load required library
library(dtw)

# Function to combine data into one profile for each individual
combine_profile <- function(matrix) {
  return(as.vector(t(matrix)))
}

# Function to check if an individual is valid
is_valid_individual <- function(matrix) {
  has_enough_data <- apply(matrix, 1, function(row) sum(!is.na(row)) >= 3)
  no_all_na_variables <- !apply(matrix, 1, function(row) all(is.na(row)))
  return(all(has_enough_data) & all(no_all_na_variables))
}

# Filter valid individuals
filtered_data_matrices <- Filter(function(individual) is_valid_individual(individual$normalized_matrix), data_matrices_normalized_with_masks)

# Combine profiles for all valid individuals
combined_profiles <- lapply(filtered_data_matrices, function(x) combine_profile(x$normalized_matrix))

# Function to calculate DTW distance between two profiles
calculate_dtw_distance <- function(profile1, profile2) {
  profile1_clean <- profile1[!is.na(profile1)]
  profile2_clean <- profile2[!is.na(profile2)]
  
  if (length(profile1_clean) > 0 && length(profile2_clean) > 0) {
    alignment <- dtw(profile1_clean, profile2_clean, keep.internals = TRUE)
    return(alignment$distance)
  } else {
    return(Inf)
  }
}

# Compute pairwise DTW distances and store them in a matrix
num_individuals <- length(combined_profiles)
distance_matrix <- matrix(0, nrow = num_individuals, ncol = num_individuals)

for (i in 1:(num_individuals - 1)) {  # Loop up to the second-to-last individual
  for (j in (i + 1):num_individuals) {  # Loop from the next individual to the last
    distance <- calculate_dtw_distance(combined_profiles[[i]], combined_profiles[[j]])
    distance_matrix[i, j] <- distance
    distance_matrix[j, i] <- distance  # Leverage symmetry
  }
  if (i %% 25 == 0) {
    cat("Computed distances for individual:", i, "\n")
  }
}

# KNN imputation function with median of each nearest neighbor for the specified variable
knn_impute_dtw <- function(matrix, k, distance_matrix, original_matrices, individual_index) {
  distances <- distance_matrix[individual_index, ]
  nearest_neighbors <- order(distances, na.last = NA)[1:k]  # Handle NA distances
  
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (is.na(matrix[i, j])) {
        neighbor_medians <- sapply(nearest_neighbors, function(n) {
          neighbor_matrix <- original_matrices[[n]]$normalized_matrix
          row_values <- neighbor_matrix[i, ]
          row_median <- median(row_values, na.rm = TRUE)
          return(row_median)
        })
        
        # Check for valid neighbor medians before calculating the final mean of medians
        valid_neighbor_medians <- neighbor_medians[!is.na(neighbor_medians)]
        
        if (length(valid_neighbor_medians) > 0) {
          matrix[i, j] <- mean(valid_neighbor_medians)
        } else {
          # Fallback mechanism: use the mean of the variable across all profiles or a constant
          matrix[i, j] <- 0  # Change this as needed
        }
      }
    }
  }
  return(matrix)
}


