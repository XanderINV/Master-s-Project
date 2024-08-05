# Load required libraries
library(dplyr)
library(lubridate)
library(dtw)

# Function to process the raw data and create the matrix for each individual
process_data <- function(bmi_df, hba1c_df, sbp_df) {
  # Convert date columns to Date class
  bmi_df$date <- as.Date(bmi_df$date)
  hba1c_df$date <- as.Date(hba1c_df$date)
  sbp_df$date <- as.Date(sbp_df$date)
  
  # Get unique IDs
  unique_ids <- unique(c(bmi_df$id, hba1c_df$id, sbp_df$id))
  
  # Initialize the final dataset list
  formatted_data <- list()
  
  for (id in unique_ids) {
    # Subset the data for the current individual
    bmi_data <- bmi_df %>% filter(id == !!id)
    hba1c_data <- hba1c_df %>% filter(id == !!id)
    sbp_data <- sbp_df %>% filter(id == !!id)
    
    # Determine the earliest and latest dates for the individual
    all_dates <- c(bmi_data$date, hba1c_data$date, sbp_data$date)
    start_date <- min(all_dates, na.rm = TRUE)
    end_date <- max(all_dates, na.rm = TRUE)
    
    # Create the intervals
    start_dates <- seq(start_date, end_date, by = "6 months")
    end_dates <- start_dates + months(6) - days(1)
    
    # Determine the number of intervals
    num_intervals <- length(start_dates)
    
    # Initialize the matrix for the individual (3 rows: BMI, HbA1c, SBP)
    individual_matrix <- matrix(NA, nrow = 3, ncol = num_intervals)
    
    for (i in seq_len(num_intervals)) {
      # Subset data within the current interval for each variable
      bmi_interval_data <- bmi_df %>% filter(date >= start_dates[i] & date <= end_dates[i])
      hba1c_interval_data <- hba1c_df %>% filter(date >= start_dates[i] & date <= end_dates[i])
      sbp_interval_data <- sbp_df %>% filter(date >= start_dates[i] & date <= end_dates[i])
      
      # Calculate the average value within the interval for each variable
      if (nrow(bmi_interval_data) > 0) {
        individual_matrix[1, i] <- mean(bmi_interval_data$value, na.rm = TRUE)
      }
      if (nrow(hba1c_interval_data) > 0) {
        individual_matrix[2, i] <- mean(hba1c_interval_data$value, na.rm = TRUE)
      }
      if (nrow(sbp_interval_data) > 0) {
        individual_matrix[3, i] <- mean(sbp_interval_data$value, na.rm = TRUE)
      }
    }
    
    # Add the formatted data for this individual to the list
    formatted_data[[as.character(id)]] <- individual_matrix
  }
  
  return(formatted_data)
}


# Process the data
formatted_data <- process_data(BMI_Data, hba1c_Data, SBP_Data)

# Remove individuals with fewer than three rows in their matrix
formatted_data <- formatted_data[sapply(formatted_data, nrow) == 3]

# Function to filter data up to and including the final non-NA HbA1c value
filter_up_to_final_non_na_hba1c <- function(formatted_data) {
  filtered_data <- list()
  
  for (id in names(formatted_data)) {
    individual_matrix <- formatted_data[[id]]
    last_non_na_index <- max(which(!is.na(individual_matrix[2, ])))
    
    if (!is.na(last_non_na_index) && last_non_na_index >= 1) {
      data_matrix <- individual_matrix[, 1:last_non_na_index]
      filtered_data[[id]] <- data_matrix
    }
  }
  
  return(filtered_data)
}

# Filter the formatted data to include only up to and including the final recorded HbA1c value
filtered_formatted_data <- filter_up_to_final_non_na_hba1c(formatted_data)



# Function to check if an individual has enough data
has_enough_data <- function(matrix, hba1c_data, id) {
  # Check if individual has at least three HbA1c observations
  hba1c_observations <- hba1c_data %>% filter(id == !!as.numeric(id))
  if (nrow(hba1c_observations) < 3) {
    return(FALSE)
  }
  
  return(TRUE)
}

# Filter out individuals with fewer than three HbA1c observations
final_filtered_data <- list()
for (id in names(filtered_formatted_data)) {
  individual_matrix <- filtered_formatted_data[[id]]
  if (has_enough_data(individual_matrix, hba1c_Data, id)) {
    final_filtered_data[[id]] <- individual_matrix
  }
}



# Function to generate proportional change labels using three different strategies
generate_proportional_change_labels <- function(formatted_data, hba1c_data, method = "last_available") {
  proportional_change_labels <- list()
  
  for (id in names(formatted_data)) {
    # Get the HbA1c data for the current individual
    individual_hba1c_data <- hba1c_data %>% filter(id == !!as.numeric(id)) %>% arrange(date)
    
    if (nrow(individual_hba1c_data) < 2) {
      next  # Skip if less than two HbA1c values are available
    }
    
    # Get the last two recorded HbA1c values
    last_hba1c_value <- tail(individual_hba1c_data$value, n = 1)
    second_last_hba1c_value <- tail(individual_hba1c_data$value, n = 2)[1]
    
    # Apply the chosen method to handle missing second last value
    if (is.na(second_last_hba1c_value)) {
      if (method == "last_available") {
        # Use the last available value before the final visit
        second_last_hba1c_value <- tail(individual_hba1c_data$value[!is.na(individual_hba1c_data$value)], n = 2)[1]
      } else if (method == "mean_imputation") {
        # Impute missing second last value with the mean of other HbA1c values for the individual
        second_last_hba1c_value <- mean(individual_hba1c_data$value, na.rm = TRUE)
      } else if (method == "exclude_incomplete") {
        # Skip individuals with missing second last value
        next
      }
    }
    
    # If there's still no valid second last value, skip this individual
    if (is.na(second_last_hba1c_value)) {
      next
    }
    
    # Calculate the percentage change
    percentage_change <- ((last_hba1c_value - second_last_hba1c_value) / second_last_hba1c_value) * 100
    
    # Store the percentage change label
    proportional_change_labels[[id]] <- percentage_change
  }
  
  return(proportional_change_labels)
}



# Example of generating proportional change labels with different methods
proportional_change_labels_last_available <- generate_proportional_change_labels(final_filtered_data, hba1c_Data, method = "last_available")
proportional_change_labels_mean_imputation <- generate_proportional_change_labels(final_filtered_data, hba1c_Data, method = "mean_imputation")
proportional_change_labels_exclude_incomplete <- generate_proportional_change_labels(final_filtered_data, hba1c_Data, method = "exclude_incomplete")


# Directory to save the files
dir.create("imputed_data_proportional_change", showWarnings = FALSE)

# Function to save labels to CSV files
save_labels_to_csv <- function(labels, dir) {
  for (id in names(labels)) {
    label <- labels[[id]]
    write.csv(data.frame(label = label), file = paste0(dir, "/label_", id, ".csv"), row.names = FALSE)
  }
}

# Save the filtered labels to CSV files
save_labels_to_csv(proportional_change_labels_last_available, "imputed_data_proportional_change")





# Remove final interval from the data and mask matrices, then normalize the matrices

# Function to remove the final interval from the data and mask matrices
remove_final_interval <- function(data_matrices) {
  for (id in names(data_matrices)) {
    data_matrices[[id]] <- data_matrices[[id]][, -ncol(data_matrices[[id]])]
  }
  return(data_matrices)
}

# Remove the final interval from the filtered formatted data
filtered_formatted_data_final <- remove_final_interval(final_filtered_data)

# Calculate global means and standard deviations for each variable (row)
combined_matrix <- do.call(cbind, lapply(filtered_formatted_data_final, as.matrix))
global_means <- apply(combined_matrix, 1, mean, na.rm = TRUE)
global_sds <- apply(combined_matrix, 1, sd, na.rm = TRUE)

# Function to check the structure of the matrices
check_matrix_structure <- function(matrix_list) {
  for (id in names(matrix_list)) {
    individual_matrix <- matrix_list[[id]]
    cat("ID:", id, "\n")
    print(dim(individual_matrix))
  }
}

# Inspect the structure of the filtered matrices
check_matrix_structure(filtered_formatted_data_final)


# Filter out individuals with matrices that do not have exactly 3 rows
filtered_formatted_data_final <- Filter(function(matrix) is.matrix(matrix) && nrow(matrix) == 3, filtered_formatted_data_final)


# Function to normalize the data matrices
normalize_individual <- function(matrix, global_means, global_sds) {
  if (is.matrix(matrix) && nrow(matrix) == 3) {
    matrix[1, ] <- (matrix[1, ] - global_means[1]) / global_sds[1]
    matrix[2, ] <- (matrix[2, ] - global_means[2]) / global_sds[2]
    matrix[3, ] <- (matrix[3, ] - global_means[3]) / global_sds[3]
  }
  return(matrix)
}

# Normalize the data matrices
formatted_data_normalized <- list()
for (id in names(filtered_formatted_data_final)) {
  individual_matrix <- filtered_formatted_data_final[[id]]
  if (is.matrix(individual_matrix) && nrow(individual_matrix) == 3) {
    formatted_data_normalized[[id]] <- normalize_individual(individual_matrix, global_means, global_sds)
  } else {
    cat("Skipping ID due to incorrect dimensions:", id, "\n")
  }
}

# Print the normalized data for verification
print(formatted_data_normalized)

# Generate masks for the normalized matrices
generate_mask <- function(matrix) {
  mask <- ifelse(is.na(matrix), 0, 1)
  return(mask)
}

# Initialize an empty list to store both data matrices and masks
formatted_data_with_masks <- list()
for (id in names(formatted_data_normalized)) {
  individual_matrix <- formatted_data_normalized[[id]]
  mask_matrix <- generate_mask(individual_matrix)
  
  formatted_data_with_masks[[id]] <- list(
    data_matrix = individual_matrix,
    mask_matrix = mask_matrix
  )
}

# Print the combined data and masks for verification
print(formatted_data_with_masks)

# Function to perform KNN imputation with the median of each nearest neighbor for the specified variable
knn_impute_dtw <- function(matrix, k, distance_matrix, original_matrices, individual_index) {
  distances <- distance_matrix[individual_index, ]
  nearest_neighbors <- order(distances, na.last = NA)[1:k]  # Handle NA distances
  
  for (i in 1:nrow(matrix)) {
    for (j in 1:ncol(matrix)) {
      if (is.na(matrix[i, j])) {
        neighbor_medians <- sapply(nearest_neighbors, function(n) {
          neighbor_matrix <- original_matrices[[n]]$data_matrix
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


# Since we are using precomputed distance matrix we need to filter to only keep individuals who have previously been used for distances (object: filtered_data_matrices)
individual_ids <- names(filtered_data_matrices)

# Filter formatted_data_with_masks to only include these individuals
formatted_data_with_masks <- formatted_data_with_masks[individual_ids]

# Print the filtered formatted data with masks for verification
print(formatted_data_with_masks)

# Function to filter labels based on individuals present in filtered_data_matrices
filter_labels <- function(labels, valid_ids) {
  return(labels[valid_ids])
}

# Filter each set of labels using the valid individual IDs
filtered_proportional_change_labels_last_available <- filter_labels(proportional_change_labels_last_available, individual_ids)
filtered_proportional_change_labels_mean_imputation <- filter_labels(proportional_change_labels_mean_imputation, individual_ids)
filtered_proportional_change_labels_exclude_incomplete <- filter_labels(proportional_change_labels_exclude_incomplete, individual_ids)

# Print the filtered labels for verification
print(filtered_proportional_change_labels_last_available)
print(filtered_proportional_change_labels_mean_imputation)
print(filtered_proportional_change_labels_exclude_incomplete)

# Now proceed with the imputation
# Perform KNN imputation on all formatted data matrices using the precomputed distance matrix
k <- 10
data_matrices_imputed_fixed <- list()

# Process each individual
for (idx in seq_along(individual_ids)) {
  id <- individual_ids[idx]
  
  if (idx %% 25 == 0) {
    cat("Processing individual number:", idx, "\n")
  }
  
  individual_matrix <- formatted_data_with_masks[[id]]$data_matrix
  
  imputed_matrix <- knn_impute_dtw(individual_matrix, k, distance_matrix, formatted_data_with_masks, idx)
  data_matrices_imputed_fixed[[id]] <- list(
    imputed_matrix = imputed_matrix,
    mask_matrix = formatted_data_with_masks[[id]]$mask_matrix
  )
}

# Print completion message
cat("Imputation completed for all individuals.\n")

# Print the imputed data for verification
print(data_matrices_imputed_fixed)

# Ensure that the main directory and subdirectories are created
dir.create("imputed_data_proportional_change", showWarnings = FALSE)
dir.create("imputed_data_proportional_change/last_available", showWarnings = FALSE)
dir.create("imputed_data_proportional_change/mean_imputation", showWarnings = FALSE)
dir.create("imputed_data_proportional_change/exclude_incomplete", showWarnings = FALSE)

# Save the proportional change labels to CSV files
save_labels_to_csv <- function(labels, dir) {
  for (id in names(labels)) {
    label <- labels[[id]]
    write.csv(data.frame(label = label), file = paste0(dir, "/label_", id, ".csv"), row.names = FALSE)
  }
}

# Save the proportional change labels using different methods
save_labels_to_csv(filtered_proportional_change_labels_last_available, "imputed_data_proportional_change/last_available")
save_labels_to_csv(filtered_proportional_change_labels_mean_imputation, "imputed_data_proportional_change/mean_imputation")
save_labels_to_csv(filtered_proportional_change_labels_exclude_incomplete, "imputed_data_proportional_change/exclude_incomplete")

# Function to save data matrices to CSV files
save_data_matrices_to_csv <- function(data_matrices, dir) {
  for (id in names(data_matrices)) {
    data_matrix <- data_matrices[[id]]$imputed_matrix
    write.csv(data_matrix, file = paste0(dir, "/data_matrix_", id, ".csv"), row.names = FALSE)
  }
}


# Save the imputed data matrices and mask matrices to CSV files
save_data_matrices_to_csv(data_matrices_imputed_fixed, "imputed_data_proportional_change")

cat("Proportional change labels, imputed data matrices, and mask matrices successfully saved to CSV files in the 'imputed_data_proportional_change' directory.\n")

# Function to generate a histogram of the proportional changes
generate_histogram <- function(proportional_change_labels, title) {
  proportional_changes <- unlist(proportional_change_labels)
  hist(proportional_changes, 
       breaks = 50, 
       main = title, 
       xlab = "Proportional Change", 
       col = "steelblue", 
       border = "white")
  
  # Print a summary of the proportional change values for verification
  print(summary(proportional_changes))
}

# Generate histograms for the different methods
generate_histogram(filtered_proportional_change_labels_last_available, "Distribution of Proportional Changes (Last Available)")
generate_histogram(filtered_proportional_change_labels_mean_imputation, "Distribution of Proportional Changes (Mean Imputation)")
generate_histogram(filtered_proportional_change_labels_exclude_incomplete, "Distribution of Proportional Changes (Exclude Incomplete)")


