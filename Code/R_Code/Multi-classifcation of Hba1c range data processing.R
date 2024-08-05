## Creating a data set for the classifications of Hba1c ranges


# Load required libraries
library(dplyr)
library(lubridate)

# Function to determine 6-month interval index
get_interval_index <- function(start_date, date) {
  return(floor(as.numeric(difftime(date, start_date, units = "days")) / (365.25 / 2)) + 1)
}

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
      bmi_interval_data <- bmi_data %>% filter(date >= start_dates[i] & date <= end_dates[i])
      hba1c_interval_data <- hba1c_data %>% filter(date >= start_dates[i] & date <= end_dates[i])
      sbp_interval_data <- sbp_data %>% filter(date >= start_dates[i] & date <= end_dates[i])
      
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



##Create the labels for each individual before we do any padding or imputation:
#Labels must be real data values taken at visits rather than rough estimates or imputations (some label smay be average of multiple vlaues within the final Hba1c bin though)


# Function to generate labels based on the last recorded HbA1c value
generate_labels <- function(formatted_data, hba1c_data) {
  labels <- list()
  data_matrices <- list()
  
  for (id in names(formatted_data)) {
    individual_matrix <- formatted_data[[id]]
    
    # Find the last recorded HbA1c value
    individual_hba1c_data <- hba1c_Data %>% filter(id == !!as.numeric(id))
    last_hba1c_value <- tail(individual_hba1c_data$value, n = 1)
    last_hba1c_date <- tail(individual_hba1c_data$date, n = 1)
    
    if (length(last_hba1c_value) == 0 || length(last_hba1c_date) == 0) {
      next  # Skip if no HbA1c data is available
    }
    
    # Find the interval that includes the last HbA1c date
    start_date <- min(as.Date(individual_hba1c_data$date), na.rm = TRUE)
    intervals <- seq(start_date, max(as.Date(individual_hba1c_data$date), na.rm = TRUE), by = "6 months")
    interval_indices <- seq_along(intervals)
    last_interval_index <- max(which(intervals <= last_hba1c_date))
    
    # Extract the matrix up to the last interval (excluding it)
    if (last_interval_index > 1) {
      data_matrix <- individual_matrix[, 1:(last_interval_index - 1)]
      labels[[id]] <- last_hba1c_value
      data_matrices[[id]] <- data_matrix
    }
  }
  
  return(list(data_matrices = data_matrices, labels = labels))
}


# Generate labels
labels_output <- generate_labels(formatted_data, hba1c_Data)

# Extract data matrices and labels
data_matrices <- labels_output$data_matrices
labels <- labels_output$labels



#Converting label into categorical classes based on the NHS criteria for Hba1c levels found here: https://www.southtees.nhs.uk/resources/the-hba1c-test/

# Function to convert HbA1c values to categorical labels
convert_to_categories <- function(hba1c_value) {
  if (hba1c_value < 48) {
    return(1)  # Below 42: Normal value – person without diabetes / Risk of hypoglycaemia (in diabetes)
  } else if (hba1c_value >= 48 && hba1c_value <= 53) {
    return(2)  # 42-53: Ideal diabetes control
  } else if (hba1c_value > 53 && hba1c_value <= 58) {
    return(3)  # 54-58: Acceptable diabetes control
  } else if (hba1c_value > 58 && hba1c_value <= 75) {
    return(4)  # 59-75: Need to discuss your current management
  } else if (hba1c_value > 75) {
    return(5)  # Above 75: High – increased risk of complications
  } else {
    return(NA)  # In case of invalid values
  }
}

# Convert the current labels object into categories
categorical_labels <- lapply(labels, convert_to_categories)

# Print the categorical labels to verify
print(categorical_labels)



#First we need to normalize all the data to achieve standardized comparable variables for nearest neighbors and DL, we normalize independently across variables


#remove any people with less than 3 rows in their data (aka, no recordings for certain variables at all)

# Filter out matrices that do not have exactly three rows
filtered_data_matrices <- data_matrices[unlist(lapply(data_matrices, function(x) is.matrix(x) && nrow(x) == 3))]

# Combine all data into a single matrix for global calculation
combined_matrix <- do.call(cbind, lapply(filtered_data_matrices, as.matrix))

# Calculate global means and standard deviations for each variable (row)
global_means <- apply(combined_matrix, 1, mean, na.rm = TRUE)
global_sds <- apply(combined_matrix, 1, sd, na.rm = TRUE)

# Function to normalize using global statistics
normalize_individual <- function(matrix, global_means, global_sds) {
  matrix[1, ] <- (matrix[1, ] - global_means[1]) / global_sds[1]
  matrix[2, ] <- (matrix[2, ] - global_means[2]) / global_sds[2]
  matrix[3, ] <- (matrix[3, ] - global_means[3]) / global_sds[3]
  return(matrix)
}

# Initialize an empty list to store normalized data
data_matrices_normalized <- list()

# Loop through each individual's data matrix to normalize
for (id in names(filtered_data_matrices)) {
  individual_matrix <- filtered_data_matrices[[id]]
  data_matrices_normalized[[id]] <- normalize_individual(individual_matrix, global_means, global_sds)
}

# Print the normalized data matrices for verification
print(data_matrices_normalized)



#Create mask for DL model later

# Function to create a mask matrix
create_mask <- function(matrix) {
  mask <- ifelse(is.na(matrix), 0, 1)
  return(mask)
}

# Initialize lists to store masks and normalized data
data_matrices_masks <- list()
data_matrices_normalized_with_masks <- list()

# Normalize individual matrices and create masks
for (id in names(filtered_data_matrices)) {
  individual_matrix <- filtered_data_matrices[[id]]
  normalized_matrix <- normalize_individual(individual_matrix, global_means, global_sds)
  
  # Create mask matrix
  mask_matrix <- create_mask(individual_matrix)
  
  # Store the normalized matrix and its mask
  data_matrices_normalized_with_masks[[id]] <- list(
    normalized_matrix = normalized_matrix,
    mask_matrix = mask_matrix
  )
}

# Print the normalized data matrices with masks for verification
print(data_matrices_normalized_with_masks)











##Imputing missing values within each individuals data matrix using Dynamic Time Warping (https://www.theaidream.com/post/dynamic-time-warping-dtw-algorithm-in-time-series#:~:text=In%20time%20series%20analysis%2C%20Dynamic,similar%20elements%20between%20time%20series.)

#Step 1 is to concatenate the rows of each individual into one long vector (excluding NA values), this will later be used to compare profiles
set.seed(123)

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



# Perform KNN imputation on all filtered data matrices using the precomputed distance matrix
k <- 10
data_matrices_imputed <- list()

# Get all individual IDs
individual_ids <- names(filtered_data_matrices)

# Process each individual
for (idx in seq_along(individual_ids)) {
  id <- individual_ids[idx]
  
  if (idx %% 25 == 0) {
    cat("Processing individual number:", idx, "\n")
  }
  
  individual_matrix <- filtered_data_matrices[[id]]$normalized_matrix
  
  imputed_matrix <- knn_impute_dtw(individual_matrix, k, distance_matrix, filtered_data_matrices, idx)
  data_matrices_imputed[[id]] <- list(
    imputed_matrix = imputed_matrix,
    mask_matrix = filtered_data_matrices[[id]]$mask_matrix
  )
}

# Print completion message
cat("Imputation completed for all individuals.\n")





#Check for any 0 values or NA values in any of the matrices
# Function to check for 0 or NA values in the imputed data matrices
check_imputed_values <- function(imputed_data) {
  any_problematic_zero_values <- FALSE
  any_na_values <- FALSE
  
  for (id in names(imputed_data)) {
    imputed_matrix <- imputed_data[[id]]$imputed_matrix
    
    if (any(imputed_matrix == 0, na.rm = TRUE)) {
      cat("Matrix for individual", id, "contains 0 values.\n")
      any_problematic_zero_values <- TRUE
    }
    
    if (any(is.na(imputed_matrix))) {
      cat("Matrix for individual", id, "contains NA values.\n")
      any_na_values <- TRUE
    }
  }
  
  if (!any_problematic_zero_values) {
    cat("No problematic 0 values found in any imputed matrix.\n")
  }
  
  if (!any_na_values) {
    cat("No NA values found in any imputed matrix.\n")
  }
}

# Perform the check on the imputed data matrices
check_imputed_values(data_matrices_imputed)




##Prepare the files for export into python to continue with the GRU Model with multi-classification. 

#We have the object data_matrices_imputed and we have the labels but we need to filter the labels to only keep those present in the imputed data


# Filter the labels to keep only those in the imputed data.
filtered_categorical_labels <- categorical_labels[names(categorical_labels) %in% names(data_matrices_imputed)]


# Directory to save the files
dir.create("imputed_data")

# Function to save matrices and labels to CSV files
save_to_csv <- function(imputed_data, labels) {
  for (id in names(imputed_data)) {
    
    # Save the imputed matrix
    imputed_matrix <- imputed_data[[id]]$imputed_matrix
    write.csv(imputed_matrix, file = paste0("imputed_data/imputed_matrix_", id, ".csv"), row.names = FALSE)
    
    # Save the mask matrix
    mask_matrix <- imputed_data[[id]]$mask_matrix
    write.csv(mask_matrix, file = paste0("imputed_data/mask_matrix_", id, ".csv"), row.names = FALSE)
    
    # Save the label
    label <- labels[id]
    write.csv(data.frame(label = label), file = paste0("imputed_data/label_", id, ".csv"), row.names = FALSE)
  }
}

# Save the filtered imputed data, mask matrices, and labels to CSV files
save_to_csv(data_matrices_imputed, filtered_categorical_labels)

cat("Data successfully saved to CSV files in the 'imputed_data' directory.\n")




##Visualize the number of different classes represented in our data

label_plot <- unlist(filtered_categorical_labels)

# Create a table of counts for each label
label_plot <- table(label_plot)

# Convert the table to a data frame for plotting
label_counts_df <- as.data.frame(label_plot)
colnames(label_counts_df) <- c("Label", "Count")

# Plot the number of individuals in each label class
ggplot(label_counts_df, aes(x = Label, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Individuals in Each Label Class",
       x = "Label Class",
       y = "Number of Individuals") +
  theme(plot.title = element_text(hjust = 0.5))

# Print the label counts for verification
print(label_counts_df)





