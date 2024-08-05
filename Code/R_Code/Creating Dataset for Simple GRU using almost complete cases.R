
## Theory: If we use individuals who have almost completed data from at least x number of visits we can then use the entire data set to impute the remaining missing values
## Then we use these complete individuals as a data sets to test a simple GRU model with more accurate data than just the left shifted data.

# Define the threshold for the maximum number of NA values allowed
X <- 10  # Change this to your desired threshold
updater <- 0
# Function to count the number of NA values in a matrix
count_na <- function(matrix) {
  sum(is.na(matrix))
}

# Filter individuals with fewer than X NA values
filtered_individuals <- names(time_series_only)[sapply(time_series_only, count_na) < X]

# Subset the data for these filtered individuals
filtered_data <- time_series_only[filtered_individuals]

# Create a new object for imputed data
filtered_imputed_data <- filtered_data

# Iterate over each individual in filtered data for imputation
for (individual_id in names(filtered_data)) {
  print(updater)
  updater <- updater + 1
  # Extract time series data and mask for the current individual
  time_series <- filtered_data[[individual_id]]
  mask <- mask_list[[individual_id]]$mask
  timestamps <- dates_list[[individual_id]]
  
  # Iterate over each variable
  for (var_index in 1:3) {
    # Iterate over each date
    for (date_index in 1:length(timestamps)) {
      # Check if the value is missing
      if (is.na(time_series[var_index, date_index])) {
        # Get the index of the other variable that is NA
        other_var_index <- which(mask[, date_index] == 0)
        other_var_index_1 <- other_var_index[other_var_index != var_index]
        
        # Check if other_var_index_1 is empty
        if (length(other_var_index_1) == 0) {
          # Both other variables are present, store them in a pair
          pair <- time_series[-var_index, date_index]
        } else {
          # Find the nearest actual value for the missing variable
          nearest_actual_value <- find_nearest_actual_value(individual_id, var_index, other_var_index_1, timestamps[date_index], time_series)
          
          # Create a pair with the other variable data and the nearest actual value
          pair <- time_series[-var_index, date_index]
          pair[is.na(pair)] <- nearest_actual_value
        }
        
        # Initialize a list to store the lowest differences for each individual
        lowest_values <- list()
        
        # Continue with finding k-nearest neighbors and imputing missing value
        for (other_individual_id in names(time_series_only)) {
          if (other_individual_id != individual_id) {
            # Extract time series data of the other individual for the current variable
            other_time_series <- time_series_only[[other_individual_id]][-var_index, , drop = FALSE]
            
            abs_differences <- abs(pair - other_time_series)
            
            # Find the index of the timestamp for which the lowest value occurred
            timestamp_index <- 0
            lowest_sum <- Inf
            
            # Iterate over each column
            for (i in 1:ncol(abs_differences)) {
              # Check if both rows have non-NA values in the column
              if (!any(is.na(abs_differences[, i]))) {
                # Calculate the column sum
                col_sum <- sum(abs_differences[, i])
                # Check if it's the lowest sum so far
                if (col_sum < lowest_sum) {
                  lowest_sum <- col_sum
                  timestamp_index <- i
                }
              }
            }
            
            # Get the original timestamp associated with the lowest value
            lowest_timestamp <- timestamp_index
            lowest_timestamps[[other_individual_id]] <- lowest_timestamp
            
            # Find columns without NA values
            non_na_columns <- apply(abs_differences, 2, function(col) !any(is.na(col)))
            
            # Filter out columns with NA values
            abs_difference_filtered <- abs_differences[, non_na_columns, drop = FALSE]
            
            # Check if there are any non-NA columns
            if (any(non_na_columns)) {
              lowest_value <- min(colSums(abs_difference_filtered))
              
              # Store the lowest value and its corresponding individual ID
              lowest_values[[other_individual_id]] <- lowest_value
            }
          }
        }
        nearest_neighbors <- list()
        
        # Sort the lowest values and get the K-nearest neighbors' IDs
        sorted_values <- sort(unlist(lowest_values))
        
        # Get the IDs of the lowest 5 values
        if (length(sorted_values) >= 10) {
          lowest_5_values <- names(sorted_values)[1:10]
        } else {
          lowest_5_values <- names(sorted_values)
        }
        
        nearest_neighbors[[individual_id]] <- lowest_5_values
        
        available_values <- list()
        
        # Iterate over the K-nearest neighbors
        for (neighbor_id in nearest_neighbors[[individual_id]]) {
          # Get the data for the missing variable at the timestamp
          missing_value <- time_series_only[[neighbor_id]][var_index, lowest_timestamps[[neighbor_id]]]
          
          # Check if missing_value is not empty
          if (length(missing_value) > 0) {
            available_values[[neighbor_id]] <- missing_value
          } else {
            # Find the nearest actual value for the missing variable
            nearest_actual_value <- find_nearest_value_for_imputation(time_series_only[[neighbor_id]], var_index, lowest_timestamps[[neighbor_id]])
            available_values[[neighbor_id]] <- nearest_actual_value
          }
        }
        
        available_values_vector <- unlist(available_values)
        mean_value <- mean(available_values_vector, na.rm = TRUE)
        
        filtered_imputed_data[[individual_id]][var_index, date_index] <- mean_value
        # Once the K-nearest neighbors are processed, move to the next missing value
      }
    }
  }
}

# Defined functions

find_nearest_actual_value <- function(individual_id, var_index, other_var_index_1, date, time_series) {
  # Extract the time series data for the other variable
  other_var_data <- time_series[other_var_index_1, ]
  
  # Find the index of the current date
  date_index <- which(timestamps == date)
  
  # Initialize variables to track nearest values
  left_index <- date_index - 1
  right_index <- date_index + 1
  
  # Search left and right for the nearest actual value
  while (left_index >= 1 || right_index <= length(timestamps)) {
    # Check left index
    if (left_index >= 1 && !is.na(other_var_data[left_index])) {
      nearest_actual_value <- other_var_data[left_index]
      break
    }
    
    # Check right index
    if (right_index <= length(timestamps) && !is.na(other_var_data[right_index])) {
      nearest_actual_value <- other_var_data[right_index]
      break
    }
    
    # Move to the next indices
    left_index <- left_index - 1
    right_index <- right_index + 1
  }
  return(nearest_actual_value)
}

find_nearest_value_for_imputation <- function(neighbor_data, var_index, timestamp_index) {
  # Extract the time series data for the other variable
  other_var_data <- neighbor_data[var_index, ]
  
  # Initialize variables to track nearest value
  left_index <- timestamp_index - 1
  right_index <- timestamp_index + 1
  nearest_value <- NA
  
  timestamps_neighbor <- dates_list[[neighbor_id]]
  
  # Search left and right for the nearest actual value
  while (left_index >= 1 || right_index <= length(timestamps_neighbor)) {
    # Check left index
    if (left_index >= 1 && !is.na(other_var_data[left_index])) {
      nearest_actual_value <- other_var_data[left_index]
      break
    }
    
    # Check right index
    if (right_index <= length(timestamps_neighbor) && !is.na(other_var_data[right_index])) {
      nearest_actual_value <- other_var_data[right_index]
      break
    }
    
    # Move to the next indices
    left_index <- left_index - 1
    right_index <- right_index + 1
  }
  return(nearest_actual_value)
}




#Adding a label column for whether the final vists was an increase in cholesterol or decrease from the second last (1 corresponds to increase, 0 to a decrease)

View(complete_cases_imputed_data)

# Pre-process: Remove individuals with fewer than 4 visits
complete_cases_imputed_data_over_4 <- complete_cases_imputed_data %>%
  group_by(id) %>%
  filter(n() >= 4) %>%
  ungroup()

# Function to determine the HBA1c change
get_hba1c_change <- function(data) {
  data <- data %>% arrange(desc(row_number()))
  last_hba1c <- data$Hba1c[1]
  second_last_hba1c <- data$Hba1c[2]
  
  if (last_hba1c > second_last_hba1c) {
    return(1)
  } else {
    return(0)
  }
}

# Create the new DataFrame with labels
label_df <- complete_cases_imputed_data_over_4 %>%
  group_by(id) %>%
  summarize(Label = get_hba1c_change(cur_data()))

# Display the new DataFrame
print(label_df)


write.csv(complete_cases_imputed_data_over_4, "categorical_label_simulated_data.csv", row.names = FALSE)
write.csv(label_df, "categorica_labels.csv", row.names = FALSE)













