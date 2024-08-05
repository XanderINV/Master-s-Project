
new_imputed_data <- time_series_only
lowest_timestamps <- list()




#Code works, below is it for one individiual full iteration:
# Specify the individual ID you want to test
test_individual_id <- "2147483663"


# Counter to track the number of processed individuals
processed_count <- 0
max_individuals <- 1

# Iterate over each individual
for (individual_id in names(time_series_only)) {
  # Break the loop if the maximum number of individuals is reached
  if (processed_count >= max_individuals) {
    break
  }
    # Extract time series data and mask for the current individual
    time_series <- time_series_only[[individual_id]]
    mask <- mask_list[[individual_id]]$mask
    timestamps <- dates_list[[individual_id]]
    print(processed_count)
    print(individual_id)
    
    # Iterate over each variable
    for (var_index in 1:3) {
      # Iterate over each date
      for (date_index in 1:length(timestamps)) {
        # Check if the value is missing
        if (is.na(time_series[var_index, date_index])) {
          # Get the index of the other variable that is NA
          other_var_index <- which(mask[, date_index] == 0)
          other_var_index_1 <-
            other_var_index[other_var_index != var_index]
          
          # Check if other_var_index_1 is empty
          if (length(other_var_index_1) == 0) {
            # Both other variables are present, store them in a pair
            pair <- time_series[-var_index, date_index]
          } else {
            # Find the nearest actual value for the missing variable
            nearest_actual_value <-
              find_nearest_actual_value(individual_id,
                                        var_index,
                                        other_var_index_1,
                                        timestamps[date_index],
                                        time_series)
            
            # Create a pair with the other variable data and the nearest actual value
            pair <- time_series[-var_index, date_index]
            pair[is.na(pair)] <- nearest_actual_value
          }
          
          # Initialize a list to store the lowest differences for each individual
          lowest_values <- list()
          
          # Continue with finding k-nearest neighbors and imputing missing value
          # Continue with finding absolute differences and imputing missing value
          # Iterate over each individual to calculate absolute differences
          for (other_individual_id in names(time_series_only)) {
            if (other_individual_id != individual_id) {
              # Exclude the current individual
              # Extract time series data of the other individual for the current variable
              other_time_series <-
                time_series_only[[other_individual_id]][-var_index, , drop = FALSE]
              
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
              lowest_timestamp <-
                timestamp_index  # Just for demonstration since we don't have actual timestamps
              
              lowest_timestamps[[other_individual_id]] <-
                lowest_timestamp
              
              # Find columns without NA values
              non_na_columns <-
                apply(abs_differences, 2, function(col)  #element-wise absolute differences between the pairs of values.
                  ! any(is.na(col)))
              
              # Filter out columns with NA values
              abs_difference_filtered <-
                abs_differences[, non_na_columns, drop = FALSE]
              
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
          
          # Get the IDs of the lowest 20 values
          if (length(sorted_values) >= 20) {
            lowest_20_values <- names(sorted_values)[1:20]
          } else {
            lowest_20_values <- names(sorted_values)
          }
          
          nearest_neighbors[[individual_id]] <- lowest_20_values
          
          available_values <- list()
          
          # Iterate over the K-nearest neighbors
          for (neighbor_id in nearest_neighbors[[individual_id]]) {
            # Get the data for the missing variable at the timestamp
            missing_value <-
              time_series_only[[neighbor_id]][var_index, lowest_timestamps[[neighbor_id]]]
            
            # Check if missing_value is not empty
            if (length(missing_value) > 0) {
              # Store the missing value directly
              available_values[[neighbor_id]] <- missing_value
            } else {
              # Find the nearest actual value for the missing variable
              nearest_actual_value <-
                find_nearest_value_for_imputation(time_series_only[[neighbor_id]],
                                                  var_index,
                                                  lowest_timestamp[[neighbor_id]])
              
              # Store the nearest actual value
              available_values[[neighbor_id]] <-
                nearest_actual_value
            }
          }
          
          available_values_vector <- unlist(available_values)
          mean_value <- mean(available_values_vector, na.rm = TRUE)
          
          new_imputed_data[[individual_id]][var_index, date_index] <-
            mean_value
          # Once the K-nearest neighbors are processed, move to the next missing value
          
        }
        else {
        }
      }
    }
   # Increment the counter after processing an individual
   processed_count <- processed_count + 1
}



#DEFINED FUNCTIONS:

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



#Function to get the nearest value for the nearest neghbour if value is NA at the timestamp.

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




# Define function to calculate mean value
calculate_mean_value <- function(available_values) {
  available_values_vector <- unlist(available_values)
  mean_value <- mean(available_values_vector, na.rm = TRUE)
  return(mean_value)
}

########################################################################


# Subset the first 42 elements in the list of individuals
python_imputed_data <- new_imputed_data[1:42]   #Imputed using K set to 20


k_nearest_set_to_5 <- new_imputed_data[1:10]  # K set to 5, run on the first 10 individuals

#Spaghetti plots to compare pre-imputation to imputed values

# Function to create a spaghetti plot for a specific individual
create_spaghetti_plot <- function(individual_id, python_imputed_data, time_series_only, master_data_format) {
  # Extract the imputed data and original data for the individual
  imputed_data <- python_imputed_data[[individual_id]]
  original_data <- time_series_only[[individual_id]]
  timestamps <- master_data_format[[individual_id]]$timestamps
  
  # Create a data frame for plotting
  data <- data.frame(
    timestamp = rep(timestamps, each = 3),
    variable = factor(rep(c("BMI", "Hba1c", "SBP"), times = length(timestamps))),
    original_value = c(original_data),
    value = c(imputed_data),
    is_imputed = c(is.na(original_data))
  )
  
  # Create the plot
  plot <- ggplot(data, aes(x = timestamp, y = value, group = variable, color = variable)) +
    geom_line() +
    geom_point(aes(shape = as.factor(is_imputed)), size = 3) +
    scale_shape_manual(values = c(16, 4), labels = c("Actual", "Imputed")) +
    labs(title = paste("Spaghetti Plot for Individual", individual_id),
         x = "Time",
         y = "Values",
         shape = "Data Type") +
    theme_minimal()
  
  return(plot)
}

# Example usage for the first individual in the python_imputed_data list
individual_id <- names(python_imputed_data)[3]
spaghetti_plot <- create_spaghetti_plot(individual_id, python_imputed_data, time_series_only, master_data_format)
print(spaghetti_plot)







# Function to create a spaghetti plot for a specific individual
create_spaghetti_plot <- function(individual_id, python_imputed_data, time_series_only, master_data_format) {
  # Extract the imputed data and original data for the individual
  imputed_data <- python_imputed_data[[individual_id]]
  original_data <- time_series_only[[individual_id]]
  timestamps <- master_data_format[[individual_id]]$timestamps
  
  # Create a data frame for plotting
  data <- data.frame(
    timestamp = rep(timestamps, each = 3),
    variable = factor(rep(c("BMI", "Hba1c", "SBP"), times = length(timestamps))),
    original_value = c(original_data),
    value = c(imputed_data),
    is_imputed = c(is.na(original_data))
  )
  
  # Create the plot
  plot <- ggplot(data, aes(x = timestamp, y = value, group = variable, color = variable)) +
    geom_point(aes(shape = as.factor(is_imputed)), size = 3) +
    scale_shape_manual(values = c(16, 4), labels = c("Actual", "Imputed")) +
    geom_smooth(data = data %>% filter(!is_imputed), method = "loess", se = FALSE, linetype = "solid") +
    labs(title = paste("Spaghetti Plot for Individual", individual_id),
         x = "Time",
         y = "Values",
         shape = "Data Type") +
    theme_minimal()
  
  return(plot)
}

# Example usage for the first individual in the python_imputed_data list
individual_id <- names(python_imputed_data)[20]
spaghetti_plot <- create_spaghetti_plot(individual_id, python_imputed_data, time_series_only, master_data_format)
print(spaghetti_plot)






