# Function to create a spaghetti plot for a specific individual
create_spaghetti_plot <- function(individual_id, k_nearest_set_to_5, time_series_only, master_data_format) {
  # Extract the imputed data and original data for the individual
  imputed_data <- k_nearest_set_to_5[[individual_id]]
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
individual_id <- names(k_nearest_set_to_5)[1]
spaghetti_plot <- create_spaghetti_plot(individual_id, k_nearest_set_to_5, time_series_only, master_data_format)
print(spaghetti_plot)




#250
k_nearest_set_to_250 <- new_imputed_data[1]
# Function to create a spaghetti plot for a specific individual
create_spaghetti_plot <- function(individual_id, k_nearest_set_to_250, time_series_only, master_data_format) {
  # Extract the imputed data and original data for the individual
  imputed_data <- k_nearest_set_to_250[[individual_id]]
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
individual_id <- names(k_nearest_set_to_250)[1]
spaghetti_plot <- create_spaghetti_plot(individual_id, k_nearest_set_to_250, time_series_only, master_data_format)
print(spaghetti_plot)


#10
k_nearest_set_to_10 <- new_imputed_data[1]
# Function to create a spaghetti plot for a specific individual
create_spaghetti_plot <- function(individual_id, k_nearest_set_to_10, time_series_only, master_data_format) {
  # Extract the imputed data and original data for the individual
  imputed_data <- k_nearest_set_to_10[[individual_id]]
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
individual_id <- names(k_nearest_set_to_10)[1]
spaghetti_plot <- create_spaghetti_plot(individual_id, k_nearest_set_to_10, time_series_only, master_data_format)
print(spaghetti_plot)
