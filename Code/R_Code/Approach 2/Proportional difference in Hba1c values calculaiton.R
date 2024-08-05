# Load required libraries
library(dplyr)

# Function to generate proportional change labels based on the last two recorded HbA1c values
generate_proportional_change_labels <- function(labels_output, hba1c_data) {
  data_matrices <- labels_output$data_matrices
  original_labels <- labels_output$labels
  proportional_change_labels <- list()
  
  for (id in names(original_labels)) {
    # Find the HbA1c data for the current individual
    individual_hba1c_data <- hba1c_data %>% filter(id == !!as.numeric(id))
    
    if (nrow(individual_hba1c_data) < 2) {
      next  # Skip if less than two HbA1c values are available
    }
    
    # Get the last two recorded HbA1c values
    last_hba1c_value <- tail(individual_hba1c_data$value, n = 1)
    second_last_hba1c_value <- tail(individual_hba1c_data$value, n = 2)[1]
    
    # Calculate the proportional change
    proportional_change <- (last_hba1c_value - second_last_hba1c_value) / second_last_hba1c_value
    print(proportional_change)
    
    # Store the proportional change label
    proportional_change_labels[[id]] <- proportional_change
  }
  
  return(proportional_change_labels)
}

# Use the labels_output and hba1c_Data to generate the proportional change labels
proportional_change_labels <- generate_proportional_change_labels(labels_output, hba1c_Data)

# Filter the proportional change labels to keep only those in the imputed data
filtered_proportional_change_labels <- proportional_change_labels[names(proportional_change_labels) %in% names(data_matrices_imputed)]

# Directory to save the files
dir.create("imputed_data_proportional_change")

# Function to save labels to CSV files
save_labels_to_csv <- function(labels) {
  for (id in names(labels)) {
    label <- labels[[id]]
    write.csv(data.frame(label = label), file = paste0("imputed_data_proportional_change/label_", id, ".csv"), row.names = FALSE)
  }
}

# Save the filtered labels to CSV files
save_labels_to_csv(filtered_proportional_change_labels)

cat("Proportional change labels successfully saved to CSV files in the 'imputed_data_proportional_change' directory.\n")

# Generate a histogram of the proportional changes
proportional_changes <- unlist(filtered_proportional_change_labels)
hist(proportional_changes, 
     breaks = 50, 
     main = "Distribution of Proportional Changes in HbA1c", 
     xlab = "Proportional Change", 
     col = "steelblue", 
     border = "white")

# Print a summary of the proportional change values for verification
summary(proportional_changes)

