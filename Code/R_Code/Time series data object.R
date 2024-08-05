#Call Libraries


#Before we can run the data formatting we need to pre process
#Remove individuals without data for all three variables.




#Convert current data into a matrix for each individual with a mask and Time stamps and Time intervals

#Data Matrix for each individual:

# Create a function to generate time series for each individual
generate_time_series <- function(individual_id) {
  # Extract data for BMI, HbA1c, and SBP for the individual
  bmi_data <- subset(BMI_Data, id == individual_id)
  hba1c_data <- subset(hba1c_Data, id == individual_id)
  sbp_data <- subset(SBP_Data, id == individual_id)
  
  # Get unique dates across all variables
  all_dates <- sort(unique(c(bmi_data$date, hba1c_data$date, sbp_data$date)))
  
  # Create empty time series matrix
  time_series <- matrix(NA, nrow = 3, ncol = length(all_dates))
  
  # Populate time series with actual values
  for (i in seq_along(all_dates)) {
    date <- all_dates[i]
    bmi_value <- bmi_data$value[bmi_data$date == date]
    hba1c_value <- hba1c_data$value[hba1c_data$date == date]
    sbp_value <- sbp_data$value[sbp_data$date == date]
    time_series[1, i] <- ifelse(length(bmi_value) > 0, bmi_value, NA)
    time_series[2, i] <- ifelse(length(hba1c_value) > 0, hba1c_value, NA)
    time_series[3, i] <- ifelse(length(sbp_value) > 0, sbp_value, NA)
  }
  
  # Return the time series matrix
  return(time_series)
}

# Create a list to store time series for each individual
time_series_list <- list()
time_series_only <- list()

for (individual_id in unique(BMI_Data$id)) {
  time_series_only[[as.character(individual_id)]] <- generate_time_series(individual_id)
}

# Iterate over unique individual IDs and generate time series
for (individual_id in unique(BMI_Data$id)) {
  time_series_list[[as.character(individual_id)]] <- generate_time_series(individual_id)
}

# View the first individual's time series
print(time_series_list[[1]])




#Create the mask for each mask for each individual.

# Create a function to generate masking matrix for each individual
generate_mask <- function(individual_id) {
  # Extract data for BMI, HbA1c, and SBP for the individual
  bmi_data <- subset(BMI_Data, id == individual_id)
  hba1c_data <- subset(hba1c_Data, id == individual_id)
  sbp_data <- subset(SBP_Data, id == individual_id)
  
  # Get unique dates across all variables for this individual
  all_dates <- sort(unique(c(bmi_data$date, hba1c_data$date, sbp_data$date)))
  
  # Create empty masking matrix
  mask <- matrix(1, nrow = 3, ncol = length(all_dates))
  
  # Update masking matrix based on available data
  for (i in seq_along(all_dates)) {
    date <- all_dates[i]
    bmi_available <- date %in% bmi_data$date
    hba1c_available <- date %in% hba1c_data$date
    sbp_available <- date %in% sbp_data$date
    mask[1, i] <- ifelse(bmi_available, 1, 0)
    mask[2, i] <- ifelse(hba1c_available, 1, 0)
    mask[3, i] <- ifelse(sbp_available, 1, 0)
  }
  
  # Return a list with named elements
  return(list(time_series = NULL, mask = mask))
}

# Create a list to store masking matrices for each individual
mask_list <- list()

# Iterate over unique individual IDs and generate masking matrices
for (individual_id in unique(BMI_Data$id)) {
  mask_list[[as.character(individual_id)]] <- generate_mask(individual_id)
}

# Store masking matrices in each individual's list
for (individual_id in unique(BMI_Data$id)) {
  time_series_list[[as.character(individual_id)]]$mask <- mask_list[[as.character(individual_id)]]
}

# View the mask for the first individual
print(time_series_list[[1]]$mask)

#NOTE: remove the first object in each mask, only need the mask not the empty time series thingy, just creates confusion


#Make the individual Time stamps vector and add it to each individuals list:

# Create a function to generate actual dates for each individual with timestamps starting from 0
generate_dates <- function(individual_id) {
  # Extract data for BMI, HbA1c, and SBP for the individual
  bmi_data <- subset(BMI_Data, id == individual_id)
  hba1c_data <- subset(hba1c_Data, id == individual_id)
  sbp_data <- subset(SBP_Data, id == individual_id)
  
  # Get unique dates across all variables
  all_dates <- sort(unique(c(bmi_data$date, hba1c_data$date, sbp_data$date)))
  
  # Get the start date (earliest date)
  start_date <- min(all_dates)
  
  # Create a sequence of dates starting from the first date
  timestamp <- as.numeric(difftime(all_dates, start_date, units = "days"))
  
  # Return the vector of actual dates with timestamps starting from 0
  return(timestamp)
}

# Create a list to store actual dates for each individual
dates_list <- list()

# Iterate over unique individual IDs and generate actual dates
for (individual_id in unique(BMI_Data$id)) {
  dates_list[[as.character(individual_id)]] <- generate_dates(individual_id)
}

# Store actual dates in each individual's list
for (individual_id in unique(BMI_Data$id)) {
  time_series_list[[as.character(individual_id)]]$dates <- dates_list[[as.character(individual_id)]]
}

# View the actual dates for the first individual
print(time_series_list[[1]]$dates)



#Create Time Intervals Object for each individual:

# Create a function to generate time intervals for each individual
generate_time_intervals <- function(time_series, mask, timestamps) {
  # Initialize the time intervals matrix
  time_intervals <- matrix(0, nrow = 3, ncol = length(timestamps))
  
  # Initialize the last observed timestamps for each variable
  last_observed_timestamps <- rep(0, 3)
  
  # Iterate over each timestamp
  for (timestamp_index in 1:length(timestamps)) {
    # Iterate over each variable
    for (var_index in 1:3) {
      # Calculate the time interval for the current timestamp
      if (mask[var_index, timestamp_index] == 1) {
        # If the value is observed, calculate time interval normally
        time_intervals[var_index, timestamp_index] <- timestamps[timestamp_index] - last_observed_timestamps[var_index]
        last_observed_timestamps[var_index] <- timestamps[timestamp_index]
      } else {
        # If the value is unobserved, use the last observed real value timestamp
        time_intervals[var_index, timestamp_index] <- timestamps[timestamp_index] - last_observed_timestamps[var_index]
      }
    }
  }
  
  # Return the time intervals matrix
  return(time_intervals)
}

# Create a list to store time intervals for each individual
time_intervals_list <- list()

# Iterate over unique individual IDs
for (individual_id in unique(BMI_Data$id)) {
  # Extract the time series, mask, and timestamps for the current individual
  individual_time_series <- time_series_only[[as.character(individual_id)]]
  individual_mask <- mask_list[[as.character(individual_id)]]
  timestamps <- dates_list[[as.character(individual_id)]]
  
  # Generate time intervals for the current individual
  time_intervals <- generate_time_intervals(individual_time_series, individual_mask$mask, timestamps)
  
  # Store time intervals in the list
  time_intervals_list[[as.character(individual_id)]] <- time_intervals
}

# View the time intervals for the first individual
print(time_intervals_list[[1]])





# Create the master_data_format list
master_data_format <- list()

# Iterate over unique individual IDs
for (individual_id in unique(BMI_Data$id)) {
  # Create an individual's data structure
  individual_data <- list()
  
  # Add time series, mask, timestamps, and time intervals to the individual's data
  individual_data$time_series <- time_series_only[[as.character(individual_id)]]
  individual_data$mask <- mask_list[[as.character(individual_id)]][[2]]  # Extract the second object
  individual_data$timestamps <- dates_list[[as.character(individual_id)]]
  individual_data$time_intervals <- time_intervals_list[[as.character(individual_id)]]
  
  # Add the individual's data to the master_data_format list
  master_data_format[[as.character(individual_id)]] <- individual_data
}

# View the structure of master_data_format
str(master_data_format)











