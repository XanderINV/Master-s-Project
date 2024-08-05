##code for visualization plots

# Load required packages
library(ggplot2)
library(dplyr)

#BMI distribution plots

# Calculate the number of entries for each patient
entries_count <- BMI_Data %>%
  group_by(id) %>%
  summarise(num_entries = n())

# Calculate the mean and median of the number of entries
mean_num_entries <- mean(entries_count$num_entries)
median_num_entries <- median(entries_count$num_entries)

# Calculate the cutoff for the top 5%
top_5_cutoff <- quantile(entries_count$num_entries, 0.95)

# Create a histogram with a normal distribution overlay
ggplot(entries_count, aes(x = num_entries, y = ..density..)) +
  geom_histogram(bins = 45, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  geom_vline(xintercept = mean_num_entries, color = "blue", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(xintercept = median_num_entries, color = "green", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(xintercept = top_5_cutoff, color = "orange", linetype = "dashed", size = 1, show.legend = TRUE) +
  labs(x = "Number of Entries", y = "Density", title = "Distribution of Number of Entries per Patient for BMI", color = "Legend") +
  scale_color_manual(values = c("blue", "green", "orange"), labels = c("Mean", "Median", "Top 5%")) +
  theme_minimal()



#HbA1c Distribution of data entries per patient

# Calculate the number of entries for each patient
entries_count_hba1c <- hba1c_Data %>%
  group_by(id) %>%
  summarise(num_entries = n())

# Calculate the mean and median of the number of entries
mean_num_entries_hba1c <- mean(entries_count_hba1c$num_entries)
median_num_entries_hba1c <- median(entries_count_hba1c$num_entries)

# Calculate the cutoff for the top 5%
top_5_cutoff_hba1c <- quantile(entries_count_hba1c$num_entries, 0.95)

# Create a histogram with a normal distribution overlay for HbA1c
ggplot(entries_count_hba1c, aes(x = num_entries, y = ..density..)) +
  geom_histogram(bins = 40, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  geom_vline(xintercept = mean_num_entries_hba1c, color = "blue", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(xintercept = median_num_entries_hba1c, color = "green", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(xintercept = top_5_cutoff_hba1c, color = "orange", linetype = "dashed", size = 1, show.legend = TRUE) +
  labs(x = "Number of Entries", y = "Density", title = "Distribution of Number of Entries per Patient for HbA1c", color = "Legend") +
  scale_color_manual(values = c("blue", "green", "orange"), labels = c("Mean", "Median", "Top 5%")) +
  theme_minimal()




#SBP Distribution of entries:

# Calculate the number of entries for each patient
entries_count_sbp <- SBP_Data %>%
  group_by(id) %>%
  summarise(num_entries = n())

# Calculate the mean and median of the number of entries
mean_num_entries_sbp <- mean(entries_count_sbp$num_entries)
median_num_entries_sbp <- median(entries_count_sbp$num_entries)

# Calculate the cutoff for the top 5%
top_5_cutoff_sbp <- quantile(entries_count_sbp$num_entries, 0.95)

# Create a histogram with a normal distribution overlay for SBP
ggplot(entries_count_sbp, aes(x = num_entries, y = ..density..)) +
  geom_histogram(bins = 45, fill = "lightblue", color = "black") +
  geom_density(color = "red") +
  geom_vline(xintercept = mean_num_entries_sbp, color = "blue", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(xintercept = median_num_entries_sbp, color = "green", linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_vline(xintercept = top_5_cutoff_sbp, color = "orange", linetype = "dashed", size = 1, show.legend = TRUE) +
  labs(x = "Number of Entries", y = "Density", title = "Distribution of Number of Entries per Patient for SBP", color = "Legend") +
  scale_color_manual(values = c("blue", "green", "orange"), labels = c("Mean", "Median", "Top 5%")) +
  theme_minimal()












#Lets plot a histogram of the average value of each variable for every individual:

library(ggplot2)

# Function to calculate average value for each individual across all variables
calculate_average <- function(master_data_format) {
  averages <- list()
  for (individual_id in names(master_data_format)) {
    time_series <- master_data_format[[individual_id]]$time_series
    
    # Calculate average for each variable, excluding NA values
    avg_bmi <- mean(time_series[1, ], na.rm = TRUE)
    avg_hba1c <- mean(time_series[2, ], na.rm = TRUE)
    avg_sbp <- mean(time_series[3, ], na.rm = TRUE)
    
    # Store averages for the individual
    averages[[individual_id]] <- c(avg_bmi = avg_bmi, avg_hba1c = avg_hba1c, avg_sbp = avg_sbp)
  }
  return(do.call(rbind, averages))
}

# Calculate averages for each individual
averages <- calculate_average(master_data_format)

# Convert to data frame
averages_df <- as.data.frame(averages)

# Histogram for average BMI     average BMI between men and women at T2D diagnosis in the UK is 25.2 and 26.9 respectively, our data has a higher average.
ggplot(averages_df, aes(x = avg_bmi)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(0,70, by = 5)) +
  labs(title = "Distribution of Average BMI",
       x = "Average BMI",
       y = "Frequency")

# Histogram for average HbA1c        target level should be below (42) in healthy ,T2D:  ideal (42 - 53),   acceptable (53 - 58). Discuss current management (59 - 75),  severe complication (75+) 
ggplot(averages_df, aes(x = avg_hba1c)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average HbA1c",
       x = "Average HbA1c",
       y = "Frequency")

# Histogram for average SBP                      Should target 130-139   but below would be even more ideal.
ggplot(averages_df, aes(x = avg_sbp)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(0,200, by = 10)) +
  labs(title = "Distribution of Average SBP",
       x = "Average SBP",
       y = "Frequency")


library(patchwork)
# Histogram for average BMI
plot_bmi <- ggplot(averages_df, aes(x = avg_bmi)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(0, 70, by = 5)) +
  labs(title = "Average BMI",
       x = "Average BMI",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Histogram for average HbA1c
plot_hba1c <- ggplot(averages_df, aes(x = avg_hba1c)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Average HbA1c",
       x = "Average HbA1c",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
# Histogram for average SBP
plot_sbp <- ggplot(averages_df, aes(x = avg_sbp)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(0, 200, by = 20)) + # Removed every second label
  labs(title = "Average SBP",
       x = "Average SBP",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Combine the three plots into one
combined_plot <- plot_bmi + plot_hba1c + plot_sbp + 
  plot_annotation(title = "Distribution of  Clinical Variables",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))

# Display the combined plot
print(combined_plot)




library(dplyr)

# Function to calculate global average values for each variable
calculate_global_averages <- function(master_data_format) {
  all_bmi <- c()
  all_hba1c <- c()
  all_sbp <- c()
  
  for (individual_id in names(master_data_format)) {
    time_series <- master_data_format[[individual_id]]$time_series
    
    # Collect all non-NA values for each variable
    all_bmi <- c(all_bmi, time_series[1, !is.na(time_series[1, ])])
    all_hba1c <- c(all_hba1c, time_series[2, !is.na(time_series[2, ])])
    all_sbp <- c(all_sbp, time_series[3, !is.na(time_series[3, ])])
  }
  
  # Calculate global averages
  global_avg_bmi <- mean(all_bmi, na.rm = TRUE)
  global_avg_hba1c <- mean(all_hba1c, na.rm = TRUE)
  global_avg_sbp <- mean(all_sbp, na.rm = TRUE)
  
  return(list(avg_bmi = global_avg_bmi, avg_hba1c = global_avg_hba1c, avg_sbp = global_avg_sbp))
}

# Calculate global averages for each variable
global_averages <- calculate_global_averages(master_data_format)

# Print global averages
print(paste("Global Average BMI:", global_averages$avg_bmi))
print(paste("Global Average HbA1c:", global_averages$avg_hba1c))
print(paste("Global Average SBP:", global_averages$avg_sbp))




# Missingness plot over time bins:  (plot showing the number of unique individuals with at least one data point every 6 months)

# Load necessary libraries
library(dplyr)
library(ggplot2)


# Function to create the histogram plot for a given data frame and bin size
plot_missingness <- function(data, bin_size_months, variable_name) {
  # Create a sequence of bins based on the minimum and maximum dates in the data
  min_date <- min(data$date)
  max_date <- max(data$date)
  bins <- seq(from = min_date, to = max_date, by = paste(bin_size_months, "months"))
  
  # Add an interval column to the data
  data <- data %>%
    mutate(interval = cut(date, breaks = bins, include.lowest = TRUE))
  
  # Count the number of unique individuals in each interval
  interval_counts <- data %>%
    group_by(interval) %>%
    summarise(num_individuals = n_distinct(id))
  
  # Convert the interval column to a factor to maintain the order in the plot
  interval_counts$interval <- factor(interval_counts$interval, levels = levels(data$interval))
  
  # Create the plot
  plot <- ggplot(interval_counts, aes(x = interval, y = num_individuals)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = paste("Missingness Over Time for", variable_name),
         x = "Time Interval",
         y = "Number of Individuals") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot)
}

# Plot missingness for each variable
plot_bmi <- plot_missingness(BMI_Data, bin_size_months = 6, variable_name = "BMI")
plot_hba1c <- plot_missingness(hba1c_Data, bin_size_months = 6, variable_name = "Hba1c")
plot_sbp <- plot_missingness(SBP_Data, bin_size_months = 6, variable_name = "SBP")

# Display the plots
print(plot_bmi)
print(plot_hba1c)
print(plot_sbp)


##Plots showing the % of individuals with data in each bin (6 months default)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Function to calculate the percentage of individuals with data in each bin
calculate_missingness <- function(data, bin_size_months, variable_name) {
  min_date <- min(data$date)
  max_date <- max(data$date)
  bins <- seq(from = min_date, to = max_date, by = paste(bin_size_months, "months"))
  
  data <- data %>%
    mutate(interval = cut(date, breaks = bins, include.lowest = TRUE))
  
  interval_counts <- data %>%
    group_by(interval) %>%
    summarise(num_individuals = n_distinct(id))
  
  total_individuals <- n_distinct(data$id)
  
  interval_counts <- interval_counts %>%
    mutate(percentage = (num_individuals / total_individuals) * 100,
           variable = variable_name)
  
  interval_counts
}

# Calculate missingness for each variable
missingness_bmi <- calculate_missingness(BMI_Data, bin_size_months = 6, "BMI")
missingness_hba1c <- calculate_missingness(hba1c_Data, bin_size_months = 6, "HbA1c")
missingness_sbp <- calculate_missingness(SBP_Data, bin_size_months = 6, "SBP")

# Combine the data for plotting
missingness_combined <- bind_rows(missingness_bmi, missingness_hba1c, missingness_sbp)

# Ensure the interval is properly ordered
missingness_combined$interval <- as.Date(missingness_combined$interval, format="%Y-%m-%d")

# Create the plot
plot_missingness <- ggplot(missingness_combined, aes(x = interval, y = percentage, color = variable, group = variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Percentage of Individuals with Data Over Time",
       x = "Time Interval (6 months)",
       y = "Percentage of Individuals with Data",
       color = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

# Display the plot
print(plot_missingness)





















