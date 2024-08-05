library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(readr)


BMI_Data <- read_csv("xander_bmi.csv")
View(BMI_data)
hba1c_Data <- read_csv("xander_hba1c.csv")
SBP_Data <- read.csv("xander_sbp.csv")


#Column classes
class(BMI_Data$date)
class(hba1c_data$date)
class(SBP_Data$date)

SBP_Data$date <- as.Date(SBP_Data$date)

#Environment not saved, re import the data

head(BMI_Data)
summary(table(unique(BMI_Data$id)))

table(duplicated(BMI_Data$date))


selected_ids <- c("2147633802","2147634662","2147633614")
filtered_data <- BMI_Data %>%
  filter(id %in% selected_ids)

individual_plot <- ggplot(filtered_data, aes(x = date, y = value, color = as.factor(id))) + 
  geom_point()+
  geom_line()+
  labs(x = "Date", y = "BMI", title = "Individual BMI Plot") + 
  theme_minimal()

print(individual_plot)


#Time series data with varying interval length between measurements and varying total recording time.

range(SBP_Data$value)
range(hba1c_Data$value)

#Exploratory Analysis
library("zoo")

bmi_ts <- zoo(BMI_Data$value, order.by = paste(BMI_Data$id, BMI_Data$date, sep = "_"))
hba1c_ts <- zoo(hba1c_Data$value, order.by = paste(hba1c_Data$id, hba1c_Data$date, sep = "_"))
sbp_ts <- zoo(SBP_Data$value, order.by = paste(SBP_Data$id, SBP_Data$date, sep = "_"))

xlim_values <- c(as.Date(2014-12-31), as.Date(2005-01.04))

plot(bmi_ts, main = "BMI Over Time", xlab = "Date", ylab = "BMI")


# Total Unique Individuals in the Data ------------------------------------


#Total number of unique individuals:

#BMI
summary(table(unique(BMI_Data$id)))  #9015

#hba1c
summary(table(unique(hba1c_Data$id))) #9410

#SBP
summary(table(unique(SBP_Data$id))) #9276

#How many ind present across all three variables?
# Get the unique IDs from each dataset
bmi_ids <- unique(BMI_Data$id)
hba1c_ids <- unique(hba1c_Data$id)
sbp_ids <- unique(SBP_Data$id)

# Find the number of unique individuals in each dataset
num_unique_bmi <- length(bmi_ids)
num_unique_hba1c <- length(hba1c_ids)
num_unique_sbp <- length(sbp_ids)

# Find the number of individuals present in all three datasets
common_ids <- intersect(intersect(bmi_ids, hba1c_ids), sbp_ids)
num_common <- length(common_ids)

#Missing Values?
table(is.na(BMI_Data))
table(is.na(hba1c_Data))
table(is.na(SBP_Data))    # No missing values anywhere



# Plot the TS for one random patient --------------------------------------

#Plot the Ts of all variables for one individual

# Step 1: Randomly select one patient
set.seed(43) # for reproducibility
random_patient <- sample(unique(BMI_Data$id), 1)

# Step 2: Subset data for the selected patient
bmi_data_rp <- subset(BMI_Data, id == random_patient)
sbp_data_rp <- subset(SBP_Data, id == random_patient)
hba1c_data_rp <- subset(hba1c_Data, id == random_patient)

# Combine data into one data frame
combined_data <- rbind(
  data.frame(type = "BMI", date = bmi_data_rp$date, value = bmi_data_rp$value),
  data.frame(type = "SBP", date = sbp_data_rp$date, value = sbp_data_rp$value),
  data.frame(type = "HbA1c", date = hba1c_data_rp$date, value = hba1c_data_rp$value)
)

# Plot using ggplot2
ggplot(combined_data, aes(x = date, y = value, color = type)) +
  geom_line() +
  geom_point() +
  ylim(c(0,260))+
  labs(x = "Date", y = "Value", title = "Time Series for Random Patient") +
  scale_x_date(date_breaks = "years", date_labels = "%Y", limits = as.Date(c("2005-01-01", "2014-12-31"))) +
  theme_minimal()



# Average Number of entries for variables across all individuals ----------

#Average entries across all patients for BMI, hbA1c and SBP:
average_bmi_entries <- nrow(BMI_Data) / length(unique(BMI_Data$id))          #9.8
average_hba1c_entries <- nrow(hba1c_Data) / length(unique(hba1c_Data$id))    #10
average_SBP_entries <- nrow(SBP_Data) / length(unique(SBP_Data$id))          #18.4


# Finding the Max number of entries and computing top 5% to find complete cases ------------------


#Max number of observations [nb. Ind. with max obs.]

# Group data by patient ID and count observations for each patient
observations_count <- SBP_Data %>%
  group_by(id) %>%
  summarise(num_observations = n())

max(observations_count$num_observations)   #45
table(observations_count$num_observations == 45)  #only one person has the max nb of observation, lets see how many have over x amount
table(observations_count$num_observations >24)  #665 people have over 20 observations for BMI

#number of individuals in the top 5% of data points per variable:

table(entries_count$num_entries >23)  #375 people have over 23 observations for BMI (Top 5%)
table(entries_count_hba1c$num_entries > 22)  #378 people have over 20 observations for hba1c (Top 5%)
table(entries_count_sbp$num_entries >38)  #418 people have over 20 observations for SBP (Top 5%)

#Now do any of these individuals appear in all three and can be used as "Completes Cases"?
complete_cases <- merge(entries_count, entries_count_hba1c, by = "id", all = FALSE)
complete_cases <- merge(complete_cases, entries_count_sbp, by = "id", all = FALSE)

complete_cases <- complete_cases[complete_cases$num_entries.x > 23 & complete_cases$num_entries.y > 22 & complete_cases$num_entries > 38, ]


# Print the result
print(complete_cases)

#Store the individuals with complete entries in a new data frame

complete_cases_df_bmi <- as.data.frame(BMI_Data[BMI_Data$id %in% complete_cases$id, ])
complete_cases_df_hba1c <- as.data.frame(hba1c_Data[hba1c_Data$id %in% complete_cases$id, ])
complete_cases_df_sbp <- as.data.frame(SBP_Data[SBP_Data$id %in% complete_cases$id, ])






# Time Intervals (6 months)----------------------------------------------------------



#If we say bin our data into 6 months intervals from the start of 2005 (01-01-2005), to the end of 2015 (31-12-2015)
#We want to make a table which has the date intervals as rows and then two columns, one showing the number of
#Individuals that have at least one data point in that interval and then a column with the total number of data points in that interval
#We want to do this for each variable so we have 3 tables at the end.



library(dplyr)
library(lubridate)

# Define the start and end dates
start_date <- as.Date("2005-01-01")
end_date <- as.Date("2015-12-31")

# Create a sequence of dates for each 6-month interval
date_intervals <- seq(start_date, end_date, by = "6 months")

# Define a function to calculate the number of individuals and total data points in each interval
count_data <- function(data, intervals) {
  data %>%
    mutate(interval = cut(date, breaks = intervals, labels = FALSE)) %>%
    group_by(interval) %>%
    summarise(num_individuals = n_distinct(id),
              total_data_points = n())
}


# Function to format date intervals
format_date_intervals <- function(intervals) {
  start_dates <- intervals
  end_dates <- c(intervals[-1] - 1, max(intervals))
  formatted_intervals <- paste(start_dates, end_dates, sep = " - ")
  return(formatted_intervals[1:nrow(bmi_table)])  # Adjust to match the number of rows in bmi_table
}


# Apply the function to each variable
bmi_table <- count_data(BMI_Data, date_intervals)
bmi_table$interval <- format_date_intervals(date_intervals)

hba1c_table <- count_data(hba1c_Data, date_intervals)
hba1c_table$interval <- format_date_intervals(date_intervals)

sbp_table <- count_data(SBP_Data, date_intervals)
sbp_table$interval <- format_date_intervals(date_intervals)


#Convert missingness tables into percent format:

bmi_table$percent_missing <- 100 - (bmi_table$num_individuals / 9015) * 100
hba1c_table$percent_missing <- 100 - (hba1c_table$num_individuals / 9410) * 100
sbp_table$percent_missing <- 100 - (sbp_table$num_individuals / 9276) * 100






# Correlation Code --------------------------------------------------------


#Calculating the correlation between hba1c and SBP across all individuals (per person averaged out)

# Create an empty dataframe to store correlation values
correlation_df <- data.frame(id = character(),
                             correlation_bmi_hba1c = numeric(),
                             correlation_bmi_sbp = numeric(),
                             correlation_hba1c_sbp = numeric(),
                             stringsAsFactors = FALSE)

# List of unique patient IDs
patient_ids <- unique(BMI_Data$id)

# Loop through each patient
for (patient_id in patient_ids) {
  # Subset data for the current patient
  bmi_data <- subset(BMI_Data, id == patient_id)
  sbp_data <- subset(SBP_Data, id == patient_id)
  hba1c_data <- subset(hba1c_Data, id == patient_id)
  
  # Calculate correlations between BMI, HbA1c, and SBP
  correlation_bmi_hba1c <- cor(bmi_data$value, hba1c_Data$value, use = "complete.obs")
  correlation_bmi_sbp <- cor(bmi_data$value, sbp_data$value, use = "complete.obs")
  correlation_hba1c_sbp <- cor(hba1c_data$value, sbp_data$value, use = "complete.obs")
  
  # Add correlation values to dataframe
  correlation_df <- rbind(correlation_df, 
                          data.frame(id = patient_id,
                                     correlation_bmi_hba1c = correlation_bmi_hba1c,
                                     correlation_bmi_sbp = correlation_bmi_sbp,
                                     correlation_hba1c_sbp = correlation_hba1c_sbp))
}

# Print the correlation dataframe
print(correlation_df)


























