library(dplyr)
library(ggplot2)

# Function to display output
display_output <- function(message) {
  cat(message, "\n")
}

# Load the data
data <- read.csv("example_dataset.csv")

# Data Cleaning and Preprocessing
display_output("Initial Dataset Structure:")
str(data)
display_output("Summary of Initial Dataset:")
print(summary(data))

# Handling Missing Values
display_output("Handling Missing Values...")
data <- na.omit(data)

# Removing Duplicates
display_output("Removing Duplicates...")
data <- unique(data)

# Handling Outliers using z-score normalization
display_output("Handling Outliers...")
data$Age <- scale(data$Age)
data$Height <- scale(data$Height)

# Encoding Categorical Variables using one-hot encoding
display_output("Encoding Categorical Variables...")
data <- cbind(data, model.matrix(~ Gender - 1, data))

# Feature Engineering (e.g., creating a new feature 'BMI')
display_output("Feature Engineering...")
data$BMI <- data$Weight / ((data$Height)^2)

# Export cleaned data
write.csv(data, "cleaned_dataset.csv")

# Visualizing Cleaned Data (scatterplot of Height vs. Weight)
display_output("Visualizing Cleaned Data...")
ggplot(data, aes(x = Height, y = Weight)) +
  geom_point() +
  labs(title = "Scatterplot of Height vs. Weight", x = "Height", y = "Weight")

# Additional Data Processing Steps
display_output("Additional Data Processing Steps...")

# Imputing missing values in other columns (e.g., Income)
# For illustration purposes, let's assume imputing with the mean
data$Income <- ifelse(is.na(data$Income), mean(data$Income, na.rm = TRUE), data$Income)

# Removing outliers in other columns (e.g., Income)
# For illustration purposes, let's assume removing values beyond 2 standard deviations
data <- filter(data, abs(scale(Income)) < 2)

# More feature engineering (e.g., creating a new feature 'IncomePerAge')
data$IncomePerAge <- data$Income / data$Age

# Export updated data
write.csv(data, "updated_dataset.csv")

# Visualizing additional plots (histogram of Income)
display_output("Visualizing Additional Plots...")
ggplot(data, aes(x = Income)) +
  geom_histogram(binwidth = 10000, fill = "blue", color = "black") +
  labs(title = "Histogram of Income", x = "Income", y = "Frequency")

# Loading the larger dataset
larger_data <- read.csv("larger_dataset.csv")

# Combining datasets
combined_data <- bind_rows(data, larger_data)

# Export combined data
write.csv(combined_data, "combined_dataset.csv")

# End of script
display_output("Data Cleaning and Preprocessing Completed.")
