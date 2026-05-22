# Imprecise timestamp: detects timestamp format inconsistencies

library(dplyr)
library(readxl)

imprecise_timestamp <- function(data_path, sample_date) {
  
  data <- read_excel(data_path)
  
  # Determine the expected length of the sample date
  expected_length <- nchar(sample_date)
  
  # function to check for timestamp format based on sample date length
  check_date_format <- function(date_string) {
    # Check for NA values first
    if (is.na(date_string)) {
      return(FALSE) # Skip NA values
    }
    # Check if the original string matches the expected length
    if (nchar(date_string) != expected_length) {
      return(TRUE) # Invalid format
    } else {
      return(FALSE) # Valid format
    }
  }
  
  # Apply the function to Start_ts and Complete_ts columns
  data$Invalid_Start_ts <- sapply(data$Start_ts, check_date_format)
  
  # Check for Complete_ts (or complete column) existence and validate if it exists
  if ("Complete_ts" %in% colnames(data)) {
    data$Invalid_Complete_ts <- sapply(data$Complete_ts, check_date_format)
  } else {
    data$Invalid_Complete_ts <- FALSE # If Complete_ts does not exist, set to FALSE
  }
  
  # Filter invalid dates
  invalid_dates_data <- data[data$Invalid_Start_ts | data$Invalid_Complete_ts, ]
  
  # Analyze most frequent Activities and Originators with invalid timestamps
  most_freq_activities <- sort(table(invalid_dates_data$Activity), decreasing = TRUE)
  most_freq_originators <- sort(table(invalid_dates_data$Originator), decreasing = TRUE)
  
  # Transform the output for activities and originators to desired format
  if (length(most_freq_activities) > 0) {
    transposed_activities <- as.data.frame(stack(most_freq_activities), stringsAsFactors = FALSE)
    transposed_activities <- transposed_activities[, c("ind", "values")]
    names(transposed_activities) <- c("Activity", "Count")
  } else {
    transposed_activities <- data.frame(Activity = character(0), Count = integer(0))
  }
  
  if (length(most_freq_originators) > 0) {
    transposed_originators <- as.data.frame(stack(most_freq_originators), stringsAsFactors = FALSE)
    transposed_originators <- transposed_originators[, c("ind", "values")]
    names(transposed_originators) <- c("Originator", "Count")
  } else {
    transposed_originators <- data.frame(Originator = character(0), Count = integer(0))
  }
  
  # Results
  list(
    InvalidData = invalid_dates_data,
    MostFrequentActivities = transposed_activities,
    MostFrequentOriginators = transposed_originators
  )
}
