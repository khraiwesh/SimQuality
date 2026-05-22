library(readxl)
library(dplyr)

imprecise_resource <- function(data_path, sample_resource) {
  df <- read_excel(data_path)
  
  cat("Sample resource: ", sample_resource, "\n")
  
  # Determine the expected format based on the sample resource
  if (grepl("^[A-Za-z]+(\\s|\\u00A0)+\\d+$", sample_resource)) {
    chosen_format <- "Word followed by a number"
    cat("Detected format: ", chosen_format, "\n")
    check_format <- function(x) {
      grepl("^[A-Za-z]+(\\s|\\u00A0)+\\d+$", x)
    }
  } else if (grepl("^\\d+$", sample_resource)) {
    chosen_format <- "Purely numeric"
    cat("Detected format: ", chosen_format, "\n")
    expected_length <- nchar(sample_resource)
    check_format <- function(x) {
      nchar(x) == expected_length && grepl("^\\d+$", x)
    }
  } else if (grepl("^[A-Za-z0-9]+$", sample_resource)) {
    chosen_format <- "Alphanumeric without spaces"
    cat("Detected format: ", chosen_format, "\n")
    expected_length <- nchar(sample_resource)
    check_format <- function(x) {
      nchar(x) == expected_length && grepl("^[A-Za-z0-9]+$", x)
    }
  } else if (grepl("^[A-Za-z]+\\s[A-Za-z]+$", sample_resource)) {
    chosen_format <- "Two words"
    cat("Detected format: ", chosen_format, "\n")
    check_format <- function(x) {
      grepl("^[A-Za-z]+\\s[A-Za-z]+$", x)
    }
  } else {
    cat("Failed to detect format for sample resource: ", sample_resource, "\n")
    stop("The sample resource format is not supported.")
  }
  
  # Filter rows that do not match the expected format
  rows_with_imprecise_originator <- df %>%
    filter(!sapply(df$Originator, check_format))
  
  return(rows_with_imprecise_originator)
}
