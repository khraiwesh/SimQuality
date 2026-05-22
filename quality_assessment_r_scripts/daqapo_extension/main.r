# Main Function: runs all data quality check functions

library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(stringr)
library(tibble)
library(knitr)
library(anytime)
library(eventdataR)

# Set working directory to script location
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
if (length(script_path) > 0) {
  setwd(dirname(script_path))
}

source("same_timestamp.r")
source("synonymous_label.r")
source("resource_activity_mismatch.r")
source("resource_inconsistency.r")
source("imprecise_resource.r")
source("incorrect_case.r")
source("imprecise_timestamp.r")
options(width = 400)

# Path to the data file
data_path <- 'Hospital Event Log.xlsx'

# 1. Same timestamp
events_by_start_ts <- same_timestamp(data_path)
cat("Same timestamp:\n")
print(events_by_start_ts, width = Inf)

# 2. Synonymous labels
non_cooccurring_pairs <- synonymous_label(data_path)
cat("Synonymous label:\n")
print(non_cooccurring_pairs)

# 3. Imprecise timestamp
sample_timestamp <- "21/11/2017 11:22:16"
invalid_dates <- imprecise_timestamp(data_path, sample_timestamp)
cat("Invalid dates:\n")
print(invalid_dates)

# 4. Resource activity mismatch
resource_mismatch <- "Doctor 1"
resource_activities_summary <- resource_activity_mismatch(data_path, resource_mismatch)
cat("Resource activities by type:\n")
print(resource_activities_summary, n = 32)

# 5. Resource inconsistency
activities <- c("Clinical exam", "Treatment evaluation")
inconsistent_resource <- resource_inconsistency(data_path, activities)
cat("Cases with resource inconsistency:\n")
print(inconsistent_resource)

# 6. Imprecise resource
sample_resource <- "Nurse 5"
imprecise_resource_rows <- imprecise_resource(data_path, sample_resource)
cat("Rows with imprecise resource identifiers:\n")
print(imprecise_resource_rows)

# 7. Incorrect cases
rule_activity <- c("Drug A treatment", "Drug B treatment")
incorrect_cases <- incorrect_case(data_path, rule_activity)
cat("Rows with incorrect cases:\n")
print(incorrect_cases)
