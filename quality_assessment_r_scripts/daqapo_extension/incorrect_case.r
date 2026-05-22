# Incorrect Case: finds cases where specified activities cooccur (rule violations)

library(dplyr)
library(readxl)

incorrect_case <- function(data_path, rule_activity) {
  dataset <- read_excel(data_path)
  # Filter cases where any of the activities in rule_activity occurred
  cases_with_either_activity <- subset(dataset, Activity %in% rule_activity)
  
  cooccurring_cases <- lapply(split(cases_with_either_activity, 
                                    cases_with_either_activity$Patient_visit_nr), function(case_data) {
    if (all(rule_activity %in% case_data$Activity)) case_data$Patient_visit_nr[1]
  })
  # Clean up the list to remove NULLs and duplicates
  cooccurring_cases <- unname(na.omit(unique(unlist(cooccurring_cases))))
  # Print the case numbers
  if (length(cooccurring_cases) > 0) {
    return(cooccurring_cases)
  } else {
    print("No cases with co-occurring specified activities were found.")
  }
}
