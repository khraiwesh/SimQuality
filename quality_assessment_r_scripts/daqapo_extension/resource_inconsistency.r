# Resource inconsistency: checks if specified activities are performed by different resources

library(dplyr)
library(readxl)

resource_inconsistency <- function(data_path, activities) {
  data <- read_excel(data_path)
  
  # Filter data to only include rows with specified activities
  data_filtered <- data %>%
    filter(Activity %in% activities)
  
  # Check for each patient visit if the specified activities are performed by different resources
  resource_inconsistency <- data_filtered %>%
    group_by(Patient_visit_nr) %>%
    filter(n_distinct(Activity) == length(activities)) %>% # Ensure all activities are accounted for
    summarise(
      Originators = paste(unique(Originator), collapse = ", "),
      Unique_Originators = n_distinct(Originator)
    ) %>%
    filter(Unique_Originators > 1) %>% # More than one unique resource found
    select(-Unique_Originators) %>%
    ungroup()
  
  return(resource_inconsistency)
}
