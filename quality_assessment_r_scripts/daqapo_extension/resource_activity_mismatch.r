# Resource-activity mismatch: analyzes activities performed by a specific resource

library(dplyr)
library(readxl)

resource_activity_mismatch <- function(data_path, resource_input) {
  
  data <- read_excel(data_path)
  
  # Filter data for the specified resource
  filtered_data <- data %>%
    filter(Originator == resource_input)
  
  # Calculate the ActivityCount directly from the filtered data
  resource_activities_by_type <- filtered_data %>%
    group_by(Originator, Activity) %>%
    summarise(ActivityCount = n(), .groups = 'drop') %>%
    arrange(Originator, ActivityCount) # Sorting in ascending order within each group
  
  return(resource_activities_by_type)
}
