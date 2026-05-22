# Same Timestamp: detects activities with identical timestamps within same patient visit

library(dplyr)
library(readxl)
library(lubridate)

same_timestamp <- function(data_path) {
  
  df <- read_excel(data_path)
  
  # Convert Start_ts and Complete_ts to datetime objects
  df$Start_ts <- ymd_hms(df$Start_ts, quiet = TRUE)
  df$Complete_ts <- ymd_hms(df$Complete_ts, quiet = TRUE)
  
  # Filter for patient visits with more than one activity
  df_multiple <- df %>%
    group_by(Patient_visit_nr) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Create separate data frames for Start_ts and Complete_ts
  df_start <- df_multiple %>%
    select(Patient_visit_nr, Activity, Timestamp = Start_ts) %>%
    filter(!is.na(Timestamp))
  
  df_complete <- df_multiple %>%
    select(Patient_visit_nr, Activity, Timestamp = Complete_ts) %>%
    filter(!is.na(Timestamp))
  
  # Find duplicates within Start_ts
  df_start_grouped <- df_start %>%
    group_by(Patient_visit_nr, Timestamp) %>%
    filter(n() > 1) %>% # Ensure there are multiple activities sharing the same timestamp
    summarise(Activities = toString(unique(Activity)), .groups = 'drop')
  
  # Find duplicates within Complete_ts
  df_complete_grouped <- df_complete %>%
    group_by(Patient_visit_nr, Timestamp) %>%
    filter(n() > 1) %>% # Ensure there are multiple activities sharing the same timestamp
    summarise(Activities = toString(unique(Activity)), .groups = 'drop')
  
  # Combine the results
  df_grouped <- bind_rows(df_start_grouped, df_complete_grouped)
  
  # Create the x variable with patient visit numbers and activity groups
  x <- df_grouped %>%
    group_by(Patient_visit_nr) %>%
    summarise(Activity_Group = toString(unique(Activities)), .groups = 'drop')
  
  # Aggregate activities and count unique case numbers
  result <- x %>%
    group_by(Activity_Group) %>%
    summarise(Occurrences = n(),
              Cases = toString(unique(Patient_visit_nr)),
              .groups = 'drop') %>%
    arrange(desc(Occurrences)) # Sorting by Occurrences in descending order
  
  # Display the x variable and the result
  return(result)
}
