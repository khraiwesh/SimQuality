# Synonymous label: finds non-cooccurring activity pairs that might be synonyms

library(dplyr)
library(readxl)

# Function to find non-cooccurring activity pairs
synonymous_label <- function(data_path) {
  
  data <- read_excel(data_path)
  data$Activity <- tolower(data$Activity)
  
  grouped_data <- data %>%
    group_by(Patient_visit_nr) %>%
    summarise(Activities = list(unique(Activity))) %>%
    ungroup()
  
  unique_activities <- sort(unique(unlist(grouped_data$Activities)))
  non_cooccurring_pairs <- vector("list", length = 0)
  
  for (i in 1:(length(unique_activities) - 1)) {
    for (j in (i + 1):length(unique_activities)) {
      activity1 <- unique_activities[i]
      activity2 <- unique_activities[j]
      cooccur <- FALSE
      
      for (k in 1:nrow(grouped_data)) {
        if (all(c(activity1, activity2) %in% grouped_data$Activities[[k]])) {
          cooccur <- TRUE
          break
        }
      }
      
      if (!cooccur) {
        # Sort the pair to ensure unique combinations
        pair <- sort(c(activity1, activity2))
        non_cooccurring_pairs <- c(non_cooccurring_pairs, list(pair))
      }
    }
  }
  
  # Convert list to dataframe and remove duplicate rows
  non_cooccurring_pairs_df <- do.call(rbind, non_cooccurring_pairs)
  non_cooccurring_pairs_df <- unique(non_cooccurring_pairs_df)
  colnames(non_cooccurring_pairs_df) <- c("Activity1", "Activity2")
  return(non_cooccurring_pairs_df)
}
