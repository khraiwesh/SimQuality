# Activity consistency: similar label detection (typos / inconsistent naming)
# Checks if the same activity is labeled differently (e.g., "Registration" vs "Registraton")

library(dplyr)
library(bupaR)
library(daqapo)

calculate_activity_consistency <- function(dataset, data_file, allowed_activities, max_edit_distance = 3) {
  
  results_dir <- "results_activity"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("ACTIVITY LABEL CONSISTENCY ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog <- dataset
  
  actlog_df <- as.data.frame(actlog)
  
  total_records <- nrow(actlog_df)
  total_cases <- n_distinct(actlog_df$case_id)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Total cases:", total_cases, "\n\n"))
  
  # 1. Detect similar labels (inconsistent naming / typos)
  cat("============================================\n")
  cat("1. DETECT SIMILAR LABELS (Inconsistent Naming)\n")
  cat("============================================\n")
  cat(paste("Looking for similar activity names (max edit distance:", max_edit_distance, ")\n\n"))
  
  similar_labels_result <- NULL
  typo_activities <- c()
  typo_count <- 0
  
  similar_labels_result <- tryCatch({
    result <- detect_similar_labels(
      activitylog = actlog,
      column_labels = "activity",
      max_edit_distance = max_edit_distance,
      show_NA = FALSE,
      ignore_capitals = FALSE
    )
    result
  }, error = function(e) {
    cat(paste("  Error:", e$message, "\n"))
    NULL
  })
  
  if(!is.null(similar_labels_result) && nrow(similar_labels_result) > 0) {
    activity_similar <- similar_labels_result %>%
      filter(column_labels == "activity")
    
    if(nrow(activity_similar) > 0) {
      cat("Similar activity labels found (potential inconsistencies):\n")
      print(activity_similar)
      
      all_similar_labels <- unique(activity_similar$labels)
      typo_activities <- all_similar_labels[!all_similar_labels %in% allowed_activities]
      
      cat(paste("\nInconsistent activities (not in allowed list):", paste(typo_activities, collapse = ", "), "\n"))
      
      typo_rows <- actlog_df %>% filter(activity %in% typo_activities)
      typo_count <- nrow(typo_rows)
      
      cat(paste("Records with inconsistent labels:", typo_count, "\n"))
      
      similar_file <- file.path(results_dir, "activity_similar_labels.csv")
      write.csv(activity_similar, similar_file, row.names = FALSE)
      cat(paste("  Saved to:", similar_file, "\n"))
    }
  } else {
    cat("No similar labels detected.\n")
  }
  
  # 2. Consistency calculation
  cat("\n============================================\n")
  cat("2. CONSISTENCY CALCULATION\n")
  cat("============================================\n\n")
  
  consistent_count <- total_records - typo_count
  consistency <- (consistent_count / total_records) * 100
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Inconsistent label count:", typo_count, "\n"))
  cat(paste("Consistent records:", consistent_count, "\n"))
  cat(paste("Activity Consistency:", round(consistency, 2), "%\n"))
  
  consistency_summary <- data.frame(
    metric = c("total_records", "typo_count", "consistent_count", "consistency_percent"),
    value = c(total_records, typo_count, consistent_count, round(consistency, 2))
  )
  
  summary_file <- file.path(results_dir, "activity_consistency_summary.csv")
  write.csv(consistency_summary, summary_file, row.names = FALSE)
  cat(paste("  Saved to:", summary_file, "\n"))
  
  return(list(
    total_records = total_records,
    total_cases = total_cases,
    typo_count = typo_count,
    typo_activities = typo_activities,
    similar_labels_result = similar_labels_result,
    consistency = consistency
  ))
}
