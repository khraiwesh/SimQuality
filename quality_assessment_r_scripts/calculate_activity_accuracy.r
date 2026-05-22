# Activity accuracy: incorrect activity names detection
# Checks for activities that are not in the allowed activities list

library(dplyr)
library(bupaR)
library(daqapo)

calculate_activity_accuracy <- function(dataset, data_file, allowed_activities) {
  
  results_dir <- "results_activity"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("ACTIVITY LABEL ACCURACY ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog <- dataset
  
  actlog_df <- as.data.frame(actlog)
  
  total_records <- nrow(actlog_df)
  unique_activities <- unique(actlog_df$activity)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Unique activities:", length(unique_activities), "\n"))
  cat(paste("Activity values:", paste(unique_activities, collapse = ", "), "\n\n"))
  
  cat("Allowed activities:", paste(allowed_activities, collapse = ", "), "\n\n")
  
  incorrect_count <- 0
  
  # 1. Detect incorrect activity names
  cat("============================================\n")
  cat("1. DETECT INCORRECT ACTIVITY NAMES\n")
  cat("============================================\n")
  cat("Checking activities not in allowed list\n\n")
  
  incorrect_activities <- NULL
  incorrect_result <- NULL
  
  incorrect_result <- tryCatch({
    result <- detect_incorrect_activity_names(
      activitylog = actlog,
      allowed_activities = allowed_activities
    )
    result
  }, error = function(e) {
    cat(paste("  Error:", e$message, "\n"))
    NULL
  })
  
  if(!is.null(incorrect_result)) {
    incorrect_df <- as.data.frame(incorrect_result)
    incorrect_count <- nrow(incorrect_df)
    
    # Exclude near-typos (Levenshtein distance <= max_edit_distance from any
    # allowed activity).  These are *consistency* violations, not accuracy
    # violations.  adist() is base R — no extra package required.
    if (incorrect_count > 0 && length(allowed_activities) > 0) {
      near_typo_mask <- vapply(incorrect_df$activity, function(act) {
        if (is.na(act)) return(FALSE)
        min(adist(as.character(act), allowed_activities,
                  ignore.case = FALSE)) <= max_edit_distance
      }, logical(1))
      if (any(near_typo_mask)) {
        cat(paste("Excluding", sum(near_typo_mask),
                  "near-typo rows (consistency issues, not accuracy violations)\n"))
        incorrect_df    <- incorrect_df[!near_typo_mask, ]
        incorrect_count <- nrow(incorrect_df)
      }
    }

    cat(paste("Rows with incorrect activity:", incorrect_count, "\n\n"))
    
    if(incorrect_count > 0) {
      cat("Rows with incorrect activity names:\n")
      print(incorrect_df[, c("case_id", "activity", "originator")])
      
      incorrect_file <- file.path(results_dir, "activity_incorrect_names.csv")
      write.csv(incorrect_df, incorrect_file, row.names = FALSE)
      cat(paste("\n  Saved to:", incorrect_file, "\n"))
    }
  } else {
    cat("No incorrect activity names found.\n")
  }
  
  # 2. Filter incorrect activities
  cat("\n============================================\n")
  cat("2. FILTER INCORRECT ACTIVITIES\n")
  cat("============================================\n\n")
  
  actlog_clean_df <- actlog_df %>%
    filter(activity %in% allowed_activities)
  
  records_final <- nrow(actlog_clean_df)
  cat(paste("Records before:", total_records, "\n"))
  cat(paste("Records after incorrect filter:", records_final, "\n"))
  cat(paste("Removed:", incorrect_count, "incorrect records\n"))
  
  actlog_clean <- actlog_clean_df %>%
    bupaR::activitylog(
      case_id = "case_id",
      activity_id = "activity",
      timestamps = c("start", "complete"),
      resource_id = "originator"
    )
  
  # 3. Accuracy calculation
  cat("\n============================================\n")
  cat("3. ACCURACY CALCULATION\n")
  cat("============================================\n\n")
  
  correct_count <- total_records - incorrect_count
  accuracy <- (correct_count / total_records) * 100
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Incorrect count:", incorrect_count, "\n"))
  cat(paste("Correct activity names:", correct_count, "\n"))
  cat(paste("Activity Accuracy:", round(accuracy, 2), "%\n"))
  
  accuracy_summary <- data.frame(
    metric = c("total_records", "incorrect_count", "correct_count", "accuracy_percent"),
    value = c(total_records, incorrect_count, correct_count, round(accuracy, 2))
  )
  
  summary_file <- file.path(results_dir, "activity_accuracy_summary.csv")
  write.csv(accuracy_summary, summary_file, row.names = FALSE)
  cat(paste("  Saved to:", summary_file, "\n"))
  
  return(list(
    total_records = total_records,
    unique_activities = unique_activities,
    allowed_activities = allowed_activities,
    incorrect_activities = incorrect_activities,
    incorrect_count = incorrect_count,
    correct_count = correct_count,
    accuracy = accuracy,
    incorrect_result = incorrect_result,
    actlog_clean = actlog_clean
  ))
}
