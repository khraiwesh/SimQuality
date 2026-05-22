# Timestamp completeness: NA detection and inactive periods

calculate_completeness <- function(dataset, data_file, inactive_threshold_minutes = 60) {
  
  actlog <- dataset %>%
    bupaR::activitylog(
      case_id = "case_id",
      activity_id = "activity",
      timestamps = c("start", "complete"),
      resource_id = "originator"
    )
  
  missing_start <- suppressMessages(detect_missing_values(
    activitylog = actlog,
    level_of_aggregation = "column",
    column = "start"
  ))
  
  missing_complete <- suppressMessages(detect_missing_values(
    activitylog = actlog,
    level_of_aggregation = "column",
    column = "complete"
  ))
  
  start_missing_count <- sum(is.na(dataset$start))
  complete_missing_count <- sum(is.na(dataset$complete))
  total_rows <- nrow(dataset)
  
  
  total_start_outliers <- start_missing_count
  total_complete_outliers <- complete_missing_count
  
  start_completeness <- round((total_rows - start_missing_count) / total_rows, 4)
  complete_completeness <- round((total_rows - complete_missing_count) / total_rows, 4)
  
  cat(paste("Start:", round(start_completeness * 100, 2), "% | Complete:", 
            round(complete_completeness * 100, 2), "%\n"))
  cat(paste("Total outliers - Start:", total_start_outliers, "| Complete:", total_complete_outliers, "\n\n"))
  
  completeness_results <- data.frame(
    timestamp_type = c("start", "complete"),
    total_records = c(total_rows, total_rows),
    missing_count = c(start_missing_count, complete_missing_count),
    non_missing_count = c(total_rows - start_missing_count, total_rows - complete_missing_count),
    total_outliers = c(total_start_outliers, total_complete_outliers),
    completeness = c(start_completeness, complete_completeness)
  )
  
  completeness_file <- "results/data_quality_completeness_results.RData"
  completeness_csv <- "results/data_quality_completeness_results.csv"
  
  save(start_completeness, complete_completeness, completeness_results, 
       file = completeness_file)
  write.csv(completeness_results, completeness_csv, row.names = FALSE)
  
  # Inactive periods detection
  cat("\n--- Inactive Periods Detection ---\n")
  
  threshold_minutes <- inactive_threshold_minutes
  cat(paste("Detecting inactive periods (threshold:", threshold_minutes, "minutes)\n"))
  
  inactive_activities <- data.frame()
  
  tryCatch({
    inactive_activities_result <- detect_inactive_periods(
      activitylog = actlog,
      threshold = threshold_minutes,
      type = "activities",
      timestamp = "start",
      details = TRUE
    )
    
    if(nrow(inactive_activities_result) > 0) {
      inactive_activities <- inactive_activities_result
      cat(paste("  Found", nrow(inactive_activities), "inactive periods for activities\n"))
      
      save(inactive_activities, file = "results/data_quality_inactive_activities.RData")
      write.csv(inactive_activities, "results/data_quality_inactive_activities.csv", row.names = FALSE)
    } else {
      cat("  No inactive activity periods detected\n")
    }
  }, error = function(e) {
    cat("  Error detecting inactive activities:", e$message, "\n")
  })
  
  inactive_arrivals <- data.frame()
  
  # Detect inactive periods for arrivals
  tryCatch({
    # Get first activity per case as start activities
    actlog_df <- as.data.frame(actlog)
    case_id_col <- bupaR::case_id(actlog)
    activity_id_col <- bupaR::activity_id(actlog)
    
    start_activities_list <- actlog_df %>%
      group_by(!!sym(case_id_col)) %>%
      arrange(start) %>%
      slice(1) %>%
      pull(!!sym(activity_id_col)) %>%
      unique()
    
    if(length(start_activities_list) > 0) {
      inactive_arrivals_result <- detect_inactive_periods(
        activitylog = actlog,
        threshold = threshold_minutes,
        type = "arrivals",
        timestamp = "start",
        start_activities = start_activities_list,
        details = TRUE
      )
      
      if(nrow(inactive_arrivals_result) > 0) {
        inactive_arrivals <- inactive_arrivals_result
        cat(paste("  Found", nrow(inactive_arrivals), "inactive periods for arrivals\n"))
        
        # Save results
        save(inactive_arrivals, file = "results/data_quality_inactive_arrivals.RData")
        write.csv(inactive_arrivals, "results/data_quality_inactive_arrivals.csv", row.names = FALSE)
      } else {
        cat("  No inactive arrival periods detected\n")
      }
    }
  }, error = function(e) {
    cat("  Error detecting inactive arrivals:", e$message, "\n")
  })
  
  cat("\n")
  
  # Return results with outlier counts
  return(list(
    start_completeness = start_completeness,
    complete_completeness = complete_completeness,
    total_start_outliers = start_missing_count,#Add the other outliers types if needed 
    total_complete_outliers = complete_missing_count#Add the other outliers types if needed 
  ))
}
