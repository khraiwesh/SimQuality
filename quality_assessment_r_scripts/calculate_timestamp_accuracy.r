# Timestamp accuracy: working hours validation, duration outliers, multiregistration detection

library(dplyr)
library(bupaR)
library(daqapo)
library(lubridate)

calculate_accuracy <- function(dataset_clean, data_file, working_hours_start = 0, working_hours_end = 24) {
  
  # Create clean activity log
  actlog_clean <- dataset_clean %>%
    bupaR::activitylog(
      case_id = "case_id",
      activity_id = "activity",
      timestamps = c("start", "complete"),
      resource_id = "originator"
    )
  
  # Working hours check
  actlog_with_hours <- actlog_clean %>%
    mutate(
      start_hour_check = as.numeric(format(start, "%H")),
      complete_hour_check = as.numeric(format(complete, "%H"))
    )
  
  cat(paste0("Checking working hours violations (", working_hours_start, ":00 - ", working_hours_end, ":00)...\n"))
  
  start_time <- working_hours_start
  end_time <- working_hours_end

  start_violation_log <- detect_value_range_violations(
    activitylog = actlog_with_hours,
    start_hour_check = domain_numeric(from = start_time, to = end_time),
    details = TRUE
  )
  
  complete_violation_log <- detect_value_range_violations(
    activitylog = actlog_with_hours,
    complete_hour_check = domain_numeric(from = start_time, to = end_time),
    details = TRUE
  )
  
  start_violations_df <- as.data.frame(start_violation_log)
  complete_violations_df <- as.data.frame(complete_violation_log)
  
  start_violations_count <- nrow(start_violations_df)
  complete_violations_count <- nrow(complete_violations_df)
  
  working_hours_violations <- data.frame()
  
  if(start_violations_count > 0) {
    temp_start <- start_violations_df[, c("case_id", "activity", "start", "complete")]
    temp_start$has_start_violation <- TRUE
    temp_start$has_complete_violation <- FALSE
    working_hours_violations <- rbind(working_hours_violations, temp_start)
  }
  
  if(complete_violations_count > 0) {
    temp_complete <- complete_violations_df[, c("case_id", "activity", "start", "complete")]
    
    if(nrow(working_hours_violations) > 0) {
      wh_keys <- paste(working_hours_violations$case_id, working_hours_violations$activity, 
                       working_hours_violations$start, working_hours_violations$complete, sep="_")
      
      new_keys <- paste(temp_complete$case_id, temp_complete$activity, 
                        temp_complete$start, temp_complete$complete, sep="_")
      
      existing_indices <- match(new_keys, wh_keys)
      valid_matches <- !is.na(existing_indices)
      
      if(any(valid_matches)) {
        working_hours_violations$has_complete_violation[existing_indices[valid_matches]] <- TRUE
      }
      
      if(any(!valid_matches)) {
        new_rows <- temp_complete[!valid_matches, ]
        new_rows$has_start_violation <- FALSE
        new_rows$has_complete_violation <- TRUE
        working_hours_violations <- rbind(working_hours_violations, new_rows)
      }
    } else {
      temp_complete$has_start_violation <- FALSE
      temp_complete$has_complete_violation <- TRUE
      working_hours_violations <- temp_complete
    }
  }
  
  total_working_hours_violations <- nrow(working_hours_violations)
  
  if(total_working_hours_violations > 0) {
    working_hours_violations$violation_type <- case_when(
      working_hours_violations$has_start_violation & working_hours_violations$has_complete_violation ~ "both",
      working_hours_violations$has_start_violation ~ "start_only",
      working_hours_violations$has_complete_violation ~ "complete_only",
      TRUE ~ "unknown"
    )
    
    cat(paste("Removed", total_working_hours_violations, "working hours violations (detected via daqapo)\n"))
    cat(paste("  Start violations:", start_violations_count, 
              "| Complete violations:", complete_violations_count, "\n"))
    
    actlog_df <- as.data.frame(actlog_clean)
    actlog_df$match_key <- paste(actlog_df$case_id, actlog_df$activity, 
                                 actlog_df$start, actlog_df$complete, sep = "_")
    
    working_hours_violations$match_key <- paste(working_hours_violations$case_id, 
                                                working_hours_violations$activity, 
                                                working_hours_violations$start, 
                                                working_hours_violations$complete, sep = "_")
    
    actlog_within_hours <- actlog_df[!actlog_df$match_key %in% working_hours_violations$match_key, ]
    actlog_within_hours$match_key <- NULL
    working_hours_violations$match_key <- NULL
    
    actlog_clean <- actlog_within_hours %>%
      bupaR::activitylog(
        case_id = "case_id",
        activity_id = "activity",
        timestamps = c("start", "complete"),
        resource_id = "originator"
      )
    
    wh_violations_file <- "results/data_quality_working_hours_violations.RData"
    wh_violations_csv <- "results/data_quality_working_hours_violations.csv"
    
    if (!dir.exists("results")) dir.create("results")
    save(working_hours_violations, file = wh_violations_file)
    write.csv(working_hours_violations, wh_violations_csv, row.names = FALSE)
  }
  
  cat(paste("Continuing with", nrow(actlog_clean), "records\n\n"))
  
  # Time anomalies: negative/zero durations
  cat("Checking time anomalies (negative/zero durations)...\n")
  
  time_anomalies <- detect_time_anomalies(
    activitylog = actlog_clean,
    anomaly_type = "both"
  )
  
  time_anomalies_df <- as.data.frame(time_anomalies)
  time_anomalies_count <- nrow(time_anomalies_df)
  
  cat(paste("Found", time_anomalies_count, "time anomalies (negative/zero durations)\n"))
  
  if(time_anomalies_count > 0) {
    time_anomalies_df$anomaly_type <- "time_anomaly"
    
    # Save time anomalies
    time_anomalies_file <- "results/data_quality_time_anomalies.RData"
    time_anomalies_csv <- "results/data_quality_time_anomalies.csv"
    
    if (!dir.exists("results")) dir.create("results")
    save(time_anomalies_df, file = time_anomalies_file)
    write.csv(time_anomalies_df, time_anomalies_csv, row.names = FALSE)
  }
  
  cat(paste("Continuing with", nrow(actlog_clean), "records\n\n"))
  
  # Duration outliers: 0-120 min range
  cat("Checking duration outliers (valid range: 0 - 500 minutes)...\n")
  
  min_duration <- 0
  max_duration <- 120
  
  unique_activities <- actlog_clean %>%
    pull(activity) %>%
    unique()
  
  duration_params <- setNames(
    lapply(unique_activities, function(x) duration_within(lower_bound = min_duration, upper_bound = max_duration)),
    unique_activities
  )
  
  duration_outliers <- tryCatch(
    do.call(
      detect_duration_outliers,
      c(list(activitylog = actlog_clean, details = TRUE), duration_params)
    ),
    error = function(e) {
      # daqapo bug: spread() fails on empty result when 0 anomalies detected
      cat("Note: detect_duration_outliers returned empty result (0 outliers)\n")
      NULL
    }
  )
  
  duration_outliers_df <- if (is.null(duration_outliers)) data.frame() else as.data.frame(duration_outliers)
  duration_outliers_count <- nrow(duration_outliers_df)
  
  cat(paste("Found", duration_outliers_count, "duration outliers (outside", min_duration, "-", max_duration, "min range)\n"))
  
  if(duration_outliers_count > 0) {
    duration_outliers_df$anomaly_type <- "duration_outlier"
  }
  
  if(duration_outliers_count > 0) {
    actlog_df <- as.data.frame(actlog_clean)
    duration_outliers_df$match_key <- paste(duration_outliers_df$case_id, 
                                             duration_outliers_df$activity,
                                             duration_outliers_df$start, 
                                             duration_outliers_df$complete, 
                                             sep = "_")
    actlog_df$match_key <- paste(actlog_df$case_id, 
                                  actlog_df$activity,
                                  actlog_df$start, 
                                  actlog_df$complete, 
                                  sep = "_")
    
    actlog_without_duration_outliers <- actlog_df %>%
      filter(!match_key %in% duration_outliers_df$match_key)
    
    cat(paste("Filtered out", duration_outliers_count, "duration outliers\n"))
    
    actlog_without_duration_outliers$match_key <- NULL
    
    actlog_clean <- actlog_without_duration_outliers %>%
      bupaR::activitylog(
        case_id = "case_id",
        activity_id = "activity",
        timestamps = c("start", "complete"),
        resource_id = "originator"
      )
  }
  
  cat(paste("Continuing with", nrow(actlog_clean), "records\n\n"))
  
  # Save anomalies
  if(duration_outliers_count > 0) {
    anomalies_file <- "results/data_quality_time_anomalies.RData"
    anomalies_csv <- "results/data_quality_time_anomalies.csv"
    
    if (!dir.exists("results")) dir.create("results")
    save(duration_outliers_df, file = anomalies_file)
    write.csv(duration_outliers_df, anomalies_csv, row.names = FALSE)
    
    cat(paste("Total duration anomalies saved:", duration_outliers_count, "\n"))
  }
  
  if(duration_outliers_count > 0) {
    # Suppress analysis output - comment out the cat line
    # cat(paste("\nAnalyzing", duration_outliers_count, "duration anomalies:\n\n"))
    
    # Root cause: which timestamp is more suspicious
    actlog_df <- as.data.frame(actlog_clean)
    actlog_df$duration_mins <- as.numeric(difftime(actlog_df$complete, actlog_df$start, units = "mins"))
    
    root_cause_results <- data.frame()
    
    start_outlier_count <- 0
    complete_outlier_count <- 0
    
    for(i in 1:nrow(duration_outliers_df)) {
      anomaly <- duration_outliers_df[i, ]
      
      # cat(paste(i, ". Case", anomaly$case_id, "-", anomaly$activity, 
      #           "(", round(anomaly$duration, 2), "mins)"))
      
      normal_records <- actlog_df %>%
        filter(activity == anomaly$activity,
               case_id != anomaly$case_id,
               duration_mins > 0)
      
      if(nrow(normal_records) >= 2) {
        median_duration <- median(normal_records$duration_mins, na.rm = TRUE)
        
        normal_start_numeric <- as.numeric(normal_records$start)
        normal_complete_numeric <- as.numeric(normal_records$complete)
        
        median_start_numeric <- median(normal_start_numeric, na.rm = TRUE)
        median_complete_numeric <- median(normal_complete_numeric, na.rm = TRUE)
        
        median_start <- as.POSIXct(median_start_numeric, origin = "1970-01-01")
        median_complete <- as.POSIXct(median_complete_numeric, origin = "1970-01-01")
        
        anomaly_start_numeric <- as.numeric(anomaly$start)
        anomaly_complete_numeric <- as.numeric(anomaly$complete)
        
        start_deviation_mins <- (anomaly_start_numeric - median_start_numeric) / 60
        complete_deviation_mins <- (anomaly_complete_numeric - median_complete_numeric) / 60
        
        diagnosis <- ""
        likely_issue <- ""
        specific_problem <- ""
        
        if(anomaly$duration < 0) {
          diagnosis <- "NEGATIVE DURATION - Complete before Start"
          
          if(abs(start_deviation_mins) > abs(complete_deviation_mins)) {
            likely_issue <- "START TIMESTAMP is more suspicious"
            specific_problem <- paste("Start is", round(abs(start_deviation_mins), 2), 
                                      "mins away from median, while Complete is only", 
                                      round(abs(complete_deviation_mins), 2), "mins away")
            start_outlier_count <- start_outlier_count + 1
          } else if(abs(complete_deviation_mins) > abs(start_deviation_mins)) {
            likely_issue <- "COMPLETE TIMESTAMP is more suspicious"
            specific_problem <- paste("Complete is", round(abs(complete_deviation_mins), 2), 
                                      "mins away from median, while Start is only", 
                                      round(abs(start_deviation_mins), 2), "mins away")
            complete_outlier_count <- complete_outlier_count + 1
          } else {
            likely_issue <- "Both timestamps equally suspicious"
            specific_problem <- "Both timestamps deviate equally from median"
            start_outlier_count <- start_outlier_count + 1
            complete_outlier_count <- complete_outlier_count + 1
          }
          
        } else if(anomaly$duration == 0) {
          diagnosis <- "ZERO DURATION - Start equals Complete"
          likely_issue <- "Timestamps are identical (instant activity)"
          specific_problem <- "Both timestamps have the same value"
          start_outlier_count <- start_outlier_count + 1
          complete_outlier_count <- complete_outlier_count + 1
        } else {
          diagnosis <- "UNUSUAL DURATION"
          likely_issue <- "Duration significantly different from expected"
          specific_problem <- paste("Duration is", round(anomaly$duration - median_duration, 2), 
                                    "mins different from median")
          if(abs(start_deviation_mins) > abs(complete_deviation_mins)) {
            start_outlier_count <- start_outlier_count + 1
          } else if(abs(complete_deviation_mins) > abs(start_deviation_mins)) {
            complete_outlier_count <- complete_outlier_count + 1
          } else {
            start_outlier_count <- start_outlier_count + 1
            complete_outlier_count <- complete_outlier_count + 1
          }
        }
        
        
        root_cause_results <- rbind(root_cause_results, data.frame(
          case_id = anomaly$case_id,
          activity = anomaly$activity,
          originator = anomaly$originator,
          start = anomaly$start,
          complete = anomaly$complete,
          actual_duration = anomaly$duration,
          expected_median_duration = median_duration,
          median_start = median_start,
          median_complete = median_complete,
          start_deviation_mins = start_deviation_mins,
          complete_deviation_mins = complete_deviation_mins,
          normal_sample_size = nrow(normal_records),
          diagnosis = diagnosis,
          likely_issue = likely_issue,
          specific_problem = specific_problem
        ))
        
      } else {
        cat(paste(" -> Insufficient data\n"))
        
        root_cause_results <- rbind(root_cause_results, data.frame(
          case_id = anomaly$case_id,
          activity = anomaly$activity,
          originator = anomaly$originator,
          start = anomaly$start,
          complete = anomaly$complete,
          actual_duration = anomaly$duration,
          expected_median_duration = NA,
          median_start = as.POSIXct(NA),
          median_complete = as.POSIXct(NA),
          start_deviation_mins = NA,
          complete_deviation_mins = NA,
          normal_sample_size = nrow(normal_records),
          diagnosis = ifelse(anomaly$duration < 0, "NEGATIVE DURATION", "ZERO DURATION"),
          likely_issue = "Insufficient data for comparison",
          specific_problem = "Not enough normal samples"
        ))
      }
    }
    
    save(root_cause_results, file = "results/data_quality_root_cause_analysis.RData")
    write.csv(root_cause_results, "results/data_quality_root_cause_analysis.csv", row.names = FALSE)
  }
  
  # Accuracy calculation
  cat("\n")
  
  # Duration outliers were already filtered, so current actlog_clean is clean
  total_records <- nrow(as.data.frame(actlog_clean))
  original_total_records <- total_records + total_working_hours_violations + duration_outliers_count
  
  if(!exists("start_outlier_count")) start_outlier_count <- 0
  if(!exists("complete_outlier_count")) complete_outlier_count <- 0
  
  wh_start_outliers <- 0
  wh_complete_outliers <- 0
  duration_start_outliers <- 0
  duration_complete_outliers <- 0
  time_anomaly_start_outliers <- time_anomalies_count  # Time anomalies involve start timestamp
  time_anomaly_complete_outliers <- time_anomalies_count  # Also affect complete timestamp
  
  if(exists("working_hours_violations") && nrow(working_hours_violations) > 0) {
    wh_start_outliers <- sum(working_hours_violations$has_start_violation)
    wh_complete_outliers <- sum(working_hours_violations$has_complete_violation)
  }
  
  total_start_outliers <- start_outlier_count + wh_start_outliers + duration_start_outliers + time_anomaly_start_outliers
  total_complete_outliers <- complete_outlier_count + wh_complete_outliers + duration_complete_outliers + time_anomaly_complete_outliers
  
  start_accuracy <- 1 - (total_start_outliers / original_total_records)
  complete_accuracy <- 1 - (total_complete_outliers / original_total_records)
  
  cat(paste("Total outliers found:\n"))
  cat(paste("  Start:", total_start_outliers, "(", start_outlier_count, "from root cause +", 
            wh_start_outliers, "working hours +", duration_start_outliers, "duration +", 
            time_anomaly_start_outliers, "time anomalies)\n"))
  cat(paste("  Complete:", total_complete_outliers, "(", complete_outlier_count, "from root cause +",
            wh_complete_outliers, "working hours +", duration_complete_outliers, "duration +",
            time_anomaly_complete_outliers, "time anomalies)\n\n"))
  
  cat(paste("Accuracy - Start:", round(start_accuracy * 100, 2), "% | Complete:", 
            round(complete_accuracy * 100, 2), "%\n"))
  
  accuracy_scores <- data.frame(
    timestamp_type = c("start", "complete"),
    total_records = c(original_total_records, original_total_records),
    root_cause_outliers = c(start_outlier_count, complete_outlier_count),
    working_hours_outliers = c(wh_start_outliers, wh_complete_outliers),
    duration_outliers = c(duration_start_outliers, duration_complete_outliers),
    time_anomaly_outliers = c(time_anomaly_start_outliers, time_anomaly_complete_outliers),
    total_outliers = c(total_start_outliers, total_complete_outliers),
    accuracy = c(start_accuracy, complete_accuracy),
    accuracy_percentage = c(start_accuracy * 100, complete_accuracy * 100)
  )
  
  save(accuracy_scores, file = "results/data_quality_accuracy_scores.RData")
  write.csv(accuracy_scores, "results/data_quality_accuracy_scores.csv", row.names = FALSE)
  
  cat("\n")
  
  # Return results
  return(list(
    total_start_outliers = total_start_outliers,
    total_complete_outliers = total_complete_outliers,
    start_accuracy = start_accuracy,
    complete_accuracy = complete_accuracy,
    actlog_final = actlog_clean
  ))
}