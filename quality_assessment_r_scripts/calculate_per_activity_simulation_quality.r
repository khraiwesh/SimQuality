# Per-Activity Simulation Parameter Quality
# For each activity, filters the event log and runs the same quality functions:
# 1. Timestamp C/A/Co → Duration Quality
# 2. Resource C/A/Co → Resource Allocation Quality
# 3. Supplementary: entropy (structural) and CV (behavioral) as descriptive metrics

library(dplyr)
library(bupaR)
library(daqapo)
library(stringr)

calculate_per_activity_simulation_quality <- function(
    actlog_df,
    data_file,
    working_hours_start = 0,
    working_hours_end = 24,
    inactive_threshold_minutes = 60,
    dimension_weights = c(completeness = 1/3, accuracy = 1/3, consistency = 1/3)
) {
  
  cat("\n############################################\n")
  cat("PER-ACTIVITY SIMULATION PARAMETER QUALITY\n")
  cat("############################################\n\n")
  
  activities <- unique(actlog_df$activity)
  activities <- activities[!is.na(activities)]
  
  cat(paste("Activities to analyze:", length(activities), "\n"))
  cat(paste("Activities:", paste(activities, collapse = ", "), "\n\n"))
  
  all_results <- list()
  summary_rows <- list()
  
  # Format detection for timestamp consistency
  formats_to_check <- list(
    "dd/mm/yyyy HH:MM:SS" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd.mm.yyyy HH:MM:SS" = "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd.Mmm.yyyy HH:MM:SS" = "^\\d{2}\\.[A-Z][a-z]{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd/mm/yyyy HH:MM" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}$",
    "dd.mm.yyyy HH:MM" = "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}$",
    "yyyy-mm-dd HH:MM:SS" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  )
  
  detect_format <- function(timestamp_str) {
    for (format_name in names(formats_to_check)) {
      pattern <- formats_to_check[[format_name]]
      if (grepl(pattern, as.character(timestamp_str))) {
        return(format_name)
      }
    }
    return("Other")
  }
  
  # Resource naming format classifier (same as calculate_resource_consistency.r)
  classify_format <- function(resource_name) {
    x <- trimws(resource_name)
    if (grepl("^[A-Za-z]+(\\s|\\u00A0)+\\d+$", x)) {
      return("Word Number")
    } else if (grepl("^[A-Za-z]+\\s[A-Za-z]+$", x)) {
      return("Two Words")
    } else if (grepl("^[A-Za-z]+_\\d+$", x)) {
      return("Word_Number")
    } else if (grepl("^[A-Za-z]+-\\d+$", x)) {
      return("Word-Number")
    } else if (grepl("^\\d+$", x)) {
      return("Numeric")
    } else if (grepl("^[A-Za-z]+$", x)) {
      return("Single Word")
    } else if (grepl("^[A-Za-z0-9]+$", x)) {
      return("Alphanumeric")
    } else {
      return("Other")
    }
  }
  
  w <- dimension_weights / sum(dimension_weights)
  
  for (act in activities) {
    
    cat("============================================\n")
    cat(paste("ACTIVITY:", act, "\n"))
    cat("============================================\n\n")
    
    # Filter event log to this activity only
    act_subset <- actlog_df[actlog_df$activity == act & !is.na(actlog_df$activity), ]
    n_events <- nrow(act_subset)
    
    cat(paste("Events:", n_events, "\n"))
    cat(paste("Cases:", n_distinct(act_subset$case_id), "\n\n"))
    
    if (n_events < 2) {
      cat("Too few events, skipping.\n\n")
      next
    }
    
    # ========================================
    # 1. TIMESTAMP C/A/Co → DURATION QUALITY
    # ========================================
    
    cat("--- TIMESTAMP QUALITY ---\n")
    
    # Completeness
    start_missing <- sum(is.na(act_subset$start))
    complete_missing <- sum(is.na(act_subset$complete))
    start_completeness <- (n_events - start_missing) / n_events
    complete_completeness <- (n_events - complete_missing) / n_events
    
    cat(paste("  Completeness  Start:", round(start_completeness * 100, 2),
              "%  Complete:", round(complete_completeness * 100, 2), "%\n"))
    
    # Accuracy
    act_clean <- act_subset[!is.na(act_subset$start) & !is.na(act_subset$complete), ]
    n_clean <- nrow(act_clean)
    
    start_accuracy <- 1.0
    complete_accuracy <- 1.0
    
    if (n_clean > 0) {
      start_hours <- as.numeric(format(act_clean$start, "%H"))
      complete_hours <- as.numeric(format(act_clean$complete, "%H"))
      
      start_wh_violations <- sum(start_hours < working_hours_start | start_hours >= working_hours_end, na.rm = TRUE)
      complete_wh_violations <- sum(complete_hours < working_hours_start | complete_hours >= working_hours_end, na.rm = TRUE)
      
      durations <- as.numeric(difftime(act_clean$complete, act_clean$start, units = "mins"))
      time_anomalies <- sum(durations <= 0, na.rm = TRUE)
      
      valid_durations <- durations[durations > 0]
      duration_outliers <- 0
      if (length(valid_durations) >= 4) {
        q1 <- quantile(valid_durations, 0.25)
        q3 <- quantile(valid_durations, 0.75)
        iqr <- q3 - q1
        iqr_multiplier <- if (length(valid_durations) < 30) 2.0 else 1.5
        lower <- max(0, q1 - iqr_multiplier * iqr)
        upper <- q3 + iqr_multiplier * iqr
        duration_outliers <- sum(valid_durations < lower | valid_durations > upper)
      }
      
      total_start_outliers <- start_wh_violations + time_anomalies + duration_outliers
      total_complete_outliers <- complete_wh_violations + time_anomalies + duration_outliers
      
      start_accuracy <- max(0, (n_clean - total_start_outliers) / n_clean)
      complete_accuracy <- max(0, (n_clean - total_complete_outliers) / n_clean)
    }
    
    cat(paste("  Accuracy      Start:", round(start_accuracy * 100, 2),
              "%  Complete:", round(complete_accuracy * 100, 2), "%\n"))
    
    # Consistency (format)
    start_consistency <- 1.0
    complete_consistency <- 1.0
    
    if (n_clean > 0) {
      start_strs <- as.character(act_clean$start)
      complete_strs <- as.character(act_clean$complete)
      
      start_formats <- sapply(start_strs, detect_format)
      complete_formats <- sapply(complete_strs, detect_format)
      
      start_dominant_count <- max(table(start_formats))
      complete_dominant_count <- max(table(complete_formats))
      
      start_consistency <- start_dominant_count / n_clean
      complete_consistency <- complete_dominant_count / n_clean
    }
    
    cat(paste("  Consistency   Start:", round(start_consistency * 100, 2),
              "%  Complete:", round(complete_consistency * 100, 2), "%\n"))
    
    # Derive duration quality
    start_ts_score <- w["completeness"] * start_completeness + w["accuracy"] * start_accuracy + w["consistency"] * start_consistency
    complete_ts_score <- w["completeness"] * complete_completeness + w["accuracy"] * complete_accuracy + w["consistency"] * complete_consistency
    duration_quality <- min(start_ts_score, complete_ts_score)
    
    cat(paste("  → Duration Quality:", round(duration_quality * 100, 2), "%\n\n"))
    
    # ========================================
    # 2. RESOURCE C/A/Co → RESOURCE QUALITY
    # ========================================
    
    cat("--- RESOURCE QUALITY ---\n")
    
    resources <- act_subset$originator
    
    # Completeness: non-NA, non-blank resource values
    resource_missing <- sum(is.na(resources) | trimws(resources) == "")
    resource_completeness <- (n_events - resource_missing) / n_events
    
    cat(paste("  Completeness:", round(resource_completeness * 100, 2), "%\n"))
    
    # Accuracy: resource time conflicts (same activity, same time, different resources in same case)
    resource_accuracy <- 1.0
    valid_res_events <- act_subset[!is.na(act_subset$originator) & trimws(act_subset$originator) != "" &
                                     !is.na(act_subset$start) & !is.na(act_subset$complete), ]
    n_valid_res <- nrow(valid_res_events)
    
    if (n_valid_res > 1) {
      conflict_count <- 0
      
      case_groups <- valid_res_events %>%
        group_by(case_id) %>%
        filter(n_distinct(originator) > 1) %>%
        group_split()
      
      for (grp in case_groups) {
        if (nrow(grp) < 2) next
        for (i in 1:(nrow(grp) - 1)) {
          for (j in (i + 1):nrow(grp)) {
            if (grp$originator[i] != grp$originator[j]) {
              if (grp$start[i] < grp$complete[j] && grp$start[j] < grp$complete[i]) {
                conflict_count <- conflict_count + 1
              }
            }
          }
        }
      }
      
      resource_accuracy <- max(0, (n_valid_res - conflict_count) / n_valid_res)
    }
    
    cat(paste("  Accuracy:", round(resource_accuracy * 100, 2), "%\n"))
    
    # Consistency: naming format consistency
    resource_consistency <- 1.0
    valid_resources <- resources[!is.na(resources) & trimws(resources) != ""]
    
    if (length(valid_resources) > 0) {
      res_formats <- sapply(valid_resources, classify_format)
      format_table <- table(res_formats)
      dominant_count <- max(format_table)
      resource_consistency <- dominant_count / length(valid_resources)
    }
    
    cat(paste("  Consistency:", round(resource_consistency * 100, 2), "%\n"))
    
    # Derive resource quality
    resource_score <- w["completeness"] * resource_completeness + w["accuracy"] * resource_accuracy + w["consistency"] * resource_consistency
    
    cat(paste("  → Resource Quality:", round(resource_score * 100, 2), "%\n\n"))
    
    # ========================================
    # 3. SUPPLEMENTARY: RELATIONSHIP METRICS
    # ========================================
    
    cat("--- SUPPLEMENTARY (descriptive) ---\n")
    
    # Entropy: how distributed are resources for this activity?
    unique_resources <- length(unique(valid_resources))
    entropy <- NA
    normalized_entropy <- NA
    
    if (unique_resources == 1) {
      entropy <- 0
      normalized_entropy <- 0
    } else if (unique_resources > 1) {
      resource_counts <- table(valid_resources)
      proportions <- as.numeric(resource_counts) / sum(resource_counts)
      entropy <- -sum(proportions * log2(proportions))
      max_entropy <- log2(unique_resources)
      normalized_entropy <- entropy / max_entropy
    }
    
    cat(paste("  Unique resources:", unique_resources, "\n"))
    cat(paste("  Resource entropy:", round(ifelse(is.na(entropy), 0, entropy), 4), "\n"))
    
    # CV: duration variability across all events of this activity
    avg_duration <- NA
    sd_duration <- NA
    cv <- NA
    
    if (n_clean > 0) {
      durations_sec <- as.numeric(difftime(act_clean$complete, act_clean$start, units = "secs"))
      valid_dur <- durations_sec[durations_sec > 0]
      if (length(valid_dur) > 1) {
        avg_duration <- mean(valid_dur)
        sd_duration <- sd(valid_dur)
        cv <- sd_duration / avg_duration
      }
    }
    
    cat(paste("  Duration CV:", round(ifelse(is.na(cv), 0, cv), 4), "\n"))
    
    # Confidence factor
    confidence <- min(1, log(n_events + 1) / log(101))
    
    cat(paste("  Confidence:", round(confidence, 4), "\n\n"))
    
    # Store results
    all_results[[act]] <- list(
      activity = act,
      n_events = n_events,
      # Timestamp C/A/Co
      start_completeness = start_completeness,
      complete_completeness = complete_completeness,
      start_accuracy = start_accuracy,
      complete_accuracy = complete_accuracy,
      start_consistency = start_consistency,
      complete_consistency = complete_consistency,
      start_ts_combined = start_ts_score,
      complete_ts_combined = complete_ts_score,
      duration_quality = duration_quality,
      # Resource C/A/Co
      resource_completeness = resource_completeness,
      resource_accuracy = resource_accuracy,
      resource_consistency = resource_consistency,
      resource_quality = resource_score,
      # Supplementary
      entropy = entropy,
      normalized_entropy = normalized_entropy,
      cv = cv
    )
    
    summary_rows[[act]] <- data.frame(
      Activity = act,
      Event_Count = n_events,
      # Timestamp scores
      Start_Completeness = round(start_completeness, 4),
      Complete_Completeness = round(complete_completeness, 4),
      Start_Accuracy = round(start_accuracy, 4),
      Complete_Accuracy = round(complete_accuracy, 4),
      Start_Consistency = round(start_consistency, 4),
      Complete_Consistency = round(complete_consistency, 4),
      Start_TS_Combined = round(start_ts_score, 4),
      Complete_TS_Combined = round(complete_ts_score, 4),
      Duration_Quality = round(duration_quality, 4),
      # Resource scores
      Resource_Completeness = round(resource_completeness, 4),
      Resource_Accuracy = round(resource_accuracy, 4),
      Resource_Consistency = round(resource_consistency, 4),
      Resource_Quality = round(resource_score, 4),
      # Supplementary
      Unique_Resources = unique_resources,
      Normalized_Entropy = round(ifelse(is.na(normalized_entropy), 0, normalized_entropy), 4),
      Duration_CV = round(ifelse(is.na(cv), 0, cv), 4),
      Confidence = round(confidence, 4),
      stringsAsFactors = FALSE
    )
  }
  
  # Combine into summary table
  summary_table <- do.call(rbind, summary_rows)
  rownames(summary_table) <- NULL
  
  # Print summary
  cat("============================================\n")
  cat("PER-ACTIVITY QUALITY SUMMARY\n")
  cat("============================================\n\n")
  
  print(summary_table[, c("Activity", "Event_Count", "Duration_Quality", "Resource_Quality",
                           "Normalized_Entropy", "Duration_CV", "Confidence")])
  
  avg_duration <- mean(summary_table$Duration_Quality, na.rm = TRUE)
  min_duration <- min(summary_table$Duration_Quality, na.rm = TRUE)
  avg_resource <- mean(summary_table$Resource_Quality, na.rm = TRUE)
  min_resource <- min(summary_table$Resource_Quality, na.rm = TRUE)
  
  cat(paste("\nDuration Quality — Average:", round(avg_duration * 100, 2),
            "% | Minimum:", round(min_duration * 100, 2), "%\n"))
  cat(paste("Resource Quality — Average:", round(avg_resource * 100, 2),
            "% | Minimum:", round(min_resource * 100, 2), "%\n\n"))
  
  # Save results
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  
  write.csv(summary_table,
            file.path(results_dir, "per_activity_quality_detailed.csv"),
            row.names = FALSE)
  cat("Saved to: results/per_activity_quality_detailed.csv\n")
  
  return(list(
    per_activity = all_results,
    summary = summary_table,
    avg_duration_quality = avg_duration,
    min_duration_quality = min_duration,
    avg_resource_quality = avg_resource,
    min_resource_quality = min_resource
  ))
}


# ============================================
# PER RESOURCE-ACTIVITY PAIR QUALITY
# ============================================
# Filters event log to each (resource, activity) pair,
# runs the same timestamp C/A/Co checks, derives duration quality

calculate_per_resource_activity_quality <- function(
    actlog_df,
    working_hours_start = 0,
    working_hours_end = 24,
    dimension_weights = c(completeness = 1/3, accuracy = 1/3, consistency = 1/3)
) {
  
  cat("\n############################################\n")
  cat("PER RESOURCE-ACTIVITY PAIR QUALITY\n")
  cat("############################################\n\n")
  
  # Get all resource-activity pairs
  pairs <- actlog_df %>%
    filter(!is.na(activity) & !is.na(originator)) %>%
    distinct(originator, activity) %>%
    arrange(originator, activity)
  
  cat(paste("Resource-Activity pairs to analyze:", nrow(pairs), "\n\n"))
  
  formats_to_check <- list(
    "dd/mm/yyyy HH:MM:SS" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd.mm.yyyy HH:MM:SS" = "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd.Mmm.yyyy HH:MM:SS" = "^\\d{2}\\.[A-Z][a-z]{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd/mm/yyyy HH:MM" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}$",
    "dd.mm.yyyy HH:MM" = "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}$",
    "yyyy-mm-dd HH:MM:SS" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  )
  
  detect_format <- function(timestamp_str) {
    for (format_name in names(formats_to_check)) {
      pattern <- formats_to_check[[format_name]]
      if (grepl(pattern, as.character(timestamp_str))) {
        return(format_name)
      }
    }
    return("Other")
  }
  
  summary_rows <- list()
  
  for (i in 1:nrow(pairs)) {
    res <- pairs$originator[i]
    act <- pairs$activity[i]
    
    # Filter to this resource-activity pair
    subset_df <- actlog_df[actlog_df$originator == res & actlog_df$activity == act &
                             !is.na(actlog_df$originator) & !is.na(actlog_df$activity), ]
    n_events <- nrow(subset_df)
    
    if (n_events < 2) next
    
    cat(paste("  ", res, "+", act, "(", n_events, "events )...\n"))
    
    # --- COMPLETENESS ---
    start_missing <- sum(is.na(subset_df$start))
    complete_missing <- sum(is.na(subset_df$complete))
    start_completeness <- (n_events - start_missing) / n_events
    complete_completeness <- (n_events - complete_missing) / n_events
    
    # --- ACCURACY ---
    clean <- subset_df[!is.na(subset_df$start) & !is.na(subset_df$complete), ]
    n_clean <- nrow(clean)
    start_accuracy <- 1.0
    complete_accuracy <- 1.0
    
    if (n_clean > 0) {
      start_hours <- as.numeric(format(clean$start, "%H"))
      complete_hours <- as.numeric(format(clean$complete, "%H"))
      
      start_wh_violations <- sum(start_hours < working_hours_start | start_hours >= working_hours_end, na.rm = TRUE)
      complete_wh_violations <- sum(complete_hours < working_hours_start | complete_hours >= working_hours_end, na.rm = TRUE)
      
      durations <- as.numeric(difftime(clean$complete, clean$start, units = "mins"))
      time_anomalies <- sum(durations <= 0, na.rm = TRUE)
      
      valid_durations <- durations[durations > 0]
      duration_outliers <- 0
      if (length(valid_durations) >= 4) {
        q1 <- quantile(valid_durations, 0.25)
        q3 <- quantile(valid_durations, 0.75)
        iqr <- q3 - q1
        iqr_multiplier <- if (length(valid_durations) < 30) 2.0 else 1.5
        lower <- max(0, q1 - iqr_multiplier * iqr)
        upper <- q3 + iqr_multiplier * iqr
        duration_outliers <- sum(valid_durations < lower | valid_durations > upper)
      }
      
      total_start_outliers <- start_wh_violations + time_anomalies + duration_outliers
      total_complete_outliers <- complete_wh_violations + time_anomalies + duration_outliers
      
      start_accuracy <- max(0, (n_clean - total_start_outliers) / n_clean)
      complete_accuracy <- max(0, (n_clean - total_complete_outliers) / n_clean)
    }
    
    # --- CONSISTENCY ---
    start_consistency <- 1.0
    complete_consistency <- 1.0
    
    if (n_clean > 0) {
      start_strs <- as.character(clean$start)
      complete_strs <- as.character(clean$complete)
      
      start_formats <- sapply(start_strs, detect_format)
      complete_formats <- sapply(complete_strs, detect_format)
      
      start_format_table <- table(start_formats)
      complete_format_table <- table(complete_formats)
      
      start_consistency <- max(start_format_table) / n_clean
      complete_consistency <- max(complete_format_table) / n_clean
    }
    
    # --- DERIVED SCORES (weighted) ---
    w <- dimension_weights / sum(dimension_weights)
    start_ts_score <- w["completeness"] * start_completeness + w["accuracy"] * start_accuracy + w["consistency"] * start_consistency
    complete_ts_score <- w["completeness"] * complete_completeness + w["accuracy"] * complete_accuracy + w["consistency"] * complete_consistency
    duration_quality <- min(start_ts_score, complete_ts_score)
    
    # --- DURATION STATS ---
    avg_duration <- NA
    sd_duration <- NA
    cv <- NA
    cv_quality <- 1.0  # Default: no penalty if CV can't be computed
    if (n_clean > 0) {
      durations_sec <- as.numeric(difftime(clean$complete, clean$start, units = "secs"))
      valid <- durations_sec[durations_sec > 0]
      if (length(valid) > 1) {
        avg_duration <- round(mean(valid), 2)
        sd_duration <- round(sd(valid), 2)
        cv <- round(sd_duration / avg_duration, 4)
        # CV quality: CV=0 → 1.0, CV=1 → 0.0 (high variability = low quality)
        cv_quality <- max(0, 1 - cv)
      }
    }
    
    # Confidence factor based on sample size
    confidence <- min(1, log(n_events + 1) / log(101))
    
    # Adjusted duration quality: integrate CV as behavioral consistency component
    # 80% structural (C/A/Co) + 20% behavioral (CV-based)
    adjusted_duration_quality <- 0.8 * duration_quality + 0.2 * cv_quality
    
    summary_rows[[paste(res, act, sep = "||")]] <- data.frame(
      Resource = res,
      Activity = act,
      Event_Count = n_events,
      Start_Completeness = round(start_completeness, 4),
      Complete_Completeness = round(complete_completeness, 4),
      Start_Accuracy = round(start_accuracy, 4),
      Complete_Accuracy = round(complete_accuracy, 4),
      Start_Consistency = round(start_consistency, 4),
      Complete_Consistency = round(complete_consistency, 4),
      Start_TS_Combined = round(start_ts_score, 4),
      Complete_TS_Combined = round(complete_ts_score, 4),
      Duration_Quality = round(duration_quality, 4),
      CV_Quality = round(cv_quality, 4),
      Adjusted_Duration_Quality = round(adjusted_duration_quality, 4),
      Confidence = round(confidence, 4),
      Avg_Duration_Min = round(avg_duration / 60, 2),
      StdDev_Duration_Min = round(sd_duration / 60, 2),
      CV = cv,
      stringsAsFactors = FALSE
    )
  }
  
  summary_table <- do.call(rbind, summary_rows)
  rownames(summary_table) <- NULL
  summary_table <- summary_table %>% arrange(Duration_Quality)
  
  # Print summary
  cat("\n============================================\n")
  cat("RESOURCE-ACTIVITY PAIR QUALITY SUMMARY\n")
  cat("============================================\n\n")
  
  print(summary_table)
  
  avg_quality <- mean(summary_table$Duration_Quality, na.rm = TRUE)
  min_quality <- min(summary_table$Duration_Quality, na.rm = TRUE)
  
  cat(paste("\nAverage Duration Quality:", round(avg_quality * 100, 2), "%\n"))
  cat(paste("Minimum Duration Quality:", round(min_quality * 100, 2), "%\n\n"))
  
  # Save
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  
  write.csv(summary_table,
            file.path(results_dir, "per_resource_activity_quality_detailed.csv"),
            row.names = FALSE)
  cat("Saved to: results/per_resource_activity_quality_detailed.csv\n")
  
  return(list(
    summary = summary_table,
    avg_quality = avg_quality,
    min_quality = min_quality
  ))
}
