# Timestamp format consistency analysis

calculate_consistency <- function(actlog_clean) {
  
  cat("============================================\n")
  cat("6. CONSISTENCY ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog_df <- as.data.frame(actlog_clean)
  total_records <- nrow(actlog_df)
  
  # Guard: nothing to analyse when all records were filtered out upstream
  if (total_records == 0) {
    cat("No records remaining after accuracy filtering — skipping consistency analysis.\n")
    empty_scores <- data.frame(
      timestamp_type = c("start", "complete"),
      total_records = c(0L, 0L),
      most_common_format = c(NA_character_, NA_character_),
      most_common_count = c(0L, 0L),
      total_outliers = c(0L, 0L),
      consistency = c(1.0, 1.0),
      consistency_percentage = c(100.0, 100.0)
    )
    write.csv(empty_scores, "results/data_quality_consistency_scores.csv", row.names = FALSE)
    save(empty_scores, file = "results/data_quality_consistency_scores.RData")
    return(list(
      start_consistency = 1.0, complete_consistency = 1.0,
      total_start_outliers = 0L, total_complete_outliers = 0L,
      syntactic_start_consistency = 1.0, syntactic_complete_consistency = 1.0,
      behavioral_start_consistency = 1.0, behavioral_complete_consistency = 1.0
    ))
  }

  formats_to_check <- list(
    "dd/mm/yyyy HH:MM:SS" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd.mm.yyyy HH:MM:SS" = "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd.Mmm.yyyy HH:MM:SS" = "^\\d{2}\\.[A-Z][a-z]{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "dd/mm/yyyy HH:MM" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}$",
    "dd.mm.yyyy HH:MM" = "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}$",
    "yyyy-mm-dd HH:MM:SS" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  )
  
  detect_format <- function(timestamp_str) {
    for(format_name in names(formats_to_check)) {
      pattern <- formats_to_check[[format_name]]
      if(grepl(pattern, as.character(timestamp_str))) {
        return(format_name)
      }
    }
    return("Other")
  }
  
  if("start_original" %in% colnames(actlog_df) && "complete_original" %in% colnames(actlog_df)) {
    cat("Using original timestamp formats for consistency analysis\n")
    actlog_df$start_format <- sapply(actlog_df$start_original, detect_format)
    actlog_df$complete_format <- sapply(actlog_df$complete_original, detect_format)
  } else {
    cat("No original formats available - analyzing POSIXct format\n")
    actlog_df$start_format <- sapply(actlog_df$start, detect_format)
    actlog_df$complete_format <- sapply(actlog_df$complete, detect_format)
  }
  
  start_format_counts <- table(actlog_df$start_format)
  complete_format_counts <- table(actlog_df$complete_format)
  
  most_common_start <- max(start_format_counts)
  most_common_complete <- max(complete_format_counts)
  
  most_common_start_name <- names(start_format_counts)[which.max(start_format_counts)]
  most_common_complete_name <- names(complete_format_counts)[which.max(complete_format_counts)]
  
  start_consistency <-  most_common_start / total_records
  complete_consistency <- most_common_complete / total_records
  
  # Outliers = most common format'tan farklı olanlar
  total_start_outliers <- total_records - most_common_start
  total_complete_outliers <- total_records - most_common_complete
  
  cat("Start timestamp formats:\n")
  print(start_format_counts)
  cat(paste("\nMost common format:", most_common_start_name, "-", most_common_start, "records\n"))
  cat(paste("Start Consistency:", round(start_consistency * 100, 2), "%\n"))
  cat(paste("Total outliers - Start:", total_start_outliers, "\n\n"))
  
  cat("Complete timestamp formats:\n")
  print(complete_format_counts)
  cat(paste("\nMost common format:", most_common_complete_name, "-", most_common_complete, "records\n"))
  cat(paste("Complete Consistency:", round(complete_consistency * 100, 2), "%\n"))
  cat(paste("Total outliers - Complete:", total_complete_outliers, "\n\n"))
  
  format_breakdown <- data.frame(
    case_id = actlog_df$case_id,
    activity = actlog_df$activity,
    start = actlog_df$start,
    complete = actlog_df$complete,
    start_format = actlog_df$start_format,
    complete_format = actlog_df$complete_format
  )
  
  consistency_scores <- data.frame(
    timestamp_type = c("start", "complete"),
    total_records = c(total_records, total_records),
    most_common_format = c(most_common_start_name, most_common_complete_name),
    most_common_count = c(most_common_start, most_common_complete),
    total_outliers = c(total_start_outliers, total_complete_outliers),
    consistency = c(start_consistency, complete_consistency),
    consistency_percentage = c(start_consistency * 100, complete_consistency * 100)
  )
  
  save(consistency_scores, file = "results/data_quality_consistency_scores.RData")
  write.csv(consistency_scores, "results/data_quality_consistency_scores.csv", row.names = FALSE)
  
  save(format_breakdown, file = "results/data_quality_format_breakdown.RData")
  write.csv(format_breakdown, "results/data_quality_format_breakdown.csv", row.names = FALSE)
  
  # Behavioral consistency: duration distribution consistency (IQR-based)
  # Check if durations are consistent across the log
  behavioral_start_consistency <- 1.0
  behavioral_complete_consistency <- 1.0
  
  if ("start" %in% colnames(actlog_df) && "complete" %in% colnames(actlog_df)) {
    durations <- as.numeric(difftime(
      as.POSIXct(actlog_df$complete), 
      as.POSIXct(actlog_df$start), 
      units = "mins"
    ))
    valid_durations <- durations[!is.na(durations) & durations > 0]
    
    if (length(valid_durations) >= 4) {
      med <- median(valid_durations)
      iqr_val <- IQR(valid_durations)
      lower <- max(0, med - 1.5 * iqr_val)
      upper <- med + 1.5 * iqr_val
      n_outliers <- sum(valid_durations < lower | valid_durations > upper)
      behavioral_score <- (length(valid_durations) - n_outliers) / length(valid_durations)
      behavioral_start_consistency <- behavioral_score
      behavioral_complete_consistency <- behavioral_score
    }
  }
  
  cat(paste("Syntactic (format) consistency - Start:", round(start_consistency * 100, 2), "%\n"))
  cat(paste("Syntactic (format) consistency - Complete:", round(complete_consistency * 100, 2), "%\n"))
  cat(paste("Behavioral (duration) consistency:", round(behavioral_start_consistency * 100, 2), "%\n"))
  
  return(list(
    start_consistency = start_consistency,
    complete_consistency = complete_consistency,
    total_start_outliers = total_start_outliers,
    total_complete_outliers = total_complete_outliers,
    syntactic_start_consistency = start_consistency,
    syntactic_complete_consistency = complete_consistency,
    behavioral_start_consistency = behavioral_start_consistency,
    behavioral_complete_consistency = behavioral_complete_consistency
  ))
}
