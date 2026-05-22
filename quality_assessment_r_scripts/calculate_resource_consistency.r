# Resource consistency: naming format + behavioral (duration) consistency

library(dplyr)
library(bupaR)
library(stringr)
library(lubridate)

calculate_resource_consistency <- function(dataset, data_file) {
  
  results_dir <- "results_resource"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("RESOURCE CONSISTENCY ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog <- dataset
  actlog_df <- as.data.frame(actlog)
  
  total_records <- nrow(actlog_df)
  
  # Work only with non-missing resources
  actlog_with_resource <- actlog_df %>%
    filter(!is.na(originator) & trimws(originator) != "")
  
  records_with_resource <- nrow(actlog_with_resource)
  unique_resources <- unique(actlog_with_resource$originator)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Records with resource:", records_with_resource, "\n"))
  cat(paste("Unique resource values:", length(unique_resources), "\n\n"))
  
  # ============================================
  # PART A: NAMING FORMAT CONSISTENCY
  # ============================================
  cat("============================================\n")
  cat("PART A: NAMING FORMAT CONSISTENCY\n")
  cat("============================================\n\n")
  
  classify_format <- function(resource_name) {
    x <- trimws(resource_name)
    if (grepl("^[A-Za-z]+([ \\u00A0]+[A-Za-z]+)*[ \\u00A0]+\\d+$", x)) {
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
  
  resource_formats <- data.frame(
    resource = unique_resources,
    format = sapply(unique_resources, classify_format),
    stringsAsFactors = FALSE
  )
  
  cat("Resource format classification:\n")
  print(resource_formats)
  
  resource_format_lookup <- setNames(resource_formats$format, resource_formats$resource)
  actlog_with_resource$resource_format <- resource_format_lookup[actlog_with_resource$originator]
  
  format_counts <- actlog_with_resource %>%
    group_by(resource_format) %>%
    summarise(
      record_count = n(),
      unique_resources = n_distinct(originator),
      .groups = "drop"
    ) %>%
    arrange(desc(record_count))
  
  cat("\nFormat distribution (by records):\n")
  print(format_counts)
  
  dominant_format <- format_counts$resource_format[1]
  dominant_count <- format_counts$record_count[1]
  
  cat(paste("\nDominant format:", dominant_format, "\n"))
  
  naming_inconsistent_count <- sum(actlog_with_resource$resource_format != dominant_format, na.rm = TRUE)
  if (is.na(naming_inconsistent_count)) naming_inconsistent_count <- 0L
  naming_consistent_count <- records_with_resource - naming_inconsistent_count
  
  cat(paste("Naming consistent records:", naming_consistent_count, "\n"))
  cat(paste("Naming inconsistent records:", naming_inconsistent_count, "\n"))
  
  if(isTRUE(naming_inconsistent_count > 0)) {
    inconsistent_file <- file.path(results_dir, "resource_inconsistent_names.csv")
    inconsistent_records <- actlog_with_resource %>% filter(resource_format != dominant_format)
    write.csv(inconsistent_records[, c("case_id", "activity", "originator", "resource_format")], 
              inconsistent_file, row.names = FALSE)
    cat(paste("  Saved to:", inconsistent_file, "\n"))
  }
  
  format_file <- file.path(results_dir, "resource_format_distribution.csv")
  write.csv(format_counts, format_file, row.names = FALSE)
  
  # ============================================
  # PART B: DURATION CONSISTENCY (per resource-activity pair)
  # ============================================
  cat("\n============================================\n")
  cat("PART B: DURATION CONSISTENCY\n")
  cat("============================================\n")
  cat("Checking if each resource takes consistent time for the same activity\n\n")
  
  duration_inconsistent_count <- 0
  
  # Compute duration for each record
  actlog_durations <- actlog_with_resource %>%
    mutate(
      start_ts = as.POSIXct(start),
      complete_ts = as.POSIXct(complete)
    ) %>%
    filter(!is.na(start_ts) & !is.na(complete_ts)) %>%
    mutate(
      duration_min = as.numeric(difftime(complete_ts, start_ts, units = "mins"))
    ) %>%
    filter(duration_min >= 0)  # exclude negative durations
  
  records_with_duration <- nrow(actlog_durations)
  cat(paste("Records with valid duration:", records_with_duration, "\n\n"))
  
  if(records_with_duration > 0) {
    # Compute median and IQR per (resource, activity) pair
    resource_activity_stats <- actlog_durations %>%
      group_by(originator, activity) %>%
      summarise(
        count = n(),
        median_duration = median(duration_min, na.rm = TRUE),
        iqr_duration = IQR(duration_min, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Only consider pairs with enough data (at least 3 observations)
      filter(count >= 3) %>%
      mutate(
        # Dynamic multiplier: wider for small samples, tighter for large
        iqr_mult = ifelse(count < 30, 3.0, 2.0),
        lower_fence = pmax(0, median_duration - iqr_mult * iqr_duration),
        upper_fence = median_duration + iqr_mult * iqr_duration
      )
    
    cat(paste("Resource-activity pairs with sufficient data (>=3):", nrow(resource_activity_stats), "\n"))
    
    if(nrow(resource_activity_stats) > 0) {
      cat("\nResource-activity duration statistics:\n")
      print(resource_activity_stats %>% 
              select(originator, activity, count, median_duration, iqr_duration, lower_fence, upper_fence) %>%
              mutate(across(where(is.numeric), ~round(., 2))))
      
      # Flag duration outliers
      actlog_durations <- actlog_durations %>%
        left_join(resource_activity_stats %>% select(originator, activity, lower_fence, upper_fence),
                  by = c("originator", "activity"))
      
      duration_outliers <- actlog_durations %>%
        filter(!is.na(lower_fence) & !is.na(upper_fence)) %>%
        filter(duration_min < lower_fence | duration_min > upper_fence)
      
      duration_inconsistent_count <- nrow(duration_outliers)
      
      cat(paste("\nDuration outliers found:", duration_inconsistent_count, "\n"))
      
      if(duration_inconsistent_count > 0) {
        cat("Sample duration outliers (first 10):\n")
        print(head(duration_outliers %>%
                     select(case_id, originator, activity, duration_min, lower_fence, upper_fence) %>%
                     mutate(across(where(is.numeric), ~round(., 2))), 10))
        
        duration_file <- file.path(results_dir, "resource_duration_outliers.csv")
        write.csv(duration_outliers[, c("case_id", "originator", "activity", "duration_min", "lower_fence", "upper_fence")],
                  duration_file, row.names = FALSE)
        cat(paste("  Saved to:", duration_file, "\n"))
      }
      
      stats_file <- file.path(results_dir, "resource_activity_duration_stats.csv")
      write.csv(resource_activity_stats, stats_file, row.names = FALSE)
      cat(paste("  Saved to:", stats_file, "\n"))
    } else {
      cat("Not enough data per resource-activity pair for duration analysis\n")
    }
  }
  
  # ============================================
  # OVERALL CONSISTENCY CALCULATION
  # ============================================
  cat("\n============================================\n")
  cat("OVERALL RESOURCE CONSISTENCY\n")
  cat("============================================\n\n")
  
  # Collect all inconsistent record indices (unique, avoid double-counting)
  # Naming inconsistencies (only among records that HAVE a resource value)
  naming_incon_idx <- which(actlog_df$originator %in% 
    actlog_with_resource$originator[actlog_with_resource$resource_format != dominant_format])
  
  # Missing resource records are a COMPLETENESS issue, not consistency.
  # Do NOT count them here so missing-resource noise only affects Completeness.
  missing_idx <- c()
  
  # Duration outlier records
  duration_idx <- c()
  if(exists("duration_outliers") && nrow(duration_outliers) > 0) {
    for(i in 1:nrow(duration_outliers)) {
      idx <- which(actlog_df$case_id == duration_outliers$case_id[i] &
                   actlog_df$originator == duration_outliers$originator[i] &
                   actlog_df$activity == duration_outliers$activity[i])
      duration_idx <- c(duration_idx, idx)
    }
  }
  
  all_inconsistent_idx <- unique(c(naming_incon_idx, missing_idx, duration_idx))
  total_inconsistent <- length(all_inconsistent_idx)
  
  consistent_count <- total_records - total_inconsistent
  consistency <- consistent_count / total_records
  consistency_percentage <- consistency * 100
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Naming inconsistencies:", naming_inconsistent_count, "records\n"))
  cat(paste("Missing resource:", length(missing_idx), "records\n"))
  cat(paste("Duration inconsistencies:", duration_inconsistent_count, "records\n"))
  cat(paste("Total inconsistent (unique):", total_inconsistent, "records\n"))
  cat(paste("Consistent records:", consistent_count, "\n"))
  cat(paste("Resource Consistency:", round(consistency_percentage, 2), "%\n"))
  
  consistency_summary <- data.frame(
    metric = c("total_records", "naming_inconsistent", "missing_resource",
               "duration_inconsistent",
               "total_inconsistent", "consistency_percent"),
    value = c(total_records, naming_inconsistent_count, length(missing_idx),
              duration_inconsistent_count,
              total_inconsistent, round(consistency_percentage, 2))
  )
  
  summary_file <- file.path(results_dir, "resource_consistency_summary.csv")
  write.csv(consistency_summary, summary_file, row.names = FALSE)
  cat(paste("  Saved to:", summary_file, "\n"))
  
  # Compute sub-scores: syntactic (naming format) vs behavioral (duration)
  syntactic_inconsistent <- naming_inconsistent_count + length(missing_idx)
  syntactic_consistency <- max(0, (total_records - syntactic_inconsistent) / total_records)
  
  behavioral_inconsistent <- length(unique(duration_idx))
  behavioral_consistency <- max(0, (total_records - behavioral_inconsistent) / total_records)
  
  cat(paste("Syntactic Consistency (naming format):", round(syntactic_consistency * 100, 2), "%\n"))
  cat(paste("Behavioral Consistency (duration):", round(behavioral_consistency * 100, 2), "%\n"))
  
  return(list(
    total_records = total_records,
    records_with_resource = records_with_resource,
    dominant_format = dominant_format,
    naming_inconsistent_count = naming_inconsistent_count,
    duration_inconsistent_count = duration_inconsistent_count,
    total_inconsistent = total_inconsistent,
    consistency = consistency,
    consistency_percentage = consistency_percentage,
    syntactic_consistency = syntactic_consistency,
    behavioral_consistency = behavioral_consistency,
    format_distribution = format_counts
  ))
}
