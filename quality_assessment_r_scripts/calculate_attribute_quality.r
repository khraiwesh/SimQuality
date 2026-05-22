# Unified Attribute Quality Assessment
# 
# Single function to compute Completeness, Consistency, and Accuracy
# for any event log attribute.
# 
# Supports:
# - CSV and XES input formats
# - Overall and per-activity analysis
# - All standard event log attributes

library(dplyr)
library(tidyr)

# ============================================
# MAIN UNIFIED FUNCTION
# ============================================

#' Calculate Quality for a Single Attribute
#' 
#' Computes Completeness, Consistency, and Accuracy for a given attribute
#' in an event log. Can analyze overall or per-activity.
#'
#' @param log_path Path to event log file (CSV or XES) OR a dataframe
#' @param attribute Name of the attribute to analyze: "start_timestamp", "complete_timestamp", 
#'                  "case_id", "activity", "resource"
#' @param per_activity Logical. If TRUE, compute metrics per activity. Default FALSE.
#' @param activity_col Name of activity column (default "Activity")
#' @param case_col Name of case ID column (default "Case.ID")
#' @param start_col Name of start timestamp column (default "Start")
#' @param complete_col Name of complete timestamp column (default "Complete")
#' @param resource_col Name of resource column (default "Resource")
#' @param valid_values Optional vector of valid values for accuracy check
#' @param min_timestamp Minimum valid timestamp (for accuracy)
#' @param max_timestamp Maximum valid timestamp (for accuracy)
#' @param date_format Expected date format for consistency check
#' @return List with completeness, consistency, accuracy scores (overall or per-activity)

calculate_attribute_quality <- function(
    log_path,
    attribute = c("start_timestamp", "complete_timestamp", "case_id", "activity", "resource"),
    per_activity = FALSE,
    activity_col = "Activity",
    case_col = "Case.ID",
    start_col = "Start",
    complete_col = "Complete",
    resource_col = "Resource",
    valid_values = NULL,
    min_timestamp = NULL,
    max_timestamp = NULL,
    date_format = "%Y-%m-%d %H:%M:%S"
) {
  
  attribute <- match.arg(attribute)
  
  cat("============================================\n")
  cat(paste("ATTRIBUTE QUALITY ASSESSMENT:", toupper(attribute), "\n"))
  cat("============================================\n\n")
  
  # ============================================
  # STEP 1: Load Event Log
  # ============================================
  
  if (is.data.frame(log_path)) {
    log <- log_path
    cat("Using provided dataframe\n")
  } else if (is.character(log_path)) {
    log <- load_event_log(log_path)
  } else {
    stop("log_path must be a file path (CSV/XES) or a dataframe")
  }
  
  cat(paste("Total events:", nrow(log), "\n"))
  cat(paste("Total cases:", length(unique(log[[case_col]])), "\n\n"))
  
  # ============================================
  # STEP 2: Map attribute to column
  # ============================================
  
  target_col <- switch(attribute,
    "start_timestamp" = start_col,
    "complete_timestamp" = complete_col,
    "case_id" = case_col,
    "activity" = activity_col,
    "resource" = resource_col
  )
  
  if (!target_col %in% names(log)) {
    stop(paste("Column", target_col, "not found in event log"))
  }
  
  cat(paste("Target column:", target_col, "\n\n"))
  
  # ============================================
  # STEP 3: Compute Quality (Overall or Per-Activity)
  # ============================================
  
  if (per_activity) {
    results <- compute_quality_per_activity(
      log = log,
      target_col = target_col,
      attribute = attribute,
      activity_col = activity_col,
      case_col = case_col,
      start_col = start_col,
      complete_col = complete_col,
      valid_values = valid_values,
      min_timestamp = min_timestamp,
      max_timestamp = max_timestamp,
      date_format = date_format
    )
  } else {
    results <- compute_quality_overall(
      log = log,
      target_col = target_col,
      attribute = attribute,
      case_col = case_col,
      start_col = start_col,
      complete_col = complete_col,
      valid_values = valid_values,
      min_timestamp = min_timestamp,
      max_timestamp = max_timestamp,
      date_format = date_format
    )
  }
  
  # ============================================
  # STEP 4: Save Results
  # ============================================
  
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir)
  
  suffix <- ifelse(per_activity, "_per_activity", "_overall")
  filename <- paste0(attribute, "_quality", suffix, ".csv")
  
  write.csv(results$table, file.path(results_dir, filename), row.names = FALSE)
  cat(paste("\nSaved to: results/", filename, "\n", sep = ""))
  
  return(results)
}


# ============================================
# LOAD EVENT LOG (CSV or XES)
# ============================================

load_event_log <- function(file_path) {
  
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  extension <- tolower(tools::file_ext(file_path))
  
  if (extension == "csv") {
    cat("Loading CSV file...\n")
    log <- read.csv(file_path, stringsAsFactors = FALSE)
    
  } else if (extension == "xes") {
    cat("Loading XES file...\n")
    
    # Check if bupaR is available
    if (!requireNamespace("bupaR", quietly = TRUE)) {
      stop("Package 'bupaR' required for XES files. Install with: install.packages('bupaR')")
    }
    
    # Try to load XES
    tryCatch({
      library(bupaR)
      log <- read_xes(file_path)
      # Convert to dataframe
      log <- as.data.frame(log)
    }, error = function(e) {
      stop(paste("Error loading XES file:", e$message))
    })
    
  } else {
    stop(paste("Unsupported file format:", extension, ". Use CSV or XES."))
  }
  
  cat(paste("Loaded", nrow(log), "events\n"))
  return(log)
}


# ============================================
# COMPUTE QUALITY OVERALL
# ============================================

compute_quality_overall <- function(
    log,
    target_col,
    attribute,
    case_col,
    start_col,
    complete_col,
    valid_values,
    min_timestamp,
    max_timestamp,
    date_format
) {
  
  n_total <- nrow(log)
  values <- log[[target_col]]
  
  cat("--- OVERALL ANALYSIS ---\n\n")
  
  # ============================================
  # COMPLETENESS
  # ============================================
  # Proportion of non-missing values
  
  n_missing <- sum(is.na(values) | values == "" | values == "NA")
  n_present <- n_total - n_missing
  completeness <- n_present / n_total
  
  cat("COMPLETENESS:\n")
  cat(paste("  Total values:", n_total, "\n"))
  cat(paste("  Present:", n_present, "\n"))
  cat(paste("  Missing:", n_missing, "\n"))
  cat(paste("  Score:", round(completeness * 100, 2), "%\n\n"))
  
  # ============================================
  # CONSISTENCY
  # ============================================
  # Depends on attribute type
  
  consistency_result <- compute_consistency(
    values = values,
    attribute = attribute,
    log = log,
    case_col = case_col,
    start_col = start_col,
    complete_col = complete_col,
    date_format = date_format
  )
  
  consistency <- consistency_result$score
  
  cat("CONSISTENCY:\n")
  cat(paste("  Check:", consistency_result$check_type, "\n"))
  cat(paste("  Consistent:", consistency_result$n_consistent, "\n"))
  cat(paste("  Inconsistent:", consistency_result$n_inconsistent, "\n"))
  cat(paste("  Score:", round(consistency * 100, 2), "%\n\n"))
  
  # ============================================
  # ACCURACY
  # ============================================
  # Depends on attribute type
  
  accuracy_result <- compute_accuracy(
    values = values,
    attribute = attribute,
    log = log,
    start_col = start_col,
    complete_col = complete_col,
    valid_values = valid_values,
    min_timestamp = min_timestamp,
    max_timestamp = max_timestamp
  )
  
  accuracy <- accuracy_result$score
  
  cat("ACCURACY:\n")
  cat(paste("  Check:", accuracy_result$check_type, "\n"))
  cat(paste("  Accurate:", accuracy_result$n_accurate, "\n"))
  cat(paste("  Inaccurate:", accuracy_result$n_inaccurate, "\n"))
  cat(paste("  Score:", round(accuracy * 100, 2), "%\n\n"))
  
  # ============================================
  # COMBINED SCORE
  # ============================================
  
  combined <- mean(c(completeness, consistency, accuracy))
  
  cat("============================================\n")
  cat(paste("COMBINED QUALITY SCORE:", round(combined * 100, 2), "%\n"))
  cat("============================================\n")
  
  # Create output table
  table <- data.frame(
    Attribute = attribute,
    Total_Events = n_total,
    Completeness = round(completeness, 4),
    Consistency = round(consistency, 4),
    Accuracy = round(accuracy, 4),
    Combined = round(combined, 4),
    Completeness_Pct = paste0(round(completeness * 100, 2), "%"),
    Consistency_Pct = paste0(round(consistency * 100, 2), "%"),
    Accuracy_Pct = paste0(round(accuracy * 100, 2), "%"),
    Combined_Pct = paste0(round(combined * 100, 2), "%")
  )
  
  return(list(
    completeness = completeness,
    consistency = consistency,
    accuracy = accuracy,
    combined = combined,
    n_total = n_total,
    n_missing = n_missing,
    consistency_details = consistency_result,
    accuracy_details = accuracy_result,
    table = table
  ))
}


# ============================================
# COMPUTE QUALITY PER ACTIVITY
# ============================================

compute_quality_per_activity <- function(
    log,
    target_col,
    attribute,
    activity_col,
    case_col,
    start_col,
    complete_col,
    valid_values,
    min_timestamp,
    max_timestamp,
    date_format
) {
  
  activities <- unique(log[[activity_col]])
  activities <- activities[!is.na(activities)]
  
  cat("--- PER-ACTIVITY ANALYSIS ---\n")
  cat(paste("Activities found:", length(activities), "\n\n"))
  
  results_table <- data.frame(
    Activity = character(),
    Event_Count = integer(),
    Completeness = numeric(),
    Consistency = numeric(),
    Accuracy = numeric(),
    Combined = numeric(),
    stringsAsFactors = FALSE
  )
  
  activity_results <- list()
  
  for (act in activities) {
    
    # Filter log for this activity
    act_log <- log[log[[activity_col]] == act & !is.na(log[[activity_col]]), ]
    n_events <- nrow(act_log)
    
    if (n_events == 0) next
    
    cat(paste("Activity:", act, "(", n_events, "events )\n"))
    
    values <- act_log[[target_col]]
    
    # COMPLETENESS
    n_missing <- sum(is.na(values) | values == "" | values == "NA")
    completeness <- (n_events - n_missing) / n_events
    
    # CONSISTENCY
    consistency_result <- compute_consistency(
      values = values,
      attribute = attribute,
      log = act_log,
      case_col = case_col,
      start_col = start_col,
      complete_col = complete_col,
      date_format = date_format
    )
    consistency <- consistency_result$score
    
    # ACCURACY
    accuracy_result <- compute_accuracy(
      values = values,
      attribute = attribute,
      log = act_log,
      start_col = start_col,
      complete_col = complete_col,
      valid_values = valid_values,
      min_timestamp = min_timestamp,
      max_timestamp = max_timestamp
    )
    accuracy <- accuracy_result$score
    
    # COMBINED
    combined <- mean(c(completeness, consistency, accuracy))
    
    cat(paste("  Completeness:", round(completeness * 100, 1), "% |",
              "Consistency:", round(consistency * 100, 1), "% |",
              "Accuracy:", round(accuracy * 100, 1), "% |",
              "Combined:", round(combined * 100, 1), "%\n"))
    
    results_table <- rbind(results_table, data.frame(
      Activity = act,
      Event_Count = n_events,
      Completeness = round(completeness, 4),
      Consistency = round(consistency, 4),
      Accuracy = round(accuracy, 4),
      Combined = round(combined, 4),
      stringsAsFactors = FALSE
    ))
    
    activity_results[[act]] <- list(
      completeness = completeness,
      consistency = consistency,
      accuracy = accuracy,
      combined = combined,
      n_events = n_events,
      consistency_details = consistency_result,
      accuracy_details = accuracy_result
    )
  }
  
  # Sort by combined score (lowest first = most problematic)
  results_table <- results_table[order(results_table$Combined), ]
  
  # Calculate overall averages
  avg_completeness <- mean(results_table$Completeness)
  avg_consistency <- mean(results_table$Consistency)
  avg_accuracy <- mean(results_table$Accuracy)
  avg_combined <- mean(results_table$Combined)
  
  cat("\n============================================\n")
  cat("SUMMARY ACROSS ALL ACTIVITIES:\n")
  cat(paste("  Avg Completeness:", round(avg_completeness * 100, 2), "%\n"))
  cat(paste("  Avg Consistency:", round(avg_consistency * 100, 2), "%\n"))
  cat(paste("  Avg Accuracy:", round(avg_accuracy * 100, 2), "%\n"))
  cat(paste("  Avg Combined:", round(avg_combined * 100, 2), "%\n"))
  cat("============================================\n")
  
  cat("\nActivities with LOWEST quality (need attention):\n")
  print(head(results_table, 10))
  
  return(list(
    per_activity = activity_results,
    table = results_table,
    avg_completeness = avg_completeness,
    avg_consistency = avg_consistency,
    avg_accuracy = avg_accuracy,
    avg_combined = avg_combined
  ))
}


# ============================================
# COMPUTE CONSISTENCY (Attribute-Specific)
# ============================================

compute_consistency <- function(
    values,
    attribute,
    log,
    case_col,
    start_col,
    complete_col,
    date_format
) {
  
  n_total <- length(values)
  non_missing <- !is.na(values) & values != "" & values != "NA"
  n_valid <- sum(non_missing)
  
  if (n_valid == 0) {
    return(list(
      score = NA,
      n_consistent = 0,
      n_inconsistent = 0,
      check_type = "No valid values to check"
    ))
  }
  
  # ============================================
  # TIMESTAMP CONSISTENCY
  # ============================================
  if (attribute %in% c("start_timestamp", "complete_timestamp")) {
    
    # Check 1: Format consistency (can all values be parsed?)
    parsed_ok <- sapply(values[non_missing], function(v) {
      result <- tryCatch({
        as.POSIXct(v, format = date_format)
        TRUE
      }, error = function(e) FALSE)
      return(result)
    })
    
    n_format_ok <- sum(parsed_ok, na.rm = TRUE)
    
    # Check 2: Temporal ordering (start <= complete within same event)
    if (attribute == "start_timestamp" && complete_col %in% names(log)) {
      # Check start vs complete
      has_both <- non_missing & !is.na(log[[complete_col]]) & log[[complete_col]] != ""
      
      if (sum(has_both) > 0) {
        start_times <- as.POSIXct(log[[start_col]][has_both])
        complete_times <- as.POSIXct(log[[complete_col]][has_both])
        
        order_ok <- start_times <= complete_times
        n_order_ok <- sum(order_ok, na.rm = TRUE)
        n_order_check <- sum(has_both)
        
        # Combined: format AND order
        consistency_score <- (n_format_ok / n_valid + n_order_ok / n_order_check) / 2
        n_consistent <- round(n_valid * consistency_score)
        
        return(list(
          score = consistency_score,
          n_consistent = n_consistent,
          n_inconsistent = n_valid - n_consistent,
          check_type = "Format parsing + Temporal order (start <= complete)"
        ))
      }
    }
    
    # Only format check
    consistency_score <- n_format_ok / n_valid
    
    return(list(
      score = consistency_score,
      n_consistent = n_format_ok,
      n_inconsistent = n_valid - n_format_ok,
      check_type = "Format consistency (parseable timestamps)"
    ))
  }
  
  # ============================================
  # CASE ID CONSISTENCY
  # ============================================
  if (attribute == "case_id") {
    
    # Check: Each case should have at least 2 events (start and complete of activities)
    case_counts <- table(values[non_missing])
    single_event_cases <- sum(case_counts == 1)
    n_cases_ok <- sum(case_counts >= 2)
    total_cases <- length(case_counts)
    
    consistency_score <- n_cases_ok / total_cases
    
    return(list(
      score = consistency_score,
      n_consistent = n_cases_ok,
      n_inconsistent = single_event_cases,
      check_type = "Case has multiple events (not orphan)"
    ))
  }
  
  # ============================================
  # ACTIVITY CONSISTENCY
  # ============================================
  if (attribute == "activity") {
    
    # Check: Activity names should be consistent (no typos/variations)
    # Use Levenshtein distance to find potential typos
    unique_activities <- unique(values[non_missing])
    
    # Check for potential duplicates (very similar names)
    n_potential_duplicates <- 0
    for (i in 1:(length(unique_activities) - 1)) {
      for (j in (i + 1):length(unique_activities)) {
        # Simple check: lowercase comparison
        if (tolower(unique_activities[i]) == tolower(unique_activities[j])) {
          n_potential_duplicates <- n_potential_duplicates + 1
        }
      }
    }
    
    # All activities consistent if no duplicates found
    if (n_potential_duplicates == 0) {
      consistency_score <- 1.0
    } else {
      # Penalize for potential duplicates
      consistency_score <- max(0, 1 - (n_potential_duplicates / length(unique_activities)))
    }
    
    return(list(
      score = consistency_score,
      n_consistent = n_valid,
      n_inconsistent = n_potential_duplicates,
      check_type = "Activity naming consistency (no case variations)"
    ))
  }
  
  # ============================================
  # RESOURCE CONSISTENCY
  # ============================================
  if (attribute == "resource") {
    
    # Check 1: Resource naming consistency (no variations)
    unique_resources <- unique(values[non_missing])
    
    # Check for potential duplicates
    n_potential_duplicates <- 0
    for (i in 1:(length(unique_resources) - 1)) {
      for (j in (i + 1):length(unique_resources)) {
        if (tolower(unique_resources[i]) == tolower(unique_resources[j])) {
          n_potential_duplicates <- n_potential_duplicates + 1
        }
      }
    }
    
    # Check 2: Same resource doing same activity at overlapping times (conflict)
    # This would require more complex analysis
    
    if (n_potential_duplicates == 0) {
      consistency_score <- 1.0
    } else {
      consistency_score <- max(0, 1 - (n_potential_duplicates / length(unique_resources)))
    }
    
    return(list(
      score = consistency_score,
      n_consistent = n_valid,
      n_inconsistent = n_potential_duplicates,
      check_type = "Resource naming consistency"
    ))
  }
  
  # Default
  return(list(
    score = 1.0,
    n_consistent = n_valid,
    n_inconsistent = 0,
    check_type = "Default (no specific check)"
  ))
}


# ============================================
# COMPUTE ACCURACY (Attribute-Specific)
# ============================================

compute_accuracy <- function(
    values,
    attribute,
    log,
    start_col,
    complete_col,
    valid_values,
    min_timestamp,
    max_timestamp
) {
  
  n_total <- length(values)
  non_missing <- !is.na(values) & values != "" & values != "NA"
  n_valid <- sum(non_missing)
  
  if (n_valid == 0) {
    return(list(
      score = NA,
      n_accurate = 0,
      n_inaccurate = 0,
      check_type = "No valid values to check"
    ))
  }
  
  # ============================================
  # TIMESTAMP ACCURACY
  # ============================================
  if (attribute %in% c("start_timestamp", "complete_timestamp")) {
    
    timestamps <- as.POSIXct(values[non_missing], origin = "1970-01-01")
    
    # Set default bounds if not provided
    if (is.null(min_timestamp)) {
      min_timestamp <- as.POSIXct("2000-01-01")
    } else if (is.character(min_timestamp)) {
      min_timestamp <- as.POSIXct(min_timestamp)
    }
    
    if (is.null(max_timestamp)) {
      max_timestamp <- Sys.time() + 86400  # Tomorrow
    } else if (is.character(max_timestamp)) {
      max_timestamp <- as.POSIXct(max_timestamp)
    }
    
    # Check: Timestamps within valid range
    in_range <- timestamps >= min_timestamp & timestamps <= max_timestamp
    n_accurate <- sum(in_range, na.rm = TRUE)
    
    # Additional check for start_timestamp: duration should be positive and reasonable
    if (attribute == "start_timestamp" && complete_col %in% names(log)) {
      has_both <- non_missing & !is.na(log[[complete_col]]) & log[[complete_col]] != ""
      
      if (sum(has_both) > 0) {
        start_times <- as.POSIXct(log[[start_col]][has_both])
        complete_times <- as.POSIXct(log[[complete_col]][has_both])
        
        durations <- as.numeric(difftime(complete_times, start_times, units = "secs"))
        
        # Reasonable duration: 0 to 30 days
        reasonable <- durations >= 0 & durations <= (86400 * 30)
        n_reasonable <- sum(reasonable, na.rm = TRUE)
        n_duration_check <- sum(has_both)
        
        # Combined accuracy
        range_score <- n_accurate / n_valid
        duration_score <- n_reasonable / n_duration_check
        accuracy_score <- (range_score + duration_score) / 2
        
        return(list(
          score = accuracy_score,
          n_accurate = round(n_valid * accuracy_score),
          n_inaccurate = round(n_valid * (1 - accuracy_score)),
          check_type = paste("Valid range + Reasonable duration (0 to 30 days)")
        ))
      }
    }
    
    accuracy_score <- n_accurate / n_valid
    
    return(list(
      score = accuracy_score,
      n_accurate = n_accurate,
      n_inaccurate = n_valid - n_accurate,
      check_type = paste("Timestamp in range:", min_timestamp, "to", max_timestamp)
    ))
  }
  
  # ============================================
  # CASE ID ACCURACY
  # ============================================
  if (attribute == "case_id") {
    
    # If valid_values provided, check against them
    if (!is.null(valid_values)) {
      in_valid <- values[non_missing] %in% valid_values
      n_accurate <- sum(in_valid)
      accuracy_score <- n_accurate / n_valid
      
      return(list(
        score = accuracy_score,
        n_accurate = n_accurate,
        n_inaccurate = n_valid - n_accurate,
        check_type = "Case ID in valid list"
      ))
    }
    
    # Default: Check format (non-empty, reasonable length)
    case_ids <- values[non_missing]
    valid_format <- nchar(as.character(case_ids)) > 0 & nchar(as.character(case_ids)) < 100
    n_accurate <- sum(valid_format)
    accuracy_score <- n_accurate / n_valid
    
    return(list(
      score = accuracy_score,
      n_accurate = n_accurate,
      n_inaccurate = n_valid - n_accurate,
      check_type = "Valid format (non-empty, reasonable length)"
    ))
  }
  
  # ============================================
  # ACTIVITY ACCURACY
  # ============================================
  if (attribute == "activity") {
    
    # If valid_values provided, check against them
    if (!is.null(valid_values)) {
      in_valid <- values[non_missing] %in% valid_values
      n_accurate <- sum(in_valid)
      accuracy_score <- n_accurate / n_valid
      
      return(list(
        score = accuracy_score,
        n_accurate = n_accurate,
        n_inaccurate = n_valid - n_accurate,
        check_type = "Activity in valid list"
      ))
    }
    
    # Default: All non-missing are accurate
    accuracy_score <- 1.0
    
    return(list(
      score = accuracy_score,
      n_accurate = n_valid,
      n_inaccurate = 0,
      check_type = "All non-missing activities accepted"
    ))
  }
  
  # ============================================
  # RESOURCE ACCURACY
  # ============================================
  if (attribute == "resource") {
    
    # If valid_values provided, check against them
    if (!is.null(valid_values)) {
      in_valid <- values[non_missing] %in% valid_values
      n_accurate <- sum(in_valid)
      accuracy_score <- n_accurate / n_valid
      
      return(list(
        score = accuracy_score,
        n_accurate = n_accurate,
        n_inaccurate = n_valid - n_accurate,
        check_type = "Resource in valid list"
      ))
    }
    
    # Default: All non-missing and non-generic are accurate
    resources <- values[non_missing]
    generic_resources <- c("Unknown", "System", "NA", "N/A", "null", "NULL", "")
    is_generic <- tolower(resources) %in% tolower(generic_resources)
    n_accurate <- sum(!is_generic)
    accuracy_score <- n_accurate / n_valid
    
    return(list(
      score = accuracy_score,
      n_accurate = n_accurate,
      n_inaccurate = sum(is_generic),
      check_type = "Not generic resource (Unknown, System, etc.)"
    ))
  }
  
  # Default
  return(list(
    score = 1.0,
    n_accurate = n_valid,
    n_inaccurate = 0,
    check_type = "Default (all accepted)"
  ))
}


# ============================================
# CONVENIENCE WRAPPER FUNCTIONS
# ============================================

#' Quick assessment of all attributes
#' 
#' @param log_path Path to event log or dataframe
#' @param per_activity Whether to analyze per-activity
#' @return List with results for all attributes

calculate_all_attributes_quality <- function(
    log_path,
    per_activity = FALSE,
    activity_col = "Activity",
    case_col = "Case.ID",
    start_col = "Start",
    complete_col = "Complete",
    resource_col = "Resource"
) {
  
  cat("############################################\n")
  cat("COMPLETE ATTRIBUTE QUALITY ASSESSMENT\n")
  cat(paste("Mode:", ifelse(per_activity, "Per-Activity", "Overall"), "\n"))
  cat("############################################\n\n")
  
  attributes <- c("start_timestamp", "complete_timestamp", "case_id", "activity", "resource")
  
  results <- list()
  summary_table <- data.frame()
  
  for (attr in attributes) {
    cat("\n", strrep("=", 50), "\n")
    
    result <- calculate_attribute_quality(
      log_path = log_path,
      attribute = attr,
      per_activity = per_activity,
      activity_col = activity_col,
      case_col = case_col,
      start_col = start_col,
      complete_col = complete_col,
      resource_col = resource_col
    )
    
    results[[attr]] <- result
    
    if (!per_activity) {
      summary_table <- rbind(summary_table, data.frame(
        Attribute = attr,
        Completeness = result$completeness,
        Consistency = result$consistency,
        Accuracy = result$accuracy,
        Combined = result$combined
      ))
    } else {
      summary_table <- rbind(summary_table, data.frame(
        Attribute = attr,
        Completeness = result$avg_completeness,
        Consistency = result$avg_consistency,
        Accuracy = result$avg_accuracy,
        Combined = result$avg_combined
      ))
    }
  }
  
  # Save summary
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir)
  
  suffix <- ifelse(per_activity, "_per_activity", "_overall")
  write.csv(summary_table, 
            file.path(results_dir, paste0("all_attributes_quality", suffix, ".csv")), 
            row.names = FALSE)
  
  cat("\n\n############################################\n")
  cat("FINAL SUMMARY\n")
  cat("############################################\n\n")
  
  print(summary_table)
  
  cat(paste("\nOverall Average Quality:", 
            round(mean(summary_table$Combined, na.rm = TRUE) * 100, 2), "%\n"))
  
  return(list(
    results = results,
    summary = summary_table
  ))
}


# ============================================
# EXAMPLE USAGE
# ============================================

# # Load from CSV
# result <- calculate_attribute_quality(
#   log_path = "event_log.csv",
#   attribute = "start_timestamp",
#   per_activity = FALSE
# )
#
# # Load from XES
# result <- calculate_attribute_quality(
#   log_path = "event_log.xes",
#   attribute = "resource",
#   per_activity = TRUE
# )
#
# # Using a dataframe
# result <- calculate_attribute_quality(
#   log_path = my_dataframe,
#   attribute = "complete_timestamp",
#   per_activity = TRUE
# )
#
# # All attributes at once
# all_results <- calculate_all_attributes_quality(
#   log_path = "event_log.csv",
#   per_activity = FALSE
# )
