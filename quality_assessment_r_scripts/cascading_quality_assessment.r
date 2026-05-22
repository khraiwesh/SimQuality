# Cascading Data Quality Assessment
# Pipeline: Completeness → Consistency → Accuracy
# Each step filters the data before the next check

library(dplyr)
library(bupaR)
library(daqapo)

#' Cascading Quality Assessment Pipeline
#' 
#' Processes event log through sequential quality checks:
#' 1. Completeness - removes incomplete cases
#' 2. Consistency - removes inconsistent cases  
#' 3. Accuracy - identifies inaccurate records
#'
#' @param actlog Activity log (bupaR format)
#' @param config Configuration list with thresholds and rules
#' @return List with scores at each stage and final clean log

cascading_quality_assessment <- function(actlog, config = list()) {
  
  cat("============================================\n")
  cat("CASCADING DATA QUALITY ASSESSMENT\n")
  cat("Pipeline: Completeness → Consistency → Accuracy\n")
  cat("============================================\n\n")
  
  # Initialize tracking
  original_records <- nrow(actlog)
  original_cases <- n_distinct(actlog$case_id)
  
  tracking <- list(
    stage = character(),
    records_in = integer(),
    records_out = integer(),
    records_removed = integer(),
    cases_in = integer(),
    cases_out = integer(),
    cases_removed = integer(),
    score = numeric()
  )
  
  current_log <- actlog
  
  # ============================================
  # STAGE 1: COMPLETENESS
  # ============================================
  
  cat("============================================\n")
  cat("STAGE 1: COMPLETENESS CHECK\n")
  cat("============================================\n\n")
  
  records_before <- nrow(current_log)
  cases_before <- n_distinct(current_log$case_id)
  
  # Check for missing values in key attributes
  current_df <- as.data.frame(current_log)
  
  # Identify incomplete records (any NA in key columns)
  key_columns <- c("case_id", "activity", "start", "complete", "originator")
  existing_cols <- key_columns[key_columns %in% colnames(current_df)]
  
  incomplete_mask <- rowSums(is.na(current_df[, existing_cols, drop = FALSE])) > 0
  incomplete_records <- sum(incomplete_mask)
  
  # Get cases with incomplete records
  incomplete_cases <- unique(current_df$case_id[incomplete_mask])
  
  cat(paste("Records with missing values:", incomplete_records, "\n"))
  cat(paste("Cases with missing values:", length(incomplete_cases), "\n"))
  
  # Remove incomplete cases (case-level removal)
  if (length(incomplete_cases) > 0) {
    current_df <- current_df[!current_df$case_id %in% incomplete_cases, ]
    cat(paste("Removed", length(incomplete_cases), "incomplete cases\n"))
  }
  
  records_after <- nrow(current_df)
  cases_after <- n_distinct(current_df$case_id)
  
  # Completeness scores by attribute
  completeness_scores <- list()
  for (col in existing_cols) {
    na_count <- sum(is.na(as.data.frame(actlog)[[col]]))
    completeness_scores[[col]] <- 1 - (na_count / original_records)
  }
  
  # Overall completeness (case-level)
  completeness_score <- cases_after / cases_before
  
  cat(paste("\nCompleteness Score (case-level):", round(completeness_score * 100, 2), "%\n"))
  cat(paste("Records remaining:", records_after, "/", records_before, "\n"))
  cat(paste("Cases remaining:", cases_after, "/", cases_before, "\n\n"))
  
  # Update tracking
  tracking$stage <- c(tracking$stage, "Completeness")
  tracking$records_in <- c(tracking$records_in, records_before)
  tracking$records_out <- c(tracking$records_out, records_after)
  tracking$records_removed <- c(tracking$records_removed, records_before - records_after)
  tracking$cases_in <- c(tracking$cases_in, cases_before)
  tracking$cases_out <- c(tracking$cases_out, cases_after)
  tracking$cases_removed <- c(tracking$cases_removed, cases_before - cases_after)
  tracking$score <- c(tracking$score, completeness_score)
  
  # Convert back to activitylog
  if (nrow(current_df) > 0) {
    current_log <- current_df %>%
      bupaR::activitylog(
        case_id = "case_id",
        activity_id = "activity",
        timestamps = c("start", "complete"),
        resource_id = "originator"
      )
  }
  
  # Save completeness results
  completeness_results <- list(
    score = completeness_score,
    attribute_scores = completeness_scores,
    removed_cases = incomplete_cases,
    records_removed = records_before - records_after
  )
  
  # ============================================
  # STAGE 2: CONSISTENCY
  # ============================================
  
  cat("============================================\n")
  cat("STAGE 2: CONSISTENCY CHECK\n")
  cat("============================================\n\n")
  
  records_before <- nrow(current_log)
  cases_before <- n_distinct(as.data.frame(current_log)$case_id)
  
  inconsistent_cases <- c()
  consistency_issues <- list()
  
  current_df <- as.data.frame(current_log)
  
  # Check 1: Timestamp format consistency
  cat("Checking timestamp format consistency...\n")
  
  formats_to_check <- list(
    "dd/mm/yyyy HH:MM:SS" = "^\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}$",
    "yyyy-mm-dd HH:MM:SS" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
  )
  
  # Detect format for start timestamps
  start_formats <- sapply(current_df$start, function(ts) {
    ts_str <- as.character(ts)
    for (fmt_name in names(formats_to_check)) {
      if (grepl(formats_to_check[[fmt_name]], ts_str)) return(fmt_name)
    }
    return("Other")
  })
  
  format_counts <- table(start_formats)
  dominant_format <- names(which.max(format_counts))
  format_inconsistent <- sum(start_formats != dominant_format)
  
  consistency_issues$format <- list(
    dominant_format = dominant_format,
    inconsistent_count = format_inconsistent
  )
  
  # Cases with format inconsistency
  if (format_inconsistent > 0) {
    format_inconsistent_cases <- unique(current_df$case_id[start_formats != dominant_format])
    inconsistent_cases <- c(inconsistent_cases, format_inconsistent_cases)
    cat(paste("  Format inconsistencies:", format_inconsistent, "records\n"))
  }
  
  # Check 2: Temporal consistency (start <= complete)
  cat("Checking temporal consistency (start <= complete)...\n")
  
  temporal_issues <- current_df$start > current_df$complete
  temporal_issue_count <- sum(temporal_issues, na.rm = TRUE)
  
  if (temporal_issue_count > 0) {
    temporal_inconsistent_cases <- unique(current_df$case_id[temporal_issues])
    inconsistent_cases <- c(inconsistent_cases, temporal_inconsistent_cases)
    cat(paste("  Temporal inconsistencies:", temporal_issue_count, "records\n"))
  }
  
  consistency_issues$temporal <- list(
    issue_count = temporal_issue_count
  )
  
  # Check 3: Activity order consistency (if expected order provided)
  if (!is.null(config$expected_order)) {
    cat("Checking activity order consistency...\n")
    
    tryCatch({
      order_violations <- detect_activity_order_violations(
        activitylog = current_log,
        activity_order = config$expected_order
      )
      
      order_violation_count <- nrow(as.data.frame(order_violations))
      
      if (order_violation_count > 0) {
        order_violation_cases <- unique(as.data.frame(order_violations)$case_id)
        inconsistent_cases <- c(inconsistent_cases, order_violation_cases)
        cat(paste("  Order violations:", order_violation_count, "records\n"))
      }
      
      consistency_issues$order <- list(
        violation_count = order_violation_count
      )
    }, error = function(e) {
      cat(paste("  Order check skipped:", e$message, "\n"))
    })
  }
  
  # Remove inconsistent cases
  inconsistent_cases <- unique(inconsistent_cases)
  
  if (length(inconsistent_cases) > 0) {
    current_df <- current_df[!current_df$case_id %in% inconsistent_cases, ]
    cat(paste("\nRemoved", length(inconsistent_cases), "inconsistent cases\n"))
  }
  
  records_after <- nrow(current_df)
  cases_after <- n_distinct(current_df$case_id)
  
  # Consistency score (case-level)
  consistency_score <- cases_after / cases_before
  
  cat(paste("\nConsistency Score (case-level):", round(consistency_score * 100, 2), "%\n"))
  cat(paste("Records remaining:", records_after, "/", records_before, "\n"))
  cat(paste("Cases remaining:", cases_after, "/", cases_before, "\n\n"))
  
  # Update tracking
  tracking$stage <- c(tracking$stage, "Consistency")
  tracking$records_in <- c(tracking$records_in, records_before)
  tracking$records_out <- c(tracking$records_out, records_after)
  tracking$records_removed <- c(tracking$records_removed, records_before - records_after)
  tracking$cases_in <- c(tracking$cases_in, cases_before)
  tracking$cases_out <- c(tracking$cases_out, cases_after)
  tracking$cases_removed <- c(tracking$cases_removed, cases_before - cases_after)
  tracking$score <- c(tracking$score, consistency_score)
  
  # Convert back to activitylog
  if (nrow(current_df) > 0) {
    current_log <- current_df %>%
      bupaR::activitylog(
        case_id = "case_id",
        activity_id = "activity",
        timestamps = c("start", "complete"),
        resource_id = "originator"
      )
  }
  
  # Save consistency results
  consistency_results <- list(
    score = consistency_score,
    issues = consistency_issues,
    removed_cases = inconsistent_cases,
    records_removed = records_before - records_after
  )
  
  # ============================================
  # STAGE 3: ACCURACY
  # ============================================
  
  cat("============================================\n")
  cat("STAGE 3: ACCURACY CHECK\n")
  cat("============================================\n\n")
  
  records_before <- nrow(current_log)
  cases_before <- n_distinct(as.data.frame(current_log)$case_id)
  
  inaccurate_cases <- c()
  accuracy_issues <- list()
  
  current_df <- as.data.frame(current_log)
  
  # Check 1: Working hours validation
  cat("Checking working hours validity...\n")
  
  start_hour <- as.numeric(format(current_df$start, "%H"))
  complete_hour <- as.numeric(format(current_df$complete, "%H"))
  
  working_start <- ifelse(!is.null(config$working_hours_start), config$working_hours_start, 0)
  working_end <- ifelse(!is.null(config$working_hours_end), config$working_hours_end, 24)
  
  working_hour_violations <- (start_hour < working_start | start_hour > working_end |
                               complete_hour < working_start | complete_hour > working_end)
  working_hour_count <- sum(working_hour_violations, na.rm = TRUE)
  
  if (working_hour_count > 0) {
    working_hour_cases <- unique(current_df$case_id[working_hour_violations])
    inaccurate_cases <- c(inaccurate_cases, working_hour_cases)
    cat(paste("  Working hour violations:", working_hour_count, "records\n"))
  }
  
  accuracy_issues$working_hours <- list(
    violation_count = working_hour_count
  )
  
  # Check 2: Duration outliers
  cat("Checking duration outliers...\n")
  
  current_df$duration_mins <- as.numeric(difftime(current_df$complete, current_df$start, units = "mins"))
  
  min_duration <- ifelse(!is.null(config$min_duration), config$min_duration, 0)
  max_duration <- ifelse(!is.null(config$max_duration), config$max_duration, 480)  # 8 hours default
  
  duration_outliers <- (current_df$duration_mins < min_duration | 
                        current_df$duration_mins > max_duration)
  duration_outlier_count <- sum(duration_outliers, na.rm = TRUE)
  
  if (duration_outlier_count > 0) {
    duration_outlier_cases <- unique(current_df$case_id[duration_outliers])
    inaccurate_cases <- c(inaccurate_cases, duration_outlier_cases)
    cat(paste("  Duration outliers:", duration_outlier_count, "records\n"))
  }
  
  accuracy_issues$duration <- list(
    outlier_count = duration_outlier_count
  )
  
  # Check 3: Invalid activity names
  if (!is.null(config$allowed_activities)) {
    cat("Checking activity name validity...\n")
    
    invalid_activities <- !current_df$activity %in% config$allowed_activities
    invalid_activity_count <- sum(invalid_activities)
    
    if (invalid_activity_count > 0) {
      invalid_activity_cases <- unique(current_df$case_id[invalid_activities])
      inaccurate_cases <- c(inaccurate_cases, invalid_activity_cases)
      cat(paste("  Invalid activities:", invalid_activity_count, "records\n"))
    }
    
    accuracy_issues$activity <- list(
      invalid_count = invalid_activity_count
    )
  }
  
  # Remove inaccurate cases
  inaccurate_cases <- unique(inaccurate_cases)
  
  if (length(inaccurate_cases) > 0) {
    current_df <- current_df[!current_df$case_id %in% inaccurate_cases, ]
    current_df$duration_mins <- NULL  # Remove temp column
    cat(paste("\nRemoved", length(inaccurate_cases), "inaccurate cases\n"))
  }
  
  records_after <- nrow(current_df)
  cases_after <- n_distinct(current_df$case_id)
  
  # Accuracy score (case-level)
  accuracy_score <- cases_after / cases_before
  
  cat(paste("\nAccuracy Score (case-level):", round(accuracy_score * 100, 2), "%\n"))
  cat(paste("Records remaining:", records_after, "/", records_before, "\n"))
  cat(paste("Cases remaining:", cases_after, "/", cases_before, "\n\n"))
  
  # Update tracking
  tracking$stage <- c(tracking$stage, "Accuracy")
  tracking$records_in <- c(tracking$records_in, records_before)
  tracking$records_out <- c(tracking$records_out, records_after)
  tracking$records_removed <- c(tracking$records_removed, records_before - records_after)
  tracking$cases_in <- c(tracking$cases_in, cases_before)
  tracking$cases_out <- c(tracking$cases_out, cases_after)
  tracking$cases_removed <- c(tracking$cases_removed, cases_before - cases_after)
  tracking$score <- c(tracking$score, accuracy_score)
  
  # Convert back to activitylog
  final_log <- NULL
  if (nrow(current_df) > 0) {
    final_log <- current_df %>%
      bupaR::activitylog(
        case_id = "case_id",
        activity_id = "activity",
        timestamps = c("start", "complete"),
        resource_id = "originator"
      )
  }
  
  # Save accuracy results
  accuracy_results <- list(
    score = accuracy_score,
    issues = accuracy_issues,
    removed_cases = inaccurate_cases,
    records_removed = records_before - records_after
  )
  
  # ============================================
  # FINAL SUMMARY
  # ============================================
  
  cat("============================================\n")
  cat("CASCADING QUALITY ASSESSMENT SUMMARY\n")
  cat("============================================\n\n")
  
  final_records <- nrow(final_log)
  final_cases <- n_distinct(as.data.frame(final_log)$case_id)
  
  # Overall quality (cumulative)
  overall_quality <- final_cases / original_cases
  
  # Create tracking dataframe
  tracking_df <- data.frame(
    Stage = tracking$stage,
    Records_In = tracking$records_in,
    Records_Out = tracking$records_out,
    Records_Removed = tracking$records_removed,
    Cases_In = tracking$cases_in,
    Cases_Out = tracking$cases_out,
    Cases_Removed = tracking$cases_removed,
    Stage_Score = round(tracking$score * 100, 2)
  )
  
  cat("PIPELINE FLOW:\n")
  print(tracking_df)
  
  cat(paste("\n\nORIGINAL LOG:", original_records, "records,", original_cases, "cases\n"))
  cat(paste("FINAL CLEAN LOG:", final_records, "records,", final_cases, "cases\n"))
  cat(paste("\nOVERALL DATA QUALITY:", round(overall_quality * 100, 2), "%\n"))
  cat(paste("(", original_cases - final_cases, "cases removed through pipeline)\n"))
  
  # Attribute-level scores for simulation parameter derivation
  attribute_scores <- list(
    # Based on cascading scores
    start_completeness = completeness_scores[["start"]],
    start_consistency = consistency_score,  # Applied to remaining data
    start_accuracy = accuracy_score,        # Applied to remaining data
    
    complete_completeness = completeness_scores[["complete"]],
    complete_consistency = consistency_score,
    complete_accuracy = accuracy_score,
    
    caseid_completeness = completeness_scores[["case_id"]],
    caseid_accuracy = accuracy_score,
    
    activity_completeness = completeness_scores[["activity"]],
    activity_consistency = consistency_score,
    activity_accuracy = accuracy_score,
    
    resource_completeness = completeness_scores[["originator"]],
    resource_consistency = consistency_score,
    resource_accuracy = accuracy_score
  )
  
  # Save results
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir)
  
  write.csv(tracking_df, file.path(results_dir, "cascading_quality_tracking.csv"), row.names = FALSE)
  
  save(tracking_df, completeness_results, consistency_results, accuracy_results,
       attribute_scores, final_log,
       file = file.path(results_dir, "cascading_quality_results.RData"))
  
  cat("\nSaved to:\n")
  cat("  - results/cascading_quality_tracking.csv\n")
  cat("  - results/cascading_quality_results.RData\n")
  
  # Return results
  return(list(
    # Stage-level results
    completeness = completeness_results,
    consistency = consistency_results,
    accuracy = accuracy_results,
    
    # Tracking
    tracking = tracking_df,
    
    # Scores for simulation parameter derivation
    attribute_scores = attribute_scores,
    
    # Stage scores
    completeness_score = completeness_score,
    consistency_score = consistency_score,
    accuracy_score = accuracy_score,
    
    # Overall
    overall_quality = overall_quality,
    
    # Data
    original_log = actlog,
    final_log = final_log,
    original_records = original_records,
    final_records = final_records,
    original_cases = original_cases,
    final_cases = final_cases
  ))
}


#' Quick cascading assessment with default config
#' 
#' @param actlog Activity log
#' @return Cascading quality results

quick_cascading_assessment <- function(actlog) {
  config <- list(
    working_hours_start = 0,
    working_hours_end = 24,
    min_duration = 0,
    max_duration = 480,
    expected_order = NULL,
    allowed_activities = NULL
  )
  
  cascading_quality_assessment(actlog, config)
}
