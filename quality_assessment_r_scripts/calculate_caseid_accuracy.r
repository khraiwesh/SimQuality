# Case ID accuracy: rule violations, abnormal duration, abnormal activity count

library(dplyr)
library(bupaR)
library(daqapo)
library(lubridate)

calculate_caseid_accuracy <- function(dataset, data_file, rule_activities) {
  
  results_dir <- "results_case_id"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("CASE ID ACCURACY ANALYSIS\n")
  cat("============================================\n\n")
  
  rule_activity <- rule_activities
  
  actlog <- dataset
  
  actlog_df <- as.data.frame(actlog)
  
  total_records <- nrow(actlog_df)
  total_cases <- n_distinct(actlog_df$case_id)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Unique cases:", total_cases, "\n\n"))
  
  # 1. Incorrect cases: cooccurring activities (rule violations)
  cat("============================================\n")
  cat("1. INCORRECT CASE CHECK (Rule Violations)\n")
  cat("============================================\n")
  cat(paste("Rule: Activities", paste(rule_activity, collapse = " + "), 
            "should NOT cooccur in same case\n\n"))
  
  incorrect_cases_count <- 0
  incorrect_cases_list <- c()
  rule_violation_records <- 0
  
  actlog_with_rule_activities <- actlog_df %>%
    filter(activity %in% rule_activity)
  
  if(nrow(actlog_with_rule_activities) > 0) {
    cases_grouped <- actlog_with_rule_activities %>%
      group_by(case_id) %>%
      summarise(
        activities_present = list(unique(activity)),
        activity_count = n_distinct(activity),
        .groups = "drop"
      )
    
    cooccurring_cases <- cases_grouped %>%
      filter(sapply(activities_present, function(acts) {
        all(rule_activity %in% acts)
      }))
    
    incorrect_cases_count <- nrow(cooccurring_cases)
    
    if(incorrect_cases_count > 0) {
      incorrect_cases_list <- cooccurring_cases$case_id
      cat(paste("Rule violations found:", incorrect_cases_count, "cases\n"))
      cat("Case IDs:", paste(incorrect_cases_list, collapse = ", "), "\n")
      
      rule_violation_records <- actlog_df %>%
        filter(case_id %in% incorrect_cases_list) %>%
        nrow()
      cat(paste("Records in violated cases:", rule_violation_records, "\n"))
      
      violation_file <- file.path(results_dir, "case_rule_violations.csv")
      cooccurring_cases_export <- cooccurring_cases %>%
        mutate(activities_present = sapply(activities_present, function(x) paste(x, collapse = ", ")))
      write.csv(cooccurring_cases_export, violation_file, row.names = FALSE)
      cat(paste("  Saved to:", violation_file, "\n"))
    } else {
      cat("No rule violations detected\n")
    }
  } else {
    cat("No cases found with rule activities (no violations possible)\n")
  }
  
  # 2. Abnormal case duration (potential merged cases)
  cat("\n============================================\n")
  cat("2. ABNORMAL CASE DURATION (Merged Case Detection)\n")
  cat("============================================\n")
  cat("Cases with abnormally long duration may be merged instances\n\n")
  
  abnormal_duration_cases <- c()
  abnormal_duration_records <- 0
  
  case_durations <- tryCatch({
    actlog_df %>%
      mutate(
        # Try strict format first; fall back to lubridate auto-parse so that
        # mixed-format timestamps (Consistency noise) don't produce NA and
        # artificially inflate case duration → false CaseID accuracy loss.
        start_ts    = lubridate::parse_date_time(start,    orders = c("Ymd HMS", "dmY HMS", "dmY HM", "Ymd HM", "Ymd_HMS")),
        complete_ts = lubridate::parse_date_time(complete, orders = c("Ymd HMS", "dmY HMS", "dmY HM", "Ymd HM", "Ymd_HMS"))
      ) %>%
      filter(!is.na(start_ts) & !is.na(complete_ts)) %>%   # skip rows where BOTH timestamps unparseable
      group_by(case_id) %>%
      summarise(
        case_start = min(start_ts, na.rm = TRUE),
        case_end = max(complete_ts, na.rm = TRUE),
        duration_hours = as.numeric(difftime(max(complete_ts, na.rm = TRUE), 
                                              min(start_ts, na.rm = TRUE), 
                                              units = "hours")),
        .groups = "drop"
      ) %>%
      filter(!is.na(duration_hours) & is.finite(duration_hours) & duration_hours >= 0)
  }, error = function(e) {
    cat(paste("  Error computing durations:", e$message, "\n"))
    NULL
  })
  
  if(!is.null(case_durations) && nrow(case_durations) > 0) {
    median_duration <- median(case_durations$duration_hours, na.rm = TRUE)
    iqr_duration <- IQR(case_durations$duration_hours, na.rm = TRUE)
    upper_fence <- median_duration + 3 * iqr_duration
    
    cat(paste("Median case duration:", round(median_duration, 2), "hours\n"))
    cat(paste("IQR:", round(iqr_duration, 2), "hours\n"))
    cat(paste("Upper fence (median + 3*IQR):", round(upper_fence, 2), "hours\n\n"))
    
    outlier_cases <- case_durations %>%
      filter(duration_hours > upper_fence)
    
    if(nrow(outlier_cases) > 0) {
      abnormal_duration_cases <- outlier_cases$case_id
      cat(paste("Abnormally long cases:", nrow(outlier_cases), "\n"))
      cat("Case IDs:", paste(abnormal_duration_cases, collapse = ", "), "\n")
      
      abnormal_duration_records <- actlog_df %>%
        filter(case_id %in% abnormal_duration_cases) %>%
        nrow()
      cat(paste("Records in abnormal cases:", abnormal_duration_records, "\n"))
      
      duration_file <- file.path(results_dir, "case_abnormal_duration.csv")
      write.csv(outlier_cases, duration_file, row.names = FALSE)
      cat(paste("  Saved to:", duration_file, "\n"))
    } else {
      cat("No abnormally long cases detected\n")
    }
  }
  
  # 3. Abnormal activity count (potential merged cases)
  cat("\n============================================\n")
  cat("3. ABNORMAL ACTIVITY COUNT (Merged Case Detection)\n")
  cat("============================================\n")
  cat("Cases with abnormally many events may be merged instances\n\n")
  
  abnormal_count_cases <- c()
  abnormal_count_records <- 0
  
  case_event_counts <- actlog_df %>%
    group_by(case_id) %>%
    summarise(event_count = n(), .groups = "drop")
  
  median_count <- median(case_event_counts$event_count, na.rm = TRUE)
  iqr_count <- IQR(case_event_counts$event_count, na.rm = TRUE)
  upper_fence_count <- median_count + 3 * iqr_count
  
  cat(paste("Median events per case:", median_count, "\n"))
  cat(paste("IQR:", round(iqr_count, 2), "\n"))
  cat(paste("Upper fence (median + 3*IQR):", round(upper_fence_count, 2), "\n\n"))
  
  outlier_count_cases <- case_event_counts %>%
    filter(event_count > upper_fence_count)
  
  if(nrow(outlier_count_cases) > 0) {
    abnormal_count_cases <- outlier_count_cases$case_id
    cat(paste("Cases with abnormally many events:", nrow(outlier_count_cases), "\n"))
    cat("Case IDs:", paste(abnormal_count_cases, collapse = ", "), "\n")
    
    abnormal_count_records <- actlog_df %>%
      filter(case_id %in% abnormal_count_cases) %>%
      nrow()
    cat(paste("Records in abnormal cases:", abnormal_count_records, "\n"))
    
    count_file <- file.path(results_dir, "case_abnormal_event_count.csv")
    write.csv(outlier_count_cases, count_file, row.names = FALSE)
    cat(paste("  Saved to:", count_file, "\n"))
  } else {
    cat("No cases with abnormally many events detected\n")
  }
  
  # 4. Out-of-range case IDs (IQR-based)
  cat("\n============================================\n")
  cat("4. OUT-OF-RANGE CASE ID DETECTION\n")
  cat("============================================\n")
  cat("Case IDs far outside the typical numeric range are inaccurate\n\n")

  outofrange_cases <- c()
  outofrange_records <- 0

  numeric_case_ids <- suppressWarnings(as.numeric(as.character(actlog_df$case_id)))
  valid_numeric <- numeric_case_ids[!is.na(numeric_case_ids)]

  if(length(valid_numeric) > 0) {
    median_id  <- median(valid_numeric)
    iqr_id     <- IQR(valid_numeric)
    # Use a generous fence — only truly out-of-distribution IDs are flagged
    lower_fence_id <- median_id - 3 * max(iqr_id, median_id * 0.5)
    upper_fence_id <- median_id + 3 * max(iqr_id, median_id * 0.5)

    cat(paste("Median case ID:", median_id, "\n"))
    cat(paste("IQR:", round(iqr_id, 2), "\n"))
    cat(paste("Valid range: [", round(lower_fence_id, 0), ",", round(upper_fence_id, 0), "]\n\n"))

    outofrange_idx <- which(!is.na(numeric_case_ids) &
                              (numeric_case_ids < lower_fence_id | numeric_case_ids > upper_fence_id))

    if(length(outofrange_idx) > 0) {
      outofrange_cases <- unique(actlog_df$case_id[outofrange_idx])
      outofrange_records <- length(outofrange_idx)
      cat(paste("Out-of-range case IDs found:", length(outofrange_cases), "unique IDs\n"))
      cat(paste("Records affected:", outofrange_records, "\n"))

      oor_file <- file.path(results_dir, "case_outofrange_ids.csv")
      write.csv(data.frame(case_id = outofrange_cases), oor_file, row.names = FALSE)
      cat(paste("  Saved to:", oor_file, "\n"))
    } else {
      cat("No out-of-range case IDs detected\n")
    }
  } else {
    cat("Case IDs are not numeric — skipping range check\n")
  }

  # 5. Accuracy calculation
  cat("\n============================================\n")
  cat("5. CASE ID ACCURACY CALCULATION\n")
  cat("============================================\n\n")
  
  # Combine all problematic case IDs (unique, avoid double-counting)
  all_problematic_cases <- unique(c(incorrect_cases_list, abnormal_duration_cases, abnormal_count_cases, outofrange_cases))
  
  total_outlier_records <- actlog_df %>%
    filter(case_id %in% all_problematic_cases) %>%
    nrow()
  
  accurate_records <- total_records - total_outlier_records
  accuracy <- accurate_records / total_records
  accuracy_percentage <- accuracy * 100
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Rule violation cases:", incorrect_cases_count, "(", rule_violation_records, "records )\n"))
  cat(paste("Abnormal duration cases:", length(abnormal_duration_cases), "(", abnormal_duration_records, "records )\n"))
  cat(paste("Abnormal activity count cases:", length(abnormal_count_cases), "(", abnormal_count_records, "records )\n"))
  cat(paste("Out-of-range case ID records:", outofrange_records, "\n"))
  cat(paste("Total problematic cases (unique):", length(all_problematic_cases), "\n"))
  cat(paste("Total outlier records:", total_outlier_records, "\n"))
  cat(paste("Accurate records:", accurate_records, "\n"))
  cat(paste("Case ID Accuracy:", round(accuracy_percentage, 2), "%\n"))
  
  accuracy_summary <- data.frame(
    metric = c("total_records", "total_cases", "rule_violation_cases", "abnormal_duration_cases",
               "abnormal_count_cases", "outofrange_cases", "total_problematic_cases", "total_outlier_records", "accuracy_percent"),
    value = c(total_records, total_cases, incorrect_cases_count, length(abnormal_duration_cases),
              length(abnormal_count_cases), length(outofrange_cases), length(all_problematic_cases), total_outlier_records, round(accuracy_percentage, 2))
  )
  
  summary_file <- file.path(results_dir, "case_accuracy_summary.csv")
  write.csv(accuracy_summary, summary_file, row.names = FALSE)
  cat(paste("  Saved to:", summary_file, "\n"))
  
  return(list(
    total_records = total_records,
    total_cases = total_cases,
    incorrect_cases_count = incorrect_cases_count,
    incorrect_cases_list = incorrect_cases_list,
    abnormal_duration_cases = abnormal_duration_cases,
    abnormal_count_cases = abnormal_count_cases,
    outofrange_cases = outofrange_cases,
    all_problematic_cases = all_problematic_cases,
    total_outlier_records = total_outlier_records,
    accuracy = accuracy,
    accuracy_percentage = accuracy_percentage
  ))
}
