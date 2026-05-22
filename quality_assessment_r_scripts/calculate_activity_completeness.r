# Activity completeness: missing activity detection and incomplete cases

library(dplyr)
library(bupaR)
library(daqapo)

calculate_activity_completeness <- function(dataset, data_file, allowed_activities, mandatory_activities, conditional_rules = list()) {
  
  results_dir <- "results_activity"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("ACTIVITY LABEL COMPLETENESS ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog <- dataset
  
  actlog_df <- as.data.frame(actlog)
  
  total_records <- nrow(actlog_df)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Unique activities:", n_distinct(actlog_df$activity, na.rm = TRUE), "\n\n"))
  
  # 1. Missing values
  cat("============================================\n")
  cat("1. DETECT MISSING VALUES (activity column)\n")
  cat("============================================\n\n")
  
  na_count <- 0
  missing_values_result <- NULL
  
  missing_values_result <- tryCatch({
    missing_vals <- detect_missing_values(
      activitylog = actlog,
      level_of_aggregation = "column",
      column = "activity"
    )
    missing_vals
  }, error = function(e) {
    cat(paste("  Error:", e$message, "\n"))
    NULL
  })
  
  if(!is.null(missing_values_result)) {
    na_count <- nrow(missing_values_result)
    cat(paste("NA values found:", na_count, "\n"))
    
    if(na_count > 0) {
      cat("\nRows with missing activity:\n")
      print(as.data.frame(missing_values_result)[, c("case_id", "activity", "originator")])
      
      missing_file <- file.path(results_dir, "activity_missing_values.csv")
      write.csv(as.data.frame(missing_values_result), missing_file, row.names = FALSE)
      cat(paste("\n  Saved to:", missing_file, "\n"))
    }
  }
  
  # 2. Filter NA values
  cat("\n============================================\n")
  cat("2. FILTERING NA VALUES\n")
  cat("============================================\n\n")
  
  actlog_filtered <- actlog_df %>%
    filter(!is.na(activity))
  
  records_after_filter <- nrow(actlog_filtered)
  cat(paste("Records before:", total_records, "\n"))
  cat(paste("Records after filtering NA:", records_after_filter, "\n"))
  cat(paste("Removed:", na_count, "records with NA activity\n"))
  
  actlog_clean <- actlog_filtered %>%
    bupaR::activitylog(
      case_id = "case_id",
      activity_id = "activity",
      timestamps = c("start", "complete"),
      resource_id = "originator"
    )
  
  # 3. Incomplete cases (missing mandatory activities)
  cat("\n============================================\n")
  cat("3. DETECT INCOMPLETE CASES\n")
  cat("============================================\n")
  cat("Mandatory activities:", paste(mandatory_activities, collapse = ", "), "\n\n")
  
  incomplete_cases_count <- 0
  incomplete_cases_result <- NULL
  
  # Suppress verbose output from daqapo
  incomplete_cases_result <- suppressWarnings(tryCatch({
    capture.output(
      incomplete <- detect_incomplete_cases(
        activitylog = actlog_clean,
        activities = mandatory_activities
      ),
      file = nullfile()
    )
    incomplete
  }, error = function(e) {
    cat(paste("  Error:", e$message, "\n"))
    NULL
  }))
  
  if(!is.null(incomplete_cases_result)) {
    incomplete_df <- as.data.frame(incomplete_cases_result)
    
    if("case_ids" %in% colnames(incomplete_df) && nrow(incomplete_df) > 0) {
      all_case_ids <- unlist(strsplit(as.character(incomplete_df$case_ids), " - "))
      unique_incomplete_cases <- unique(all_case_ids)
      incomplete_cases_count <- length(unique_incomplete_cases)
      
      cat(paste("\nIncomplete cases found:", incomplete_cases_count, "unique cases\n"))
      cat("Incomplete case IDs:", paste(unique_incomplete_cases, collapse = ", "), "\n")
      
      cat("\nIncomplete cases details (by activity):\n")
      print(incomplete_cases_result)
      
      incomplete_file <- file.path(results_dir, "activity_incomplete_cases.csv")
      write.csv(incomplete_df, incomplete_file, row.names = FALSE)
      cat(paste("\n  Saved to:", incomplete_file, "\n"))
    } else {
      cat("\nNo incomplete cases found.\n")
    }
  }
  
  # 4. Conditional activity presence
  cat("\n============================================\n")
  cat("4. DETECT CONDITIONAL ACTIVITY PRESENCE\n")
  cat("============================================\n")
  cat("Condition: activity == 'prepare drinks' -> Review drinks must be present\n\n")
  
  conditional_result <- NULL
  conditional_violations_count <- 0
  
  if(length(conditional_rules) > 0) {
    actlog_df_clean <- as.data.frame(actlog_clean)
    
    for(rule in conditional_rules) {
      cat(paste("Condition: if", rule$condition_activity, "-> then", rule$required_activity, "must be present\n"))
      
      # Find cases where A exists but B is missing
      cases_with_a <- actlog_df_clean %>%
        filter(activity == rule$condition_activity) %>%
        pull(case_id) %>%
        unique()
      
      cases_with_b <- actlog_df_clean %>%
        filter(activity == rule$required_activity) %>%
        pull(case_id) %>%
        unique()
      
      cases_missing_b <- setdiff(cases_with_a, cases_with_b)
      
      rule_violations <- length(cases_missing_b)
      conditional_violations_count <- conditional_violations_count + rule_violations
      cat(paste("Violations found:", rule_violations, "cases (A present but B missing)\n"))
    }
  } else {
    cat("No conditional rules defined\n")
  }
  
  cond_file <- file.path(results_dir, "activity_conditional_violations.csv")
  cond_df <- data.frame(
    condition = ifelse(length(conditional_rules) > 0,
      paste(sapply(conditional_rules, function(r) paste(r$condition_activity, "->", r$required_activity)), collapse = "; "),
      "None"),
    violations_count = conditional_violations_count
  )
  write.csv(cond_df, cond_file, row.names = FALSE)
  cat(paste("  Saved to:", cond_file, "\n"))
  
  # 5. Per-Activity Completeness (case-based)
  cat("\n============================================\n")
  cat("5. PER-ACTIVITY COMPLETENESS\n")
  cat("============================================\n")
  cat("Calculating completeness for each mandatory activity across all cases\n\n")
  
  # Get unique cases
  unique_cases <- unique(actlog_filtered$case_id)
  total_cases <- length(unique_cases)
  
  cat(paste("Total unique cases:", total_cases, "\n\n"))
  
  # For each mandatory activity, count how many cases have it
  per_activity_completeness <- data.frame(
    activity = character(),
    cases_with_activity = integer(),
    cases_missing_activity = integer(),
    missing_case_ids = character(),
    completeness = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(act in mandatory_activities) {
    # Find cases that have this activity
    cases_with_act <- actlog_filtered %>%
      filter(activity == act) %>%
      pull(case_id) %>%
      unique()
    
    cases_with_count <- length(cases_with_act)
    cases_missing <- setdiff(unique_cases, cases_with_act)
    cases_missing_count <- length(cases_missing)
    completeness_pct <- round((cases_with_count / total_cases) * 100, 2)
    
    per_activity_completeness <- rbind(per_activity_completeness, data.frame(
      activity = act,
      cases_with_activity = cases_with_count,
      cases_missing_activity = cases_missing_count,
      missing_case_ids = paste(cases_missing, collapse = ", "),
      completeness = completeness_pct,
      stringsAsFactors = FALSE
    ))
    
    cat(paste(act, ":", completeness_pct, "% (", cases_missing_count, "cases missing)\n"))
    if(cases_missing_count > 0 && cases_missing_count <= 10) {
      cat(paste("  Missing in:", paste(cases_missing, collapse = ", "), "\n"))
    }
  }
  
  cat("\n")
  print(per_activity_completeness[, c("activity", "cases_with_activity", "cases_missing_activity", "completeness")])
  
  # Save per-activity completeness
  per_act_file <- file.path(results_dir, "activity_per_activity_completeness.csv")
  write.csv(per_activity_completeness, per_act_file, row.names = FALSE)
  cat(paste("\n  Saved to:", per_act_file, "\n"))
  
  # 6. Outliers summary
  cat("\n============================================\n")
  cat("6. OUTLIERS SUMMARY\n")
  cat("============================================\n")
  
  # Total outliers: NA + incomplete cases
  total_outliers <- na_count + incomplete_cases_count
  
  cat(paste("Total outliers:", total_outliers, "cases\n"))
  cat(paste("  - NA records:", na_count, "\n"))
  cat(paste("  - Incomplete cases:", incomplete_cases_count, "\n"))
  cat(paste("Conditional violations (info only):", conditional_violations_count, "\n"))
  cat(paste("Total cases:", total_cases, "\n"))
  
  # Calculate completeness at case level
  complete_cases <- total_cases - incomplete_cases_count
  activity_completeness <- round((complete_cases / total_cases) * 100, 2)
  
  cat(paste("Complete cases:", complete_cases, "\n"))
  cat(paste("Activity Completeness:", activity_completeness, "%\n"))
  
  return(list(
    total_records = total_records,
    total_cases = total_cases,
    na_count = na_count,
    incomplete_cases_count = incomplete_cases_count,
    conditional_violations_count = conditional_violations_count,
    total_outliers = total_outliers,
    complete_cases = complete_cases,
    activity_completeness = activity_completeness,
    missing_values_result = missing_values_result,
    incomplete_cases_result = incomplete_cases_result,
    conditional_result = conditional_result,
    per_activity_completeness = per_activity_completeness,
    actlog_clean = actlog_clean
  ))
}
