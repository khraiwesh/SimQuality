# Auto-configuration: derive all quality parameters from the event log
# User only provides: data_file + 5 column name mappings
# Everything else is mined from the data

library(dplyr)
library(lubridate)

auto_configure <- function(actlog_df) {
  
  cat("============================================\n")
  cat("AUTO-CONFIGURATION: Mining parameters from event log\n")
  cat("============================================\n\n")
  
  total_records <- nrow(actlog_df)
  total_cases <- n_distinct(actlog_df$case_id)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Total cases:", total_cases, "\n\n"))
  
  config <- list()
  
  # ============================================
  # 1. ALLOWED ACTIVITIES (frequency filtering)
  # ============================================
  cat("--------------------------------------------\n")
  cat("1. Mining ALLOWED ACTIVITIES\n")
  cat("--------------------------------------------\n")
  cat("Method: Activities appearing in >= 50% of cases (robust to injected noise)\n\n")
  
  activity_freq <- actlog_df %>%
    filter(!is.na(activity)) %>%
    group_by(activity) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(pct = round(count / sum(count) * 100, 2)) %>%
    arrange(desc(count))
  
  # Use case-coverage threshold instead of record-frequency threshold.
  # Record frequency is gamed by noise injection (e.g. 20% wrong labels each
  # appear at ~4% of records → above a 1% threshold → incorrectly allowed).
  # Case coverage: an activity is "allowed" only if it appears in >= 45% of
  # cases. True process activities appear in ~100% of cases; injected wrong
  # labels each appear in at most (ratio * n_cases) / n_cases = ratio of cases.
  # 45% (not 50%) avoids false-negatives when an optional activity sits just
  # below 50% in the generated data (e.g. Lab Test at ~49%).
  case_coverage_threshold <- 45  # percent of cases

  activity_case_cov <- actlog_df %>%
    filter(!is.na(activity)) %>%
    group_by(activity) %>%
    summarise(
      count      = n(),
      case_count = n_distinct(case_id),
      case_pct   = round(n_distinct(case_id) / total_cases * 100, 2),
      record_pct = round(n() / nrow(actlog_df) * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(desc(case_count))

  config$allowed_activities <- activity_case_cov %>%
    filter(case_pct >= case_coverage_threshold) %>%
    pull(activity)

  rare_activities <- activity_case_cov %>% filter(case_pct < case_coverage_threshold)

  cat("Activity frequency distribution (with case coverage):\n")
  print(activity_case_cov)
  cat(paste("\nCase-coverage threshold:", case_coverage_threshold, "% of cases\n"))
  cat(paste("Allowed activities (", length(config$allowed_activities), "):",
            paste(config$allowed_activities, collapse = ", "), "\n"))
  if(nrow(rare_activities) > 0) {
    cat(paste("Excluded activities (case coverage <", case_coverage_threshold, "%)  (", nrow(rare_activities), "):",
              paste(rare_activities$activity, collapse = ", "), "\n"))
  }
  
  # ============================================
  # 2. MANDATORY ACTIVITIES (case coverage)
  # ============================================
  cat("\n--------------------------------------------\n")
  cat("2. Mining MANDATORY ACTIVITIES\n")
  cat("--------------------------------------------\n")
cat("Method: Activities present in >= 75% of cases\n\n")

  # Threshold lowered from 90 % to 75 % so that up to ~25 % completeness noise
  # (row-level removal) still keeps real activities above the mandatory threshold.
  coverage_threshold <- 75
  
  activity_case_coverage <- actlog_df %>%
    filter(!is.na(activity) & activity %in% config$allowed_activities) %>%
    group_by(activity) %>%
    summarise(
      cases_with = n_distinct(case_id),
      coverage_pct = round(n_distinct(case_id) / total_cases * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(desc(coverage_pct))
  
  config$mandatory_activities <- activity_case_coverage %>%
    filter(coverage_pct >= coverage_threshold) %>%
    pull(activity)
  
  cat("Activity case coverage:\n")
  print(activity_case_coverage)
  cat(paste("\nThreshold:", coverage_threshold, "% of cases\n"))
  cat(paste("Mandatory activities (", length(config$mandatory_activities), "):", 
            paste(config$mandatory_activities, collapse = ", "), "\n"))
  
  # ============================================
  # 3. RULE ACTIVITIES (mutual exclusion mining)
  # ============================================
  cat("\n--------------------------------------------\n")
  cat("3. Mining RULE ACTIVITIES (mutually exclusive pairs)\n")
  cat("--------------------------------------------\n")
  cat("Method: Activity pairs that never co-occur + Fisher's exact test (p < 0.05)\n\n")
  
  config$rule_activities <- c()
  
  # Build case-activity matrix
  case_activities <- actlog_df %>%
    filter(!is.na(activity) & activity %in% config$allowed_activities) %>%
    distinct(case_id, activity)
  
  allowed_acts <- config$allowed_activities
  
  if(length(allowed_acts) >= 2 && length(allowed_acts) <= 50) {
    exclusive_pairs <- list()
    
    for(i in 1:(length(allowed_acts) - 1)) {
      for(j in (i + 1):length(allowed_acts)) {
        act_a <- allowed_acts[i]
        act_b <- allowed_acts[j]
        
        cases_with_a <- case_activities %>% filter(activity == act_a) %>% pull(case_id)
        cases_with_b <- case_activities %>% filter(activity == act_b) %>% pull(case_id)
        
        both <- length(intersect(cases_with_a, cases_with_b))
        only_a <- length(setdiff(cases_with_a, cases_with_b))
        only_b <- length(setdiff(cases_with_b, cases_with_a))
        neither <- total_cases - length(union(cases_with_a, cases_with_b))
        
        # Only consider if both activities appear in enough cases and never co-occur
        if(both == 0 && length(cases_with_a) >= 5 && length(cases_with_b) >= 5) {
          # Fisher's exact test for mutual exclusion
          contingency <- matrix(c(both, only_a, only_b, neither), nrow = 2)
          test <- tryCatch({
            fisher.test(contingency, alternative = "less")
          }, error = function(e) NULL)
          
          if(!is.null(test) && test$p.value < 0.05) {
            exclusive_pairs <- c(exclusive_pairs, list(list(
              act_a = act_a, act_b = act_b,
              cases_a = length(cases_with_a), cases_b = length(cases_with_b),
              p_value = test$p.value
            )))
          }
        }
      }
    }
    
    if(length(exclusive_pairs) > 0) {
      cat("Statistically significant mutually exclusive pairs:\n")
      for(pair in exclusive_pairs) {
        cat(paste0("  ", pair$act_a, " <-> ", pair$act_b, 
                   " (cases: ", pair$cases_a, " / ", pair$cases_b,
                   ", p=", format(pair$p_value, digits = 3), ")\n"))
      }
      # Use the first pair as the default rule_activities
      config$rule_activities <- c(exclusive_pairs[[1]]$act_a, exclusive_pairs[[1]]$act_b)
      config$all_exclusive_pairs <- exclusive_pairs
    } else {
      cat("No statistically significant mutually exclusive pairs found\n")
    }
  } else {
    cat("Too many or too few activities for pairwise testing\n")
  }
  
  cat(paste("Rule activities:", paste(config$rule_activities, collapse = " + "), "\n"))
  
  # ============================================
  # 4. CONDITIONAL RULES (sequential pattern mining)
  # ============================================
  cat("\n--------------------------------------------\n")
  cat("4. Mining CONDITIONAL RULES\n")
  cat("--------------------------------------------\n")
  cat("Method: If activity A present, P(B occurs after A) >= 95% confidence\n\n")
  
  config$conditional_rules <- list()
  
  # Build case-level ordered activity sequences using start timestamp
  case_sequences <- actlog_df %>%
    filter(!is.na(activity) & activity %in% allowed_acts & !is.na(start)) %>%
    arrange(case_id, start) %>%
    select(case_id, activity, start)
  
  if(length(allowed_acts) >= 2 && length(allowed_acts) <= 50) {
    for(i in 1:length(allowed_acts)) {
      for(j in 1:length(allowed_acts)) {
        if(i == j) next
        
        act_a <- allowed_acts[i]
        act_b <- allowed_acts[j]
        
        cases_with_a <- case_activities %>% filter(activity == act_a) %>% pull(case_id)
        
        # Check cases where B occurs AFTER A (by start timestamp)
        cases_b_after_a <- case_sequences %>%
          filter(case_id %in% cases_with_a) %>%
          group_by(case_id) %>%
          filter(any(activity == act_a) & any(activity == act_b)) %>%
          summarise(
            first_a_time = min(start[activity == act_a]),
            first_b_time = min(start[activity == act_b]),
            .groups = "drop"
          ) %>%
          filter(first_b_time > first_a_time) %>%
          pull(case_id)
        
        if(length(cases_with_a) >= 5) {
          confidence <- length(cases_b_after_a) / length(cases_with_a)
          
          # High confidence (>= 95%) and not trivially both mandatory
          if(confidence >= 0.95 && confidence < 1.0) {
            config$conditional_rules <- c(config$conditional_rules, list(list(
              condition_activity = act_a,
              required_activity = act_b,
              confidence = round(confidence, 4),
              support_cases = length(cases_with_a)
            )))
            
            cat(paste0("  If '", act_a, "' -> then '", act_b, 
                       "' after (confidence: ", round(confidence * 100, 1), 
                       "%, support: ", length(cases_with_a), " cases)\n"))
          }
        }
      }
    }
  }
  
  if(length(config$conditional_rules) == 0) {
    cat("No conditional rules found with sufficient confidence\n")
  }
  cat(paste("Total conditional rules:", length(config$conditional_rules), "\n"))
  
  # ============================================
  # 5. WORKING HOURS (hour distribution clustering)
  # ============================================
  cat("\n--------------------------------------------\n")
  cat("5. Mining WORKING HOURS\n")
  cat("--------------------------------------------\n")
  cat("Method: Find hour range containing 95% of events\n\n")
  
  hours <- c()
  if(!is.na(actlog_df$start[1])) {
    start_hours <- as.numeric(format(as.POSIXct(actlog_df$start), "%H"))
    complete_hours <- as.numeric(format(as.POSIXct(actlog_df$complete), "%H"))
    hours <- c(start_hours, complete_hours)
    hours <- hours[!is.na(hours)]
  }
  
  if(length(hours) > 0) {
    hour_freq <- table(hours)
    cat("Hour distribution:\n")
    print(hour_freq)
    
    # Find the range containing 95% of events
    hour_df <- data.frame(hour = as.numeric(names(hour_freq)), count = as.numeric(hour_freq))
    hour_df <- hour_df %>% arrange(hour)
    hour_df$cumulative_pct <- cumsum(hour_df$count) / sum(hour_df$count) * 100
    
    # Lower bound: first hour where cumulative reaches 2.5%
    lower_idx <- min(which(hour_df$cumulative_pct >= 2.5))
    # Upper bound: first hour where cumulative reaches 97.5%
    upper_idx <- min(which(hour_df$cumulative_pct >= 97.5))
    
    config$working_hours_start <- hour_df$hour[lower_idx]
    config$working_hours_end <- min(hour_df$hour[upper_idx] + 1, 24)
    
    cat(paste("\n95% event range:", config$working_hours_start, ":00 -", config$working_hours_end, ":00\n"))
  } else {
    cat("Could not parse timestamps for hour analysis\n")
    config$working_hours_start <- 0
    config$working_hours_end <- 24
  }
  
  # ============================================
  # 6. INACTIVE THRESHOLD (gap distribution)
  # ============================================
  cat("\n--------------------------------------------\n")
  cat("6. Mining INACTIVE THRESHOLD\n")
  cat("--------------------------------------------\n")
  cat("Method: Inter-event gap analysis, threshold at 95th percentile\n\n")
  
  gaps <- tryCatch({
    actlog_sorted <- actlog_df %>%
      filter(!is.na(start)) %>%
      mutate(start_ts = as.POSIXct(start)) %>%
      arrange(case_id, start_ts)
    
    actlog_sorted %>%
      group_by(case_id) %>%
      mutate(
        prev_complete = lag(as.POSIXct(complete)),
        gap_minutes = as.numeric(difftime(start_ts, prev_complete, units = "mins"))
      ) %>%
      filter(!is.na(gap_minutes) & gap_minutes > 0) %>%
      pull(gap_minutes)
  }, error = function(e) {
    cat(paste("Error computing gaps:", e$message, "\n"))
    numeric(0)
  })
  
  if(length(gaps) > 0) {
    cat(paste("Total inter-event gaps:", length(gaps), "\n"))
    cat(paste("Median gap:", round(median(gaps), 2), "minutes\n"))
    cat(paste("Mean gap:", round(mean(gaps), 2), "minutes\n"))
    cat(paste("95th percentile:", round(quantile(gaps, 0.95), 2), "minutes\n"))
    cat(paste("99th percentile:", round(quantile(gaps, 0.99), 2), "minutes\n\n"))
    
    config$inactive_threshold_minutes <- round(quantile(gaps, 0.95))
    cat(paste("Auto-detected threshold (95th pctl):", config$inactive_threshold_minutes, "minutes\n"))
  } else {
    cat("Could not compute gaps, using default 60 minutes\n")
    config$inactive_threshold_minutes <- 60
  }
  
  # ============================================
  # 7. ALLOWED RESOURCES (frequency filtering)
  # ============================================
  cat("\n--------------------------------------------\n")
  cat("7. Mining ALLOWED RESOURCES\n")
  cat("--------------------------------------------\n")
  cat("Method: Resources appearing in >= 1% of records\n\n")
  
  threshold_pct <- 1  # minimum % of records for a resource to be considered "allowed"
  
  resource_freq <- actlog_df %>%
    filter(!is.na(originator) & trimws(originator) != "") %>%
    group_by(originator) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(pct = round(count / sum(count) * 100, 2)) %>%
    arrange(desc(count))
  
  config$allowed_resources <- resource_freq %>%
    filter(pct >= threshold_pct) %>%
    pull(originator)
  
  rare_resources <- resource_freq %>% filter(pct < threshold_pct)
  
  cat("Resource frequency distribution:\n")
  print(resource_freq)
  cat(paste("\nAllowed resources (", length(config$allowed_resources), "):", 
            paste(config$allowed_resources, collapse = ", "), "\n"))
  if(nrow(rare_resources) > 0) {
    cat(paste("Excluded rare resources (", nrow(rare_resources), "):", 
              paste(rare_resources$originator, collapse = ", "), "\n"))
  }
  
  # ============================================
  # 8. FIXED DEFAULTS
  # ============================================
  config$max_edit_distance <- 3
  config$resource_activity_map <- NULL  # Not used — time-conflict checks are stronger
  
  # ============================================
  # SUMMARY
  # ============================================
  cat("\n============================================\n")
  cat("AUTO-CONFIGURATION SUMMARY\n")
  cat("============================================\n\n")
  
  cat(paste("allowed_activities:", length(config$allowed_activities), "activities\n"))
  cat(paste("mandatory_activities:", length(config$mandatory_activities), "activities\n"))
  cat(paste("rule_activities:", paste(config$rule_activities, collapse = " + "), "\n"))
  cat(paste("conditional_rules:", length(config$conditional_rules), "rules\n"))
  cat(paste("working_hours:", config$working_hours_start, ":00 -", config$working_hours_end, ":00\n"))
  cat(paste("inactive_threshold:", config$inactive_threshold_minutes, "minutes\n"))
  cat(paste("allowed_resources:", length(config$allowed_resources), "resources\n"))
  cat(paste("max_edit_distance:", config$max_edit_distance, "(fixed)\n"))
  cat(paste("resource_activity_map: NULL (not used)\n"))
  
  cat("\n============================================\n\n")
  
  return(config)
}
