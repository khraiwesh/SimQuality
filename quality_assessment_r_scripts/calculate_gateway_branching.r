# Gateway Branching Probability Mining
# 
# Discovers decision points (gateways) from the event log:
# - If Activity A has more than one possible next activity across cases → gateway
# - Computes branching probability for each outgoing branch
# - Filters noise: rare transitions below a threshold are flagged as noise
# - Assesses data quality per gateway using same C/A/Co approach as attribute-level
# - Keeps descriptive metrics (noise, entropy, probabilities) as supplementary output

library(dplyr)

calculate_gateway_branching <- function(
    actlog_df,
    case_col = "case_id",
    activity_col = "activity",
    start_col = "start",
    complete_col = "complete",
    allowed_activities = NULL,
    max_edit_distance = 3,
    working_hours_start = 0,
    working_hours_end = 24,
    noise_threshold = 0.05,  # Transitions appearing in < 5% of cases at a gateway are noise
    min_cases = 3,           # Minimum cases to consider a transition real
    dimension_weights = c(completeness = 1/3, accuracy = 1/3, consistency = 1/3)
) {
  
  cat("\n############################################\n")
  cat("GATEWAY BRANCHING PROBABILITY ANALYSIS\n")
  cat("############################################\n\n")
  
  # ============================================
  # 1. BUILD DIRECTLY-FOLLOWS RELATIONS
  # ============================================
  
  cat("1. Mining directly-follows relations...\n\n")
  
  # Sort events within each case by start timestamp
  log_sorted <- actlog_df %>%
    filter(!is.na(!!sym(activity_col)) & !is.na(!!sym(start_col))) %>%
    arrange(!!sym(case_col), !!sym(start_col)) %>%
    group_by(!!sym(case_col)) %>%
    mutate(
      next_activity = lead(!!sym(activity_col))
    ) %>%
    ungroup()
  
  # Extract all directly-follows pairs (excluding end-of-case NAs)
  df_relations <- log_sorted %>%
    filter(!is.na(next_activity)) %>%
    select(!!sym(case_col), source = !!sym(activity_col), target = next_activity)
  
  total_transitions <- nrow(df_relations)
  cat(paste("Total transitions:", total_transitions, "\n"))
  cat(paste("Unique source activities:", n_distinct(df_relations$source), "\n"))
  cat(paste("Unique transition types:", nrow(distinct(df_relations, source, target)), "\n\n"))
  
  # ============================================
  # 2. IDENTIFY GATEWAYS (DECISION POINTS)
  # ============================================
  
  cat("2. Identifying gateways (activities with >1 successor)...\n\n")
  
  # Count transitions per source → target
  transition_counts <- df_relations %>%
    group_by(source, target) %>%
    summarise(
      count = n(),
      cases = n_distinct(!!sym(case_col)),
      .groups = "drop"
    )
  
  # For each source, count how many distinct targets
  source_stats <- transition_counts %>%
    group_by(source) %>%
    summarise(
      total_count = sum(count),
      total_cases = sum(cases),
      n_successors = n_distinct(target),
      successors = paste(target, collapse = ", "),
      .groups = "drop"
    )
  
  # Gateways = activities with more than 1 successor
  gateways <- source_stats %>% filter(n_successors > 1)
  non_gateways <- source_stats %>% filter(n_successors == 1)
  
  cat(paste("Decision points (gateways):", nrow(gateways), "\n"))
  cat(paste("Sequential activities (1 successor):", nrow(non_gateways), "\n\n"))
  
  if (nrow(gateways) == 0) {
    cat("No gateways found — all activities have exactly one successor.\n")
    cat("This means the process has no branching (purely sequential).\n\n")
    
    result_df <- data.frame(
      Gateway_Activity = character(),
      Branch_Target = character(),
      Transition_Count = integer(),
      Case_Count = integer(),
      Branch_Probability = numeric(),
      Is_Noise = logical(),
      stringsAsFactors = FALSE
    )
    
    write.csv(result_df, "results/gateway_branching_probabilities.csv", row.names = FALSE)
    
    return(list(
      gateways = result_df,
      gateway_summary = data.frame(),
      gateway_quality = data.frame(),
      per_gateway_data_quality = data.frame(),
      noise_transitions = data.frame(),
      n_gateways = 0,
      avg_data_quality = NA,
      min_data_quality = NA
    ))
  }
  
  # ============================================
  # 3. COMPUTE BRANCHING PROBABILITIES
  # ============================================
  
  cat("3. Computing branching probabilities per gateway...\n\n")
  
  gateway_details <- list()
  
  for (i in 1:nrow(gateways)) {
    gw <- gateways$source[i]
    
    # Get transitions from this gateway
    gw_transitions <- transition_counts %>%
      filter(source == gw) %>%
      mutate(
        probability = count / sum(count),
        case_probability = cases / sum(cases)
      ) %>%
      arrange(desc(probability))
    
    total_at_gateway <- sum(gw_transitions$count)
    
    # Dynamic noise threshold: stricter when more data is available
    # For small gateway data (<50 cases), use provided threshold
    # For larger, scale down: threshold = max(0.01, noise_threshold * 50 / total_at_gateway)
    effective_noise_threshold <- if (total_at_gateway > 50) {
      max(0.01, noise_threshold * 50 / total_at_gateway)
    } else {
      noise_threshold
    }
    
    # Flag noise: transitions below dynamic threshold OR with too few cases
    gw_transitions <- gw_transitions %>%
      mutate(
        is_noise = (probability < effective_noise_threshold) | (cases < min_cases)
      )
    
    n_real <- sum(!gw_transitions$is_noise)
    n_noise <- sum(gw_transitions$is_noise)
    
    cat(paste("  Gateway:", gw, "\n"))
    cat(paste("    Total transitions:", total_at_gateway, "\n"))
    cat(paste("    Branches:", nrow(gw_transitions),
              "(", n_real, "real +", n_noise, "noise )\n"))
    
    for (j in 1:nrow(gw_transitions)) {
      row <- gw_transitions[j, ]
      noise_tag <- if (row$is_noise) " [NOISE]" else ""
      cat(paste0("      → ", row$target, ": ", 
                 round(row$probability * 100, 1), "% (",
                 row$count, " transitions, ", row$cases, " cases)",
                 noise_tag, "\n"))
    }
    cat("\n")
    
    gateway_details[[gw]] <- gw_transitions
  }
  
  # ============================================
  # 4. COMBINE RESULTS
  # ============================================
  
  all_gateway_branches <- bind_rows(lapply(names(gateway_details), function(gw) {
    gateway_details[[gw]] %>%
      transmute(
        Gateway_Activity = source,
        Branch_Target = target,
        Transition_Count = count,
        Case_Count = cases,
        Branch_Probability = round(probability, 4),
        Case_Probability = round(case_probability, 4),
        Is_Noise = is_noise
      )
  }))
  
  noise_transitions <- all_gateway_branches %>% filter(Is_Noise)
  real_transitions <- all_gateway_branches %>% filter(!Is_Noise)
  
  # ============================================
  # 5. RECOMPUTE CLEAN PROBABILITIES (without noise)
  # ============================================
  
  cat("4. Recomputing clean probabilities (noise removed)...\n\n")
  
  clean_branches <- real_transitions %>%
    group_by(Gateway_Activity) %>%
    mutate(
      Clean_Probability = round(Transition_Count / sum(Transition_Count), 4)
    ) %>%
    ungroup()
  
  # ============================================
  # 6. GATEWAY QUALITY ASSESSMENT
  # ============================================
  
  cat("5. Assessing gateway quality...\n\n")
  
  gateway_quality <- data.frame(
    Gateway_Activity = character(),
    Total_Branches = integer(),
    Real_Branches = integer(),
    Noise_Branches = integer(),
    Total_Transitions = integer(),
    Noise_Transitions = integer(),
    Noise_Ratio = numeric(),
    Data_Support = integer(),       # cases going through this gateway
    Entropy = numeric(),            # higher entropy = more uncertain branching
    Quality_Score = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (gw in names(gateway_details)) {
    gw_data <- gateway_details[[gw]]
    
    total_branches <- nrow(gw_data)
    real_branches <- sum(!gw_data$is_noise)
    noise_branches <- sum(gw_data$is_noise)
    total_trans <- sum(gw_data$count)
    noise_trans <- sum(gw_data$count[gw_data$is_noise])
    noise_ratio <- noise_trans / total_trans
    data_support <- sum(gw_data$cases)
    
    # Shannon entropy of the probability distribution (real branches only)
    real_probs <- gw_data$probability[!gw_data$is_noise]
    if (length(real_probs) > 0) {
      real_probs_norm <- real_probs / sum(real_probs)  # re-normalize
      entropy <- -sum(real_probs_norm * log2(real_probs_norm))
      max_entropy <- log2(length(real_probs_norm))  # max possible entropy
    } else {
      entropy <- 0
      max_entropy <- 0
    }
    
    # Quality score components:
    # (a) Low noise ratio is good
    noise_quality <- 1 - noise_ratio
    
    # (b) Sufficient data support (more cases = more reliable probability estimate)
    # Use log scale: 10 cases = 0.5, 50 cases = 0.85, 100+ cases = ~1.0
    support_quality <- min(1, log(max(data_support, 1) + 1) / log(101))
    
    # (c) Not too many branches (could indicate noise even if individually above threshold)
    branch_quality <- if (real_branches <= 5) 1.0 else max(0.5, 1 - (real_branches - 5) * 0.1)
    
    quality_score <- mean(c(noise_quality, support_quality, branch_quality))
    
    gateway_quality <- rbind(gateway_quality, data.frame(
      Gateway_Activity = gw,
      Total_Branches = total_branches,
      Real_Branches = real_branches,
      Noise_Branches = noise_branches,
      Total_Transitions = total_trans,
      Noise_Transitions = noise_trans,
      Noise_Ratio = round(noise_ratio, 4),
      Data_Support = data_support,
      Entropy = round(entropy, 4),
      Quality_Score = round(quality_score, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  # ============================================
  # 7. PER-GATEWAY DATA QUALITY ASSESSMENT (C/A/Co)
  # ============================================
  # Same attribute-level quality checks applied to the subset of events
  # involved in transitions at each gateway (gateway events + successor events).
  # This provides a uniform quality score comparable to per-activity duration quality.
  
  cat("6. Per-gateway data quality assessment (C/A/Co)...\n\n")
  
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
  
  per_gateway_quality <- data.frame(
    Gateway_Activity = character(),
    N_Events = integer(),
    Activity_Completeness = numeric(),
    Activity_Accuracy = numeric(),
    Activity_Consistency = numeric(),
    Activity_Score = numeric(),
    Start_TS_Completeness = numeric(),
    Start_TS_Accuracy = numeric(),
    Start_TS_Consistency = numeric(),
    Start_TS_Score = numeric(),
    Branching_Data_Quality = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Build the transition events: for each gateway, get the gateway event and successor event
  log_with_next <- actlog_df %>%
    arrange(!!sym(case_col), !!sym(start_col)) %>%
    group_by(!!sym(case_col)) %>%
    mutate(
      next_activity = lead(!!sym(activity_col)),
      next_start = lead(!!sym(start_col)),
      next_complete = lead(!!sym(complete_col))
    ) %>%
    ungroup()
  
  w <- dimension_weights / sum(dimension_weights)
  
  for (gw in gateways$source) {
    
    cat(paste("  Gateway:", gw, "\n"))
    
    # Get events at this gateway (the source events of transitions)
    gw_events <- log_with_next %>%
      filter(!!sym(activity_col) == gw & !is.na(next_activity))
    
    n_gw <- nrow(gw_events)
    
    if (n_gw < 2) {
      cat("    Too few events, skipping quality assessment.\n\n")
      next
    }
    
    # --- ACTIVITY quality (assessed on the SUCCESSOR events) ---
    # The branching decision = what activity comes next
    successor_activities <- gw_events$next_activity
    n_successors <- length(successor_activities)
    
    # Completeness: non-NA successor activity labels
    act_na <- sum(is.na(successor_activities))
    act_completeness <- (n_successors - act_na) / n_successors
    
    # Accuracy: successor activities in allowed list
    act_accuracy <- 1.0
    if (!is.null(allowed_activities)) {
      valid_successors <- successor_activities[!is.na(successor_activities)]
      incorrect <- sum(!valid_successors %in% allowed_activities)
      act_accuracy <- max(0, (length(valid_successors) - incorrect) / length(valid_successors))
    }
    
    # Consistency: similar labels among successors (Levenshtein check)
    act_consistency <- 1.0
    valid_successors <- unique(successor_activities[!is.na(successor_activities)])
    if (length(valid_successors) > 1) {
      typo_count <- 0
      for (i in 1:(length(valid_successors) - 1)) {
        for (j in (i + 1):length(valid_successors)) {
          dist <- adist(valid_successors[i], valid_successors[j])
          if (dist > 0 && dist <= max_edit_distance) {
            # One of them might be a typo — count records of the less frequent one
            freq_i <- sum(successor_activities == valid_successors[i], na.rm = TRUE)
            freq_j <- sum(successor_activities == valid_successors[j], na.rm = TRUE)
            typo_count <- typo_count + min(freq_i, freq_j)
          }
        }
      }
      act_consistency <- max(0, (n_successors - typo_count) / n_successors)
    }
    
    act_score <- w["completeness"] * act_completeness + 
                 w["accuracy"] * act_accuracy + 
                 w["consistency"] * act_consistency
    
    # --- START TIMESTAMP quality (assessed on gateway events) ---
    # Ordering depends on start timestamps being correct
    start_vals <- gw_events[[start_col]]
    
    # Completeness: non-NA start timestamps
    start_na <- sum(is.na(start_vals))
    start_completeness <- (n_gw - start_na) / n_gw
    
    # Accuracy: working hours violations + duration anomalies
    start_accuracy <- 1.0
    non_na_starts <- start_vals[!is.na(start_vals)]
    if (length(non_na_starts) > 0) {
      start_hours <- as.numeric(format(non_na_starts, "%H"))
      wh_violations <- sum(start_hours < working_hours_start | start_hours >= working_hours_end, na.rm = TRUE)
      
      # Also check if complete timestamps exist to detect zero/negative durations
      complete_vals <- gw_events[[complete_col]]
      non_na_mask <- !is.na(start_vals) & !is.na(complete_vals)
      time_anomalies <- 0
      if (sum(non_na_mask) > 0) {
        durations <- as.numeric(difftime(complete_vals[non_na_mask], start_vals[non_na_mask], units = "mins"))
        time_anomalies <- sum(durations <= 0, na.rm = TRUE)
      }
      
      total_issues <- wh_violations + time_anomalies
      start_accuracy <- max(0, (length(non_na_starts) - total_issues) / length(non_na_starts))
    }
    
    # Consistency: format consistency
    start_consistency <- 1.0
    if (length(non_na_starts) > 0) {
      start_strs <- as.character(non_na_starts)
      start_formats <- sapply(start_strs, detect_format)
      format_table <- table(start_formats)
      dominant_count <- max(format_table)
      start_consistency <- dominant_count / length(non_na_starts)
    }
    
    start_score <- w["completeness"] * start_completeness + 
                   w["accuracy"] * start_accuracy + 
                   w["consistency"] * start_consistency
    
    # --- BRANCHING DATA QUALITY = aggregate(activity_score, start_ts_score) ---
    branching_quality <- min(act_score, start_score)
    
    cat(paste("    Activity  C:", round(act_completeness * 100, 1),
              "% A:", round(act_accuracy * 100, 1),
              "% Co:", round(act_consistency * 100, 1), "%\n"))
    cat(paste("    Start TS  C:", round(start_completeness * 100, 1),
              "% A:", round(start_accuracy * 100, 1),
              "% Co:", round(start_consistency * 100, 1), "%\n"))
    cat(paste("    Branching Data Quality:", round(branching_quality * 100, 2), "%\n\n"))
    
    per_gateway_quality <- rbind(per_gateway_quality, data.frame(
      Gateway_Activity = gw,
      N_Events = n_gw,
      Activity_Completeness = round(act_completeness, 4),
      Activity_Accuracy = round(act_accuracy, 4),
      Activity_Consistency = round(act_consistency, 4),
      Activity_Score = round(act_score, 4),
      Start_TS_Completeness = round(start_completeness, 4),
      Start_TS_Accuracy = round(start_accuracy, 4),
      Start_TS_Consistency = round(start_consistency, 4),
      Start_TS_Score = round(start_score, 4),
      Branching_Data_Quality = round(branching_quality, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  avg_data_quality <- if (nrow(per_gateway_quality) > 0) mean(per_gateway_quality$Branching_Data_Quality, na.rm = TRUE) else NA
  min_data_quality <- if (nrow(per_gateway_quality) > 0) min(per_gateway_quality$Branching_Data_Quality, na.rm = TRUE) else NA
  
  # ============================================
  # 8. SUMMARY
  # ============================================
  
  cat("============================================\n")
  cat("GATEWAY BRANCHING SUMMARY\n")
  cat("============================================\n\n")
  
  cat(paste("Total gateways found:", nrow(gateway_quality), "\n"))
  cat(paste("Total branches (real):", sum(gateway_quality$Real_Branches), "\n"))
  cat(paste("Total branches (noise):", sum(gateway_quality$Noise_Branches), "\n"))
  cat(paste("Noise threshold:", noise_threshold * 100, "%\n"))
  cat(paste("Min cases threshold:", min_cases, "\n\n"))
  
  cat("Gateway Quality:\n")
  print(gateway_quality[, c("Gateway_Activity", "Real_Branches", "Noise_Branches",
                             "Data_Support", "Noise_Ratio", "Entropy", "Quality_Score")])
  
  if (nrow(noise_transitions) > 0) {
    cat(paste("\nNoise transitions (", nrow(noise_transitions), "):\n"))
    print(noise_transitions[, c("Gateway_Activity", "Branch_Target", 
                                 "Transition_Count", "Branch_Probability")])
  }
  
  avg_quality <- mean(gateway_quality$Quality_Score, na.rm = TRUE)
  min_quality <- min(gateway_quality$Quality_Score, na.rm = TRUE)
  
  cat(paste("\nAverage Gateway Quality (descriptive):", round(avg_quality * 100, 2), "%\n"))
  cat(paste("Minimum Gateway Quality (descriptive):", round(min_quality * 100, 2), "%\n"))
  
  if (nrow(per_gateway_quality) > 0) {
    cat(paste("\nPER-GATEWAY DATA QUALITY (C/A/Co based):\n"))
    print(per_gateway_quality[, c("Gateway_Activity", "Activity_Score", "Start_TS_Score", "Branching_Data_Quality")])
    cat(paste("\nAverage Branching Data Quality:", round(avg_data_quality * 100, 2), "%\n"))
    cat(paste("Minimum Branching Data Quality:", round(min_data_quality * 100, 2), "%\n"))
  }
  
  # ============================================
  # 8. SAVE RESULTS
  # ============================================
  
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  
  write.csv(all_gateway_branches, 
            file.path(results_dir, "gateway_branching_probabilities.csv"),
            row.names = FALSE)
  
  if (nrow(clean_branches) > 0) {
    write.csv(clean_branches,
              file.path(results_dir, "gateway_clean_probabilities.csv"),
              row.names = FALSE)
  }
  
  write.csv(gateway_quality,
            file.path(results_dir, "gateway_quality_scores.csv"),
            row.names = FALSE)
  
  if (nrow(noise_transitions) > 0) {
    write.csv(noise_transitions,
              file.path(results_dir, "gateway_noise_transitions.csv"),
              row.names = FALSE)
  }
  
  if (nrow(per_gateway_quality) > 0) {
    write.csv(per_gateway_quality,
              file.path(results_dir, "gateway_data_quality.csv"),
              row.names = FALSE)
  }
  
  cat("\nSaved to:\n")
  cat("  - results/gateway_branching_probabilities.csv\n")
  cat("  - results/gateway_clean_probabilities.csv\n")
  cat("  - results/gateway_quality_scores.csv (descriptive)\n")
  cat("  - results/gateway_data_quality.csv (C/A/Co based)\n")
  if (nrow(noise_transitions) > 0) {
    cat("  - results/gateway_noise_transitions.csv\n")
  }
  
  return(list(
    gateways = all_gateway_branches,
    clean_branches = clean_branches,
    gateway_quality = gateway_quality,
    per_gateway_data_quality = per_gateway_quality,
    noise_transitions = noise_transitions,
    n_gateways = nrow(gateway_quality),
    avg_quality = avg_quality,
    min_quality = min_quality,
    avg_data_quality = avg_data_quality,
    min_data_quality = min_data_quality
  ))
}
