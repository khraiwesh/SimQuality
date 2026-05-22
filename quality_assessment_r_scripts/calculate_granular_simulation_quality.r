# Granular Simulation Parameter Quality
# Calculates quality scores at activity, resource, and resource-activity level
#
# This extends the overall simulation parameter quality to provide:
# 1. Per-activity duration quality
# 2. Per-resource quality
# 3. Per resource-activity relationship quality
# 4. Structural consistency (resource-activity relationship patterns)
# 5. Behavioral consistency (duration consistency + sequence correctness)

library(dplyr)
library(tidyr)


# ============================================
# STRUCTURAL CONSISTENCY
# ============================================
# Checks if resource-activity relationships follow expected patterns
# - Does each activity have consistent resource assignments?
# - Are there unexpected/rare resource-activity pairings?

#' Calculate Structural Consistency
#' 
#' Measures consistency of resource-activity relationships.
#' For each activity: are resources consistently assigned?
#' For each resource: do they consistently perform certain activities?
#'
#' @param log Event log dataframe
#' @param activity_col Name of activity column
#' @param resource_col Name of resource column
#' @param min_frequency Minimum frequency to consider a pair as "expected" (default 0.05 = 5%)
#' @return List with structural consistency scores

calculate_structural_consistency <- function(
    log,
    activity_col = "Activity",
    resource_col = "Resource",
    min_frequency = 0.05
) {
  
  cat("============================================\n")
  cat("STRUCTURAL CONSISTENCY ANALYSIS\n")
  cat("============================================\n\n")
  
  # Filter to events with both resource and activity
  valid_log <- log[!is.na(log[[resource_col]]) & !is.na(log[[activity_col]]), ]
  
  if (nrow(valid_log) == 0) {
    cat("No valid events with both resource and activity.\n")
    return(list(
      per_activity = data.frame(),
      per_resource = data.frame(),
      overall_structural_consistency = NA
    ))
  }
  
  # ============================================
  # 1. Activity-centric: For each activity, how consistent are resource assignments?
  # ============================================
  
  cat("1. ACTIVITY-CENTRIC CONSISTENCY\n")
  cat("   (For each activity: how concentrated are resource assignments?)\n\n")
  
  activity_results <- data.frame(
    Activity = character(),
    Total_Events = integer(),
    Unique_Resources = integer(),
    Dominant_Resource = character(),
    Dominant_Percentage = numeric(),
    Consistency_Score = numeric(),
    stringsAsFactors = FALSE
  )
  
  activities <- unique(valid_log[[activity_col]])
  
  for (act in activities) {
    act_events <- valid_log[valid_log[[activity_col]] == act, ]
    total <- nrow(act_events)
    
    # Count resources for this activity
    resource_counts <- table(act_events[[resource_col]])
    unique_resources <- length(resource_counts)
    
    # Find dominant resource
    dominant_res <- names(which.max(resource_counts))
    dominant_count <- max(resource_counts)
    dominant_pct <- dominant_count / total
    
    # Consistency score: 
    # High if few resources handle this activity (concentrated)
    # Uses entropy-based measure: 1 - normalized entropy
    if (unique_resources == 1) {
      consistency <- 1.0  # Perfect: only one resource
    } else {
      # Calculate normalized entropy
      proportions <- as.numeric(resource_counts) / total
      entropy <- -sum(proportions * log2(proportions))
      max_entropy <- log2(unique_resources)
      normalized_entropy <- entropy / max_entropy
      consistency <- 1 - normalized_entropy
    }
    
    activity_results <- rbind(activity_results, data.frame(
      Activity = act,
      Total_Events = total,
      Unique_Resources = unique_resources,
      Dominant_Resource = dominant_res,
      Dominant_Percentage = round(dominant_pct, 4),
      Consistency_Score = round(consistency, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  activity_results <- activity_results[order(-activity_results$Consistency_Score), ]
  
  cat("Activities with LOWEST consistency (most scattered resource assignments):\n")
  print(head(activity_results[order(activity_results$Consistency_Score), ], 5))
  cat("\n")
  
  # ============================================
  # 2. Resource-centric: For each resource, how consistent are their activities?
  # ============================================
  
  cat("2. RESOURCE-CENTRIC CONSISTENCY\n")
  cat("   (For each resource: how focused are they on specific activities?)\n\n")
  
  resource_results <- data.frame(
    Resource = character(),
    Total_Events = integer(),
    Unique_Activities = integer(),
    Primary_Activity = character(),
    Primary_Percentage = numeric(),
    Consistency_Score = numeric(),
    stringsAsFactors = FALSE
  )
  
  resources <- unique(valid_log[[resource_col]])
  
  for (res in resources) {
    res_events <- valid_log[valid_log[[resource_col]] == res, ]
    total <- nrow(res_events)
    
    # Count activities for this resource
    activity_counts <- table(res_events[[activity_col]])
    unique_activities <- length(activity_counts)
    
    # Find primary activity
    primary_act <- names(which.max(activity_counts))
    primary_count <- max(activity_counts)
    primary_pct <- primary_count / total
    
    # Consistency score using entropy
    if (unique_activities == 1) {
      consistency <- 1.0
    } else {
      proportions <- as.numeric(activity_counts) / total
      entropy <- -sum(proportions * log2(proportions))
      max_entropy <- log2(unique_activities)
      normalized_entropy <- entropy / max_entropy
      consistency <- 1 - normalized_entropy
    }
    
    resource_results <- rbind(resource_results, data.frame(
      Resource = res,
      Total_Events = total,
      Unique_Activities = unique_activities,
      Primary_Activity = primary_act,
      Primary_Percentage = round(primary_pct, 4),
      Consistency_Score = round(consistency, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  resource_results <- resource_results[order(-resource_results$Consistency_Score), ]
  
  cat("Resources with LOWEST consistency (perform many different activities):\n")
  print(head(resource_results[order(resource_results$Consistency_Score), ], 5))
  cat("\n")
  
  # ============================================
  # 3. Identify rare/unexpected resource-activity pairs
  # ============================================
  
  cat("3. RARE RESOURCE-ACTIVITY PAIRS\n")
  cat(paste("   (Pairs occurring < ", min_frequency * 100, "% of activity's events)\n\n"))
  
  rare_pairs <- data.frame(
    Activity = character(),
    Resource = character(),
    Pair_Count = integer(),
    Activity_Total = integer(),
    Pair_Percentage = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (act in activities) {
    act_events <- valid_log[valid_log[[activity_col]] == act, ]
    total <- nrow(act_events)
    resource_counts <- table(act_events[[resource_col]])
    
    for (res in names(resource_counts)) {
      count <- resource_counts[res]
      pct <- count / total
      
      if (pct < min_frequency) {
        rare_pairs <- rbind(rare_pairs, data.frame(
          Activity = act,
          Resource = res,
          Pair_Count = as.integer(count),
          Activity_Total = total,
          Pair_Percentage = round(pct, 4),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(rare_pairs) > 0) {
    rare_pairs <- rare_pairs[order(rare_pairs$Pair_Percentage), ]
    cat(paste("Found", nrow(rare_pairs), "rare pairs:\n"))
    print(head(rare_pairs, 10))
  } else {
    cat("No rare pairs found.\n")
  }
  cat("\n")
  
  # ============================================
  # Overall Structural Consistency
  # ============================================
  
  avg_activity_consistency <- mean(activity_results$Consistency_Score, na.rm = TRUE)
  avg_resource_consistency <- mean(resource_results$Consistency_Score, na.rm = TRUE)
  overall_structural <- mean(c(avg_activity_consistency, avg_resource_consistency))
  
  cat("============================================\n")
  cat("STRUCTURAL CONSISTENCY SUMMARY:\n")
  cat(paste("  Activity-centric avg:", round(avg_activity_consistency * 100, 2), "%\n"))
  cat(paste("  Resource-centric avg:", round(avg_resource_consistency * 100, 2), "%\n"))
  cat(paste("  OVERALL STRUCTURAL:", round(overall_structural * 100, 2), "%\n"))
  cat("============================================\n\n")
  
  return(list(
    per_activity = activity_results,
    per_resource = resource_results,
    rare_pairs = rare_pairs,
    activity_consistency_avg = avg_activity_consistency,
    resource_consistency_avg = avg_resource_consistency,
    overall_structural_consistency = overall_structural
  ))
}


# ============================================
# BEHAVIORAL CONSISTENCY
# ============================================
# Checks if resource behavior is consistent:
# - Duration consistency: Does this resource take consistent time for activities?
# - Sequence consistency: Does this resource follow expected activity order?

#' Calculate Behavioral Consistency
#' 
#' Measures consistency of resource behavior:
#' 1. Duration consistency: CV of durations per resource-activity pair
#' 2. Sequence consistency: Does the resource follow expected activity order?
#'
#' @param log Event log dataframe
#' @param case_col Name of case ID column
#' @param activity_col Name of activity column
#' @param resource_col Name of resource column
#' @param start_col Name of start timestamp column
#' @param complete_col Name of complete timestamp column
#' @param expected_order Optional vector of expected activity order
#' @return List with behavioral consistency scores

calculate_behavioral_consistency <- function(
    log,
    case_col = "Case.ID",
    activity_col = "Activity",
    resource_col = "Resource",
    start_col = "Start",
    complete_col = "Complete",
    expected_order = NULL
) {
  
  cat("============================================\n")
  cat("BEHAVIORAL CONSISTENCY ANALYSIS\n")
  cat("============================================\n\n")
  
  # Filter to valid events
  valid_log <- log[!is.na(log[[resource_col]]) & 
                    !is.na(log[[activity_col]]) &
                    !is.na(log[[start_col]]) &
                    !is.na(log[[complete_col]]), ]
  
  if (nrow(valid_log) == 0) {
    cat("No valid events with all required fields.\n")
    return(list(
      duration_consistency = data.frame(),
      sequence_consistency = data.frame(),
      overall_behavioral_consistency = NA
    ))
  }
  
  # ============================================
  # 1. DURATION CONSISTENCY
  # ============================================
  # For each resource-activity pair: how consistent are the durations?
  # Uses Coefficient of Variation (CV): lower CV = more consistent
  
  cat("1. DURATION CONSISTENCY\n")
  cat("   (For each resource-activity pair: how consistent are execution times?)\n\n")
  
  # Calculate duration for each event
  valid_log$Duration_Sec <- as.numeric(difftime(
    valid_log[[complete_col]], 
    valid_log[[start_col]], 
    units = "secs"
  ))
  
  # Filter to positive durations
  valid_log <- valid_log[valid_log$Duration_Sec >= 0, ]
  
  duration_results <- data.frame(
    Resource = character(),
    Activity = character(),
    Event_Count = integer(),
    Avg_Duration_Sec = numeric(),
    StdDev_Duration = numeric(),
    CV = numeric(),
    Duration_Consistency = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Group by resource-activity
  pairs <- valid_log %>%
    group_by(across(all_of(c(resource_col, activity_col)))) %>%
    summarise(
      Event_Count = n(),
      Avg_Duration = mean(Duration_Sec, na.rm = TRUE),
      StdDev = sd(Duration_Sec, na.rm = TRUE),
      .groups = "drop"
    )
  
  for (i in 1:nrow(pairs)) {
    res <- pairs[[resource_col]][i]
    act <- pairs[[activity_col]][i]
    n <- pairs$Event_Count[i]
    avg_dur <- pairs$Avg_Duration[i]
    sd_dur <- pairs$StdDev[i]
    
    # CV = StdDev / Mean (coefficient of variation)
    if (is.na(sd_dur) || n == 1) {
      cv <- 0  # Only one event or no variation
      consistency <- 1.0
    } else if (avg_dur == 0) {
      cv <- 0
      consistency <- 1.0
    } else {
      cv <- sd_dur / avg_dur
      # Convert CV to consistency: lower CV = higher consistency
      # CV of 0 = perfect (1.0), CV of 1 = moderate (0.5), CV > 2 = poor
      consistency <- max(0, 1 - (cv / 2))
    }
    
    duration_results <- rbind(duration_results, data.frame(
      Resource = res,
      Activity = act,
      Event_Count = n,
      Avg_Duration_Sec = round(avg_dur, 2),
      StdDev_Duration = round(ifelse(is.na(sd_dur), 0, sd_dur), 2),
      CV = round(cv, 4),
      Duration_Consistency = round(consistency, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  duration_results <- duration_results[order(duration_results$Duration_Consistency), ]
  
  cat("Resource-Activity pairs with LOWEST duration consistency (high variability):\n")
  print(head(duration_results, 10))
  cat("\n")
  
  avg_duration_consistency <- mean(duration_results$Duration_Consistency, na.rm = TRUE)
  cat(paste("Average Duration Consistency:", round(avg_duration_consistency * 100, 2), "%\n\n"))
  
  # ============================================
  # 2. SEQUENCE CONSISTENCY
  # ============================================
  # For each resource: do they follow expected activity order within cases?
  
  cat("2. SEQUENCE CONSISTENCY\n")
  cat("   (For each resource: do they follow expected activity order?)\n\n")
  
  sequence_results <- data.frame(
    Resource = character(),
    Cases_Handled = integer(),
    Total_Transitions = integer(),
    Valid_Transitions = integer(),
    Sequence_Consistency = numeric(),
    stringsAsFactors = FALSE
  )
  
  # If expected_order provided, use it; otherwise infer from data
  if (is.null(expected_order)) {
    # Infer order from most common first activities
    cat("   (No expected order provided - inferring from data)\n")
    
    # Get order by average position in cases
    case_positions <- valid_log %>%
      group_by(across(all_of(case_col))) %>%
      arrange(across(all_of(start_col))) %>%
      mutate(Position = row_number()) %>%
      ungroup()
    
    avg_positions <- case_positions %>%
      group_by(across(all_of(activity_col))) %>%
      summarise(Avg_Position = mean(Position), .groups = "drop") %>%
      arrange(Avg_Position)
    
    expected_order <- avg_positions[[activity_col]]
    cat(paste("   Inferred order:", paste(head(expected_order, 5), collapse = " -> "), "...\n\n"))
  }
  
  # Create position lookup
  position_lookup <- setNames(1:length(expected_order), expected_order)
  
  resources <- unique(valid_log[[resource_col]])
  
  for (res in resources) {
    res_events <- valid_log[valid_log[[resource_col]] == res, ]
    
    if (nrow(res_events) < 2) {
      sequence_results <- rbind(sequence_results, data.frame(
        Resource = res,
        Cases_Handled = length(unique(res_events[[case_col]])),
        Total_Transitions = 0,
        Valid_Transitions = 0,
        Sequence_Consistency = 1.0,
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # For each case this resource handled
    cases <- unique(res_events[[case_col]])
    total_transitions <- 0
    valid_transitions <- 0
    
    for (case_id in cases) {
      case_res_events <- res_events[res_events[[case_col]] == case_id, ]
      case_res_events <- case_res_events[order(case_res_events[[start_col]]), ]
      
      if (nrow(case_res_events) < 2) next
      
      # Check transitions
      activities <- case_res_events[[activity_col]]
      for (j in 1:(length(activities) - 1)) {
        from_act <- activities[j]
        to_act <- activities[j + 1]
        
        total_transitions <- total_transitions + 1
        
        # Check if transition follows expected order
        from_pos <- position_lookup[from_act]
        to_pos <- position_lookup[to_act]
        
        if (!is.na(from_pos) && !is.na(to_pos)) {
          if (to_pos >= from_pos) {
            valid_transitions <- valid_transitions + 1
          }
        } else {
          # Unknown activity, assume valid
          valid_transitions <- valid_transitions + 1
        }
      }
    }
    
    consistency <- ifelse(total_transitions > 0, 
                          valid_transitions / total_transitions, 
                          1.0)
    
    sequence_results <- rbind(sequence_results, data.frame(
      Resource = res,
      Cases_Handled = length(cases),
      Total_Transitions = total_transitions,
      Valid_Transitions = valid_transitions,
      Sequence_Consistency = round(consistency, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  sequence_results <- sequence_results[order(sequence_results$Sequence_Consistency), ]
  
  cat("Resources with LOWEST sequence consistency (most out-of-order transitions):\n")
  print(head(sequence_results, 10))
  cat("\n")
  
  avg_sequence_consistency <- mean(sequence_results$Sequence_Consistency, na.rm = TRUE)
  cat(paste("Average Sequence Consistency:", round(avg_sequence_consistency * 100, 2), "%\n\n"))
  
  # ============================================
  # Overall Behavioral Consistency
  # ============================================
  
  overall_behavioral <- mean(c(avg_duration_consistency, avg_sequence_consistency), na.rm = TRUE)
  
  cat("============================================\n")
  cat("BEHAVIORAL CONSISTENCY SUMMARY:\n")
  cat(paste("  Duration Consistency:", round(avg_duration_consistency * 100, 2), "%\n"))
  cat(paste("  Sequence Consistency:", round(avg_sequence_consistency * 100, 2), "%\n"))
  cat(paste("  OVERALL BEHAVIORAL:", round(overall_behavioral * 100, 2), "%\n"))
  cat("============================================\n\n")
  
  return(list(
    duration_consistency = duration_results,
    sequence_consistency = sequence_results,
    avg_duration_consistency = avg_duration_consistency,
    avg_sequence_consistency = avg_sequence_consistency,
    overall_behavioral_consistency = overall_behavioral,
    inferred_order = expected_order
  ))
}


# ============================================
# RESOURCE-ACTIVITY ACCURACY
# ============================================
# Checks if resource-activity assignments are accurate based on:
# 1. Probability/frequency matching (does observed R-A probability match expected?)
# 2. Workload accuracy (is the workload distribution reasonable?)

#' Calculate Resource-Activity Probability Accuracy
#' 
#' Measures accuracy of resource-activity assignments by comparing
#' observed probabilities with expected patterns.
#' 
#' Accuracy checks:
#' - Does each resource perform activities with expected probabilities?
#' - Are there unexpected assignments (rare pairings)?
#' - Do activity assignments match resource specializations?
#'
#' @param log Event log dataframe
#' @param activity_col Name of activity column
#' @param resource_col Name of resource column
#' @param expected_probabilities Optional matrix/dataframe of expected R-A probabilities
#' @param tolerance Tolerance for probability deviation (default 0.15 = 15%)
#' @return List with probability accuracy scores

calculate_resource_activity_probability_accuracy <- function(
    log,
    activity_col = "Activity",
    resource_col = "Resource",
    expected_probabilities = NULL,
    tolerance = 0.15
) {
  
  cat("============================================\n")
  cat("RESOURCE-ACTIVITY PROBABILITY ACCURACY\n")
  cat("============================================\n\n")
  
  # Filter to events with both resource and activity
  valid_log <- log[!is.na(log[[resource_col]]) & !is.na(log[[activity_col]]), ]
  
  if (nrow(valid_log) == 0) {
    cat("No valid events with both resource and activity.\n")
    return(list(
      resource_accuracy = data.frame(),
      activity_accuracy = data.frame(),
      overall_accuracy = NA
    ))
  }
  
  # ============================================
  # Calculate Observed Probability Distributions
  # ============================================
  
  total_events <- nrow(valid_log)
  resources <- unique(valid_log[[resource_col]])
  activities <- unique(valid_log[[activity_col]])
  
  cat(paste("Total valid events:", total_events, "\n"))
  cat(paste("Unique resources:", length(resources), "\n"))
  cat(paste("Unique activities:", length(activities), "\n\n"))
  
  # 1. For each RESOURCE: What % of their work goes to each activity?
  # P(Activity | Resource) - "Given this resource, what's the probability they do activity A?"
  
  resource_activity_probs <- valid_log %>%
    group_by(across(all_of(c(resource_col, activity_col)))) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(across(all_of(resource_col))) %>%
    mutate(
      Resource_Total = sum(Count),
      Observed_Probability = Count / Resource_Total
    ) %>%
    ungroup()
  
  # 2. For each ACTIVITY: What % is done by each resource?
  # P(Resource | Activity) - "Given this activity, what's the probability resource R does it?"
  
  activity_resource_probs <- valid_log %>%
    group_by(across(all_of(c(activity_col, resource_col)))) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(across(all_of(activity_col))) %>%
    mutate(
      Activity_Total = sum(Count),
      Observed_Probability = Count / Activity_Total
    ) %>%
    ungroup()
  
  # ============================================
  # Accuracy Assessment Method 1: Specialization Accuracy
  # ============================================
  # A resource is "accurate" if they specialize (concentrate on few activities)
  # rather than doing everything uniformly
  # 
  # We use concentration/entropy as a proxy for accuracy:
  # - High concentration = specialist = likely accurate assignments
  # - Uniform distribution = generalist = potentially inaccurate assignments
  
  cat("--- RESOURCE SPECIALIZATION ACCURACY ---\n")
  cat("(Do resources specialize in certain activities?)\n\n")
  
  resource_accuracy <- valid_log %>%
    group_by(across(all_of(resource_col))) %>%
    summarise(
      Total_Events = n(),
      Unique_Activities = n_distinct(.data[[activity_col]]),
      .groups = "drop"
    )
  
  # Calculate entropy and concentration for each resource
  resource_accuracy_detail <- data.frame(
    Resource = character(),
    Total_Events = integer(),
    Unique_Activities = integer(),
    Max_Probability = numeric(),  # Probability of most frequent activity
    Concentration_Index = numeric(),  # Herfindahl-Hirschman Index
    Specialization_Accuracy = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (res in resources) {
    res_probs <- resource_activity_probs[resource_activity_probs[[resource_col]] == res, ]
    
    # Max probability (how much of their work is their most frequent activity)
    max_prob <- max(res_probs$Observed_Probability, na.rm = TRUE)
    
    # Herfindahl-Hirschman Index (sum of squared probabilities)
    # = 1 for perfect specialization, = 1/n for uniform distribution
    hhi <- sum(res_probs$Observed_Probability^2)
    
    n_activities <- nrow(res_probs)
    
    # Normalize HHI: (HHI - 1/n) / (1 - 1/n)
    # This gives 0 for uniform, 1 for perfect specialization
    if (n_activities > 1) {
      normalized_hhi <- (hhi - 1/n_activities) / (1 - 1/n_activities)
    } else {
      normalized_hhi <- 1  # Only does one activity = perfect specialist
    }
    
    # Specialization accuracy: higher is more accurate (more focused)
    spec_accuracy <- (normalized_hhi + max_prob) / 2  # Average of both measures
    
    resource_accuracy_detail <- rbind(resource_accuracy_detail, data.frame(
      Resource = res,
      Total_Events = sum(res_probs$Count),
      Unique_Activities = n_activities,
      Max_Probability = round(max_prob, 4),
      Concentration_Index = round(hhi, 4),
      Specialization_Accuracy = round(spec_accuracy, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  resource_accuracy_detail <- resource_accuracy_detail[
    order(-resource_accuracy_detail$Specialization_Accuracy), ]
  
  cat("Top Specialists (HIGH accuracy - focused resources):\n")
  print(head(resource_accuracy_detail, 10))
  cat("\n")
  
  cat("Generalists (LOWER accuracy - scattered resources):\n")
  print(tail(resource_accuracy_detail, 10))
  cat("\n")
  
  avg_resource_accuracy <- mean(resource_accuracy_detail$Specialization_Accuracy, na.rm = TRUE)
  cat(paste("Average Resource Specialization Accuracy:", round(avg_resource_accuracy * 100, 2), "%\n\n"))
  
  # ============================================
  # Accuracy Assessment Method 2: Activity Assignment Accuracy
  # ============================================
  # For each activity, are the right resources doing it?
  # Check if assignments are concentrated (few resources) vs scattered (everyone does everything)
  
  cat("--- ACTIVITY ASSIGNMENT ACCURACY ---\n")
  cat("(Are activities assigned to appropriate resources?)\n\n")
  
  activity_accuracy_detail <- data.frame(
    Activity = character(),
    Total_Events = integer(),
    Unique_Resources = integer(),
    Max_Probability = numeric(),
    Concentration_Index = numeric(),
    Assignment_Accuracy = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (act in activities) {
    act_probs <- activity_resource_probs[activity_resource_probs[[activity_col]] == act, ]
    
    max_prob <- max(act_probs$Observed_Probability, na.rm = TRUE)
    hhi <- sum(act_probs$Observed_Probability^2)
    n_resources <- nrow(act_probs)
    
    if (n_resources > 1) {
      normalized_hhi <- (hhi - 1/n_resources) / (1 - 1/n_resources)
    } else {
      normalized_hhi <- 1
    }
    
    assign_accuracy <- (normalized_hhi + max_prob) / 2
    
    activity_accuracy_detail <- rbind(activity_accuracy_detail, data.frame(
      Activity = act,
      Total_Events = sum(act_probs$Count),
      Unique_Resources = n_resources,
      Max_Probability = round(max_prob, 4),
      Concentration_Index = round(hhi, 4),
      Assignment_Accuracy = round(assign_accuracy, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  activity_accuracy_detail <- activity_accuracy_detail[
    order(-activity_accuracy_detail$Assignment_Accuracy), ]
  
  cat("Activities with HIGH assignment accuracy (clear ownership):\n")
  print(head(activity_accuracy_detail, 10))
  cat("\n")
  
  cat("Activities with LOWER assignment accuracy (many resources involved):\n")
  print(tail(activity_accuracy_detail, 10))
  cat("\n")
  
  avg_activity_accuracy <- mean(activity_accuracy_detail$Assignment_Accuracy, na.rm = TRUE)
  cat(paste("Average Activity Assignment Accuracy:", round(avg_activity_accuracy * 100, 2), "%\n\n"))
  
  # ============================================
  # Method 3: Expected vs Observed Probability (if provided)
  # ============================================
  
  expected_comparison <- NULL
  expected_accuracy <- NA
  
  if (!is.null(expected_probabilities)) {
    cat("--- EXPECTED vs OBSERVED PROBABILITY COMPARISON ---\n\n")
    
    # expected_probabilities should be a dataframe with columns:
    # Resource, Activity, Expected_Probability
    
    # Merge with observed
    comparison <- merge(
      resource_activity_probs,
      expected_probabilities,
      by = c(resource_col, activity_col),
      all = TRUE
    )
    
    comparison$Observed_Probability[is.na(comparison$Observed_Probability)] <- 0
    comparison$Expected_Probability[is.na(comparison$Expected_Probability)] <- 0
    
    # Calculate deviation
    comparison$Deviation <- abs(comparison$Observed_Probability - comparison$Expected_Probability)
    comparison$Within_Tolerance <- comparison$Deviation <= tolerance
    comparison$Accuracy_Score <- 1 - pmin(comparison$Deviation / tolerance, 1)
    
    expected_comparison <- comparison
    expected_accuracy <- mean(comparison$Accuracy_Score, na.rm = TRUE)
    
    cat(paste("Pairs within tolerance (", tolerance * 100, "%):", 
              sum(comparison$Within_Tolerance), "/", nrow(comparison), "\n"))
    cat(paste("Average probability accuracy:", round(expected_accuracy * 100, 2), "%\n\n"))
    
    # Show largest deviations
    cat("Largest probability deviations:\n")
    comparison_sorted <- comparison[order(-comparison$Deviation), ]
    print(head(comparison_sorted[, c(resource_col, activity_col, 
                                     "Expected_Probability", "Observed_Probability", 
                                     "Deviation", "Accuracy_Score")], 10))
    cat("\n")
  }
  
  # ============================================
  # Overall Probability Accuracy
  # ============================================
  
  overall_accuracy <- mean(c(avg_resource_accuracy, avg_activity_accuracy), na.rm = TRUE)
  
  if (!is.na(expected_accuracy)) {
    overall_accuracy <- mean(c(avg_resource_accuracy, avg_activity_accuracy, expected_accuracy), na.rm = TRUE)
  }
  
  cat("============================================\n")
  cat("PROBABILITY ACCURACY SUMMARY:\n")
  cat(paste("  Resource Specialization:", round(avg_resource_accuracy * 100, 2), "%\n"))
  cat(paste("  Activity Assignment:", round(avg_activity_accuracy * 100, 2), "%\n"))
  if (!is.na(expected_accuracy)) {
    cat(paste("  Expected vs Observed:", round(expected_accuracy * 100, 2), "%\n"))
  }
  cat(paste("  OVERALL PROBABILITY ACCURACY:", round(overall_accuracy * 100, 2), "%\n"))
  cat("============================================\n\n")
  
  return(list(
    resource_accuracy = resource_accuracy_detail,
    activity_accuracy = activity_accuracy_detail,
    observed_probabilities = resource_activity_probs,
    expected_comparison = expected_comparison,
    avg_resource_accuracy = avg_resource_accuracy,
    avg_activity_accuracy = avg_activity_accuracy,
    expected_accuracy = expected_accuracy,
    overall_probability_accuracy = overall_accuracy
  ))
}


#' Calculate Workload Accuracy
#' 
#' Measures if workload distribution across resources is reasonable/accurate.
#' 
#' Checks:
#' - Is workload balanced (not extremely skewed)?
#' - Is workload within capacity bounds (not impossible)?
#' - Is duration worked reasonable (not 24/7)?
#'
#' @param log Event log dataframe
#' @param resource_col Name of resource column
#' @param start_col Name of start timestamp column
#' @param complete_col Name of complete timestamp column
#' @param max_daily_hours Maximum reasonable daily working hours (default 12)
#' @param expected_workload Optional vector of expected events per resource
#' @return List with workload accuracy scores

calculate_workload_accuracy <- function(
    log,
    resource_col = "Resource",
    start_col = "Start",
    complete_col = "Complete",
    max_daily_hours = 12,
    expected_workload = NULL
) {
  
  cat("============================================\n")
  cat("WORKLOAD ACCURACY ANALYSIS\n")
  cat("============================================\n\n")
  
  # Filter to events with resource
  valid_log <- log[!is.na(log[[resource_col]]), ]
  
  if (nrow(valid_log) == 0) {
    cat("No valid events with resources.\n")
    return(list(
      workload_summary = data.frame(),
      overall_workload_accuracy = NA
    ))
  }
  
  # ============================================
  # Calculate Workload Metrics Per Resource
  # ============================================
  
  resources <- unique(valid_log[[resource_col]])
  total_events <- nrow(valid_log)
  
  cat(paste("Total events:", total_events, "\n"))
  cat(paste("Unique resources:", length(resources), "\n\n"))
  
  workload_results <- data.frame(
    Resource = character(),
    Event_Count = integer(),
    Workload_Share = numeric(),
    Total_Duration_Hours = numeric(),
    Avg_Duration_Minutes = numeric(),
    Days_Active = integer(),
    Avg_Events_Per_Day = numeric(),
    Avg_Hours_Per_Day = numeric(),
    Workload_Balance_Score = numeric(),
    Capacity_Score = numeric(),
    Workload_Accuracy = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate time span of log
  all_times <- c()
  if (!is.null(valid_log[[start_col]])) {
    all_times <- c(all_times, as.numeric(valid_log[[start_col]][!is.na(valid_log[[start_col]])]))
  }
  if (!is.null(valid_log[[complete_col]])) {
    all_times <- c(all_times, as.numeric(valid_log[[complete_col]][!is.na(valid_log[[complete_col]])]))
  }
  
  if (length(all_times) > 0) {
    log_duration_days <- max(
      1, 
      as.numeric(difftime(
        max(as.POSIXct(all_times, origin = "1970-01-01"), na.rm = TRUE),
        min(as.POSIXct(all_times, origin = "1970-01-01"), na.rm = TRUE),
        units = "days"
      ))
    )
  } else {
    log_duration_days <- 1
  }
  
  cat(paste("Log spans approximately", round(log_duration_days, 1), "days\n\n"))
  
  # Expected fair share (if workload were perfectly balanced)
  expected_share <- 1 / length(resources)
  
  for (res in resources) {
    res_events <- valid_log[valid_log[[resource_col]] == res, ]
    n_events <- nrow(res_events)
    
    # Workload share
    share <- n_events / total_events
    
    # Duration calculations
    has_duration <- !is.na(res_events[[start_col]]) & !is.na(res_events[[complete_col]])
    
    if (sum(has_duration) > 0) {
      durations <- as.numeric(difftime(
        res_events[[complete_col]][has_duration],
        res_events[[start_col]][has_duration],
        units = "mins"
      ))
      durations <- durations[durations >= 0]  # Remove negative durations
      
      total_duration_hours <- sum(durations, na.rm = TRUE) / 60
      avg_duration_mins <- mean(durations, na.rm = TRUE)
    } else {
      total_duration_hours <- NA
      avg_duration_mins <- NA
    }
    
    # Days active (unique days with events)
    if (!is.null(res_events[[start_col]]) && sum(!is.na(res_events[[start_col]])) > 0) {
      unique_days <- length(unique(as.Date(res_events[[start_col]][!is.na(res_events[[start_col]])])))
    } else {
      unique_days <- 1
    }
    
    # Average events and hours per day
    avg_events_day <- n_events / max(1, unique_days)
    avg_hours_day <- ifelse(!is.na(total_duration_hours), 
                            total_duration_hours / max(1, unique_days), 
                            NA)
    
    # ============================================
    # Accuracy Scoring
    # ============================================
    
    # 1. WORKLOAD BALANCE SCORE
    # How close is this resource's share to fair share?
    # Score = 1 - normalized_deviation
    deviation_from_fair <- abs(share - expected_share)
    max_possible_deviation <- max(expected_share, 1 - expected_share)
    balance_score <- 1 - (deviation_from_fair / max_possible_deviation)
    
    # 2. CAPACITY SCORE
    # Is the daily workload reasonable?
    if (!is.na(avg_hours_day)) {
      # Score based on how close to max capacity
      if (avg_hours_day <= max_daily_hours) {
        capacity_score <- 1.0  # Within capacity
      } else {
        # Penalize for overcapacity
        over_ratio <- avg_hours_day / max_daily_hours
        capacity_score <- max(0, 1 - (over_ratio - 1))  # Linear penalty
      }
    } else {
      capacity_score <- 1.0  # No duration data, assume OK
    }
    
    # Combined workload accuracy
    workload_accuracy <- mean(c(balance_score, capacity_score), na.rm = TRUE)
    
    workload_results <- rbind(workload_results, data.frame(
      Resource = res,
      Event_Count = n_events,
      Workload_Share = round(share, 4),
      Total_Duration_Hours = round(total_duration_hours, 2),
      Avg_Duration_Minutes = round(avg_duration_mins, 2),
      Days_Active = unique_days,
      Avg_Events_Per_Day = round(avg_events_day, 2),
      Avg_Hours_Per_Day = round(avg_hours_day, 2),
      Workload_Balance_Score = round(balance_score, 4),
      Capacity_Score = round(capacity_score, 4),
      Workload_Accuracy = round(workload_accuracy, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  workload_results <- workload_results[order(-workload_results$Workload_Accuracy), ]
  
  cat("--- WORKLOAD DISTRIBUTION ---\n\n")
  cat("Resources with HIGHEST workload accuracy:\n")
  print(head(workload_results[, c("Resource", "Event_Count", "Workload_Share", 
                                   "Avg_Hours_Per_Day", "Workload_Accuracy")], 10))
  cat("\n")
  
  cat("Resources with LOWEST workload accuracy (potential issues):\n")
  print(tail(workload_results[, c("Resource", "Event_Count", "Workload_Share", 
                                   "Avg_Hours_Per_Day", "Workload_Accuracy")], 10))
  cat("\n")
  
  # ============================================
  # Overall Workload Statistics
  # ============================================
  
  # Calculate Gini coefficient for workload distribution
  shares <- sort(workload_results$Workload_Share)
  n <- length(shares)
  gini_sum <- sum((2 * (1:n) - n - 1) * shares)
  gini <- gini_sum / (n * sum(shares))
  gini <- max(0, min(1, gini))  # Bound between 0 and 1
  
  # Workload fairness = 1 - Gini (higher is more balanced)
  workload_fairness <- 1 - gini
  
  cat("--- WORKLOAD BALANCE STATISTICS ---\n")
  cat(paste("  Gini Coefficient:", round(gini, 4), "(0=perfect equality, 1=perfect inequality)\n"))
  cat(paste("  Workload Fairness:", round(workload_fairness * 100, 2), "%\n"))
  
  # Check for extremely skewed workload
  max_share <- max(workload_results$Workload_Share)
  min_share <- min(workload_results$Workload_Share)
  skew_ratio <- max_share / max(min_share, 0.001)
  
  cat(paste("  Max workload share:", round(max_share * 100, 2), "%\n"))
  cat(paste("  Min workload share:", round(min_share * 100, 2), "%\n"))
  cat(paste("  Skew ratio (max/min):", round(skew_ratio, 2), "\n\n"))
  
  # Resources with capacity issues
  over_capacity <- sum(workload_results$Avg_Hours_Per_Day > max_daily_hours, na.rm = TRUE)
  cat(paste("Resources exceeding", max_daily_hours, "hours/day:", over_capacity, "/", n, "\n\n"))
  
  # ============================================
  # Optional: Compare to Expected Workload
  # ============================================
  
  expected_accuracy <- NA
  
  if (!is.null(expected_workload)) {
    cat("--- EXPECTED vs OBSERVED WORKLOAD ---\n\n")
    
    if (is.vector(expected_workload) && length(expected_workload) == length(resources)) {
      workload_results$Expected_Events <- expected_workload
      workload_results$Workload_Deviation <- abs(workload_results$Event_Count - expected_workload)
      workload_results$Expected_Match <- 1 - pmin(
        workload_results$Workload_Deviation / pmax(expected_workload, 1), 
        1
      )
      
      expected_accuracy <- mean(workload_results$Expected_Match, na.rm = TRUE)
      cat(paste("Average match with expected workload:", round(expected_accuracy * 100, 2), "%\n\n"))
    }
  }
  
  # ============================================
  # Overall Workload Accuracy
  # ============================================
  
  avg_workload_accuracy <- mean(workload_results$Workload_Accuracy, na.rm = TRUE)
  avg_balance_score <- mean(workload_results$Workload_Balance_Score, na.rm = TRUE)
  avg_capacity_score <- mean(workload_results$Capacity_Score, na.rm = TRUE)
  
  # Overall combines: individual resource accuracy + overall fairness
  overall_workload_accuracy <- mean(c(avg_workload_accuracy, workload_fairness), na.rm = TRUE)
  
  if (!is.na(expected_accuracy)) {
    overall_workload_accuracy <- mean(c(avg_workload_accuracy, workload_fairness, expected_accuracy), na.rm = TRUE)
  }
  
  cat("============================================\n")
  cat("WORKLOAD ACCURACY SUMMARY:\n")
  cat(paste("  Avg Balance Score:", round(avg_balance_score * 100, 2), "%\n"))
  cat(paste("  Avg Capacity Score:", round(avg_capacity_score * 100, 2), "%\n"))
  cat(paste("  Workload Fairness (1-Gini):", round(workload_fairness * 100, 2), "%\n"))
  if (!is.na(expected_accuracy)) {
    cat(paste("  Expected Match:", round(expected_accuracy * 100, 2), "%\n"))
  }
  cat(paste("  OVERALL WORKLOAD ACCURACY:", round(overall_workload_accuracy * 100, 2), "%\n"))
  cat("============================================\n\n")
  
  return(list(
    workload_summary = workload_results,
    gini_coefficient = gini,
    workload_fairness = workload_fairness,
    avg_balance_score = avg_balance_score,
    avg_capacity_score = avg_capacity_score,
    expected_accuracy = expected_accuracy,
    overall_workload_accuracy = overall_workload_accuracy
  ))
}


#' Calculate Per-Activity Duration Quality
#' 
#' Computes quality scores for activity duration calculation per activity.
#' Duration depends on: Start Timestamp + Complete Timestamp
#'
#' @param log Event log dataframe
#' @param activity_col Name of activity column
#' @param start_col Name of start timestamp column
#' @param complete_col Name of complete timestamp column
#' @param min_duration Minimum valid duration (seconds)
#' @param max_duration Maximum valid duration (seconds)
#' @return Dataframe with quality scores per activity

calculate_per_activity_duration_quality <- function(
    log,
    activity_col = "Activity",
    start_col = "Start",
    complete_col = "Complete",
    min_duration = 0,
    max_duration = 86400 * 30  # 30 days default
) {
  
  cat("============================================\n")
  cat("PER-ACTIVITY DURATION QUALITY\n")
  cat("============================================\n\n")
  
  # Get unique activities
  activities <- unique(log[[activity_col]])
  activities <- activities[!is.na(activities)]
  
  results <- data.frame(
    Activity = character(),
    Event_Count = integer(),
    Completeness = numeric(),
    Consistency = numeric(),
    Accuracy = numeric(),
    Combined_Quality = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (act in activities) {
    # Filter events for this activity
    act_events <- log[log[[activity_col]] == act & !is.na(log[[activity_col]]), ]
    n_events <- nrow(act_events)
    
    if (n_events == 0) next
    
    # ----- COMPLETENESS -----
    # Both start and complete timestamps must be present
    complete_start <- sum(!is.na(act_events[[start_col]]))
    complete_end <- sum(!is.na(act_events[[complete_col]]))
    both_complete <- sum(!is.na(act_events[[start_col]]) & !is.na(act_events[[complete_col]]))
    
    completeness <- both_complete / n_events
    
    # ----- CONSISTENCY -----
    # Start timestamp should be <= Complete timestamp
    valid_events <- act_events[!is.na(act_events[[start_col]]) & !is.na(act_events[[complete_col]]), ]
    
    if (nrow(valid_events) > 0) {
      consistent <- sum(valid_events[[start_col]] <= valid_events[[complete_col]])
      consistency <- consistent / nrow(valid_events)
    } else {
      consistency <- NA
    }
    
    # ----- ACCURACY -----
    # Duration should be within valid range
    if (nrow(valid_events) > 0) {
      durations <- as.numeric(difftime(valid_events[[complete_col]], 
                                        valid_events[[start_col]], 
                                        units = "secs"))
      # Only consider consistent events (positive durations)
      durations <- durations[durations >= 0]
      
      if (length(durations) > 0) {
        accurate <- sum(durations >= min_duration & durations <= max_duration)
        accuracy <- accurate / length(durations)
      } else {
        accuracy <- NA
      }
    } else {
      accuracy <- NA
    }
    
    # ----- COMBINED QUALITY -----
    scores <- c(completeness, consistency, accuracy)
    scores <- scores[!is.na(scores)]
    combined <- if (length(scores) > 0) mean(scores) else NA
    
    results <- rbind(results, data.frame(
      Activity = act,
      Event_Count = n_events,
      Completeness = round(completeness, 4),
      Consistency = round(ifelse(is.na(consistency), NA, consistency), 4),
      Accuracy = round(ifelse(is.na(accuracy), NA, accuracy), 4),
      Combined_Quality = round(combined, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by combined quality (ascending to highlight problems)
  results <- results[order(results$Combined_Quality), ]
  
  # Print summary
  cat("Activities analyzed:", nrow(results), "\n\n")
  
  cat("QUALITY DISTRIBUTION:\n")
  cat(paste("  High (>= 90%):", sum(results$Combined_Quality >= 0.9, na.rm = TRUE), "\n"))
  cat(paste("  Moderate (70-90%):", sum(results$Combined_Quality >= 0.7 & results$Combined_Quality < 0.9, na.rm = TRUE), "\n"))
  cat(paste("  Low (< 70%):", sum(results$Combined_Quality < 0.7, na.rm = TRUE), "\n\n"))
  
  # Show problematic activities
  low_quality <- results[!is.na(results$Combined_Quality) & results$Combined_Quality < 0.7, ]
  if (nrow(low_quality) > 0) {
    cat("LOW QUALITY ACTIVITIES (< 70%):\n")
    print(low_quality)
    cat("\n")
  }
  
  return(results)
}


#' Calculate Per-Resource Quality
#' 
#' Computes quality scores for each resource in the log.
#'
#' @param log Event log dataframe
#' @param resource_col Name of resource column
#' @param activity_col Name of activity column
#' @param valid_resources Optional vector of valid resource names
#' @return Dataframe with quality scores per resource

calculate_per_resource_quality <- function(
    log,
    resource_col = "Resource",
    activity_col = "Activity",
    valid_resources = NULL
) {
  
  cat("============================================\n")
  cat("PER-RESOURCE QUALITY\n")
  cat("============================================\n\n")
  
  # Get unique resources (excluding NA)
  resources <- unique(log[[resource_col]])
  resources <- resources[!is.na(resources)]
  
  results <- data.frame(
    Resource = character(),
    Event_Count = integer(),
    Activity_Count = integer(),
    Completeness = numeric(),
    Consistency = numeric(),
    Accuracy = numeric(),
    Combined_Quality = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Overall completeness baseline
  total_events <- nrow(log)
  events_with_resource <- sum(!is.na(log[[resource_col]]))
  overall_completeness <- events_with_resource / total_events
  
  for (res in resources) {
    # Filter events for this resource
    res_events <- log[log[[resource_col]] == res & !is.na(log[[resource_col]]), ]
    n_events <- nrow(res_events)
    
    if (n_events == 0) next
    
    # Count unique activities for this resource
    n_activities <- length(unique(res_events[[activity_col]][!is.na(res_events[[activity_col]])]))
    
    # ----- COMPLETENESS -----
    # Resource is present (by definition 100% for this resource)
    # But we measure: how many events have this resource vs expected
    completeness <- 1.0  # Resource exists for all these events
    
    # ----- CONSISTENCY -----
    # Check for naming variations (same resource with different names)
    # Simple check: resource name doesn't have unusual patterns
    # More sophisticated: check for similar names (Levenshtein distance)
    
    # For now: consistent if resource name is well-formed
    # (no leading/trailing spaces, consistent casing within log)
    res_trimmed <- trimws(res)
    consistency <- ifelse(res == res_trimmed, 1.0, 0.8)
    
    # ----- ACCURACY -----
    # If valid_resources provided, check if resource is in the list
    if (!is.null(valid_resources)) {
      accuracy <- ifelse(res %in% valid_resources, 1.0, 0.0)
    } else {
      # Assume all resources are valid if no list provided
      accuracy <- 1.0
    }
    
    # ----- COMBINED QUALITY -----
    scores <- c(completeness, consistency, accuracy)
    combined <- mean(scores)
    
    results <- rbind(results, data.frame(
      Resource = res,
      Event_Count = n_events,
      Activity_Count = n_activities,
      Completeness = round(completeness, 4),
      Consistency = round(consistency, 4),
      Accuracy = round(accuracy, 4),
      Combined_Quality = round(combined, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by event count (descending)
  results <- results[order(-results$Event_Count), ]
  
  # Print summary
  cat("Resources analyzed:", nrow(results), "\n")
  cat(paste("Overall resource completeness:", round(overall_completeness * 100, 2), "%\n\n"))
  
  cat("TOP 10 RESOURCES BY EVENT COUNT:\n")
  print(head(results, 10))
  cat("\n")
  
  return(results)
}


#' Calculate Resource Completeness Per Activity
#' 
#' For each activity, calculates what percentage of events have a resource assigned.
#' This answers: "For activity X, how often is the resource missing?"
#' 
#' Completeness = 1 - (events with missing resource / total events for activity)
#'
#' @param log Event log dataframe
#' @param activity_col Name of activity column
#' @param resource_col Name of resource column
#' @return Dataframe with resource completeness per activity

calculate_resource_completeness_per_activity <- function(
    log,
    activity_col = "Activity",
    resource_col = "Resource"
) {
  
  cat("============================================\n")
  cat("RESOURCE COMPLETENESS PER ACTIVITY\n")
  cat("============================================\n\n")
  
  # Get unique activities
  activities <- unique(log[[activity_col]])
  activities <- activities[!is.na(activities)]
  
  results <- data.frame(
    Activity = character(),
    Total_Events = integer(),
    Events_With_Resource = integer(),
    Events_Missing_Resource = integer(),
    Resource_Completeness = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (act in activities) {
    # Filter events for this activity
    act_events <- log[log[[activity_col]] == act & !is.na(log[[activity_col]]), ]
    total <- nrow(act_events)
    
    if (total == 0) next
    
    # Count events with resource present vs missing
    with_resource <- sum(!is.na(act_events[[resource_col]]))
    missing_resource <- total - with_resource
    
    # Completeness = 1 - (missing / total)
    completeness <- 1 - (missing_resource / total)
    
    results <- rbind(results, data.frame(
      Activity = act,
      Total_Events = total,
      Events_With_Resource = with_resource,
      Events_Missing_Resource = missing_resource,
      Resource_Completeness = round(completeness, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by completeness (ascending to show problems first)
  results <- results[order(results$Resource_Completeness), ]
  
  # Print summary
  cat("Activities analyzed:", nrow(results), "\n\n")
  
  # Overall
  total_all <- sum(results$Total_Events)
  missing_all <- sum(results$Events_Missing_Resource)
  overall_completeness <- 1 - (missing_all / total_all)
  
  cat(paste("OVERALL RESOURCE COMPLETENESS:", round(overall_completeness * 100, 2), "%\n"))
  cat(paste("  Total events:", total_all, "\n"))
  cat(paste("  Events with resource:", total_all - missing_all, "\n"))
  cat(paste("  Events missing resource:", missing_all, "\n\n"))
  
  # Show activities with missing resources
  incomplete <- results[results$Resource_Completeness < 1.0, ]
  if (nrow(incomplete) > 0) {
    cat("ACTIVITIES WITH MISSING RESOURCES:\n")
    print(incomplete)
    cat("\n")
  } else {
    cat("All activities have complete resource assignments.\n\n")
  }
  
  return(results)
}


#' Calculate Resource-Activity Relationship Quality
#' 
#' Computes quality scores for each resource-activity pair.
#' This is critical for resource allocation in simulation.
#'
#' @param log Event log dataframe
#' @param resource_col Name of resource column
#' @param activity_col Name of activity column
#' @param start_col Name of start timestamp column
#' @param complete_col Name of complete timestamp column
#' @param valid_mappings Optional dataframe with valid resource-activity mappings
#' @return Dataframe with quality scores per resource-activity pair

calculate_resource_activity_quality <- function(
    log,
    resource_col = "Resource",
    activity_col = "Activity",
    start_col = "Start",
    complete_col = "Complete",
    valid_mappings = NULL
) {
  
  cat("============================================\n")
  cat("RESOURCE-ACTIVITY RELATIONSHIP QUALITY\n")
  cat("============================================\n\n")
  
  # Filter to events with both resource and activity
  valid_log <- log[!is.na(log[[resource_col]]) & !is.na(log[[activity_col]]), ]
  
  # Get unique resource-activity pairs
  pairs <- valid_log %>%
    group_by(across(all_of(c(resource_col, activity_col)))) %>%
    summarise(
      Event_Count = n(),
      .groups = "drop"
    )
  
  results <- data.frame(
    Resource = character(),
    Activity = character(),
    Event_Count = integer(),
    Completeness = numeric(),
    Consistency = numeric(),
    Accuracy = numeric(),
    Avg_Duration_Sec = numeric(),
    Duration_StdDev = numeric(),
    Combined_Quality = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(pairs)) {
    res <- pairs[[resource_col]][i]
    act <- pairs[[activity_col]][i]
    
    # Filter events for this pair
    pair_events <- valid_log[valid_log[[resource_col]] == res & 
                              valid_log[[activity_col]] == act, ]
    n_events <- nrow(pair_events)
    
    if (n_events == 0) next
    
    # ----- COMPLETENESS -----
    # Both resource and activity present (by definition 100%)
    # But check timestamp completeness for this pair
    both_ts <- sum(!is.na(pair_events[[start_col]]) & !is.na(pair_events[[complete_col]]))
    completeness <- both_ts / n_events
    
    # ----- CONSISTENCY -----
    # Check if durations are consistent for this resource-activity pair
    # High variance might indicate data quality issues
    valid_ts <- pair_events[!is.na(pair_events[[start_col]]) & 
                             !is.na(pair_events[[complete_col]]), ]
    
    if (nrow(valid_ts) > 1) {
      durations <- as.numeric(difftime(valid_ts[[complete_col]], 
                                        valid_ts[[start_col]], 
                                        units = "secs"))
      durations <- durations[durations >= 0]
      
      if (length(durations) > 1) {
        avg_dur <- mean(durations)
        sd_dur <- sd(durations)
        cv <- ifelse(avg_dur > 0, sd_dur / avg_dur, 0)  # Coefficient of variation
        
        # Lower CV = more consistent = higher quality
        # CV > 1 is very inconsistent, CV < 0.3 is consistent
        consistency <- max(0, 1 - cv)
        consistency <- min(1, consistency)  # Cap at 1
      } else {
        avg_dur <- durations[1]
        sd_dur <- 0
        consistency <- 1.0  # Only one event, assume consistent
      }
    } else if (nrow(valid_ts) == 1) {
      durations <- as.numeric(difftime(valid_ts[[complete_col]], 
                                        valid_ts[[start_col]], 
                                        units = "secs"))
      avg_dur <- max(0, durations)
      sd_dur <- 0
      consistency <- 1.0
    } else {
      avg_dur <- NA
      sd_dur <- NA
      consistency <- NA
    }
    
    # ----- ACCURACY -----
    # If valid_mappings provided, check if this pair is valid
    if (!is.null(valid_mappings)) {
      is_valid <- any(valid_mappings[[resource_col]] == res & 
                       valid_mappings[[activity_col]] == act)
      accuracy <- ifelse(is_valid, 1.0, 0.0)
    } else {
      # Assume valid if no mapping provided
      accuracy <- 1.0
    }
    
    # ----- COMBINED QUALITY -----
    scores <- c(completeness, consistency, accuracy)
    scores <- scores[!is.na(scores)]
    combined <- if (length(scores) > 0) mean(scores) else NA
    
    results <- rbind(results, data.frame(
      Resource = res,
      Activity = act,
      Event_Count = n_events,
      Completeness = round(completeness, 4),
      Consistency = round(ifelse(is.na(consistency), NA, consistency), 4),
      Accuracy = round(accuracy, 4),
      Avg_Duration_Sec = round(ifelse(is.na(avg_dur), NA, avg_dur), 2),
      Duration_StdDev = round(ifelse(is.na(sd_dur), NA, sd_dur), 2),
      Combined_Quality = round(combined, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by combined quality (ascending to highlight problems)
  results <- results[order(results$Combined_Quality), ]
  
  # Print summary
  cat("Resource-Activity pairs analyzed:", nrow(results), "\n\n")
  
  cat("QUALITY DISTRIBUTION:\n")
  cat(paste("  High (>= 90%):", sum(results$Combined_Quality >= 0.9, na.rm = TRUE), "\n"))
  cat(paste("  Moderate (70-90%):", sum(results$Combined_Quality >= 0.7 & results$Combined_Quality < 0.9, na.rm = TRUE), "\n"))
  cat(paste("  Low (< 70%):", sum(results$Combined_Quality < 0.7, na.rm = TRUE), "\n\n"))
  
  # Show problematic pairs
  low_quality <- results[!is.na(results$Combined_Quality) & results$Combined_Quality < 0.7, ]
  if (nrow(low_quality) > 0 && nrow(low_quality) <= 20) {
    cat("LOW QUALITY PAIRS (< 70%):\n")
    print(low_quality)
    cat("\n")
  } else if (nrow(low_quality) > 20) {
    cat(paste("LOW QUALITY PAIRS:", nrow(low_quality), "(showing top 20)\n"))
    print(head(low_quality, 20))
    cat("\n")
  }
  
  return(results)
}


#' Calculate All Granular Quality Scores
#' 
#' Master function that calculates all granular quality scores
#'
#' @param log Event log dataframe
#' @param activity_col Name of activity column
#' @param resource_col Name of resource column
#' @param start_col Name of start timestamp column
#' @param complete_col Name of complete timestamp column
#' @param min_duration Minimum valid duration (seconds)
#' @param max_duration Maximum valid duration (seconds)
#' @param valid_resources Optional vector of valid resource names
#' @param valid_mappings Optional dataframe with valid resource-activity mappings
#' @return List with all granular quality results

calculate_granular_simulation_quality <- function(
    log,
    case_col = "Case.ID",
    activity_col = "Activity",
    resource_col = "Resource",
    start_col = "Start",
    complete_col = "Complete",
    min_duration = 0,
    max_duration = 86400 * 30,
    valid_resources = NULL,
    valid_mappings = NULL,
    expected_order = NULL,
    expected_probabilities = NULL,  # For probability accuracy comparison
    expected_workload = NULL,       # For workload accuracy comparison
    max_daily_hours = 12            # Maximum reasonable daily working hours
) {

  # Helper: coerce NULL / non-finite values to NA_real_ so round() never crashes
  safe_num <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    x <- suppressWarnings(as.numeric(x))
    ifelse(is.finite(x), x, NA_real_)
  }

  cat("############################################\n")
  cat("GRANULAR SIMULATION PARAMETER QUALITY\n")
  cat("############################################\n\n")
  
  # 1. Per-Activity Duration Quality
  activity_quality <- calculate_per_activity_duration_quality(
    log = log,
    activity_col = activity_col,
    start_col = start_col,
    complete_col = complete_col,
    min_duration = min_duration,
    max_duration = max_duration
  )
  
  # 2. Per-Resource Quality
  resource_quality <- calculate_per_resource_quality(
    log = log,
    resource_col = resource_col,
    activity_col = activity_col,
    valid_resources = valid_resources
  )
  
  # 3. Resource Completeness Per Activity (YOUR DEFINITION)
  # For each activity: Completeness = 1 - (missing resources / total events)
  resource_completeness_per_activity <- calculate_resource_completeness_per_activity(
    log = log,
    activity_col = activity_col,
    resource_col = resource_col
  )
  
  # 4. Resource-Activity Relationship Quality
  resource_activity_quality <- calculate_resource_activity_quality(
    log = log,
    resource_col = resource_col,
    activity_col = activity_col,
    start_col = start_col,
    complete_col = complete_col,
    valid_mappings = valid_mappings
  )
  
  # 5. STRUCTURAL CONSISTENCY (Resource-Activity relationship patterns)
  structural_consistency <- calculate_structural_consistency(
    log = log,
    activity_col = activity_col,
    resource_col = resource_col
  )
  
  # 6. BEHAVIORAL CONSISTENCY (Duration + Sequence)
  behavioral_consistency <- calculate_behavioral_consistency(
    log = log,
    case_col = case_col,
    activity_col = activity_col,
    resource_col = resource_col,
    start_col = start_col,
    complete_col = complete_col,
    expected_order = expected_order
  )
  
  # 7. RESOURCE-ACTIVITY PROBABILITY ACCURACY
  probability_accuracy <- calculate_resource_activity_probability_accuracy(
    log = log,
    activity_col = activity_col,
    resource_col = resource_col,
    expected_probabilities = expected_probabilities
  )
  
  # 8. WORKLOAD ACCURACY
  workload_accuracy <- calculate_workload_accuracy(
    log = log,
    resource_col = resource_col,
    start_col = start_col,
    complete_col = complete_col,
    max_daily_hours = max_daily_hours,
    expected_workload = expected_workload
  )
  
  # ============================================
  # AGGREGATE SCORES
  # ============================================
  
  cat("============================================\n")
  cat("AGGREGATE GRANULAR QUALITY SUMMARY\n")
  cat("============================================\n\n")
  
  # Activity Duration aggregate
  avg_activity_quality <- mean(activity_quality$Combined_Quality, na.rm = TRUE)
  min_activity_quality <- min(activity_quality$Combined_Quality, na.rm = TRUE)
  
  cat("ACTIVITY DURATION QUALITY:\n")
  cat(paste("  Average across activities:", round(avg_activity_quality * 100, 2), "%\n"))
  cat(paste("  Minimum (worst activity):", round(min_activity_quality * 100, 2), "%\n\n"))
  
  # Resource aggregate
  avg_resource_quality <- mean(resource_quality$Combined_Quality, na.rm = TRUE)
  min_resource_quality <- min(resource_quality$Combined_Quality, na.rm = TRUE)
  
  cat("RESOURCE QUALITY:\n")
  cat(paste("  Average across resources:", round(avg_resource_quality * 100, 2), "%\n"))
  cat(paste("  Minimum (worst resource):", round(min_resource_quality * 100, 2), "%\n\n"))
  
  # Resource Completeness Per Activity (YOUR DEFINITION)
  avg_res_completeness <- mean(resource_completeness_per_activity$Resource_Completeness, na.rm = TRUE)
  min_res_completeness <- min(resource_completeness_per_activity$Resource_Completeness, na.rm = TRUE)
  
  cat("RESOURCE COMPLETENESS PER ACTIVITY:\n")
  cat(paste("  Average across activities:", round(avg_res_completeness * 100, 2), "%\n"))
  cat(paste("  Minimum (worst activity):", round(min_res_completeness * 100, 2), "%\n\n"))
  
  # Resource-Activity aggregate
  avg_ra_quality <- mean(resource_activity_quality$Combined_Quality, na.rm = TRUE)
  min_ra_quality <- min(resource_activity_quality$Combined_Quality, na.rm = TRUE)
  
  cat("RESOURCE-ACTIVITY RELATIONSHIP QUALITY:\n")
  cat(paste("  Average across pairs:", round(avg_ra_quality * 100, 2), "%\n"))
  cat(paste("  Minimum (worst pair):", round(min_ra_quality * 100, 2), "%\n\n"))
  
  # STRUCTURAL CONSISTENCY
  cat("STRUCTURAL CONSISTENCY:\n")
  cat(paste("  Activity-centric:", round(structural_consistency$activity_consistency_avg * 100, 2), "%\n"))
  cat(paste("  Resource-centric:", round(structural_consistency$resource_consistency_avg * 100, 2), "%\n"))
  cat(paste("  Overall:", round(structural_consistency$overall_structural_consistency * 100, 2), "%\n\n"))
  
  # BEHAVIORAL CONSISTENCY
  cat("BEHAVIORAL CONSISTENCY:\n")
  cat(paste("  Duration Consistency:", round(behavioral_consistency$avg_duration_consistency * 100, 2), "%\n"))
  cat(paste("  Sequence Consistency:", round(behavioral_consistency$avg_sequence_consistency * 100, 2), "%\n"))
  cat(paste("  Overall:", round(behavioral_consistency$overall_behavioral_consistency * 100, 2), "%\n\n"))
  
  # PROBABILITY ACCURACY (Resource-Activity assignment accuracy)
  cat("PROBABILITY ACCURACY (R-A Assignment):\n")
  cat(paste("  Resource Specialization:", round(safe_num(probability_accuracy$avg_resource_accuracy) * 100, 2), "%\n"))
  cat(paste("  Activity Assignment:", round(safe_num(probability_accuracy$avg_activity_accuracy) * 100, 2), "%\n"))
  cat(paste("  Overall:", round(safe_num(probability_accuracy$overall_probability_accuracy) * 100, 2), "%\n\n"))
  
  # WORKLOAD ACCURACY
  cat("WORKLOAD ACCURACY:\n")
  cat(paste("  Balance Score:", round(safe_num(workload_accuracy$avg_balance_score) * 100, 2), "%\n"))
  cat(paste("  Capacity Score:", round(safe_num(workload_accuracy$avg_capacity_score) * 100, 2), "%\n"))
  cat(paste("  Workload Fairness:", round(safe_num(workload_accuracy$workload_fairness) * 100, 2), "%\n"))
  cat(paste("  Overall:", round(safe_num(workload_accuracy$overall_workload_accuracy) * 100, 2), "%\n\n"))
  
  # Overall granular quality (now includes consistency AND accuracy)
  overall_granular <- mean(c(
    avg_activity_quality, 
    avg_resource_quality, 
    avg_res_completeness, 
    avg_ra_quality,
    structural_consistency$overall_structural_consistency,
    behavioral_consistency$overall_behavioral_consistency,
    safe_num(probability_accuracy$overall_probability_accuracy),
    safe_num(workload_accuracy$overall_workload_accuracy)
  ), na.rm = TRUE)
  
  overall_conservative <- min(c(
    min_activity_quality, 
    min_resource_quality, 
    min_res_completeness, 
    min_ra_quality,
    structural_consistency$overall_structural_consistency,
    behavioral_consistency$overall_behavioral_consistency,
    safe_num(probability_accuracy$overall_probability_accuracy),
    safe_num(workload_accuracy$overall_workload_accuracy)
  ), na.rm = TRUE)
  
  cat("============================================\n")
  cat("OVERALL GRANULAR SIMULATION QUALITY:\n")
  cat(paste("  Average:", round(overall_granular * 100, 2), "%\n"))
  cat(paste("  Conservative (minimum):", round(overall_conservative * 100, 2), "%\n"))
  cat("============================================\n\n")
  
  # ============================================
  # SAVE RESULTS
  # ============================================
  
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir)
  
  write.csv(activity_quality, 
            file.path(results_dir, "per_activity_duration_quality.csv"), 
            row.names = FALSE)
  
  write.csv(resource_quality, 
            file.path(results_dir, "per_resource_quality.csv"), 
            row.names = FALSE)
  
  write.csv(resource_completeness_per_activity,
            file.path(results_dir, "resource_completeness_per_activity.csv"),
            row.names = FALSE)
  
  write.csv(resource_activity_quality, 
            file.path(results_dir, "resource_activity_quality.csv"), 
            row.names = FALSE)
  
  # Save structural consistency results
  write.csv(structural_consistency$activity_consistency,
            file.path(results_dir, "structural_consistency_by_activity.csv"),
            row.names = FALSE)
  
  write.csv(structural_consistency$resource_consistency,
            file.path(results_dir, "structural_consistency_by_resource.csv"),
            row.names = FALSE)
  
  if (!is.null(structural_consistency$unexpected_pairs) && nrow(structural_consistency$unexpected_pairs) > 0) {
    write.csv(structural_consistency$unexpected_pairs,
              file.path(results_dir, "structural_unexpected_pairs.csv"),
              row.names = FALSE)
  }
  
  # Save behavioral consistency results
  write.csv(behavioral_consistency$duration_consistency,
            file.path(results_dir, "behavioral_duration_consistency.csv"),
            row.names = FALSE)
  
  write.csv(behavioral_consistency$sequence_consistency,
            file.path(results_dir, "behavioral_sequence_consistency.csv"),
            row.names = FALSE)
  
  # Save probability accuracy results
  write.csv(probability_accuracy$resource_accuracy,
            file.path(results_dir, "probability_accuracy_by_resource.csv"),
            row.names = FALSE)
  
  write.csv(probability_accuracy$activity_accuracy,
            file.path(results_dir, "probability_accuracy_by_activity.csv"),
            row.names = FALSE)
  
  # Save workload accuracy results
  write.csv(workload_accuracy$workload_summary,
            file.path(results_dir, "workload_accuracy.csv"),
            row.names = FALSE)
  
  # Summary table (now includes consistency AND accuracy)
  summary_table <- data.frame(
    Granular_Level = c("Per-Activity Duration", "Per-Resource", "Resource Completeness/Activity", 
                       "Resource-Activity Pairs", "Structural Consistency", "Behavioral Consistency",
                       "Probability Accuracy", "Workload Accuracy", "Overall"),
    Count = c(nrow(activity_quality), nrow(resource_quality), 
              nrow(resource_completeness_per_activity), nrow(resource_activity_quality), 
              NA, NA, NA, NA, NA),
    Average_Quality = c(round(safe_num(avg_activity_quality), 4), round(safe_num(avg_resource_quality), 4),
                        round(safe_num(avg_res_completeness), 4), round(safe_num(avg_ra_quality), 4), 
                        round(safe_num(structural_consistency$overall_structural_consistency), 4),
                        round(safe_num(behavioral_consistency$overall_behavioral_consistency), 4),
                        round(safe_num(probability_accuracy$overall_probability_accuracy), 4),
                        round(safe_num(workload_accuracy$overall_workload_accuracy), 4),
                        round(safe_num(overall_granular), 4)),
    Minimum_Quality = c(round(safe_num(min_activity_quality), 4), round(safe_num(min_resource_quality), 4),
                        round(safe_num(min_res_completeness), 4), round(safe_num(min_ra_quality), 4), 
                        NA, NA, NA, NA, round(safe_num(overall_conservative), 4)),
    Quality_Percentage = paste0(round(c(safe_num(avg_activity_quality), safe_num(avg_resource_quality), 
                                        safe_num(avg_res_completeness), safe_num(avg_ra_quality), 
                                        safe_num(structural_consistency$overall_structural_consistency),
                                        safe_num(behavioral_consistency$overall_behavioral_consistency),
                                        safe_num(probability_accuracy$overall_probability_accuracy),
                                        workload_accuracy$overall_workload_accuracy,
                                        overall_granular) * 100, 2), "%")
  )
  
  write.csv(summary_table, 
            file.path(results_dir, "granular_quality_summary.csv"), 
            row.names = FALSE)
  
  cat("Saved to:\n")
  cat("  - results/per_activity_duration_quality.csv\n")
  cat("  - results/per_resource_quality.csv\n")
  cat("  - results/resource_completeness_per_activity.csv\n")
  cat("  - results/resource_activity_quality.csv\n")
  cat("  - results/structural_consistency_by_activity.csv\n")
  cat("  - results/structural_consistency_by_resource.csv\n")
  cat("  - results/behavioral_duration_consistency.csv\n")
  cat("  - results/behavioral_sequence_consistency.csv\n")
  cat("  - results/probability_accuracy_by_resource.csv\n")
  cat("  - results/probability_accuracy_by_activity.csv\n")
  cat("  - results/workload_accuracy.csv\n")
  cat("  - results/granular_quality_summary.csv\n\n")
  
  # Return all results
  return(list(
    # Detailed results
    per_activity = activity_quality,
    per_resource = resource_quality,
    resource_completeness_per_activity = resource_completeness_per_activity,
    resource_activity = resource_activity_quality,
    
    # Structural Consistency
    structural_consistency = structural_consistency,
    
    # Behavioral Consistency
    behavioral_consistency = behavioral_consistency,
    
    # Probability Accuracy (Resource-Activity assignment)
    probability_accuracy = probability_accuracy,
    
    # Workload Accuracy
    workload_accuracy = workload_accuracy,
    
    # Aggregates
    activity_quality_avg = avg_activity_quality,
    activity_quality_min = min_activity_quality,
    resource_quality_avg = avg_resource_quality,
    resource_quality_min = min_resource_quality,
    resource_completeness_avg = avg_res_completeness,
    resource_completeness_min = min_res_completeness,
    resource_activity_quality_avg = avg_ra_quality,
    resource_activity_quality_min = min_ra_quality,
    structural_consistency_overall = structural_consistency$overall_structural_consistency,
    behavioral_consistency_overall = behavioral_consistency$overall_behavioral_consistency,
    probability_accuracy_overall = probability_accuracy$overall_probability_accuracy,
    workload_accuracy_overall = workload_accuracy$overall_workload_accuracy,
    
    # Overall
    overall_average = overall_granular,
    overall_conservative = overall_conservative,
    
    # Summary table
    summary = summary_table
  ))
}


#' Identify Problematic Elements
#' 
#' Returns activities, resources, and pairs that need attention
#'
#' @param granular_results Results from calculate_granular_simulation_quality()
#' @param threshold Quality threshold (default 0.7)
#' @return List of problematic elements

identify_quality_issues <- function(granular_results, threshold = 0.7) {
  
  cat("============================================\n")
  cat("QUALITY ISSUES IDENTIFIED\n")
  cat(paste("Threshold:", threshold * 100, "%\n"))
  cat("============================================\n\n")
  
  # Problematic activities
  problem_activities <- granular_results$per_activity[
    !is.na(granular_results$per_activity$Combined_Quality) & 
    granular_results$per_activity$Combined_Quality < threshold, ]
  
  cat(paste("Problematic Activities:", nrow(problem_activities), "\n"))
  if (nrow(problem_activities) > 0) {
    print(problem_activities[, c("Activity", "Event_Count", "Combined_Quality")])
  }
  cat("\n")
  
  # Problematic resources
  problem_resources <- granular_results$per_resource[
    !is.na(granular_results$per_resource$Combined_Quality) & 
    granular_results$per_resource$Combined_Quality < threshold, ]
  
  cat(paste("Problematic Resources:", nrow(problem_resources), "\n"))
  if (nrow(problem_resources) > 0) {
    print(problem_resources[, c("Resource", "Event_Count", "Combined_Quality")])
  }
  cat("\n")
  
  # Activities with incomplete resource assignments
  incomplete_resources <- granular_results$resource_completeness_per_activity[
    granular_results$resource_completeness_per_activity$Resource_Completeness < threshold, ]
  
  cat(paste("Activities with Missing Resources:", nrow(incomplete_resources), "\n"))
  if (nrow(incomplete_resources) > 0) {
    print(incomplete_resources[, c("Activity", "Total_Events", "Events_Missing_Resource", "Resource_Completeness")])
  }
  cat("\n")
  
  # Problematic resource-activity pairs
  problem_pairs <- granular_results$resource_activity[
    !is.na(granular_results$resource_activity$Combined_Quality) & 
    granular_results$resource_activity$Combined_Quality < threshold, ]
  
  cat(paste("Problematic Resource-Activity Pairs:", nrow(problem_pairs), "\n"))
  if (nrow(problem_pairs) > 0 && nrow(problem_pairs) <= 20) {
    print(problem_pairs[, c("Resource", "Activity", "Event_Count", "Combined_Quality")])
  } else if (nrow(problem_pairs) > 20) {
    print(head(problem_pairs[, c("Resource", "Activity", "Event_Count", "Combined_Quality")], 20))
    cat(paste("... and", nrow(problem_pairs) - 20, "more\n"))
  }
  cat("\n")
  
  # STRUCTURAL CONSISTENCY ISSUES
  # Activities with scattered resource assignments (low focus)
  structural_activity_issues <- data.frame()
  if (!is.null(granular_results$structural_consistency) && 
      !is.null(granular_results$structural_consistency$per_activity) &&
      is.data.frame(granular_results$structural_consistency$per_activity) &&
      "Consistency_Score" %in% colnames(granular_results$structural_consistency$per_activity)) {
    structural_activity_issues <- granular_results$structural_consistency$per_activity[
      granular_results$structural_consistency$per_activity$Consistency_Score < threshold, ]
    
    cat(paste("Activities with Scattered Resource Patterns:", nrow(structural_activity_issues), "\n"))
    if (nrow(structural_activity_issues) > 0) {
      print(structural_activity_issues[, c("Activity", "Unique_Resources", "Consistency_Score")])
    }
  }
  cat("\n")
  
  # Resources with scattered activity assignments (low specialization)
  structural_resource_issues <- data.frame()
  if (!is.null(granular_results$structural_consistency) && 
      !is.null(granular_results$structural_consistency$per_resource) &&
      is.data.frame(granular_results$structural_consistency$per_resource) &&
      "Consistency_Score" %in% colnames(granular_results$structural_consistency$per_resource)) {
    structural_resource_issues <- granular_results$structural_consistency$per_resource[
      granular_results$structural_consistency$per_resource$Consistency_Score < threshold, ]
    
    cat(paste("Resources with Scattered Activity Patterns:", nrow(structural_resource_issues), "\n"))
    if (nrow(structural_resource_issues) > 0) {
      print(structural_resource_issues[, c("Resource", "Unique_Activities", "Consistency_Score")])
    }
  }
  cat("\n")
  
  # BEHAVIORAL CONSISTENCY ISSUES
  # Activities with high duration variability
  duration_issues <- data.frame()
  if (!is.null(granular_results$behavioral_consistency) && 
      !is.null(granular_results$behavioral_consistency$duration_consistency)) {
    duration_issues <- granular_results$behavioral_consistency$duration_consistency[
      granular_results$behavioral_consistency$duration_consistency$Duration_Consistency < threshold, ]
    
    cat(paste("Activities with High Duration Variability:", nrow(duration_issues), "\n"))
    if (nrow(duration_issues) > 0) {
      cols_to_show <- intersect(c("Resource", "Activity", "CV", "Duration_Consistency"), colnames(duration_issues))
      print(duration_issues[, cols_to_show])
    }
  }
  cat("\n")
  
  # Cases with sequence issues
  sequence_issues <- data.frame()
  if (!is.null(granular_results$behavioral_consistency) && 
      !is.null(granular_results$behavioral_consistency$sequence_consistency)) {
    sequence_issues <- granular_results$behavioral_consistency$sequence_consistency[
      granular_results$behavioral_consistency$sequence_consistency$Sequence_Consistency < 1.0, ]
    
    cat(paste("Cases with Sequence Deviations:", nrow(sequence_issues), "\n"))
    if (nrow(sequence_issues) > 0 && nrow(sequence_issues) <= 20) {
      print(sequence_issues[, c("Case", "Correct_Transitions", "Total_Transitions", "Sequence_Consistency")])
    } else if (nrow(sequence_issues) > 20) {
      print(head(sequence_issues[, c("Case", "Correct_Transitions", "Total_Transitions", "Sequence_Consistency")], 20))
      cat(paste("... and", nrow(sequence_issues) - 20, "more\n"))
    }
  }
  cat("\n")
  
  # PROBABILITY ACCURACY ISSUES
  # Resources with low specialization (generalists)
  resource_specialization_issues <- data.frame()
  if (!is.null(granular_results$probability_accuracy) && 
      !is.null(granular_results$probability_accuracy$resource_accuracy)) {
    resource_specialization_issues <- granular_results$probability_accuracy$resource_accuracy[
      granular_results$probability_accuracy$resource_accuracy$Specialization_Accuracy < threshold, ]
    
    cat(paste("Resources with Low Specialization (Generalists):", nrow(resource_specialization_issues), "\n"))
    if (nrow(resource_specialization_issues) > 0) {
      print(resource_specialization_issues[, c("Resource", "Unique_Activities", "Specialization_Accuracy")])
    }
  }
  cat("\n")
  
  # Activities with scattered assignment (low ownership)
  activity_assignment_issues <- data.frame()
  if (!is.null(granular_results$probability_accuracy) && 
      !is.null(granular_results$probability_accuracy$activity_accuracy)) {
    activity_assignment_issues <- granular_results$probability_accuracy$activity_accuracy[
      granular_results$probability_accuracy$activity_accuracy$Assignment_Accuracy < threshold, ]
    
    cat(paste("Activities with Scattered Assignment (Low Ownership):", nrow(activity_assignment_issues), "\n"))
    if (nrow(activity_assignment_issues) > 0) {
      print(activity_assignment_issues[, c("Activity", "Unique_Resources", "Assignment_Accuracy")])
    }
  }
  cat("\n")
  
  # WORKLOAD ACCURACY ISSUES
  # Resources with workload issues (imbalanced or over capacity)
  workload_issues <- data.frame()
  if (!is.null(granular_results$workload_accuracy) && 
      !is.null(granular_results$workload_accuracy$workload_summary)) {
    workload_issues <- granular_results$workload_accuracy$workload_summary[
      granular_results$workload_accuracy$workload_summary$Workload_Accuracy < threshold, ]
    
    cat(paste("Resources with Workload Issues:", nrow(workload_issues), "\n"))
    if (nrow(workload_issues) > 0) {
      print(workload_issues[, c("Resource", "Event_Count", "Workload_Share", 
                                "Avg_Hours_Per_Day", "Workload_Accuracy")])
    }
  }
  cat("\n")
  
  return(list(
    activities = problem_activities,
    resources = problem_resources,
    incomplete_resources = incomplete_resources,
    pairs = problem_pairs,
    structural_activity_issues = structural_activity_issues,
    structural_resource_issues = structural_resource_issues,
    duration_issues = duration_issues,
    sequence_issues = sequence_issues,
    resource_specialization_issues = resource_specialization_issues,
    activity_assignment_issues = activity_assignment_issues,
    workload_issues = workload_issues,
    total_issues = nrow(problem_activities) + nrow(problem_resources) + 
                   nrow(incomplete_resources) + nrow(problem_pairs) +
                   nrow(structural_activity_issues) + nrow(structural_resource_issues) +
                   nrow(duration_issues) + nrow(sequence_issues) +
                   nrow(resource_specialization_issues) + nrow(activity_assignment_issues) +
                   nrow(workload_issues)
  ))
}
