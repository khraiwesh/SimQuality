# Simulation Parameter Quality Derivation
# Maps event log attribute quality scores to BPS parameter reliability scores
# 
# Supports two modes:
# 1. Independent assessment (parallel checks)
# 2. Cascading assessment (sequential: Completeness → Consistency → Accuracy)

library(dplyr)

#' Calculate Simulation Parameter Quality Scores
#' 
#' Derives quality scores for simulation parameters based on 
#' the quality scores of their dependent event log attributes.
#'
#' @param attribute_scores A list containing quality scores for each attribute
#' @param aggregation_method Method to combine attribute scores: "min", "mean", "product"
#' @return A list with quality scores for each simulation parameter

calculate_simulation_parameter_quality <- function(
    attribute_scores,
    aggregation_method = "mean",
    dimension_weights = c(completeness = 1/3, accuracy = 1/3, consistency = 1/3),
    sample_sizes = NULL  # Named list: e.g. list(total_events=2000, total_cases=500)
) {
  
  cat("============================================\n")
  cat("SIMULATION PARAMETER QUALITY DERIVATION\n")
  cat("============================================\n\n")
  
  # Extract attribute quality scores (as decimals 0-1)
  # Each attribute has: completeness, accuracy, consistency
  
  start_ts <- list(
    completeness = attribute_scores$start_completeness,
    accuracy = attribute_scores$start_accuracy,
    consistency = attribute_scores$start_consistency
  )
  
  complete_ts <- list(
    completeness = attribute_scores$complete_completeness,
    accuracy = attribute_scores$complete_accuracy,
    consistency = attribute_scores$complete_consistency
  )
  
  case_id <- list(
    completeness = attribute_scores$caseid_completeness,
    accuracy = attribute_scores$caseid_accuracy,
    consistency = NA  # Not measured — excluded from aggregation rather than inflating score
  )
  
  activity <- list(
    completeness = attribute_scores$activity_completeness,
    accuracy = attribute_scores$activity_accuracy,
    consistency = attribute_scores$activity_consistency
  )
  
  resource <- list(
    completeness = attribute_scores$resource_completeness,
    accuracy = attribute_scores$resource_accuracy,
    consistency = attribute_scores$resource_consistency
  )
  
  # ============================================
  # STEP 1: Compute combined score per attribute
  # ============================================
  
  combine_dimensions <- function(attr_scores, method = "mean", weights = dimension_weights) {
    scores <- c(attr_scores$completeness, attr_scores$accuracy, attr_scores$consistency)
    dim_names <- c("completeness", "accuracy", "consistency")
    valid_mask <- !is.na(scores)
    scores <- scores[valid_mask]
    dim_names <- dim_names[valid_mask]
    
    if (length(scores) == 0) return(NA)
    
    # Get matching weights, re-normalize to sum to 1
    w <- weights[dim_names]
    w <- w / sum(w)
    
    switch(method,
      "min" = min(scores),
      "mean" = sum(scores * w),  # Weighted mean (equal weights by default)
      "product" = prod(scores),
      "harmonic" = sum(w) / sum(w / scores),
      sum(scores * w)  # default: weighted mean
    )
  }
  
  # Combine C, A, Co into single attribute score
  start_ts_score <- combine_dimensions(start_ts, "mean")
  complete_ts_score <- combine_dimensions(complete_ts, "mean")
  case_id_score <- combine_dimensions(case_id, "mean")
  activity_score <- combine_dimensions(activity, "mean")
  resource_score <- combine_dimensions(resource, "mean")
  
  cat("ATTRIBUTE COMBINED SCORES:\n")
  cat(paste("  Start Timestamp:", round(start_ts_score * 100, 2), "%\n"))
  cat(paste("  Complete Timestamp:", round(complete_ts_score * 100, 2), "%\n"))
  cat(paste("  Case ID:", round(case_id_score * 100, 2), "%\n"))
  cat(paste("  Activity:", round(activity_score * 100, 2), "%\n"))
  cat(paste("  Resource:", round(resource_score * 100, 2), "%\n\n"))
  
  # ============================================
  # STEP 1b: Sample size confidence factor
  # ============================================
  # confidence = min(1, log(n+1) / log(101))  →  n=100 gives 1.0
  
  confidence_factor <- 1.0
  if (!is.null(sample_sizes)) {
    n <- ifelse(!is.null(sample_sizes$total_events), sample_sizes$total_events, 100)
    confidence_factor <- min(1, log(n + 1) / log(101))
    cat(paste("SAMPLE SIZE CONFIDENCE:\n"))
    cat(paste("  Events:", n, "\n"))
    cat(paste("  Confidence factor:", round(confidence_factor, 4), "\n\n"))
  }
  
  # ============================================
  # STEP 2: Define dependency mapping
  # ============================================
  
  # Simulation Parameter -> Dependent Attributes
  # Activity Duration: Start TS + Complete TS
  # Arrival Time: Case ID + Start TS
  # Branching Probability: Activity + Start TS (sequence matters)
  # Resource Allocation: Resource
  
  dependencies <- list(
    activity_duration = c("start_ts", "complete_ts"),
    arrival_time = c("case_id", "start_ts"),
    branching_probability = c("activity", "start_ts", "case_id"),  # sequence depends on case_id for ordering
    resource_allocation = c("resource", "activity")               # assignment depends on activity label correctness
  )
  
  # ============================================
  # STEP 3: Aggregate dependent attribute scores
  # ============================================
  
  aggregate_scores <- function(attr_names, method = "mean") {
    scores <- c()
    
    for (attr in attr_names) {
      score <- switch(attr,
        "start_ts" = start_ts_score,
        "complete_ts" = complete_ts_score,
        "case_id" = case_id_score,
        "activity" = activity_score,
        "resource" = resource_score,
        NA
      )
      if (!is.na(score)) scores <- c(scores, score)
    }
    
    if (length(scores) == 0) return(NA)
    
    switch(method,
      "min" = min(scores),              # Conservative: weakest link
      "mean" = mean(scores),            # Average
      "product" = prod(scores),         # Probabilistic (if independent)
      "weighted_mean" = mean(scores),   # Could add weights later
      mean(scores)  # default
    )
  }
  
  # ============================================
  # STEP 4: Calculate simulation parameter scores
  # ============================================
  
  cat("SIMULATION PARAMETER QUALITY SCORES:\n")
  cat(paste("Aggregation method:", aggregation_method, "\n\n"))
  
  # Activity Duration Quality
  duration_score <- aggregate_scores(dependencies$activity_duration, aggregation_method)
  cat("1. ACTIVITY DURATION:\n")
  cat(paste("   Depends on: Start Timestamp + Complete Timestamp\n"))
  cat(paste("   Quality Score:", round(duration_score * 100, 2), "%\n\n"))
  
  # Arrival Time Quality
  arrival_score <- aggregate_scores(dependencies$arrival_time, aggregation_method)
  cat("2. ARRIVAL TIME (Inter-arrival Time):\n")
  cat(paste("   Depends on: Case ID + Start Timestamp\n"))
  cat(paste("   Quality Score:", round(arrival_score * 100, 2), "%\n\n"))
  
  # Branching Probability Quality
  branching_score <- aggregate_scores(dependencies$branching_probability, aggregation_method)
  cat("3. BRANCHING PROBABILITY:\n")
  cat(paste("   Depends on: Activity + Start Timestamp\n"))
  cat(paste("   Quality Score:", round(branching_score * 100, 2), "%\n\n"))
  
  # Resource Allocation Quality
  resource_alloc_score <- aggregate_scores(dependencies$resource_allocation, aggregation_method)
  cat("4. RESOURCE ALLOCATION:\n")
  cat(paste("   Depends on: Resource\n"))
  cat(paste("   Quality Score:", round(resource_alloc_score * 100, 2), "%\n\n"))
  
  # ============================================
  # STEP 5: Overall Simulation Model Quality
  # ============================================
  
  param_scores <- c(duration_score, arrival_score, branching_score, 
                    resource_alloc_score)
  param_scores <- param_scores[!is.na(param_scores)]
  
  overall_score <- mean(param_scores)
  overall_score_min <- min(param_scores)
  
  # Confidence-adjusted scores
  overall_score_conf <- overall_score * confidence_factor
  
  cat("============================================\n")
  cat("OVERALL SIMULATION MODEL QUALITY:\n")
  cat(paste("   Average:", round(overall_score * 100, 2), "%\n"))
  cat(paste("   Minimum (Conservative):", round(overall_score_min * 100, 2), "%\n"))
  if (confidence_factor < 1.0) {
    cat(paste("   Confidence-Adjusted:", round(overall_score_conf * 100, 2), "%\n"))
  }
  cat("============================================\n\n")
  
  # ============================================
  # STEP 6: Create output tables
  # ============================================
  
  # Attribute scores table
  attribute_scores_table <- data.frame(
    Attribute = c("Start Timestamp", "Complete Timestamp", "Case ID", "Activity", "Resource"),
    Completeness = c(start_ts$completeness, complete_ts$completeness, 
                     case_id$completeness, activity$completeness, resource$completeness),
    Accuracy = c(start_ts$accuracy, complete_ts$accuracy,
                 case_id$accuracy, activity$accuracy, resource$accuracy),
    Consistency = c(start_ts$consistency, complete_ts$consistency,
                    case_id$consistency, activity$consistency, resource$consistency),
    Consistency_Type = c("syntactic+behavioral", "syntactic+behavioral", 
                         "N/A", "syntactic", "syntactic+behavioral"),
    Combined_Score = c(start_ts_score, complete_ts_score, case_id_score, 
                       activity_score, resource_score),
    Dimensions_Used = c(
      sum(!is.na(c(start_ts$completeness, start_ts$accuracy, start_ts$consistency))),
      sum(!is.na(c(complete_ts$completeness, complete_ts$accuracy, complete_ts$consistency))),
      sum(!is.na(c(case_id$completeness, case_id$accuracy, case_id$consistency))),
      sum(!is.na(c(activity$completeness, activity$accuracy, activity$consistency))),
      sum(!is.na(c(resource$completeness, resource$accuracy, resource$consistency)))
    )
  )
  
  # Simulation parameter scores table
  simulation_param_table <- data.frame(
    Simulation_Parameter = c("Activity Duration", "Arrival Time", 
                             "Branching Probability", "Resource Allocation"),
    Dependent_Attributes = c("Start TS, Complete TS", "Case ID, Start TS",
                             "Activity, Start TS", "Resource"),
    Quality_Score = c(duration_score, arrival_score, branching_score,
                      resource_alloc_score),
    Quality_Percentage = c(round(duration_score * 100, 2),
                           round(arrival_score * 100, 2),
                           round(branching_score * 100, 2),
                           round(resource_alloc_score * 100, 2)),
    Confidence_Factor = confidence_factor,
    Confidence_Adjusted = c(round(duration_score * confidence_factor * 100, 2),
                            round(arrival_score * confidence_factor * 100, 2),
                            round(branching_score * confidence_factor * 100, 2),
                            round(resource_alloc_score * confidence_factor * 100, 2))
  )
  
  # Save results
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir)
  
  write.csv(attribute_scores_table, 
            file.path(results_dir, "attribute_quality_scores.csv"), 
            row.names = FALSE)
  
  write.csv(simulation_param_table, 
            file.path(results_dir, "simulation_parameter_quality.csv"), 
            row.names = FALSE)
  
  save(attribute_scores_table, simulation_param_table,
       file = file.path(results_dir, "simulation_parameter_quality.RData"))
  
  cat("Saved to:\n")
  cat("  - results/attribute_quality_scores.csv\n")
  cat("  - results/simulation_parameter_quality.csv\n")
  cat("  - results/simulation_parameter_quality.RData\n")
  
  # Return results
  return(list(
    # Attribute scores
    attribute_scores = list(
      start_ts = start_ts_score,
      complete_ts = complete_ts_score,
      case_id = case_id_score,
      activity = activity_score,
      resource = resource_score
    ),
    
    # Simulation parameter scores
    simulation_parameter_scores = list(
      activity_duration = duration_score,
      arrival_time = arrival_score,
      branching_probability = branching_score,
      resource_allocation = resource_alloc_score
    ),
    
    # Overall scores
    overall_average = overall_score,
    overall_minimum = overall_score_min,
    overall_confidence_adjusted = overall_score_conf,
    confidence_factor = confidence_factor,
    
    # Tables
    attribute_table = attribute_scores_table,
    simulation_table = simulation_param_table,
    
    # Metadata
    aggregation_method = aggregation_method,
    dimension_weights = dimension_weights
  ))
}


#' Compare different aggregation methods
#' 
#' Shows how different methods affect the final scores
#' 
#' @param attribute_scores A list containing quality scores for each attribute
#' @return Comparison table

compare_aggregation_methods <- function(attribute_scores) {
  
  methods <- c("min", "mean", "product")
  
  results <- data.frame(
    Parameter = c("Activity Duration", "Arrival Time", "Branching Probability",
                  "Resource Allocation", "Overall")
  )
  
  for (method in methods) {
    # Suppress output during comparison
    invisible(capture.output(
      scores <- calculate_simulation_parameter_quality(attribute_scores, method)
    ))
    
    results[[method]] <- c(
      round(scores$simulation_parameter_scores$activity_duration * 100, 2),
      round(scores$simulation_parameter_scores$arrival_time * 100, 2),
      round(scores$simulation_parameter_scores$branching_probability * 100, 2),
      round(scores$simulation_parameter_scores$resource_allocation * 100, 2),
      round(scores$overall_average * 100, 2)
    )
  }
  
  cat("\n============================================\n")
  cat("AGGREGATION METHOD COMPARISON\n")
  cat("============================================\n\n")
  print(results)
  
  write.csv(results, "results/aggregation_method_comparison.csv", row.names = FALSE)
  
  return(results)
}


#' Calculate Simulation Parameter Quality from Cascading Assessment
#' 
#' Derives simulation parameter reliability scores using cascading quality results.
#' In cascading mode:
#' - Completeness is measured on original data
#' - Consistency is measured on complete data only
#' - Accuracy is measured on complete + consistent data only
#'
#' @param cascading_results Results from cascading_quality_assessment()
#' @param aggregation_method Method to combine scores: "min", "mean", "product", "cascading"
#' @return List with simulation parameter quality scores

calculate_simulation_parameter_quality_cascading <- function(
    cascading_results,
    aggregation_method = "cascading"
) {
  
  cat("============================================\n")
  cat("SIMULATION PARAMETER QUALITY (CASCADING)\n")
  cat("============================================\n\n")
  
  # Extract cascading scores
  completeness <- cascading_results$completeness_score
  consistency <- cascading_results$consistency_score
  accuracy <- cascading_results$accuracy_score
  overall <- cascading_results$overall_quality
  
  cat("CASCADING QUALITY SCORES:\n")
  cat(paste("  Completeness:", round(completeness * 100, 2), "%\n"))
  cat(paste("  Consistency:", round(consistency * 100, 2), "%\n"))
  cat(paste("  Accuracy:", round(accuracy * 100, 2), "%\n"))
  cat(paste("  Overall (cumulative):", round(overall * 100, 2), "%\n\n"))
  
  # ============================================
  # CASCADING SCORE INTERPRETATION
  # ============================================
  # 
  # In cascading mode, the overall quality is the PRODUCT of stage scores:
  # Overall = Completeness × Consistency × Accuracy
  #
  # This represents: "What fraction of original cases survived all checks?"
  #
  # For simulation parameters, we consider:
  # - Which quality dimensions affect each parameter
  # - Cascading nature: later stages only see "good" data from earlier stages
  
  # ============================================
  # SIMULATION PARAMETER DERIVATION
  # ============================================
  
  # Define which quality dimensions affect each simulation parameter
  # Key insight: In cascading mode, each parameter inherits quality from all stages
  
  # Activity Duration: 
  #   - Needs complete timestamps (Completeness)
  #   - Needs consistent timestamps (Consistency: start <= complete)
  #   - Needs accurate timestamps (Accuracy: valid durations)
  #   → All three stages matter
  
  # Arrival Time:
  #   - Needs complete case IDs and timestamps (Completeness)
  #   - Needs consistent ordering (Consistency)
  #   - Needs accurate timestamps (Accuracy)
  #   → All three stages matter
  
  # Branching Probability:
  #   - Needs complete activity labels (Completeness)
  #   - Needs consistent activity sequences (Consistency: order)
  #   - Needs accurate activity labels (Accuracy: valid names)
  #   → All three stages matter
  
  # Resource Allocation:
  #   - Needs complete resource values (Completeness)
  #   - Needs consistent resource naming (Consistency)
  #   - Needs accurate resource assignments (Accuracy)
  #   → All three stages matter
  
  # In cascading mode, since each stage filters data:
  # - Parameter quality = Overall (product of all stages)
  # OR
  # - Parameter quality = Weighted combination based on importance
  
  if (aggregation_method == "cascading") {
    # Pure cascading: all parameters inherit overall quality
    # This is the most conservative interpretation
    
    duration_score <- overall
    arrival_score <- overall
    branching_score <- overall
    resource_score <- overall
    
  } else if (aggregation_method == "weighted") {
    # Equal weights for all dimensions (0.33 each)
    # No empirical basis exists for differential weighting in BPS literature
    # Equal weights provide a neutral baseline that can be refined through future research
    
    w <- 1/3  # Equal weight for each dimension
    
    duration_score <- (completeness * w) + (consistency * w) + (accuracy * w)
    arrival_score <- (completeness * w) + (consistency * w) + (accuracy * w)
    branching_score <- (completeness * w) + (consistency * w) + (accuracy * w)
    resource_score <- (completeness * w) + (consistency * w) + (accuracy * w)
    
  } else {
    # Fallback to min
    min_score <- min(completeness, consistency, accuracy)
    duration_score <- min_score
    arrival_score <- min_score
    branching_score <- min_score
    resource_score <- min_score
  }
  
  cat("SIMULATION PARAMETER QUALITY SCORES:\n")
  cat(paste("Aggregation method:", aggregation_method, "\n\n"))
  
  cat("1. ACTIVITY DURATION:\n")
  cat(paste("   Quality Score:", round(duration_score * 100, 2), "%\n\n"))
  
  cat("2. ARRIVAL TIME:\n")
  cat(paste("   Quality Score:", round(arrival_score * 100, 2), "%\n\n"))
  
  cat("3. BRANCHING PROBABILITY:\n")
  cat(paste("   Quality Score:", round(branching_score * 100, 2), "%\n\n"))
  
  cat("4. RESOURCE ALLOCATION:\n")
  cat(paste("   Quality Score:", round(resource_score * 100, 2), "%\n\n"))
  
  # Overall
  param_scores <- c(duration_score, arrival_score, branching_score, 
                    resource_score)
  
  overall_param_score <- mean(param_scores)
  
  cat("============================================\n")
  cat(paste("OVERALL SIMULATION QUALITY:", round(overall_param_score * 100, 2), "%\n"))
  cat("============================================\n\n")
  
  # Create output table
  simulation_param_table <- data.frame(
    Simulation_Parameter = c("Activity Duration", "Arrival Time", 
                             "Branching Probability", "Resource Allocation"),
    Quality_Score = c(duration_score, arrival_score, branching_score,
                      resource_score),
    Quality_Percentage = paste0(round(c(duration_score, arrival_score, branching_score,
                                        resource_score) * 100, 2), "%"),
    Interpretation = c(
      ifelse(duration_score >= 0.9, "High reliability", 
             ifelse(duration_score >= 0.7, "Moderate reliability", "Low reliability")),
      ifelse(arrival_score >= 0.9, "High reliability", 
             ifelse(arrival_score >= 0.7, "Moderate reliability", "Low reliability")),
      ifelse(branching_score >= 0.9, "High reliability", 
             ifelse(branching_score >= 0.7, "Moderate reliability", "Low reliability")),
      ifelse(resource_score >= 0.9, "High reliability", 
             ifelse(resource_score >= 0.7, "Moderate reliability", "Low reliability"))
    )
  )
  
  cascading_summary <- data.frame(
    Stage = c("Completeness", "Consistency", "Accuracy", "Overall"),
    Score = c(completeness, consistency, accuracy, overall),
    Percentage = paste0(round(c(completeness, consistency, accuracy, overall) * 100, 2), "%"),
    Cases_Remaining = c(
      cascading_results$tracking$Cases_Out[1],
      cascading_results$tracking$Cases_Out[2],
      cascading_results$tracking$Cases_Out[3],
      cascading_results$final_cases
    )
  )
  
  # Save results
  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir)
  
  write.csv(simulation_param_table, 
            file.path(results_dir, "simulation_parameter_quality_cascading.csv"), 
            row.names = FALSE)
  
  write.csv(cascading_summary,
            file.path(results_dir, "cascading_quality_summary.csv"),
            row.names = FALSE)
  
  cat("Saved to:\n")
  cat("  - results/simulation_parameter_quality_cascading.csv\n")
  cat("  - results/cascading_quality_summary.csv\n")
  
  return(list(
    # Cascading stage scores
    completeness = completeness,
    consistency = consistency,
    accuracy = accuracy,
    overall_cascading = overall,
    
    # Simulation parameter scores
    simulation_parameter_scores = list(
      activity_duration = duration_score,
      arrival_time = arrival_score,
      branching_probability = branching_score,
      resource_allocation = resource_score
    ),
    
    # Overall simulation quality
    overall_simulation_quality = overall_param_score,
    
    # Tables
    simulation_table = simulation_param_table,
    cascading_summary = cascading_summary,
    
    # Metadata
    aggregation_method = aggregation_method
  ))
}
