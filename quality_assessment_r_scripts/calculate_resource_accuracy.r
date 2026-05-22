# Resource accuracy:
# 1. Two resources doing same activity at same time in same case (conflict)
# 2. Resource-activity role violation: resource performs an activity that is
#    normally assigned to a different resource (wrong assignment noise)

library(dplyr)
library(bupaR)
library(lubridate)

calculate_resource_accuracy <- function(dataset, data_file, allowed_resources = NULL, resource_activity_map = NULL) {
  
  results_dir <- "results_resource"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("RESOURCE ACCURACY ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog <- dataset
  actlog_df <- as.data.frame(actlog)
  
  total_records <- nrow(actlog_df)
  
  # Work only with non-missing resources and valid timestamps
  actlog_valid <- actlog_df %>%
    filter(!is.na(originator) & trimws(originator) != "" &
           !is.na(start) & !is.na(complete))
  
  valid_records <- nrow(actlog_valid)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Valid records (resource + timestamps):", valid_records, "\n"))
  cat(paste("Unique resources:", n_distinct(actlog_valid$originator), "\n\n"))
  
  # Ensure timestamps are POSIXct
  if(!inherits(actlog_valid$start, "POSIXct")) {
    actlog_valid$start <- as.POSIXct(actlog_valid$start)
    actlog_valid$complete <- as.POSIXct(actlog_valid$complete)
  }
  
  # ============================================
  # 1. SAME ACTIVITY, SAME TIME, DIFFERENT RESOURCES (in same case)
  # ============================================
  cat("============================================\n")
  cat("1. SAME ACTIVITY + SAME TIME + DIFFERENT RESOURCES\n")
  cat("============================================\n")
  cat("Two resources doing the same activity at the same time in the same case\n\n")
  
  conflict_type1 <- data.frame()
  conflict_type1_count <- 0
  
  # Group by case_id + activity, find overlapping time windows with different resources
  case_activity_groups <- actlog_valid %>%
    group_by(case_id, activity) %>%
    filter(n_distinct(originator) > 1) %>%
    ungroup()
  
  if(nrow(case_activity_groups) > 0) {
    # For each case-activity group, check time overlaps between different resources
    conflict_rows <- list()
    
    groups <- case_activity_groups %>%
      group_by(case_id, activity) %>%
      group_split()
    
    for(grp in groups) {
      if(nrow(grp) < 2) next
      
      for(i in 1:(nrow(grp) - 1)) {
        for(j in (i + 1):nrow(grp)) {
          if(grp$originator[i] != grp$originator[j]) {
            # Check time overlap: A starts before B ends AND B starts before A ends
            if(grp$start[i] < grp$complete[j] && grp$start[j] < grp$complete[i]) {
              conflict_rows <- c(conflict_rows, list(data.frame(
                case_id = grp$case_id[i],
                activity = grp$activity[i],
                resource_1 = grp$originator[i],
                start_1 = grp$start[i],
                complete_1 = grp$complete[i],
                resource_2 = grp$originator[j],
                start_2 = grp$start[j],
                complete_2 = grp$complete[j],
                conflict_type = "same_activity_diff_resources"
              )))
            }
          }
        }
      }
    }
    
    if(length(conflict_rows) > 0) {
      conflict_type1 <- do.call(rbind, conflict_rows)
      conflict_type1_count <- nrow(conflict_type1)
    }
  }
  
  cat(paste("Conflicts found:", conflict_type1_count, "\n"))
  
  if(conflict_type1_count > 0) {
    cat("Sample conflicts (first 10):\n")
    print(head(conflict_type1, 10))
    
    conflict1_file <- file.path(results_dir, "resource_conflict_same_activity.csv")
    write.csv(conflict_type1, conflict1_file, row.names = FALSE)
    cat(paste("\n  Saved to:", conflict1_file, "\n"))
  }
  
  # ============================================
  # 2. RESOURCE-ACTIVITY ROLE VIOLATION DETECTION
  # ============================================
  cat("\n============================================\n")
  cat("2. RESOURCE-ACTIVITY ROLE VIOLATION DETECTION\n")
  cat("============================================\n\n")
  cat("Detecting resources performing activities outside their expected role\n")
  cat("(expected role = resource doing >= 70% of events for that activity)\n\n")

  # For each activity, find the dominant resource(s):
  # those whose share of events for that activity is >= dominance_threshold.
  # Any event assigned to a non-dominant resource is a role violation.
  #
  # IMPORTANT: normalize names before dominance check so that spelling variants
  # (e.g. "Doctor1", "doctor 1", "Dr. 1") of the same person are not mistaken
  # for a different person/role.  We compare on a canonical key = tolower + no spaces.
  dominance_threshold <- 0.70

  actlog_for_dominance <- actlog_valid %>%
    mutate(resource_key = tolower(gsub("[[:space:][:punct:]]", "", originator)),
           activity_key = tolower(trimws(activity)))

  activity_resource_counts <- actlog_for_dominance %>%
    group_by(activity, resource_key) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(activity) %>%
    mutate(
      total_for_activity = sum(n),
      share = n / total_for_activity
    ) %>%
    ungroup()

  # Expected resource keys per activity (dominant ones)
  expected_resource_keys <- activity_resource_counts %>%
    filter(share >= dominance_threshold) %>%
    select(activity, resource_key) %>%
    rename(expected_key = resource_key)

  cat("Expected resource assignments (dominant >= 70%, normalized key):\n")
  print(as.data.frame(expected_resource_keys))
  cat("\n")

  # Flag events where the normalized resource key is NOT among expected keys.
  # Only flag activities that have at least one clearly dominant resource key.
  activities_with_expected <- unique(expected_resource_keys$activity)

  role_violations <- actlog_for_dominance %>%
    filter(activity %in% activities_with_expected) %>%
    left_join(expected_resource_keys, by = "activity", relationship = "many-to-many") %>%
    group_by(case_id, activity, originator, start, complete) %>%
    summarise(
      is_expected = any(resource_key == expected_key),
      .groups = "drop"
    ) %>%
    filter(!is_expected)

  role_violation_count <- nrow(role_violations)

  cat(paste("Role violations found:", role_violation_count, "\n"))

  if(role_violation_count > 0) {
    # Per-activity breakdown
    breakdown <- role_violations %>%
      group_by(activity, originator) %>%
      summarise(violations = n(), .groups = "drop")
    cat("\nBreakdown by activity and resource:\n")
    print(as.data.frame(breakdown))

    violation_file <- file.path(results_dir, "resource_role_violations.csv")
    write.csv(role_violations, violation_file, row.names = FALSE)
    cat(paste("\n  Saved to:", violation_file, "\n"))
  }

  # ============================================
  # 3. ACCURACY CALCULATION
  # ============================================
  cat("\n============================================\n")
  cat("3. RESOURCE ACCURACY CALCULATION\n")
  cat("============================================\n\n")

  # Collect all problematic record indices (unique, avoid double-counting)
  problematic_indices <- c()

  # Type 1 conflicts: find the original rows involved
  if(conflict_type1_count > 0) {
    for(i in 1:nrow(conflict_type1)) {
      idx <- which(actlog_valid$case_id == conflict_type1$case_id[i] &
                   actlog_valid$activity == conflict_type1$activity[i] &
                   (actlog_valid$originator == conflict_type1$resource_1[i] |
                    actlog_valid$originator == conflict_type1$resource_2[i]))
      problematic_indices <- c(problematic_indices, idx)
    }
  }

  # Type 2: role violations — match by case_id + activity + originator + start
  if(role_violation_count > 0) {
    violation_keys <- paste(role_violations$case_id, role_violations$activity,
                            role_violations$originator,
                            format(role_violations$start), sep = "_")
    valid_keys     <- paste(actlog_valid$case_id, actlog_valid$activity,
                            actlog_valid$originator,
                            format(actlog_valid$start), sep = "_")
    role_idx <- which(valid_keys %in% violation_keys)
    problematic_indices <- c(problematic_indices, role_idx)
  }

  # Unique problematic records
  problematic_indices <- unique(problematic_indices)
  total_outlier_records <- length(problematic_indices)

  accurate_records <- total_records - total_outlier_records
  accuracy <- accurate_records / total_records
  accuracy_percentage <- accuracy * 100

  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Time conflict (same activity, diff resources):", conflict_type1_count, "conflicts\n"))
  cat(paste("Role violations (wrong resource for activity):", role_violation_count, "\n"))
  cat(paste("Total outlier records (unique):", total_outlier_records, "\n"))
  cat(paste("Accurate records:", accurate_records, "\n"))
  cat(paste("Resource Accuracy:", round(accuracy_percentage, 2), "%\n"))

  accuracy_summary <- data.frame(
    metric = c("total_records", "conflict_type1_count",
               "role_violation_count",
               "total_outlier_records", "accuracy_percent"),
    value = c(total_records, conflict_type1_count,
              role_violation_count,
              total_outlier_records, round(accuracy_percentage, 2))
  )

  summary_file <- file.path(results_dir, "resource_accuracy_summary.csv")
  write.csv(accuracy_summary, summary_file, row.names = FALSE)
  cat(paste("  Saved to:", summary_file, "\n"))

  return(list(
    total_records = total_records,
    conflict_type1_count = conflict_type1_count,
    role_violation_count = role_violation_count,
    total_outlier_records = total_outlier_records,
    accuracy = accuracy,
    accuracy_percentage = accuracy_percentage,
    conflict_type1 = conflict_type1,
    role_violations = role_violations
  ))
}
