# Data quality analysis orchestrator
# 
# Supports two modes:
# 1. PARALLEL: All quality checks run independently on same data
# 2. CASCADING: Sequential filtering (Completeness → Consistency → Accuracy)
#
# Set USE_CASCADING_MODE = TRUE to switch to cascading mode

if(!exists("USE_CASCADING_MODE")) USE_CASCADING_MODE <- FALSE  # Set to TRUE for cascading assessment
if(!exists("USE_AUTO_CONFIG")) USE_AUTO_CONFIG <- TRUE         # Set to TRUE to auto-derive parameters from the event log

library(daqapo)
library(bupaR)
library(xesreadR)
library(stringr)
library(dplyr)

script_dir <- if(exists("trustvalues_dir")) {
  normalizePath(trustvalues_dir, winslash = "/", mustWork = TRUE)
} else {
  tryCatch({
    dirname(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))
  }, error = function(e) {
    getwd()
  })
}

source_local <- function(filename) {
  source(file.path(script_dir, filename), chdir = FALSE)
}

source_local("auto_configure.r")
source_local("calculate_timestamp_completeness.r")
source_local("calculate_timestamp_accuracy.r")
source_local("calculate_timestamp_consistency.r")
source_local("calculate_caseid_completeness.r")
source_local("calculate_caseid_accuracy.r")
source_local("calculate_activity_completeness.r")
source_local("calculate_activity_accuracy.r")
source_local("calculate_activity_consistency.r")
source_local("calculate_resource_completeness.r")
source_local("calculate_resource_accuracy.r")
source_local("calculate_resource_consistency.r")
source_local("calculate_simulation_parameter_quality.r")
source_local("cascading_quality_assessment.r")
source_local("calculate_granular_simulation_quality.r")
source_local("calculate_per_activity_simulation_quality.r")
source_local("calculate_gateway_branching.r")
source_local("calculate_duration_ts_diagnosis.r")


# data_file <- "datasets/hospital_view.csv"
# case_id_col <- "patient_visit_nr"  # Will be renamed to 'case_id' internally
# activity_col <- "activity"
# resource_col <- "originator"
# start_timestamp_col <- "start"
# complete_timestamp_col <- "complete"
# allowed_activities <- c(
#   "Registration",
#   "Triage",
#   "Clinical exam",
#   "Treatment",
#   "Treatment evaluation"
# )

# ---- OPTION 2: Pub/Restaurant Dataset (CSV) ----

# data_file <- "datasets/pub_first_half.csv"
# case_id_col <- "Case.ID"
# activity_col <- "Activity"
# resource_col <- "Resource"
# start_timestamp_col <- "Start.Timestamp"
# complete_timestamp_col <- "Complete.Timestamp"

# ---- OPTION 3: XES Dataset ----

if(!exists("data_file")) {
  stop(
    "\n\n",
    "================================================================\n",
    "ERROR: 'data_file' is not defined.\n",
    "\n",
    "Do not run main.r directly. Instead, launch the tool via:\n",
    "    python main.py          (quality assessment GUI)\n",
    "\n",
    "The GUI writes a _run_wrapper.r that sets data_file and all\n",
    "required column-name variables before sourcing this script.\n",
    "================================================================\n"
  )
}

if(grepl("\\.csv$", data_file, ignore.case = TRUE)) {
  actlog <- read.csv(data_file, stringsAsFactors = FALSE, na.strings = c("NA", ""), check.names = FALSE)
  cat("Loaded CSV file\n")
} else if(grepl("\\.xes$", data_file, ignore.case = TRUE)) {
  # XES is the standard IEEE process mining format
  actlog <- xesreadR::read_xes(data_file)
  cat(paste("Loaded XES file:", nrow(actlog), "events\n"))
  cat(paste("Class:", paste(class(actlog), collapse = ", "), "\n"))
  
  # XES files are loaded as eventlog; convert to activitylog for our pipeline
  if(inherits(actlog, "eventlog")) {
    # Fix missing activity_instance_id by pairing start/complete lifecycle events
    # within the same case and activity, ordered by timestamp
    elog_df <- as.data.frame(actlog)
    lc_col <- as.character(bupaR::lifecycle_id(actlog))
    ts_col <- as.character(bupaR::timestamp(actlog))
    ci_col <- as.character(bupaR::case_id(actlog))
    ai_col <- as.character(bupaR::activity_id(actlog))
    aii_col <- as.character(bupaR::activity_instance_id(actlog))
    
    # Check if activity_instance_id is just row numbers (auto-generated)
    if(all(elog_df[[aii_col]] == seq_len(nrow(elog_df)))) {
      cat("Fixing auto-generated activity_instance_id by pairing lifecycle events...\n")
      elog_df <- elog_df %>%
        arrange(!!sym(ci_col), !!sym(ai_col), !!sym(ts_col)) %>%
        group_by(!!sym(ci_col), !!sym(ai_col)) %>%
        mutate(pair_id = paste0(!!sym(ci_col), "_", !!sym(ai_col), "_", 
                                ceiling(row_number() / 2))) %>%
        ungroup()
      elog_df[[aii_col]] <- elog_df$pair_id
      elog_df$pair_id <- NULL
      
      # Reconstruct eventlog with corrected activity_instance_id
      actlog <- bupaR::eventlog(
        elog_df,
        case_id = ci_col,
        activity_id = ai_col,
        activity_instance_id = aii_col,
        lifecycle_id = lc_col,
        timestamp = ts_col,
        resource_id = as.character(bupaR::resource_id(actlog))
      )
    }
    
    actlog <- bupaR::to_activitylog(actlog)
    cat("Converted eventlog to activitylog\n")
  }
  
  # Ensure standard column names
  col_names <- colnames(actlog)
  cat(paste("Columns:", paste(col_names, collapse = ", "), "\n"))
  
  # Map bupaR standard names to our expected names
  id_col <- as.character(bupaR::case_id(actlog))
  act_col <- as.character(bupaR::activity_id(actlog))
  res_col <- as.character(bupaR::resource_id(actlog))
  
  # Rename columns using base R (bupaR's rename method doesn't support !!sym())
  cnames <- colnames(actlog)
  if(id_col != "case_id" && id_col %in% cnames) {
    colnames(actlog)[colnames(actlog) == id_col] <- "case_id"
  }
  if(act_col != "activity" && act_col %in% cnames) {
    colnames(actlog)[colnames(actlog) == act_col] <- "activity"
  }
  if(res_col != "originator" && res_col %in% cnames) {
    colnames(actlog)[colnames(actlog) == res_col] <- "originator"
  }
  
  # Update column name vars to standard names
  case_id_col <- "case_id"
  activity_col <- "activity"
  resource_col <- "originator"
  
  # Convert to plain data frame so the downstream re-creation logic handles activitylog properly
  actlog <- as.data.frame(actlog)
  
  cat(paste("Event log: ", nrow(actlog), "activities,", 
            n_distinct(actlog$case_id), "cases\n"))
  
} else if(grepl("\\.RData$", data_file, ignore.case = TRUE)) {
  loaded_vars <- load(data_file)
  
  if (length(loaded_vars) > 0) {
    data_name <- loaded_vars[1]
    actlog <- get(data_name)
    cat(paste("Loaded RData file. Object name:", data_name, "\n"))
  } else {
    stop("ERROR: No objects found in RData file.")
  }
} else {
  stop("ERROR: Unsupported file format. Please use .csv, .xes, or .RData files.")
}

if(!inherits(actlog, "activitylog")) {
  formats_to_try <- c(
    "%Y-%m-%dT%H:%M:%S%z",  # ISO 8601 with T and timezone offset: 2011-01-01T00:00:00+01:00
    "%Y-%m-%dT%H:%M:%OS%z", # ISO 8601 with T, milliseconds and timezone offset
    "%Y-%m-%dT%H:%M:%S",   # ISO 8601 with T separator (no timezone): 2011-01-01T00:00:00
    "%Y-%m-%dT%H:%M:%OS",  # ISO 8601 with T and milliseconds: 2011-01-01T00:00:00.000
    "%Y-%m-%d %H:%M:%OS",  # ISO with milliseconds: 2024-08-08 19:30:00.000
    "%Y-%m-%d %H:%M:%S",   # ISO format: 2017-11-20 10:18:17
    "%Y-%m-%d %H:%M",      # ISO no seconds: 2017-11-20 10:18
    "%Y-%m-%d",            # Date only: 2024-07-24 (will be 00:00:00)
    "%d/%m/%Y %H:%M:%S",   # Original: 20/11/2017 10:18:17
    "%d.%m.%Y %H:%M:%S",   # Dot format: 20.11.2017 10:18:17
    "%d.%b.%Y %H:%M:%S",   # Month name: 20.Nov.2017 10:18:17
    "%d.%m.%Y %H:%M",      # No seconds: 20.11.2017 10:18
    "%d/%m/%Y %H:%M",      # No seconds slash: 20/11/2017 10:18
    "%d/%m/%Y",            # Date only slash: 24/07/2024
    "%d.%m.%Y"             # Date only dot: 24.07.2024
  )
  
  if(exists("start_timestamp_col") && exists("complete_timestamp_col") &&
     start_timestamp_col %in% colnames(actlog) &&
     complete_timestamp_col %in% colnames(actlog)) {
    start_col <- start_timestamp_col
    complete_col <- complete_timestamp_col
  } else if("start_ts" %in% colnames(actlog)) {
    start_col <- "start_ts"
    complete_col <- "complete_ts"
  } else if("Start.Timestamp" %in% colnames(actlog)) {
    start_col <- "Start.Timestamp"
    complete_col <- "Complete.Timestamp"
  } else {
    start_col <- "start"
    complete_col <- "complete"
  }
  
  if(!inherits(actlog[[start_col]], "POSIXct")) {
    # Vectorized parsing - tries multiple formats, tz="UTC" prevents timezone conversion
    parse_timestamps_vectorized <- function(timestamps, formats) {
      result <- rep(as.POSIXct(NA, tz = "UTC"), length(timestamps))

      parse_elapsed_time <- function(values) {
        parsed <- rep(as.POSIXct(NA, tz = "UTC"), length(values))
        text_values <- trimws(as.character(values))

        for(i in seq_along(text_values)) {
          value <- text_values[[i]]
          if(is.na(value) || value == "") next

          parts <- strsplit(value, ":", fixed = TRUE)[[1]]
          total_seconds <- NA_real_

          if(length(parts) == 2) {
            minutes <- suppressWarnings(as.numeric(parts[[1]]))
            seconds <- suppressWarnings(as.numeric(parts[[2]]))
            if(!is.na(minutes) && !is.na(seconds)) {
              total_seconds <- minutes * 60 + seconds
            }
          } else if(length(parts) == 3) {
            hours <- suppressWarnings(as.numeric(parts[[1]]))
            minutes <- suppressWarnings(as.numeric(parts[[2]]))
            seconds <- suppressWarnings(as.numeric(parts[[3]]))
            if(!is.na(hours) && !is.na(minutes) && !is.na(seconds)) {
              total_seconds <- hours * 3600 + minutes * 60 + seconds
            }
          }

          if(!is.na(total_seconds)) {
            parsed[[i]] <- as.POSIXct(total_seconds, origin = "1970-01-01", tz = "UTC")
          }
        }

        parsed
      }
      
      for(fmt in formats) {
        na_mask <- is.na(result)
        if(!any(na_mask)) break
        
        parsed <- as.POSIXct(timestamps[na_mask], format = fmt, tz = "UTC")
        result[na_mask] <- parsed
      }

      # Lubridate fallback: handles ISO 8601 with timezone offset on Windows
      # e.g. "2011-01-01T00:00:00+01:00" which strptime %z may miss on Windows
      na_mask <- is.na(result)
      if(any(na_mask)) {
        iso8601_mask <- na_mask & grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", timestamps)
        if(any(iso8601_mask)) {
          suppressWarnings({
            ldt <- lubridate::parse_date_time(
              timestamps[iso8601_mask],
              orders = c("YmdHMSz", "YmdHMSOSz", "YmdHMS", "YmdHMSOS"),
              tz = "UTC"
            )
          })
          result[iso8601_mask] <- ldt
        }
      }

      na_mask <- is.na(result)
      if(any(na_mask)) {
        result[na_mask] <- parse_elapsed_time(timestamps[na_mask])
      }
      
      return(result)
    }
    
# Preserve raw strings BEFORE parsing so the timestamp-consistency checker
    # can compare original format strings (yyyy-mm-dd vs dd/mm/yyyy etc.).
    # calculate_timestamp_consistency.r uses start_original / complete_original
    # when those columns exist.
    actlog$start_original    <- as.character(actlog[[start_col]])
    actlog$complete_original <- as.character(actlog[[complete_col]])

    cat("Parsing start timestamps...\n")
    actlog[[start_col]] <- parse_timestamps_vectorized(actlog[[start_col]], formats_to_try)
    
    cat("Parsing complete timestamps...\n")
    actlog[[complete_col]] <- parse_timestamps_vectorized(actlog[[complete_col]], formats_to_try)
    
    failed_start <- sum(is.na(actlog[[start_col]]))
    failed_complete <- sum(is.na(actlog[[complete_col]]))
    
    if(failed_start > 0 || failed_complete > 0) {
      cat(paste("Warning: Failed to parse", failed_start, "start and", 
                failed_complete, "complete timestamps\n"))
    } else {
      cat("All timestamps parsed successfully\n")
    }
  } else {
    cat("Timestamps already in POSIXct format\n")
  }
  
  if(start_col != "start") {
    actlog <- actlog %>%
      rename(start = !!sym(start_col), complete = !!sym(complete_col))
  }
  
  if(case_id_col != "case_id" && case_id_col %in% colnames(actlog)) {
    actlog <- actlog %>%
      rename(case_id = !!sym(case_id_col))
    case_id_col <- "case_id"
  }
  if(activity_col != "activity" && activity_col %in% colnames(actlog)) {
    actlog <- actlog %>%
      rename(activity = !!sym(activity_col))
    activity_col <- "activity"
  }
  if(resource_col != "originator" && resource_col %in% colnames(actlog)) {
    actlog <- actlog %>%
      rename(originator = !!sym(resource_col))
    resource_col <- "originator"
  }
  
  actlog <- actlog %>%
    bupaR::activitylog(
      case_id = case_id_col,
      activity_id = activity_col,
      timestamps = c("start", "complete"),
      resource_id = resource_col
    )
}

cat(paste("Loaded:", nrow(actlog), "records\n\n"))


# ============================================
# AUTO-CONFIGURATION (if enabled)
# ============================================

if(USE_AUTO_CONFIG) {
  auto_config <- auto_configure(as.data.frame(actlog))
  
  allowed_activities    <- auto_config$allowed_activities
  mandatory_activities  <- auto_config$mandatory_activities
  conditional_rules     <- auto_config$conditional_rules
  rule_activities       <- auto_config$rule_activities
  working_hours_start   <- auto_config$working_hours_start
  working_hours_end     <- auto_config$working_hours_end
  inactive_threshold_minutes <- auto_config$inactive_threshold_minutes
  max_edit_distance     <- auto_config$max_edit_distance
  allowed_resources     <- auto_config$allowed_resources
  resource_activity_map <- auto_config$resource_activity_map
}

# ============================================
# CHOOSE ASSESSMENT MODE
# ============================================

if (USE_CASCADING_MODE) {
  
  cat("============================================\n")
  cat("CASCADING MODE ENABLED\n")
  cat("Pipeline: Completeness → Consistency → Accuracy\n")
  cat("============================================\n\n")
  
  # Configure cascading assessment
  cascading_config <- list(
    working_hours_start = 0,
    working_hours_end = 24,
    min_duration = 0,
    max_duration = 120,
    expected_order = c("start", "Take order", "Deliver to customer", "EVENT 8 END"),
    allowed_activities = allowed_activities
  )
  
  # Run cascading assessment
  cascading_results <- cascading_quality_assessment(actlog, cascading_config)
  
  # Derive simulation parameter quality from cascading results
  cat("\n")
  simulation_quality_cascading <- calculate_simulation_parameter_quality_cascading(
    cascading_results,
    aggregation_method = "cascading"
  )
  
  # Also show weighted version for comparison
  cat("\n--- Weighted Aggregation Comparison ---\n")
  simulation_quality_weighted <- calculate_simulation_parameter_quality_cascading(
    cascading_results,
    aggregation_method = "weighted"
  )
  
  # ============================================
  # GRANULAR QUALITY ANALYSIS (Cascading Mode)
  # ============================================
  
  cat("\n")
  granular_quality <- calculate_granular_simulation_quality(
    log = cascading_results$clean_log,  # Use the clean log from cascading
    activity_col = "Activity",
    resource_col = "Resource", 
    start_col = "Start",
    complete_col = "Complete",
    min_duration = cascading_config$min_duration,
    max_duration = cascading_config$max_duration
  )
  
  # Identify quality issues
  quality_issues <- identify_quality_issues(granular_quality, threshold = 0.7)
  
  cat("\n============================================\n")
  cat("CASCADING ASSESSMENT COMPLETE\n")
  cat("============================================\n")
  cat(paste("Original cases:", cascading_results$original_cases, "\n"))
  cat(paste("Clean cases:", cascading_results$final_cases, "\n"))
  cat(paste("Overall data quality:", round(cascading_results$overall_quality * 100, 2), "%\n"))
  cat(paste("Simulation model reliability:", round(simulation_quality_cascading$overall_simulation_quality * 100, 2), "%\n"))
  
} else {
  
  # ============================================
  # PARALLEL MODE (Original implementation)
  # ============================================

cat("============================================\n")
cat("2. COMPLETENESS ANALYSIS\n")
cat("============================================\n\n")

completeness_results <- calculate_completeness(actlog, data_file, inactive_threshold_minutes)


cat("============================================\n")
cat("3. FILTERING NA VALUES\n")
cat("============================================\n\n")

records_before <- nrow(actlog)
actlog_clean <- actlog[!is.na(actlog$start) & !is.na(actlog$complete), ]
records_after <- nrow(actlog_clean)
removed_records <- records_before - records_after

if (removed_records > 0) {
  cat(paste("Removed", removed_records, "NA records\n"))
  na_records <- actlog[is.na(actlog$start) | is.na(actlog$complete), ]
  save(na_records, file = "results/data_quality_na_records.RData")
  write.csv(na_records, "results/data_quality_na_records.csv", row.names = FALSE)
}

if (records_after == 0) {
  stop("ERROR: No records left after filtering NA values!")
}

cat(paste("Continuing with", records_after, "records\n\n"))

# ============================================
# 4. ACTIVITY NAME VALIDATION
# ============================================

cat("============================================\n")
cat("4. ACTIVITY NAME VALIDATION\n")
cat("============================================\n\n")

incorrect_activities_result <- detect_incorrect_activity_names(
  activitylog = actlog_clean,
  allowed_activities = allowed_activities
)

incorrect_df <- as.data.frame(incorrect_activities_result)
incorrect_count <- nrow(incorrect_df)

if(incorrect_count > 0) {
  incorrect_names <- unique(incorrect_df$activity)
  cat(paste("Removed", incorrect_count, "incorrect activities:", paste(incorrect_names, collapse = ", "), "\n"))
  
  save(incorrect_activities_result, file = "results/data_quality_incorrect_activities.RData")
  write.csv(incorrect_df, "results/data_quality_incorrect_activities.csv", row.names = FALSE)
  
  actlog_df <- as.data.frame(actlog_clean)
  actlog_df_clean <- actlog_df %>%
    filter(activity %in% allowed_activities)
  
  actlog_clean <- actlog_df_clean %>%
    bupaR::activitylog(
      case_id = case_id_col,
      activity_id = activity_col,
      timestamps = c("start", "complete"),
      resource_id = resource_col
    )
  
  cat(paste("Continuing with", nrow(actlog_clean), "records\n\n"))
} else {
  cat("All activity names are valid\n\n")
}

cat("============================================\n")
cat("5. ACCURACY ANALYSIS\n")
cat("============================================\n\n")

accuracy_results <- calculate_accuracy(actlog_clean, data_file, working_hours_start, working_hours_end)

actlog_final <- accuracy_results$actlog_final

consistency_results <- calculate_consistency(actlog_final)

cat("\n============================================\n")
cat("7. CASE ID COMPLETENESS ANALYSIS\n")
cat("============================================\n\n")

caseid_completeness_results <- calculate_caseid_completeness(actlog, data_file)

cat("\n============================================\n")
cat("8. CASE ID ACCURACY ANALYSIS\n")
cat("============================================\n\n")

caseid_accuracy_results <- calculate_caseid_accuracy(actlog, data_file, rule_activities)

cat("\n============================================\n")
cat("9. ACTIVITY LABEL COMPLETENESS ANALYSIS\n")
cat("============================================\n\n")

activity_completeness_results <- calculate_activity_completeness(actlog, data_file, allowed_activities, mandatory_activities, conditional_rules)

cat("\n============================================\n")
cat("10. ACTIVITY LABEL ACCURACY ANALYSIS\n")
cat("============================================\n\n")

activity_accuracy_results <- calculate_activity_accuracy(actlog, data_file, allowed_activities)

cat("\n============================================\n")
cat("11. ACTIVITY LABEL CONSISTENCY ANALYSIS\n")
cat("============================================\n\n")

activity_consistency_results <- calculate_activity_consistency(
  actlog, 
  data_file,
  allowed_activities,
  max_edit_distance
)

cat("\n============================================\n")
cat("12. RESOURCE COMPLETENESS ANALYSIS\n")
cat("============================================\n\n")

resource_completeness_results <- calculate_resource_completeness(actlog, data_file)

cat("\n============================================\n")
cat("13. RESOURCE ACCURACY ANALYSIS\n")
cat("============================================\n\n")

# Optional: define allowed resources and role-activity mapping
# allowed_resources <- c("Waiter 1", "Waiter 2", "Chef 1", "Chef 2")
# resource_activity_map <- list(
#   "Waiter 1" = c("Take order", "Bring drinks", "Bring food", "Deliver to customer"),
#   "Chef 1" = c("Prepare starter", "Prepare main course")
# )
allowed_resources <- NULL
resource_activity_map <- NULL

resource_accuracy_results <- calculate_resource_accuracy(
  actlog, data_file,
  allowed_resources = allowed_resources,
  resource_activity_map = resource_activity_map
)

cat("\n============================================\n")
cat("14. RESOURCE CONSISTENCY ANALYSIS\n")
cat("============================================\n\n")

resource_consistency_results <- calculate_resource_consistency(actlog, data_file)

cat("\n============================================\n")
cat("15. DURATION & TIMESTAMP DIAGNOSIS\n")
cat("(Distribution-based, no clean reference)\n")
cat("============================================\n\n")

duration_diagnosis_results <- calculate_duration_ts_diagnosis(actlog)

# Final summary table
cat("\n============================================\n")
cat("DATA QUALITY SUMMARY\n")
cat("============================================\n\n")

summary_table <- data.frame(
  Attribute = c("Start Timestamp", "Complete Timestamp", "Case ID", "Activity Label", "Resource"),
  Completeness = c(
    paste0(round(completeness_results$start_completeness * 100, 2), "%"),
    paste0(round(completeness_results$complete_completeness * 100, 2), "%"),
    paste0(round(caseid_completeness_results$completeness_percentage, 2), "%"),
    paste0(activity_completeness_results$activity_completeness, "%"),
    paste0(round(resource_completeness_results$completeness_percentage, 2), "%")
  ),
  Accuracy = c(
    paste0(round(min(accuracy_results$start_accuracy,   duration_diagnosis_results$start_accuracy) * 100, 2), "%"),
    paste0(round(min(accuracy_results$complete_accuracy, duration_diagnosis_results$end_accuracy)   * 100, 2), "%"),
    paste0(round(caseid_accuracy_results$accuracy_percentage, 2), "%"),
    paste0(round(activity_accuracy_results$accuracy, 2), "%"),
    paste0(round(resource_accuracy_results$accuracy_percentage, 2), "%")
  ),
  Consistency = c(
    paste0(round(consistency_results$start_consistency * 100, 2), "%"),
    paste0(round(consistency_results$complete_consistency * 100, 2), "%"),
    "-",
    paste0(round(activity_consistency_results$consistency, 2), "%"),
    paste0(round(resource_consistency_results$consistency_percentage, 2), "%")
  )
)

print(summary_table)

# Save summary
write.csv(summary_table, "results/data_quality_summary.csv", row.names = FALSE)
cat("\nSaved to: results/data_quality_summary.csv\n")

# ============================================
# TIMESTAMP WEIGHTED OUTLIER SCORE
# ============================================

cat("\n============================================\n")
cat("TIMESTAMP WEIGHTED OUTLIER SCORE\n")
cat("============================================\n\n")

# Total records for normalization
total_records <- nrow(actlog)

# Weight = 1/3 (Completeness, Accuracy, Consistency)
weight <- 1/3

# START TIMESTAMP
start_completeness_outliers <- completeness_results$total_start_outliers
# Combine accuracy outliers from both methods, cap at total records to avoid double-counting
start_accuracy_outliers <- min(
  accuracy_results$total_start_outliers + duration_diagnosis_results$outliers %>%
    filter(ts_diagnosis %in% c("Start timestamp corrupted", "Both timestamps corrupted")) %>% nrow(),
  total_records
)
start_consistency_outliers <- consistency_results$total_start_outliers

start_weighted_outliers <- (start_completeness_outliers * weight) + 
                           (start_accuracy_outliers * weight) + 
                           (start_consistency_outliers * weight)

# Normalize to 0-1 range
start_normalized_score <- start_weighted_outliers / total_records

cat("START TIMESTAMP:\n")
cat(paste("  Completeness outliers:", start_completeness_outliers, "\n"))
cat(paste("  Accuracy outliers:", start_accuracy_outliers, "\n"))
cat(paste("  Consistency outliers:", start_consistency_outliers, "\n"))
cat(paste("  Weighted outliers (1/3 each):", round(start_weighted_outliers, 2), "\n"))
cat(paste("  Normalized score (0-1):", round(start_normalized_score, 4), "\n\n"))

# COMPLETE TIMESTAMP
complete_completeness_outliers <- completeness_results$total_complete_outliers
# Combine accuracy outliers from both methods, cap at total records to avoid double-counting
complete_accuracy_outliers <- min(
  accuracy_results$total_complete_outliers + duration_diagnosis_results$outliers %>%
    filter(ts_diagnosis %in% c("Complete timestamp corrupted", "Both timestamps corrupted")) %>% nrow(),
  total_records
)
complete_consistency_outliers <- consistency_results$total_complete_outliers

complete_weighted_outliers <- (complete_completeness_outliers * weight) + 
                              (complete_accuracy_outliers * weight) + 
                              (complete_consistency_outliers * weight)

# Normalize to 0-1 range
complete_normalized_score <- complete_weighted_outliers / total_records

cat("COMPLETE TIMESTAMP:\n")
cat(paste("  Completeness outliers:", complete_completeness_outliers, "\n"))
cat(paste("  Accuracy outliers:", complete_accuracy_outliers, "\n"))
cat(paste("  Consistency outliers:", complete_consistency_outliers, "\n"))
cat(paste("  Weighted outliers (1/3 each):", round(complete_weighted_outliers, 2), "\n"))
cat(paste("  Normalized score (0-1):", round(complete_normalized_score, 4), "\n\n"))

# Save weighted scores
timestamp_weighted_scores <- data.frame(
  timestamp_type = c("start", "complete"),
  completeness_outliers = c(start_completeness_outliers, complete_completeness_outliers),
  accuracy_outliers = c(start_accuracy_outliers, complete_accuracy_outliers),
  consistency_outliers = c(start_consistency_outliers, complete_consistency_outliers),
  weighted_outliers = c(start_weighted_outliers, complete_weighted_outliers),
  normalized_score = c(start_normalized_score, complete_normalized_score),
  total_records = c(total_records, total_records)
)

write.csv(timestamp_weighted_scores, "results/timestamp_weighted_scores.csv", row.names = FALSE)
save(timestamp_weighted_scores, file = "results/timestamp_weighted_scores.RData")
cat("Saved to: results/timestamp_weighted_scores.csv\n")

# ============================================
# SIMULATION PARAMETER QUALITY DERIVATION
# ============================================

# Collect all attribute quality scores
attribute_scores <- list(
  # Start Timestamp (C, A, Co)
  start_completeness = completeness_results$start_completeness,
  start_accuracy = min(accuracy_results$start_accuracy, duration_diagnosis_results$start_accuracy),
  start_consistency = consistency_results$start_consistency,
  
  # Complete Timestamp (C, A, Co)
  complete_completeness = completeness_results$complete_completeness,
  complete_accuracy = min(accuracy_results$complete_accuracy, duration_diagnosis_results$end_accuracy),
  complete_consistency = consistency_results$complete_consistency,
  
  # Case ID (C, A)
  caseid_completeness = caseid_completeness_results$completeness,
  caseid_accuracy = caseid_accuracy_results$accuracy,
  
  # Activity (C, A, Co)
  activity_completeness = activity_completeness_results$activity_completeness / 100,  # Convert from %
  activity_accuracy = activity_accuracy_results$accuracy / 100,  # Convert from %
  activity_consistency = activity_consistency_results$consistency / 100,  # Convert from %
  
  # Resource (C, A, Co)
  resource_completeness = resource_completeness_results$completeness,
  resource_accuracy = resource_accuracy_results$accuracy,
  resource_consistency = resource_consistency_results$consistency
)

# Calculate simulation parameter quality scores
cat("\n")
simulation_quality <- calculate_simulation_parameter_quality(
  attribute_scores = attribute_scores,
  aggregation_method = "mean",  # Equal-weight approach
  dimension_weights = c(completeness = 1/3, accuracy = 1/3, consistency = 1/3),
  sample_sizes = list(total_events = nrow(as.data.frame(actlog)),
                      total_cases = length(unique(as.data.frame(actlog)$case_id)))
)

# Compare different aggregation methods
cat("\n")
aggregation_comparison <- compare_aggregation_methods(attribute_scores)

# ============================================
# GRANULAR QUALITY ANALYSIS
# ============================================

cat("\n")
granular_quality <- calculate_granular_simulation_quality(
  log = as.data.frame(actlog_final),
  case_col = "case_id",
  activity_col = "activity",
  resource_col = "originator",
  start_col = "start",
  complete_col = "complete",
  min_duration = 0,
  max_duration = 86400  # 24 hours in seconds
)

# Identify quality issues
quality_issues <- identify_quality_issues(granular_quality, threshold = 0.7)

# ============================================
# PER-ACTIVITY SIMULATION PARAMETER QUALITY
# ============================================

per_activity_quality <- calculate_per_activity_simulation_quality(
  actlog_df = as.data.frame(actlog),
  data_file = data_file,
  working_hours_start = working_hours_start,
  working_hours_end = working_hours_end,
  inactive_threshold_minutes = inactive_threshold_minutes
)

# ============================================
# PER RESOURCE-ACTIVITY PAIR QUALITY
# ============================================

resource_activity_quality <- calculate_per_resource_activity_quality(
  actlog_df = as.data.frame(actlog),
  working_hours_start = working_hours_start,
  working_hours_end = working_hours_end
)

# ============================================
# GATEWAY BRANCHING PROBABILITY ANALYSIS
# ============================================

gateway_results <- calculate_gateway_branching(
  actlog_df = as.data.frame(actlog),
  case_col = "case_id",
  activity_col = "activity",
  start_col = "start",
  complete_col = "complete",
  allowed_activities = allowed_activities,
  max_edit_distance = max_edit_distance,
  working_hours_start = working_hours_start,
  working_hours_end = working_hours_end,
  noise_threshold = 0.05,
  min_cases = 3
)

# ============================================
# FINAL SUMMARY
# ============================================

cat("\n============================================\n")
cat("FINAL QUALITY ASSESSMENT SUMMARY\n")
cat("============================================\n\n")

cat("EVENT LOG ATTRIBUTE QUALITY:\n")
print(simulation_quality$attribute_table)

cat("\nSIMULATION PARAMETER RELIABILITY:\n")
print(simulation_quality$simulation_table)

cat("\n============================================\n")
cat(paste("OVERALL SIMULATION MODEL RELIABILITY:", 
          round(simulation_quality$overall_minimum * 100, 2), "% (conservative)\n"))
cat("============================================\n")

}  # End of PARALLEL MODE (else block)
