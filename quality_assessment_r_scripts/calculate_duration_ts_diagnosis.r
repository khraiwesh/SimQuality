# calculate_duration_ts_diagnosis.r
# ─────────────────────────────────────────────────────────────────────────────
# Distribution-based detection of abnormal event durations WITHOUT a clean
# reference dataset.
#
# For every event, duration = complete - start (in minutes).
# Per-activity robust statistics (median + IQR) flag duration outliers.
# For each flagged event, we independently score the start and complete
# timestamps against their per-activity distributions to identify WHICH
# timestamp is likely corrupted.
#
# Diagnosis rules (using Median Absolute Deviation z-scores):
#   z_start >> z_end  → Start Timestamp likely corrupted
#   z_end >> z_start  → Complete Timestamp likely corrupted
#   both extreme      → Both timestamps likely corrupted
#   neither extreme   → Duration anomaly (e.g. process delay, not TS error)
#
# Outputs:
#   results/duration_outliers_diagnosis.csv   – flagged events with diagnosis
#   results/duration_quality_summary.csv      – attribute-level accuracy scores
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(lubridate)

calculate_duration_ts_diagnosis <- function(dataset,
                                            iqr_multiplier    = 3.0,
                                            mad_z_threshold   = 5.0,
                                            mad_dominance_ratio = 1.5) {

  results_dir <- "results"
  if (!dir.exists(results_dir)) dir.create(results_dir)

  cat("============================================\n")
  cat("DURATION & TIMESTAMP DIAGNOSIS\n")
  cat("(no clean reference required)\n")
  cat("============================================\n\n")

  df <- as.data.frame(dataset)

  # ── Ensure timestamp columns are POSIXct ──────────────────────────────────
  if (!inherits(df$start, "POSIXct")) {
    df$start <- as.POSIXct(df$start, tz = "UTC")
  }
  if (!inherits(df$complete, "POSIXct")) {
    df$complete <- as.POSIXct(df$complete, tz = "UTC")
  }

  total_records <- nrow(df)
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Activities  :", n_distinct(df$activity), "\n\n"))

  # ── Per-event duration (minutes) ─────────────────────────────────────────
  df <- df %>%
    mutate(
      duration_min = as.numeric(difftime(complete, start, units = "mins")),
      start_epoch  = as.numeric(start),    # seconds since Unix epoch
      end_epoch    = as.numeric(complete)
    )

  # ── Per-activity robust stats for duration ───────────────────────────────
  activity_stats <- df %>%
    group_by(activity) %>%
    summarise(
      n_events       = n(),
      dur_median     = median(duration_min, na.rm = TRUE),
      dur_q1         = quantile(duration_min, 0.25, na.rm = TRUE),
      dur_q3         = quantile(duration_min, 0.75, na.rm = TRUE),
      dur_iqr        = IQR(duration_min, na.rm = TRUE),
      dur_mad        = mad(duration_min, constant = 1.4826, na.rm = TRUE),
      start_median   = median(start_epoch, na.rm = TRUE),
      start_mad      = mad(start_epoch, constant = 1.4826, na.rm = TRUE),
      end_median     = median(end_epoch, na.rm = TRUE),
      end_mad        = mad(end_epoch, constant = 1.4826, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Upper fence for duration outliers
      dur_upper_fence = dur_q3 + iqr_multiplier * dur_iqr,
      # Floor at zero to avoid false positives for fast activities
      dur_upper_fence = pmax(dur_upper_fence, dur_median * 10)
    )

  # ── Join stats back and flag duration outliers ───────────────────────────
  df <- df %>%
    left_join(activity_stats, by = "activity") %>%
    mutate(
      duration_outlier = !is.na(duration_min) & duration_min > dur_upper_fence,

      # MAD-based z-scores for start and complete timestamps (per activity)
      z_start = ifelse(start_mad > 0,
                       abs(start_epoch - start_median) / start_mad,
                       0),
      z_end   = ifelse(end_mad > 0,
                       abs(end_epoch - end_median) / end_mad,
                       0)
    )

  # ── Diagnosis for outlier events ─────────────────────────────────────────
  outliers <- df %>%
    filter(duration_outlier) %>%
    mutate(
      start_extreme = z_start >= mad_z_threshold,
      end_extreme   = z_end   >= mad_z_threshold,

      ts_diagnosis = case_when(
        start_extreme & end_extreme  ~ "Both timestamps corrupted",
        start_extreme & !end_extreme ~ "Start timestamp corrupted",
        !start_extreme & end_extreme ~ "Complete timestamp corrupted",
        TRUE                         ~ "Duration anomaly (process delay, not TS error)"
      )
    ) %>%
    select(case_id, activity, start, complete,
           duration_min, dur_median, dur_upper_fence,
           z_start, z_end, start_extreme, end_extreme,
           ts_diagnosis)

  n_outliers <- nrow(outliers)
  cat(paste("Duration outliers detected:", n_outliers,
            sprintf("(%.1f%% of records)\n", 100 * n_outliers / total_records)))

  if (n_outliers > 0) {
    diag_counts <- table(outliers$ts_diagnosis)
    cat("\nDiagnosis breakdown:\n")
    for (nm in names(diag_counts)) {
      cat(sprintf("  %-50s %d events\n", nm, diag_counts[[nm]]))
    }
    cat("\n")
  }

  # ── Export flagged events ─────────────────────────────────────────────────
  outlier_file <- file.path(results_dir, "duration_outliers_diagnosis.csv")
  write.csv(outliers, outlier_file, row.names = FALSE)
  cat(paste("Flagged events saved to:", outlier_file, "\n"))

  # ── Compute attribute-level accuracy impact ───────────────────────────────
  #
  # For each timestamp attribute, accuracy = 1 - (corrupted events / total)
  start_corrupted  <- sum(outliers$ts_diagnosis %in%
                            c("Start timestamp corrupted",
                              "Both timestamps corrupted"))
  end_corrupted    <- sum(outliers$ts_diagnosis %in%
                            c("Complete timestamp corrupted",
                              "Both timestamps corrupted"))
  process_delays   <- sum(outliers$ts_diagnosis ==
                            "Duration anomaly (process delay, not TS error)")

  start_accuracy    <- 1 - start_corrupted  / total_records
  end_accuracy      <- 1 - end_corrupted    / total_records
  duration_accuracy <- 1 - n_outliers       / total_records

  summary_df <- data.frame(
    Attribute     = c("Start Timestamp (duration-based)",
                      "Complete Timestamp (duration-based)",
                      "Overall Duration Quality"),
    Total_Records = total_records,
    Outlier_Events = c(start_corrupted, end_corrupted, n_outliers),
    Accuracy      = round(c(start_accuracy, end_accuracy, duration_accuracy), 4),
    Accuracy_Pct  = round(c(start_accuracy, end_accuracy, duration_accuracy) * 100, 2),
    Note          = c(
      paste(start_corrupted,  "events where Start TS is the likely corrupted timestamp"),
      paste(end_corrupted,    "events where Complete TS is the likely corrupted timestamp"),
      paste(process_delays,   "additional events are process-delay anomalies (not TS errors)")
    )
  )

  summary_file <- file.path(results_dir, "duration_quality_summary.csv")
  write.csv(summary_df, summary_file, row.names = FALSE)
  cat(paste("Accuracy summary saved to:", summary_file, "\n\n"))

  cat("--- Duration-based Accuracy Scores ---\n")
  cat(sprintf("  Start Timestamp    : %.2f%%\n", start_accuracy * 100))
  cat(sprintf("  Complete Timestamp : %.2f%%\n", end_accuracy * 100))
  cat(sprintf("  Overall Duration   : %.2f%%\n", duration_accuracy * 100))
  cat("---------------------------------------\n\n")

  return(list(
    outliers         = outliers,
    summary          = summary_df,
    activity_stats   = activity_stats,
    start_accuracy   = start_accuracy,
    end_accuracy     = end_accuracy,
    duration_accuracy = duration_accuracy
  ))
}
