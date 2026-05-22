# Resource completeness: detect missing/blank resource values

library(dplyr)
library(bupaR)

calculate_resource_completeness <- function(dataset, data_file) {
  
  results_dir <- "results_resource"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("RESOURCE COMPLETENESS ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog <- dataset
  actlog_df <- as.data.frame(actlog)
  
  total_records <- nrow(actlog_df)
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Unique resources:", n_distinct(actlog_df$originator, na.rm = TRUE), "\n\n"))
  
  # 1. Detect missing resource values (NA, blank, whitespace-only)
  cat("============================================\n")
  cat("1. DETECT MISSING RESOURCE VALUES\n")
  cat("============================================\n\n")
  
  missing_mask <- is.na(actlog_df$originator) | 
                  trimws(actlog_df$originator) == ""
  
  missing_count <- sum(missing_mask)
  missing_rows <- actlog_df[missing_mask, ]
  
  cat(paste("Records with missing resource:", missing_count, "\n"))
  cat(paste("Records with resource present:", total_records - missing_count, "\n\n"))
  
  if(missing_count > 0) {
    cat("Sample missing resource records (first 10):\n")
    print(head(missing_rows[, c("case_id", "activity", "originator")], 10))
    
    missing_file <- file.path(results_dir, "resource_missing_values.csv")
    write.csv(missing_rows, missing_file, row.names = FALSE)
    cat(paste("\n  Saved to:", missing_file, "\n"))
  }
  
  # 2. Completeness calculation
  cat("\n============================================\n")
  cat("2. RESOURCE COMPLETENESS CALCULATION\n")
  cat("============================================\n\n")
  
  complete_count <- total_records - missing_count
  completeness <- complete_count / total_records
  completeness_percentage <- completeness * 100
  
  cat(paste("Total records:", total_records, "\n"))
  cat(paste("Missing resource count:", missing_count, "\n"))
  cat(paste("Complete records:", complete_count, "\n"))
  cat(paste("Resource Completeness:", round(completeness_percentage, 2), "%\n"))
  
  completeness_summary <- data.frame(
    metric = c("total_records", "missing_count", "complete_count", "completeness_percent"),
    value = c(total_records, missing_count, complete_count, round(completeness_percentage, 2))
  )
  
  summary_file <- file.path(results_dir, "resource_completeness_summary.csv")
  write.csv(completeness_summary, summary_file, row.names = FALSE)
  cat(paste("  Saved to:", summary_file, "\n"))
  
  return(list(
    total_records = total_records,
    missing_count = missing_count,
    complete_count = complete_count,
    completeness = completeness,
    completeness_percentage = completeness_percentage,
    total_outliers = missing_count
  ))
}
