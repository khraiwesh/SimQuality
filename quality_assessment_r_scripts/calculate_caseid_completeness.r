# Case ID completeness: sequence gaps detection

library(dplyr)
library(bupaR)
library(daqapo)
library(lubridate)

calculate_caseid_completeness <- function(dataset, data_file) {
  
  results_dir <- "results_case_id"
  if (!dir.exists(results_dir)) {
    dir.create(results_dir)
  }
  
  cat("============================================\n")
  cat("CASE ID COMPLETENESS ANALYSIS\n")
  cat("============================================\n\n")
  
  actlog <- dataset
  
  cat(paste("Total records:", nrow(actlog), "\n"))
  cat(paste("Unique cases:", n_distinct(actlog$case_id), "\n\n"))
  
  cat("Detecting case ID sequence gaps...\n")
  
  case_id_gaps <- tryCatch({
    gaps <- detect_case_id_sequence_gaps(
      activitylog = actlog
    )
    gaps
  }, error = function(e) {
    cat(paste("  Error:", e$message, "\n"))
    NULL
  })
  
  case_id_gaps_count <- 0
  case_id_gaps_df <- data.frame()
  
  if(!is.null(case_id_gaps)) {
    case_id_gaps_df <- as.data.frame(case_id_gaps)
    case_id_gaps_count <- nrow(case_id_gaps_df)
    
    cat(paste("Found", case_id_gaps_count, "case ID sequence gaps\n"))
    
    cat("\nGaps dataframe structure:\n")
    print(str(case_id_gaps_df))
    
    if(case_id_gaps_count > 0) {
      cat("\nFirst few gaps:\n")
      print(head(case_id_gaps_df, 10))
    }
    
    gaps_file <- file.path(results_dir, "case_id_sequence_gaps.RData")
    gaps_csv <- file.path(results_dir, "case_id_sequence_gaps.csv")
    
    save(case_id_gaps_df, file = gaps_file)
    write.csv(case_id_gaps_df, gaps_csv, row.names = FALSE)
    
    cat(paste("\n  Saved to:", gaps_csv, "\n"))
  } else {
    cat("No case ID sequence gaps found or function not available\n")
    
    gaps_file <- file.path(results_dir, "case_id_sequence_gaps.RData")
    gaps_csv <- file.path(results_dir, "case_id_sequence_gaps.csv")
    
    save(case_id_gaps_df, file = gaps_file)
    write.csv(case_id_gaps_df, gaps_csv, row.names = FALSE)
  }
  
  cat("\n")
  
  # Completeness: unique / (missing + unique)
  cat("============================================\n")
  cat("CASE ID COMPLETENESS CALCULATION\n")
  cat("============================================\n")
  
  total_records <- nrow(actlog)
  unique_cases <- n_distinct(actlog$case_id)
  
# NaN-based completeness: fraction of rows that have a non-missing case ID.
  # Previously used min/max gap formula, which caused out-of-range IDs (from the
  # accuracy injector) to artificially collapse completeness to near 0 %.
  missing_count     <- sum(is.na(actlog$case_id))
  non_missing_count <- total_records - missing_count

  completeness <- non_missing_count / total_records
  completeness_percentage <- completeness * 100

  cat(paste("Unique case IDs:", unique_cases, "\n"))
  cat(paste("Missing (NA) case IDs:", missing_count, "\n"))
  cat(paste("Case ID Completeness:", round(completeness_percentage, 2), "%\n"))
  cat(paste("Total outliers:", missing_count, "\n"))

  return(list(
    unique_cases = unique_cases,
    missing_cases = missing_count,
    completeness = completeness,
    completeness_percentage = completeness_percentage,
    total_outliers = missing_count,
    case_id_gaps_df = case_id_gaps_df,
    total_records = total_records
  ))
}
