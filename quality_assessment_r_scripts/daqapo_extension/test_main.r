library(testthat)

cat("\n========================================\n")
cat("Running Data Quality Analysis Tests\n")
cat("========================================\n\n")

# Get all test files in the Test directory
test_dir <- "Test"
test_files <- list.files(test_dir, pattern = "^test_.*\\.r$", full.names = TRUE)

if (length(test_files) == 0) {
  cat("No test files found in Test/ directory\n")
  quit(status = 1)
}

cat("Found", length(test_files), "test file(s):\n")
for (f in test_files) {
  cat("  -", basename(f), "\n")
}
cat("\n")

# Run tests and capture results
test_results <- test_dir(test_dir, reporter = "progress", pattern = "^test_.*\\.r$")

cat("\n========================================\n")
cat("Test Summary\n")
cat("========================================\n")

# Extract test statistics
num_tests <- length(test_results)
num_passed <- sum(sapply(test_results, function(x) length(x$results) == 0 || all(sapply(x$results, function(r) is.null(r$error)))))
num_failed <- num_tests - num_passed

cat("Total tests: ", num_tests, "\n")
cat("Passed: ", num_passed, "\n")
cat("Failed: ", num_failed, "\n\n")

# Print detailed results for failures
if (num_failed > 0) {
  cat("Failed Tests:\n")
  for (test in test_results) {
    if (length(test$results) > 0) {
      for (result in test$results) {
        if (!is.null(result$error)) {
          cat("  - ", test$test, ": ", result$error$message, "\n")
        }
      }
    }
  }
  cat("\n")
}

# Return appropriate exit code
if (num_failed > 0) {
  cat("Tests FAILED\n")
  quit(status = 1)
} else {
  cat("All tests PASSED\n")
  quit(status = 0)
}
