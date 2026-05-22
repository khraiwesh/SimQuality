# Data Quality Analysis Pipeline - Documentation

## Overview

This R-based pipeline analyzes data quality for process mining event logs using **daqapo** and **bupaR** packages. It evaluates three main quality dimensions: **Completeness**, **Accuracy**, and **Consistency** across four attributes:

- Start Timestamp
- Complete Timestamp  
- Case ID
- Activity Label

---

## Architecture

### Module Structure

```
main.r                              # Orchestrator - loads data, calls all modules
├── calculate_completeness.r        # Timestamp completeness (NA detection)
├── calculate_accuracy.r            # Timestamp accuracy (duration outliers, working hours)
├── calculate_consistency.r         # Timestamp format consistency
├── calculate_caseid_completeness.r # Case ID sequence gaps
├── calculate_caseid_accuracy.r     # Case ID duplication check
├── calculate_activity_completeness.r   # Missing activity labels
└── calculate_activity_accuracy.r       # Typos + incorrect activity names
```

### Data Flow

1. `main.r` loads CSV/RData → converts to bupaR `activitylog` object
2. Columns renamed internally: `patient_visit_nr` → `case_id`, `start_ts` → `start`
3. Each module receives `actlog` and returns results + cleaned data
4. Results saved to `results/`, `results_activity/`, `results_case_id/`

---

## Quality Dimensions & Outlier Definitions

### 1. TIMESTAMP COMPLETENESS

**What it measures:** Presence of timestamp values (non-NA)

**Formula:** 
```
Completeness = (Total Records - Missing Count) / Total Records
```

**Outliers:** Records with `NA` in start or complete timestamp

**daqapo function:** `detect_missing_values()`

---

### 2. TIMESTAMP ACCURACY

**What it measures:** Correctness of timestamp values

**Outliers detected:**

| Check | Description | daqapo Function |
|-------|-------------|-----------------|
| Duration Outliers | Duration outside 0-120 min range | `detect_duration_outliers()` |
| Negative Duration | Complete timestamp before start; detected as a time anomaly and then passed to root cause analysis | `detect_time_anomalies()` |
| Zero Duration | Start equals complete; detected as a time anomaly and then passed to root cause analysis | `detect_time_anomalies()` |
| Working Hours | Timestamps outside allowed hours | `detect_value_range_violations()` |
| Multiregistration | Multiple events within 1 second | `detect_multiregistration()` (info only) |

**Root Cause Analysis:** For each duration anomaly (including negative and zero durations), we compare start and complete timestamps against median values to determine which timestamp is more suspicious.

**Formula:**

---

### 3. TIMESTAMP CONSISTENCY

**What it measures:** Format uniformity of timestamps

**Formula:**
```
Consistency = Most Common Format Count / Total Records
```

**Formats checked:**
- `dd/mm/yyyy HH:MM:SS`
- `dd.mm.yyyy HH:MM:SS`
- `dd.Mmm.yyyy HH:MM:SS`
- `yyyy-mm-dd HH:MM:SS`
- And others...

---

### 4. CASE ID COMPLETENESS

**What it measures:** Presence of all expected case IDs in sequence

**Formula:**
```
Completeness = Unique Case IDs / (Missing Case IDs + Unique Case IDs)
```

**Outliers:** Missing case IDs in the sequence (gaps between min and max case ID)

**daqapo function:** `detect_case_id_sequence_gaps()`

---

### 5. CASE ID ACCURACY

**What it measures:** Uniqueness of case_id + activity combinations

**Formula:**
```
Accuracy = Unique Combinations / Total Records
```

**Outliers:** Duplicate (case_id, activity) combinations - indicates same activity registered multiple times for same case

**daqapo function:** `detect_unique_values()`

**Important:** bupaR's activitylog class overrides dplyr functions. Must use `as.data.frame()` before `distinct()`:
```r
# ❌ WRONG
actlog %>% select(case_id, activity) %>% distinct()

# ✅ CORRECT
as.data.frame(actlog) %>% select(case_id, activity) %>% distinct()
```

---

### 6. ACTIVITY LABEL COMPLETENESS

**What it measures:** Presence of activity values (non-NA) and case completeness

**Outliers:**

| Type | Description | Counted as Outlier? |
|------|-------------|---------------------|
| NA Activity | Missing activity label | ✅ Yes |
| Incomplete Cases | Cases missing mandatory activities | ❌ No (info only) |
| Conditional Violations | Activity missing when condition holds | ❌ No (info only) |

**Formula:**
```
Completeness = (Total Records - NA Count) / Total Records
```

**Incomplete Cases explained:** A case is "incomplete" if it doesn't have all 5 mandatory activities (Registration, Triage, Clinical exam, Treatment, Treatment evaluation). The output shows which activities ARE recorded in incomplete cases, not which ones are missing, because `detect_incomplete_cases()` is designed to report the actually observed activity pattern per case; we then derive which mandatory steps are missing from that pattern during analysis rather than having daqapo compute the missing list explicitly.

**daqapo functions:**
- `detect_missing_values()` - NA detection
- `detect_incomplete_cases()` - Missing mandatory activities
- `detect_conditional_activity_presence()` - Conditional checks

---

### 7. ACTIVITY LABEL ACCURACY

**What it measures:** Correctness of activity names

**Outliers:**

| Type | Description | Example |
|------|-------------|---------|
| Typos | Similar to allowed activities (edit distance ≤ 3) | `registration` → `Registration` |
| Incorrect | Not in allowed list and not a typo | `0`, `NA` |

**Formula:**
```
Accuracy = (Total Records - Typo Count - Incorrect Count) / Total Records
```

**Process:**
1. `detect_similar_labels()` finds typos (case-sensitive!)
2. Filter out typos
3. `detect_incorrect_activity_names()` finds remaining incorrect names
4. Filter out incorrect names

**Allowed activities (case-sensitive):**
- Registration
- Triage
- Clinical exam
- Treatment
- Treatment evaluation

---

## Output Summary Table

| Attribute | Completeness | Accuracy | Consistency |
|-----------|--------------|----------|-------------|
| Start Timestamp | 98.11% | 97.83% | 100% |
| Complete Timestamp | 100% | 89.13% | 100% |
| Case ID | 81.48% | 92.45% | - |
| Activity Label | 96.23% | 88.68% | - |

---

## Output Files

### results/
- `data_quality_summary.csv` - Final summary table
- `data_quality_completeness_results.csv` - Timestamp completeness
- `data_quality_accuracy_scores.csv` - Timestamp accuracy
- `data_quality_consistency_scores.csv` - Timestamp consistency
- `data_quality_time_anomalies.csv` - Duration outliers
- `data_quality_root_cause_analysis.csv` - Root cause for each outlier
- `data_quality_working_hours_violations.csv` - Working hours violations
- `data_quality_multiregistration.csv` - Multi-registration cases

### results_case_id/
- `case_id_sequence_gaps.csv` - Missing case IDs
- `case_activity_duplications.csv` - Duplicate combinations

### results_activity/
- `activity_missing_values.csv` - NA activities
- `activity_incomplete_cases.csv` - Cases missing mandatory activities
- `activity_similar_labels.csv` - Detected typos
- `activity_incorrect_names.csv` - Invalid activity names
- `activity_accuracy_summary.csv` - Activity accuracy metrics

---

## Running the Pipeline

```bash
Rscript main.r
```

### Switch Datasets

Edit `main.r` configuration section:
```r
data_file <- "datasets/hospital_view.csv"    # CSV format
data_file <- "datasets/hospital.RData"       # RData format
```

---

## Key Insights & Known Issues

### 1. Activity Name Case Sensitivity
Activity names are **case-sensitive**. `registration` ≠ `Registration`. The typo detection catches this.

### 2. Incomplete Cases Output
`detect_incomplete_cases()` output shows which activities ARE recorded in incomplete cases, NOT which ones are missing. Example:
```
activity   n  case_ids
Triage    10  512 - 517 - 521...
```
This means: "Triage exists in 10 incomplete cases", not "Triage is missing from 10 cases".

### 3. bupaR + dplyr Quirk
bupaR's activitylog class overrides dplyr's `select()`, `distinct()`, etc. Always convert to data.frame first when doing dplyr operations.

### 4. Empty String vs NA
Empty strings `""` are not the same as `NA`. The pipeline treats empty strings as NA during CSV loading (`na.strings = c("NA", "")`).

---

## Dependencies

- **daqapo** (0.3.x) - Data quality functions
- **bupaR** - Process mining activitylog class
- **dplyr** - Data manipulation
- **lubridate** - Timestamp parsing
- **stringr** - String operations
