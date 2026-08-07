# Event Log Quality Assessment Tool

A research prototype for assessing the quality of event logs for Business Process Simulation (BPS).  
It evaluates four event-log attributes — **Timestamp**, **Activity Label**, **Case ID**, and **Resource** — across three quality dimensions (Completeness, Accuracy, Consistency), and maps the results to four BPS simulation parameters (Activity Duration, Inter-arrival Time, Routing Probabilities, Resource Allocation).

A companion **Log Distance Evaluation** tool (third-party metrics) is also included for comparing real vs. simulated event logs.

---

## Project Structure

```
/
│
│  ── Entry points ──────────────────────────────────────────────────────────
├── main.py                      # Launch the quality assessment GUI
├── run_log_distance.py          # Launch the log distance evaluation GUI
├── requirements.txt             # Python dependencies
├── LICENSE                      # Project licence
├── README.md                    # This file
│
│  ── Quality assessment pipeline ───────────────────────────────────────────
├── pipeline_modules/
│   ├── __init__.py              # Public API exports
│   ├── config.py                # Paths (R scripts dir, Output dir) and column candidates
│   ├── column_detector.py       # Auto-detects CSV column mapping from headers
│   ├── r_runner.py              # Locates Rscript, writes wrapper, runs R pipeline
│   ├── excel_exporter.py        # Reads R output CSVs → builds Excel reports
│   └── gui.py                   # Tkinter GUI (file list, progress log, run button)
│
│  ── R assessment scripts ──────────────────────────────────────────────────
├── quality_assessment_r_scripts/
│   ├── main.r                               # Orchestrator — sources all sub-scripts
│   ├── auto_configure.r                     # Data-driven threshold derivation (IQR, patterns)
│   │
│   │   Timestamp
│   ├── calculate_timestamp_accuracy.r       # Working hours, duration outliers, root cause
│   ├── calculate_timestamp_completeness.r   # Missing start / complete timestamps
│   ├── calculate_timestamp_consistency.r    # Start ≤ complete, format consistency
│   ├── calculate_duration_ts_diagnosis.r    # Duration-based timestamp diagnosis
│   │
│   │   Activity Label
│   ├── calculate_activity_accuracy.r        # Incorrect labels (edit-distance > 3)
│   ├── calculate_activity_completeness.r    # Missing activity names
│   ├── calculate_activity_consistency.r     # Similar labels (variants, edit-distance ≤ 3)
│   ├── calculate_attribute_quality.r        # Aggregates per-attribute dimension scores
│   │
│   │   Case ID
│   ├── calculate_caseid_accuracy.r          # Structural / format errors in case IDs
│   ├── calculate_caseid_completeness.r      # Missing case IDs
│   │
│   │   Resource
│   ├── calculate_resource_accuracy.r        # Invalid resource names
│   ├── calculate_resource_completeness.r    # Missing resource values
│   ├── calculate_resource_consistency.r     # Resource–activity dominance model (≥ 70 %)
│   │
│   │   Simulation parameter quality
│   ├── calculate_simulation_parameter_quality.r    # Maps attribute scores → BPS parameters
│   ├── calculate_granular_simulation_quality.r     # Per-activity & per-resource-activity quality
│   ├── calculate_per_activity_simulation_quality.r # Per-resource-activity detailed scores
│   ├── calculate_gateway_branching.r               # Routing / branching probability quality
│   ├── cascading_quality_assessment.r              # Sequential (cascading) quality mode
│   │
│   │   Documentation
│   ├── DOCUMENTATION.md         # Technical documentation of R scripts
│   ├── README.md                # R-side readme
│   └── daqapo.md                # Notes on the DaQAPO R package integration
│
│   └── daqapo_extension/        # Custom extensions to the DaQAPO package
│       ├── main.r
│       ├── imprecise_timestamp.r
│       ├── imprecise_resource.r
│       ├── incorrect_case.r
│       ├── resource_activity_mismatch.r
│       ├── resource_inconsistency.r
│       ├── same_timestamp.r
│       ├── synonymous_label.r
│       └── test_main.r
│
│  ── Log distance (third-party) ─────────────────────────────────────────────
├── log_distance/                # ⚠ NOT the author's work — see README inside
│   ├── README.md                # Attribution: Camargo et al., PeerJ CS 2021
│   ├── __init__.py
│   ├── config.py                # EventLogIDs dataclass, DistanceMetric enum
│   ├── earth_movers_distance.py # EMD helper
│   ├── n_gram_distribution.py   # Bigram / Trigram distribution distance
│   ├── absolute_event_distribution.py
│   ├── relative_event_distribution.py
│   ├── case_arrival_distribution.py
│   ├── cycle_time_distribution.py
│   ├── circadian_event_distribution.py
│   ├── circadian_workforce_distribution.py
│   ├── work_in_progress.py
│   ├── remaining_time_distribution.py
│   └── control_flow_log_distance.py    # Control-Flow Log Distance (CFLD, slow)
│
│  ── Evaluation utilities ───────────────────────────────────────────────────
├── evaluation_utils/
│   ├── __init__.py
│   ├── log_distance_runner.py              # Runs all log-distance metrics, exports Excel
│   ├── log_distance_gui.py                 # Tkinter GUI for the log distance tool
│   ├── core.py                             # (Legacy — unused placeholder)
│   │
│   │   Summary results
│   ├── SimulationParameters_Summary_Hospital.xlsx      # Quality + LD results — Hospital log
│   ├── Simulation_Parameters_Summary_LoanApplication.xlsx  # Quality + LD results — Loan Application log
│   │
│   │   Hospital process datasets (synthetic, 1 000 cases)
│   ├── HospitalDatasets/
│   │   ├── generate_synthetic_logs.py      # Script that generated these datasets
│   │   ├── datasetwith1000cases_clean.csv  # Clean baseline (no noise)
│   │   │
│   │   │   Activity Label degradation (5 / 10 / 20 %)
│   │   ├── datasetwith1000cases_ActivityLabel_Accuracy_5%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Accuracy_10%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Accuracy_20%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Completeness_5%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Completeness_10%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Completeness_20%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Consistency_5%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Consistency_10%.csv
│   │   ├── datasetwith1000cases_ActivityLabel_Consistency_20%.csv
│   │   │
│   │   │   Case ID degradation
│   │   ├── datasetwith1000cases_CaseID_Accuracy_5%.csv
│   │   ├── datasetwith1000cases_CaseID_Accuracy_10%.csv
│   │   ├── datasetwith1000cases_CaseID_Accuracy_20%.csv
│   │   ├── datasetwith1000cases_CaseID_Completeness_5%.csv
│   │   ├── datasetwith1000cases_CaseID_Completeness_10%.csv
│   │   ├── datasetwith1000cases_CaseID_Completeness_20%.csv
│   │   │
│   │   │   Resource degradation
│   │   ├── datasetwith1000cases_Resource_Accuracy_5%.csv
│   │   ├── datasetwith1000cases_Resource_Accuracy_10%.csv
│   │   ├── datasetwith1000cases_Resource_Accuracy_20%.csv
│   │   ├── datasetwith1000cases_Resource_Completeness_5%.csv
│   │   ├── datasetwith1000cases_Resource_Completeness_10%.csv
│   │   ├── datasetwith1000cases_Resource_Completeness_20%.csv
│   │   ├── datasetwith1000cases_Resource_Consistency_5%.csv
│   │   ├── datasetwith1000cases_Resource_Consistency_10%.csv
│   │   ├── datasetwith1000cases_Resource_Consistency_20%.csv
│   │   │
│   │   │   Timestamp degradation
│   │   ├── datasetwith1000cases_Timestamp_Accuracy_5%.csv
│   │   ├── datasetwith1000cases_Timestamp_Accuracy_10%.csv
│   │   ├── datasetwith1000cases_Timestamp_Accuracy_20%.csv
│   │   ├── datasetwith1000cases_Timestamp_Completeness_5%.csv
│   │   ├── datasetwith1000cases_Timestamp_Completeness_10%.csv
│   │   ├── datasetwith1000cases_Timestamp_Completeness_20%.csv
│   │   ├── datasetwith1000cases_Timestamp_Consistency_5%.csv
│   │   ├── datasetwith1000cases_Timestamp_Consistency_10%.csv
│   │   └── datasetwith1000cases_Timestamp_Consistency_20%.csv
│   │
│   │   Loan Application datasets (BPI Challenge 2017 process structure)
│   ├── LoanApplication/
│   │   ├── datasetwith1000cases_clean.csv  # Clean baseline (no noise)
│   │   │
│   │   │   Same attribute / dimension degradations as HospitalDatasets (5 / 10 / 20 %)
│   │   ├── datasetwith1000cases_ActivityLabel_Accuracy_5%.csv  ... (×3)
│   │   ├── datasetwith1000cases_ActivityLabel_Completeness_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_ActivityLabel_Consistency_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_CaseID_Accuracy_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_CaseID_Completeness_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_Resource_Accuracy_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_Resource_Completeness_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_Resource_Consistency_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_Timestamp_Accuracy_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_Timestamp_Completeness_5%.csv ... (×3)
│   │   ├── datasetwith1000cases_Timestamp_Consistency_5%.csv ... (×3)
│   │   │
│   │   │   Combined degradation (all attributes simultaneously)
│   │   ├── datasetwith1000cases_Combined_5%.csv
│   │   ├── datasetwith1000cases_Combined_10%.csv
│   │   ├── datasetwith1000cases_Combined_20%.csv
│   │   │
│   │   │   Large-scale datasets (3 000 cases, all dimensions degraded)
│   │   ├── datasetwith3000cases_ActivityLabel_AllDimensions_5%.csv
│   │   ├── datasetwith3000cases_ActivityLabel_AllDimensions_10%.csv
│   │   ├── datasetwith3000cases_ActivityLabel_AllDimensions_20%.csv
│   │   ├── datasetwith3000cases_CaseID_AllDimensions_5%.csv
│   │   ├── datasetwith3000cases_CaseID_AllDimensions_10%.csv
│   │   ├── datasetwith3000cases_CaseID_AllDimensions_20%.csv
│   │   ├── datasetwith3000cases_Resource_AllDimensions_5%.csv
│   │   ├── datasetwith3000cases_Resource_AllDimensions_10%.csv
│   │   ├── datasetwith3000cases_Resource_AllDimensions_20%.csv
│   │   ├── datasetwith3000cases_Timestamp_AllDimensions_5%.csv
│   │   ├── datasetwith3000cases_Timestamp_AllDimensions_10%.csv
│   │   ├── datasetwith3000cases_Timestamp_AllDimensions_20%.csv
│   │   │
│   │   └── output.xlsx      # Log distance computation results for this dataset
│   │
│   │   Real-world log
│   └── BPI challange 2012/
│       └── BPIChallenge2012.csv     # BPI Challenge 2012 real-world event log
│
│  ── Output ─────────────────────────────────────────────────────────────────
└── Output/                      # All Excel results saved here (auto-created, not tracked by git)
```


---

## Requirements

### Quality Assessment

- **Python** 3.11 or later
- **R** 4.x with the following R packages:
  - `daqapo`
  - `bupaR`
  - `xesreadR`
  - `stringdist`
  - `stringr`
  - `dplyr`

### Log Distance Evaluation

- Python packages (in addition to `pandas` and `openpyxl`):
  - `jellyfish` — Damerau-Levenshtein distance
  - `scipy` — Wasserstein distance
  - `pulp` — Earth Mover's Distance (linear programming)

---

## Installation

1. Create and activate a virtual environment:

```powershell
python -m venv .venv
.\.venv\Scripts\Activate.ps1
```

2. Install all Python dependencies:

```powershell
pip install -r requirements.txt
```

3. Ensure R is installed and `Rscript` is on your PATH  
   (or located under `C:\Program Files\R\R-x.y.z\bin\Rscript.exe`).

4. Install the required R packages (run once inside R):

```r
install.packages(c("daqapo", "bupaR", "xesreadR", "stringdist", "stringr", "dplyr"))
```

---

## Usage

### Quality Assessment

```powershell
python main.py
```

1. Click **Add files...** → select one or more CSV event logs.
2. Click **Run Quality Assessment**.
3. The tool auto-detects column names, runs the R pipeline, and saves results to `Output/`.

### Log Distance Evaluation

```powershell
python run_log_distance.py
```

1. Click **Browse...** → select the **original** (real) event log CSV.
2. Click **Add files...** → add one or more **simulated / degraded** event log CSVs.
3. Click **Run Log Distance Analysis** → results saved to `Output/`.

---

## Output

### Quality Assessment — up to three Excel files per log

| File | Contents |
|------|----------|
| `Assessment for <log> - Attributes.xlsx` | Accuracy / Consistency / Completeness per attribute |
| `Assessment for <log> - Simulation Parameters.xlsx` | BPS parameter quality + per-activity and per-resource-activity detail |
| `Assessment for <log> - Root Cause Analysis.xlsx` | Per-event diagnosis of duration anomalies *(only when anomalies exist)* |

A raw R log (`r_run.log`) is saved next to the input CSV for debugging.

### Log Distance Evaluation — one Excel file per original log

| File | Contents |
|------|----------|
| `Log Distance - <original_log>.xlsx` | **Distance Metrics** sheet: one row per simulated log, columns = all metrics; **BPS Parameter Mapping** sheet: which metric corresponds to which BPS parameter |

### Pre-computed summary tables (in `evaluation_utils/`)

| File | Contents |
|------|----------|
| `SimulationParameters_Summary_Hospital.xlsx` | Quality scores + log distance metrics for all Hospital process degradation datasets |
| `Simulation_Parameters_Summary_LoanApplication.xlsx` | Quality scores + log distance metrics for all Loan Application degradation datasets |

---

## Configuration

To change the output directory, edit `pipeline_modules/config.py`:

```python
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "Output")   # change this path
```

---

## How It Works

### Quality Assessment pipeline

1. **Column detection** (`pipeline_modules/column_detector.py`): matches CSV headers against candidate names for case ID, activity, resource, start timestamp, complete timestamp.
2. **Auto-configuration** (`quality_assessment_r_scripts/auto_configure.r`): derives data-driven thresholds from the log distribution — IQR-based duration bounds, majority timestamp format, dominant resource–activity associations.
3. **Attribute assessment**: modular R scripts evaluate each attribute across all applicable quality dimensions (Completeness, Accuracy, Consistency) at event, case, or log level.
4. **BPS parameter mapping** (`calculate_simulation_parameter_quality.r`): aggregates attribute-level dimension scores into four simulation parameter quality scores.
5. **Granular analysis** (`calculate_granular_simulation_quality.r`): per-activity duration quality and per-resource-activity pair quality.
6. **Excel export** (`pipeline_modules/excel_exporter.py`): reads R output CSVs and builds formatted Excel reports.

### Log Distance pipeline

1. **Column auto-detection**: reuses `column_detector.py` to map CSV headers to `EventLogIDs`.
2. **Metric computation** (`evaluation_utils/log_distance_runner.py`): runs 13 distance metrics from the third-party `log_distance` package (Camargo et al., 2021).
3. **Export**: results written to `Output/Log Distance - <name>.xlsx` with a BPS parameter mapping reference sheet.

---

## Evaluation Datasets

Two process domains are evaluated, each with the same degradation structure.

### Hospital Process (`HospitalDatasets/`)

Synthetic logs based on a simple 5-activity hospital care process (Registration → Diagnosis → Lab Test → X-Ray → Treatment).  
Generated by `HospitalDatasets/generate_synthetic_logs.py`.

| Attribute | Dimensions degraded | Noise levels |
|-----------|-------------------|-------------|
| Activity Label | Accuracy, Completeness, Consistency | 5 %, 10 %, 20 % |
| Case ID | Accuracy, Completeness | 5 %, 10 %, 20 % |
| Resource | Accuracy, Completeness, Consistency | 5 %, 10 %, 20 % |
| Timestamp | Accuracy, Completeness, Consistency | 5 %, 10 %, 20 % |

**36 CSV files** (34 degraded + 1 clean baseline) — 1 000 cases each.

### Loan Application Process (`LoanApplication/`)

Synthetic logs based on the BPI Challenge 2017 loan application process structure.

| Dataset group | Count | Description |
|---|---|---|
| Single-attribute degradation | 33 CSVs | Same 11 attribute–dimension combinations as Hospital × 3 noise levels |
| Combined degradation | 3 CSVs | All attributes degraded simultaneously at 5 / 10 / 20 % |
| Large-scale (3 000 cases) | 12 CSVs | All 4 attributes × all dimensions degraded, at 5 / 10 / 20 % |
| Clean baseline | 1 CSV | No noise |
| Log distance results | `output.xlsx` | Computed distances between clean and all degraded logs |

**50 files** total.

### Real-world log (`BPI challange 2012/`)

`BPIChallenge2012.csv` — the BPI Challenge 2017 real-world loan application event log, used to validate findings from synthetic experiments.

---

## Attribution

The `log_distance/` folder contains code from:

> Camargo M, Dumas M, González-Rojas O.  
> *Discovering generative models from event logs: data-driven simulation vs deep learning.*  
> PeerJ Computer Science 7:e577, 2021. https://doi.org/10.7717/peerj-cs.577  
> GitHub: https://github.com/AutomatedProcessImprovement/log-distance-measures

Only the internal import paths were adjusted to match the package location in this repository.  
No algorithmic changes were made. See `log_distance/README.md` for the full notice.
