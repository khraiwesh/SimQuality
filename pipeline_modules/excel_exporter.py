"""
excel_exporter.py
-----------------
Reads R output CSVs and produces three structured Excel tables per log:
  - Table 1: Event-Log Attribute Quality  (Accuracy / Consistency / Completeness per attribute)
  - Table 2: Simulation Parameter Quality (per parameter, broken down by dimension)
  - Table 3: Detailed Analysis            (per activity duration + per resource-activity pair)
"""

import os
import pandas as pd
from .config import OUTPUT_DIR


# ── Helpers ───────────────────────────────────────────────────────────────────

def _load_pct(value) -> float | None:
    """Parse a percentage string like '95.3%' or '-' into a float (0–1) or None."""
    text = str(value).strip()
    if text in ("-", "NA", "nan", ""):
        return None
    return float(text.strip("%")) / 100.0


def _safe_float(value) -> float | None:
    """Convert a value to float, returning None for NaN."""
    try:
        f = float(value)
        return None if (f != f) else f   # NaN check
    except Exception:
        return None


# ── Table builders ────────────────────────────────────────────────────────────

def build_table1(results_dir: str, case_results_dir: str) -> pd.DataFrame:
    """
    Table 1 — Event-Log Attribute Quality.
    Rows: Accuracy | Consistency | Completeness | Overall Score
    Cols: Start Timestamp | Complete Timestamp | Case ID | Activity Label | Resource
    Note: Case ID has no Consistency dimension (shown as NaN).
    """
    summary    = pd.read_csv(os.path.join(results_dir, "data_quality_summary.csv"))
    attributes = ["Start Timestamp", "Complete Timestamp", "Case ID", "Activity Label", "Resource"]
    metrics    = ["Accuracy", "Consistency", "Completeness", "Overall Score"]
    table      = pd.DataFrame(index=metrics, columns=attributes, dtype=float)

    for _, row in summary.iterrows():
        attr = row["Attribute"]
        vals = {
            "Accuracy":     _load_pct(row["Accuracy"]),
            "Consistency":  _load_pct(row["Consistency"]),   # None for Case ID ("-")
            "Completeness": _load_pct(row["Completeness"]),
        }
        table.at["Accuracy",     attr] = vals["Accuracy"]
        table.at["Consistency",  attr] = vals["Consistency"]
        table.at["Completeness", attr] = vals["Completeness"]
        valid = [v for v in vals.values() if v is not None]
        table.at["Overall Score", attr] = sum(valid) / len(valid) if valid else None

    return table.reset_index().rename(columns={"index": "Metric"})


def build_table2(results_dir: str) -> pd.DataFrame:
    """
    Table 2 — Simulation Parameter Quality.
    Aggregates attribute-level dimension scores for each of the four BPS parameters.
    """
    param_path = os.path.join(results_dir, "simulation_parameter_quality.csv")
    if not os.path.isfile(param_path):
        return pd.DataFrame()

    # Load per-dimension scores for each attribute
    attr_dim: dict = {}
    attr_path = os.path.join(results_dir, "attribute_quality_scores.csv")
    if os.path.isfile(attr_path):
        for _, row in pd.read_csv(attr_path).iterrows():
            attr = str(row.get("Attribute", "")).strip()
            attr_dim[attr] = {
                "Accuracy":     _safe_float(row.get("Accuracy")),
                "Consistency":  _safe_float(row.get("Consistency")),
                "Completeness": _safe_float(row.get("Completeness")),
            }

    def dim_mean(dep_attrs: list, dim: str) -> float | None:
        vals = [attr_dim[a][dim] for a in dep_attrs
                if a in attr_dim and attr_dim[a][dim] is not None]
        return (sum(vals) / len(vals)) if vals else None

    # Attribute dependencies per simulation parameter
    # (must mirror calculate_simulation_parameter_quality.r)
    param_deps = {
        "Activity Duration":     ["Start Timestamp", "Complete Timestamp"],
        "Inter-arrival Time":    ["Case ID", "Start Timestamp"],
        "Routing Probabilities": ["Activity", "Start Timestamp", "Case ID"],
        "Resource Allocation":   ["Resource", "Activity"],
    }
    related_attrs = {
        "Activity Duration":     "Start Timestamp, End Timestamp",
        "Inter-arrival Time":    "Case ID, Start Timestamp",
        "Routing Probabilities": "Activity Label, Start Timestamp, Case ID",
        "Resource Allocation":   "Resource, Activity Label",
    }

    rows = []
    for param, dep_attrs in param_deps.items():
        acc  = dim_mean(dep_attrs, "Accuracy")
        con  = dim_mean(dep_attrs, "Consistency")
        comp = dim_mean(dep_attrs, "Completeness")
        valid = [v for v in [acc, con, comp] if v is not None]
        rows.append({
            "Simulation Parameter": param,
            "Related Attributes":   related_attrs[param],
            "Accuracy":             acc,
            "Consistency":          con,
            "Completeness":         comp,
            "Overall Score":        sum(valid) / len(valid) if valid else None,
        })
    return pd.DataFrame(rows)


def build_table3(results_dir: str) -> pd.DataFrame:
    """
    Table 3 — Detailed Analysis.
    Per-activity duration quality + per resource-activity quality.
    """
    act_path = os.path.join(results_dir, "per_activity_duration_quality.csv")
    res_path = os.path.join(results_dir, "resource_activity_quality.csv")
    parts    = []

    if os.path.isfile(act_path):
        act_df = pd.read_csv(act_path)
        act_df = act_df[
            ["Activity", "Accuracy", "Consistency", "Completeness", "Combined_Quality"]
        ].rename(columns={"Activity": "Entity", "Combined_Quality": "Overall Score"})
        act_df.insert(0, "Entity Type", "Activity Duration")
        parts.append(act_df)

    if os.path.isfile(res_path):
        res_df = pd.read_csv(res_path)
        res_df["Entity"] = (
            res_df["Resource"].astype(str) + " - " + res_df["Activity"].astype(str)
        )
        res_df = res_df[
            ["Entity", "Accuracy", "Consistency", "Completeness", "Combined_Quality"]
        ].rename(columns={"Combined_Quality": "Overall Score"})
        res_df.insert(0, "Entity Type", "Resource-Activity Relationship")
        parts.append(res_df)

    return pd.concat(parts, ignore_index=True) if parts else pd.DataFrame()


# ── Root cause table ─────────────────────────────────────────────────────────

def build_root_cause_table(results_dir: str) -> pd.DataFrame:
    """
    Root Cause Analysis table.
    Reads data_quality_root_cause_analysis.csv produced by the R pipeline.
    Returns an empty DataFrame if the file is absent (root cause output is optional).
    """
    rc_path = os.path.join(results_dir, "data_quality_root_cause_analysis.csv")
    if not os.path.isfile(rc_path):
        return pd.DataFrame()

    df = pd.read_csv(rc_path)

    # Friendly column order / rename for the Excel report
    col_order = [
        "case_id", "activity", "originator",
        "start", "complete",
        "actual_duration", "expected_median_duration",
        "diagnosis", "likely_issue", "specific_problem",
        "start_deviation_mins", "complete_deviation_mins", "normal_sample_size",
        "median_start", "median_complete",
    ]
    # Keep only columns that actually exist in the file (R output may vary)
    col_order = [c for c in col_order if c in df.columns]
    # Append any extra columns not in the preferred order
    extras = [c for c in df.columns if c not in col_order]
    df = df[col_order + extras]

    rename_map = {
        "case_id":                  "Case ID",
        "activity":                 "Activity",
        "originator":               "Resource",
        "start":                    "Start Timestamp",
        "complete":                 "Complete Timestamp",
        "actual_duration":          "Actual Duration (min)",
        "expected_median_duration": "Expected Duration (min)",
        "diagnosis":                "Diagnosis",
        "likely_issue":             "Likely Issue",
        "specific_problem":         "Specific Problem",
        "start_deviation_mins":     "Start Deviation (min)",
        "complete_deviation_mins":  "Complete Deviation (min)",
        "normal_sample_size":       "Sample Size",
        "median_start":             "Median Start",
        "median_complete":          "Median Complete",
    }
    df = df.rename(columns={k: v for k, v in rename_map.items() if k in df.columns})
    return df


# ── Excel export ──────────────────────────────────────────────────────────────

def save_excel_tables(
    results_dir: str,
    case_results_dir: str,
    log_name: str,
) -> tuple[str, str, str | None]:
    """
    Build all tables and save them as three Excel files in OUTPUT_DIR.
    Returns (attr_excel_path, param_excel_path, root_cause_excel_path).
    root_cause_excel_path is None when the R pipeline did not produce a root-cause file.
    """
    table1     = build_table1(results_dir, case_results_dir)
    table2     = build_table2(results_dir)
    table3     = build_table3(results_dir)
    table_rc   = build_root_cause_table(results_dir)

    os.makedirs(OUTPUT_DIR, exist_ok=True)

    attr_path  = os.path.join(OUTPUT_DIR, f"Assessment for {log_name} - Attributes.xlsx")
    param_path = os.path.join(OUTPUT_DIR, f"Assessment for {log_name} - Simulation Parameters.xlsx")

    with pd.ExcelWriter(attr_path, engine="openpyxl") as writer:
        table1.to_excel(writer, sheet_name="Attribute Quality", index=False)

    with pd.ExcelWriter(param_path, engine="openpyxl") as writer:
        table2.to_excel(writer, sheet_name="Simulation Parameter Quality", index=False)
        table3.to_excel(writer, sheet_name="Detailed Analysis",            index=False)

    # Root cause file — only written when data is available
    rc_path: str | None = None
    if not table_rc.empty:
        rc_path = os.path.join(OUTPUT_DIR, f"Assessment for {log_name} - Root Cause Analysis.xlsx")
        with pd.ExcelWriter(rc_path, engine="openpyxl") as writer:
            table_rc.to_excel(writer, sheet_name="Root Cause Analysis", index=False)

    return attr_path, param_path, rc_path
