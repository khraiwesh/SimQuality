"""
log_distance_runner.py
----------------------
Runs all log-distance measures between one original event log
and one or more simulated event logs.

Uses the third-party `log_distance` package (see log_distance/README.md).
Column names are auto-detected using the same detector as the quality tool.

Output: one Excel file per original log in the Output/ folder.
"""

import datetime
import math
import os
import sys

import pandas as pd

# ── Path setup ────────────────────────────────────────────────────────────────
_HERE = os.path.dirname(os.path.abspath(__file__))
_ROOT = os.path.dirname(_HERE)
if _ROOT not in sys.path:
    sys.path.insert(0, _ROOT)

from log_distance.config import DistanceMetric, EventLogIDs
from log_distance import (
    absolute_event_distribution,
    case_arrival_distribution,
    circadian_event_distribution,
    circadian_workforce_distribution,
    cycle_time_distribution,
    n_gram_distribution,
    relative_event_distribution,
    work_in_progress,
)
from pipeline_modules.column_detector import detect_columns
from pipeline_modules.config import OUTPUT_DIR


# ── Column mapping ────────────────────────────────────────────────────────────

def _make_ids(csv_path: str) -> EventLogIDs:
    """Auto-detect CSV columns and return a matching EventLogIDs."""
    col_map = detect_columns(csv_path)
    return EventLogIDs(
        case       = col_map.get("case_id",  "case_id"),
        activity   = col_map.get("activity", "activity"),
        start_time = col_map.get("start",    "start_time"),
        end_time   = col_map.get("complete", "end_time"),
        resource   = col_map.get("resource", "resource"),
    )


def _load(csv_path: str) -> tuple[pd.DataFrame, EventLogIDs]:
    """Read CSV, parse timestamps, return (DataFrame, EventLogIDs)."""
    ids = _make_ids(csv_path)
    df  = pd.read_csv(csv_path)

    for col in (ids.start_time, ids.end_time):
        if col in df.columns:
            df[col] = pd.to_datetime(df[col], utc=True, errors="coerce")

    return df, ids


# ── Distance metric catalogue ─────────────────────────────────────────────────
#
# Each entry: (display_name, BPS_parameter_group, callable)
# callable signature: (orig_df, orig_ids, sim_df, sim_ids, bin_size) → float
#
# BPS parameter groups (for the second Excel sheet):
#   AD  = Activity Duration
#   IAT = Inter-arrival Time
#   RP  = Routing Probabilities
#   RA  = Resource Allocation

_METRICS: list[tuple[str, str, callable]] = [
    # Control flow → Routing Probabilities
    ("Bigram Distance (N=2)",           "RP",
     lambda o, oi, s, si, bs: n_gram_distribution.n_gram_distribution_distance(o, oi, s, si, n=2)),
    ("Trigram Distance (N=3)",          "RP",
     lambda o, oi, s, si, bs: n_gram_distribution.n_gram_distribution_distance(o, oi, s, si, n=3)),

    # Timing → Activity Duration / IAT
    ("Cycle Time Distance (Wass.)",     "AD",
     lambda o, oi, s, si, bs: cycle_time_distribution.cycle_time_distribution_distance(o, oi, s, si, bs, DistanceMetric.WASSERSTEIN)),
    ("Case Arrival EMD",                "IAT",
     lambda o, oi, s, si, bs: case_arrival_distribution.case_arrival_distribution_distance(o, oi, s, si, DistanceMetric.EMD)),
    ("Case Arrival Wasserstein",        "IAT",
     lambda o, oi, s, si, bs: case_arrival_distribution.case_arrival_distribution_distance(o, oi, s, si, DistanceMetric.WASSERSTEIN)),

    # Resource → Resource Allocation
    ("Circadian Workforce EMD",         "RA",
     lambda o, oi, s, si, bs: circadian_workforce_distribution.circadian_workforce_distribution_distance(o, oi, s, si, DistanceMetric.EMD)),
    ("Circadian Workforce Wasserstein", "RA",
     lambda o, oi, s, si, bs: circadian_workforce_distribution.circadian_workforce_distribution_distance(o, oi, s, si, DistanceMetric.WASSERSTEIN)),

    # Global timing
    ("Absolute Event EMD",              "AD",
     lambda o, oi, s, si, bs: absolute_event_distribution.absolute_event_distribution_distance(o, oi, s, si, DistanceMetric.EMD)),
    ("Absolute Event Wasserstein",      "AD",
     lambda o, oi, s, si, bs: absolute_event_distribution.absolute_event_distribution_distance(o, oi, s, si, DistanceMetric.WASSERSTEIN)),
    ("Circadian Event EMD",             "AD",
     lambda o, oi, s, si, bs: circadian_event_distribution.circadian_event_distribution_distance(o, oi, s, si, DistanceMetric.EMD)),
    ("Circadian Event Wasserstein",     "AD",
     lambda o, oi, s, si, bs: circadian_event_distribution.circadian_event_distribution_distance(o, oi, s, si, DistanceMetric.WASSERSTEIN)),
    ("Relative Event EMD",              "AD",
     lambda o, oi, s, si, bs: relative_event_distribution.relative_event_distribution_distance(o, oi, s, si, DistanceMetric.EMD)),
    ("Relative Event Wasserstein",      "AD",
     lambda o, oi, s, si, bs: relative_event_distribution.relative_event_distribution_distance(o, oi, s, si, DistanceMetric.WASSERSTEIN)),

    # Work in progress
    ("Work-in-Progress Distance",       "AD",
     lambda o, oi, s, si, bs: work_in_progress.work_in_progress_distance(o, oi, s, si, DistanceMetric.EMD)),
]


# ── Core runner ───────────────────────────────────────────────────────────────

def _cycle_time_bin_size(orig_df: pd.DataFrame, ids: EventLogIDs) -> datetime.timedelta:
    """Compute bin size for cycle-time metric (max case duration / 1000)."""
    try:
        durations = [
            events[ids.end_time].max() - events[ids.start_time].min()
            for _, events in orig_df.groupby(ids.case)
            if events[ids.end_time].notna().any() and events[ids.start_time].notna().any()
        ]
        if durations:
            return max(durations) / 1000
    except Exception:
        pass
    return datetime.timedelta(hours=1)   # safe fallback


def run_log_distance(
    original_path: str,
    simulated_paths: list[str],
    log_lines: list[str],
) -> dict:
    """
    Compute all log-distance metrics for each simulated log vs the original.

    Returns a dict:
        ok       – True on success
        error    – error message on failure
        excel    – path to the saved Excel file (or None)
        rows     – list of result dicts (one per simulated log)
    """
    # Load original log
    try:
        orig_df, orig_ids = _load(original_path)
        log_lines.append(f"  Original log loaded: {len(orig_df)} rows, "
                         f"columns → case={orig_ids.case}, activity={orig_ids.activity}, "
                         f"start={orig_ids.start_time}, end={orig_ids.end_time}")
    except Exception as exc:
        return {"ok": False, "error": f"Cannot load original log: {exc}", "excel": None, "rows": []}

    bin_size = _cycle_time_bin_size(orig_df, orig_ids)

    rows: list[dict] = []
    for sim_path in simulated_paths:
        sim_name = os.path.splitext(os.path.basename(sim_path))[0]
        log_lines.append(f"\n  ── {sim_name} ──")

        try:
            sim_df, sim_ids = _load(sim_path)
            log_lines.append(f"    Loaded: {len(sim_df)} rows")
        except Exception as exc:
            log_lines.append(f"    ✗ Load failed: {exc}")
            rows.append({"Simulated Log": sim_name, **{m[0]: None for m in _METRICS}})
            continue

        row: dict = {"Simulated Log": sim_name}
        for metric_name, _bps_group, metric_fn in _METRICS:
            try:
                val = metric_fn(orig_df, orig_ids, sim_df, sim_ids, bin_size)
                row[metric_name] = round(float(val), 6)
                log_lines.append(f"    {metric_name}: {row[metric_name]:.4f}")
            except Exception as exc:
                row[metric_name] = None
                log_lines.append(f"    {metric_name}: ERROR — {exc}")

        rows.append(row)

    # Save Excel
    excel_path = _save_excel(original_path, rows, log_lines)

    return {
        "ok":    True,
        "error": "",
        "excel": excel_path,
        "rows":  rows,
    }


# ── Excel export ──────────────────────────────────────────────────────────────

def _save_excel(original_path: str, rows: list[dict], log_lines: list[str]) -> str | None:
    """Write results + BPS mapping sheet to an Excel file in OUTPUT_DIR."""
    if not rows:
        return None

    orig_name = os.path.splitext(os.path.basename(original_path))[0]
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    out_path = os.path.join(OUTPUT_DIR, f"Log Distance - {orig_name}.xlsx")

    results_df = pd.DataFrame(rows)

    # BPS parameter mapping reference sheet
    mapping_df = pd.DataFrame(
        [(name, grp) for name, grp, _ in _METRICS],
        columns=["Metric", "BPS Parameter Group"],
    )
    bps_labels = {"AD": "Activity Duration", "IAT": "Inter-arrival Time",
                  "RP": "Routing Probabilities", "RA": "Resource Allocation"}
    mapping_df["BPS Parameter"] = mapping_df["BPS Parameter Group"].map(bps_labels)

    try:
        with pd.ExcelWriter(out_path, engine="openpyxl") as writer:
            results_df.to_excel(writer, sheet_name="Distance Metrics",   index=False)
            mapping_df.to_excel(writer, sheet_name="BPS Parameter Mapping", index=False)
        log_lines.append(f"\n  ✔ Saved → {out_path}")
        return out_path
    except Exception as exc:
        log_lines.append(f"\n  ✗ Excel export failed: {exc}")
        return None
