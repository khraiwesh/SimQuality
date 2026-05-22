"""
config.py
---------
Central configuration: resolved paths and column-name candidates.
Edit OUTPUT_DIR if you want results saved elsewhere.
"""

import os

# Project root = one level above this file (functions/)
_HERE        = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(_HERE)

TRUSTVALUES = os.path.join(PROJECT_ROOT, "quality_assessment_r_scripts")
MAIN_R      = os.path.join(TRUSTVALUES, "main.r")
OUTPUT_DIR  = os.path.join(PROJECT_ROOT, "Output")   # change if needed

# Column-name candidates → internal keys expected by main.r
COLUMN_CANDIDATES: dict[str, list[str]] = {
    "case_id":   ["Case", "case_id", "case id", "case:concept:name", "Case ID"],
    "activity":  ["Activity", "activity", "concept:name", "Task"],
    "resource":  ["Resource", "resource", "org:resource", "Originator"],
    "start":     ["Timestamp", "start_time", "start", "Start Timestamp", "time:start"],
    "complete":  ["Complete Timestamp", "end_time", "complete", "Complete", "time:timestamp"],
}
