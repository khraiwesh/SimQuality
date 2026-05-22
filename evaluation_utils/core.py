"""Core helper functions for the Refector assessment tool."""

import json
import os
from typing import Any, Dict, List, Optional, Sequence

import pandas as pd
from sklearn.metrics import accuracy_score, f1_score, precision_score, recall_score

try:
    import tkinter as tk
    from tkinter import filedialog, messagebox
except ImportError:
    tk = None
    filedialog = None
    messagebox = None


def ensure_directory(file_path: str) -> None:
    """Ensure the output directory exists."""
    directory = os.path.dirname(file_path)
    if directory:
        os.makedirs(directory, exist_ok=True)


def load_csv_data(file_path: str) -> pd.DataFrame:
    """Load CSV data from a file path."""
    return pd.read_csv(file_path)


def save_json_data(data: Any, file_path: str) -> None:
    """Save structured data as JSON."""
    ensure_directory(file_path)
    with open(file_path, "w", encoding="utf-8") as handle:
        json.dump(data, handle, indent=2, ensure_ascii=False)


def clean_text(text: str) -> str:
    """Normalize a text string by trimming whitespace and lowering case."""
    if text is None:
        return ""
    return " ".join(str(text).strip().split()).lower()


def preprocess_dataframe(df: pd.DataFrame, text_column: str = "text") -> pd.DataFrame:
    """Preprocess the text column in a DataFrame."""
    if text_column not in df.columns:
        raise ValueError(f"Text column '{text_column}' not found in DataFrame")

    df = df.copy()
    df[text_column] = df[text_column].astype(str).apply(clean_text)
    return df


def compute_quality_metrics(
    true_labels: List[Any], predicted_labels: List[Any]
) -> Dict[str, float]:
    """Compute standard classification quality metrics."""
    return {
        "accuracy": float(accuracy_score(true_labels, predicted_labels)),
        "precision": float(precision_score(true_labels, predicted_labels, average="weighted", zero_division=0)),
        "recall": float(recall_score(true_labels, predicted_labels, average="weighted", zero_division=0)),
        "f1_score": float(f1_score(true_labels, predicted_labels, average="weighted", zero_division=0)),
    }


def find_columns_by_keywords(columns: Sequence[str], keywords: Sequence[str]) -> List[str]:
    """Return columns whose names match any of the given keywords."""
    lowered = [col.lower() for col in columns]
    matches: List[str] = []
    for keyword in keywords:
        for col, lowered_col in zip(columns, lowered):
            if keyword in lowered_col and col not in matches:
                matches.append(col)
    return matches


def derive_configuration(
    df: pd.DataFrame,
    label_column: Optional[str] = None,
    prediction_column: Optional[str] = None,
) -> Dict[str, Any]:
    """Derive configuration parameters automatically from the event log."""
    columns = list(df.columns)
    case_columns = find_columns_by_keywords(columns, ["case_id", "case", "process_instance"])
    activity_columns = find_columns_by_keywords(columns, ["activity", "event", "task", "name"])
    timestamp_columns = find_columns_by_keywords(columns, ["timestamp", "time", "date", "datetime"])

    if label_column and label_column in columns:
        inferred_label = label_column
    else:
        label_candidates = find_columns_by_keywords(columns, ["label", "true_label", "outcome"])
        inferred_label = label_candidates[0] if label_candidates else None

    if prediction_column and prediction_column in columns:
        inferred_prediction = prediction_column
    else:
        prediction_candidates = find_columns_by_keywords(columns, ["prediction", "predicted", "pred"])
        inferred_prediction = prediction_candidates[0] if prediction_candidates else None

    simulation_columns = find_columns_by_keywords(columns, ["simulation", "sim", "param", "scenario", "config"])
    if not simulation_columns:
        simulation_columns = [
            col for col in columns
            if col not in {inferred_label, inferred_prediction}
            and col not in set(case_columns + activity_columns + timestamp_columns)
            and df[col].nunique() <= min(20, max(5, int(len(df) / 10)))
        ]

    attribute_columns = [
        col for col in columns
        if col not in {inferred_label, inferred_prediction}
        and col not in set(simulation_columns + case_columns + activity_columns + timestamp_columns)
    ]

    return {
        "case_column": case_columns[0] if case_columns else None,
        "activity_column": activity_columns[0] if activity_columns else None,
        "timestamp_column": timestamp_columns[0] if timestamp_columns else None,
        "label_column": inferred_label,
        "prediction_column": inferred_prediction,
        "attribute_columns": attribute_columns,
        "simulation_columns": simulation_columns,
    }


def compute_group_quality_scores(
    df: pd.DataFrame,
    group_column: str,
    label_column: str,
    prediction_column: str,
) -> pd.DataFrame:
    """Compute quality metrics for each value of a grouping column."""
    rows: List[Dict[str, Any]] = []
    for value, group in df.groupby(group_column, dropna=False):
        metrics = compute_quality_metrics(group[label_column].tolist(), group[prediction_column].tolist())
        rows.append(
            {
                group_column: value,
                "count": len(group),
                **metrics,
            }
        )
    return pd.DataFrame(rows)


def build_root_cause_report(
    df: pd.DataFrame,
    config: Dict[str, Any],
) -> pd.DataFrame:
    """Create a root-cause report for events that do not match labels and predictions."""
    if config["label_column"] is None or config["prediction_column"] is None:
        return pd.DataFrame()

    mismatch = df[df[config["label_column"]] != df[config["prediction_column"]]].copy()
    if mismatch.empty:
        return pd.DataFrame()

    event_columns = [
        col for col in [config["case_column"], config["activity_column"], config["timestamp_column"]]
        if col is not None and col in df.columns
    ]

    rows: List[Dict[str, Any]] = []
    for index, row in mismatch.iterrows():
        item = {
            "event_index": int(index),
            "label": row[config["label_column"]],
            "prediction": row[config["prediction_column"]],
            "issue_type": "label_prediction_mismatch",
            "quality_dimensions": "accuracy",
        }
        for event_column in event_columns:
            item[event_column] = row[event_column]
        rows.append(item)

    return pd.DataFrame(rows)


def export_excel_report(
    summary: Dict[str, Any],
    attribute_scores: pd.DataFrame,
    parameter_scores: pd.DataFrame,
    root_cause: pd.DataFrame,
    output_path: str,
) -> None:
    """Export assessment results to an Excel workbook."""
    ensure_directory(output_path)
    with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
        pd.DataFrame([summary]).to_excel(writer, sheet_name="Summary", index=False)
        if not attribute_scores.empty:
            attribute_scores.to_excel(writer, sheet_name="AttributeScores", index=False)
        if not parameter_scores.empty:
            parameter_scores.to_excel(writer, sheet_name="SimulationScores", index=False)
        if not root_cause.empty:
            root_cause.to_excel(writer, sheet_name="RootCause", index=False)


def create_report(summary: Dict[str, Any], output_path: str) -> None:
    """Write a human-readable analysis report to a text file."""
    ensure_directory(output_path)
    with open(output_path, "w", encoding="utf-8") as handle:
        handle.write("Refector Assessment Report\n")
        handle.write("===========================\n\n")
        handle.write(f"Records processed: {summary['record_count']}\n")
        handle.write(f"Label field: {summary['label_column']}\n")
        handle.write(f"Prediction field: {summary['prediction_column']}\n")
        handle.write("Metrics:\n")
        for metric_name, metric_value in summary["metrics"].items():
            handle.write(f"- {metric_name}: {metric_value:.4f}\n")


def run_assessment(
    df: pd.DataFrame,
    text_column: str = "text",
    label_column: str = "label",
    prediction_column: str = "prediction",
) -> Dict[str, Any]:
    """Run the core assessment flow on a DataFrame."""
    if label_column not in df.columns or prediction_column not in df.columns:
        raise ValueError(
            f"Required columns not found. Need '{label_column}' and '{prediction_column}'"
        )

    df = preprocess_dataframe(df, text_column=text_column)
    metrics = compute_quality_metrics(df[label_column].tolist(), df[prediction_column].tolist())

    return {
        "record_count": len(df),
        "text_column": text_column,
        "label_column": label_column,
        "prediction_column": prediction_column,
        "metrics": metrics,
    }


def select_event_log() -> Optional[str]:
    """Open a simple file dialog so the user can select an event log CSV."""
    if filedialog is None:
        raise RuntimeError("Tkinter is not available in this environment")

    root = tk.Tk()
    root.withdraw()
    file_path = filedialog.askopenfilename(
        title="Select event log CSV file",
        filetypes=[("CSV files", "*.csv"), ("All files", "*")],
    )
    root.destroy()
    return file_path
