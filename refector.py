"""Refector assessment tool.

This module provides the standalone entrypoint for the Refector assessment tool.
It uses the helper functions in refector_functions.core to select the event log,
derive configuration parameters, compute quality scores, and export Excel reports.
"""

import argparse
from typing import Optional

import pandas as pd

from evaluation_utils.core import (
    build_root_cause_report,
    compute_group_quality_scores,
    create_report,
    derive_configuration,
    export_excel_report,
    load_csv_data,
    run_assessment,
    save_json_data,
    select_event_log,
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run Refector assessment and export results.")
    parser.add_argument(
        "--input",
        help="Input CSV file path containing assessment data. If omitted, the GUI selection dialog is shown.",
    )
    parser.add_argument(
        "--output-json",
        default="output/assessment_summary.json",
        help="Output JSON summary file path.",
    )
    parser.add_argument(
        "--output-report",
        default="output/assessment_report.txt",
        help="Output human-readable report file path.",
    )
    parser.add_argument(
        "--output-excel",
        default="output/assessment_report.xlsx",
        help="Output Excel report file path.",
    )
    parser.add_argument(
        "--text-column",
        default="text",
        help="Name of the text column to preprocess.",
    )
    parser.add_argument(
        "--label-column",
        default="label",
        help="Name of the true label column.",
    )
    parser.add_argument(
        "--prediction-column",
        default="prediction",
        help="Name of the predicted label column.",
    )
    parser.add_argument(
        "--gui",
        action="store_true",
        help="Launch the simple GUI to select the event log CSV file.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    if args.gui or not args.input:
        args.input = select_event_log()
        if not args.input:
            raise SystemExit("No event log was selected.")

    df = load_csv_data(args.input)
    config = derive_configuration(
        df,
        label_column=args.label_column,
        prediction_column=args.prediction_column,
    )

    if config["label_column"] is None or config["prediction_column"] is None:
        available = ", ".join(df.columns)
        raise ValueError(
            "Could not infer label or prediction columns from the event log. "
            f"Available columns are: {available}. "
            "Please pass --label-column and --prediction-column if needed."
        )

    summary = run_assessment(
        df,
        text_column=args.text_column,
        label_column=config["label_column"],
        prediction_column=config["prediction_column"],
    )

    attribute_scores = pd.DataFrame()
    if config["attribute_columns"]:
        frames = []
        for attribute_column in config["attribute_columns"]:
            group_scores = compute_group_quality_scores(
                df,
                attribute_column,
                config["label_column"],
                config["prediction_column"],
            )
            group_scores.insert(0, "attribute", attribute_column)
            frames.append(group_scores)
        attribute_scores = pd.concat(frames, ignore_index=True)

    parameter_scores = pd.DataFrame()
    if config["simulation_columns"]:
        frames = []
        for parameter_column in config["simulation_columns"]:
            group_scores = compute_group_quality_scores(
                df,
                parameter_column,
                config["label_column"],
                config["prediction_column"],
            )
            group_scores.insert(0, "simulation_parameter", parameter_column)
            frames.append(group_scores)
        parameter_scores = pd.concat(frames, ignore_index=True)

    root_cause = build_root_cause_report(df, config)
    save_json_data({"summary": summary, "config": config}, args.output_json)
    create_report(summary, args.output_report)
    export_excel_report(
        summary,
        attribute_scores,
        parameter_scores,
        root_cause,
        args.output_excel,
    )

    print(f"Saved JSON summary to {args.output_json}")
    print(f"Saved report to {args.output_report}")
    print(f"Saved Excel report to {args.output_excel}")


if __name__ == "__main__":
    main()
