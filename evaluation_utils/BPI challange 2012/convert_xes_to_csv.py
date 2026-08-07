"""
convert_xes_to_csv.py
---------------------
Converts a .xes event log to a CSV compatible with the quality assessment tool.

Output columns:
  Case, Activity, Resource, Timestamp (start), Complete Timestamp
"""

import sys
import os
import pandas as pd
import pm4py

_HERE    = os.path.dirname(os.path.abspath(__file__))
XES_PATH = os.path.join(_HERE, "BPI_Challenge_2012 .xes")
OUT_DIR  = _HERE

os.makedirs(OUT_DIR, exist_ok=True)

print(f"Reading: {XES_PATH}")
log = pm4py.read_xes(XES_PATH)
df  = pm4py.convert_to_dataframe(log)

print("Columns found in XES:")
for c in df.columns.tolist():
    print(f"  {c}")

print(f"\nShape: {df.shape}")

TS_FMT = "%Y-%m-%d %H:%M:%S"

# ── Normalise timestamp column ────────────────────────────────────────────────
df["time:timestamp"] = pd.to_datetime(df["time:timestamp"], utc=True, errors="coerce") \
                         .dt.tz_convert(None)

# ── Handle lifecycle:transition (start / complete pairing) ────────────────────
if "lifecycle:transition" in df.columns:
    print("\nLifecycle transitions detected — pairing start/complete events.")

    # Normalise transition labels to lowercase
    df["lifecycle:transition"] = df["lifecycle:transition"].str.lower().str.strip()

    start_df    = df[df["lifecycle:transition"] == "start"].copy()
    complete_df = df[df["lifecycle:transition"].isin(["complete", "ate_abort"])].copy()

    if start_df.empty:
        # No start events — use time:timestamp as both columns
        print("  No 'start' events found — using time:timestamp for both columns.")
        result = complete_df.copy() if not complete_df.empty else df.copy()
        result = result.rename(columns={
            "case:concept:name": "Case",
            "concept:name":      "Activity",
            "org:resource":      "Resource",
            "time:timestamp":    "Complete Timestamp",
        })
        result["Timestamp"] = result["Complete Timestamp"]
    else:
        # Add sequence counter per (case, activity) to align multiple occurrences
        for subset in [start_df, complete_df]:
            subset["_seq"] = subset.groupby(
                ["case:concept:name", "concept:name"]
            ).cumcount()

        merged = pd.merge(
            start_df[["case:concept:name", "concept:name", "org:resource",
                       "time:timestamp", "_seq"]].rename(
                columns={"time:timestamp": "Timestamp"}),
            complete_df[["case:concept:name", "concept:name",
                          "time:timestamp", "_seq"]].rename(
                columns={"time:timestamp": "Complete Timestamp"}),
            on=["case:concept:name", "concept:name", "_seq"],
            how="outer",
        )
        # Fill missing start with complete if one side is missing
        merged["Timestamp"].fillna(merged["Complete Timestamp"], inplace=True)
        merged["Complete Timestamp"].fillna(merged["Timestamp"], inplace=True)

        result = merged.rename(columns={
            "case:concept:name": "Case",
            "concept:name":      "Activity",
            "org:resource":      "Resource",
        }).drop(columns=["_seq"])
else:
    print("\nNo lifecycle column — using time:timestamp as Complete Timestamp.")
    result = df.rename(columns={
        "case:concept:name": "Case",
        "concept:name":      "Activity",
        "org:resource":      "Resource",
        "time:timestamp":    "Complete Timestamp",
    })
    result["Timestamp"] = result["Complete Timestamp"]

# ── Keep only required columns ────────────────────────────────────────────────
keep = ["Case", "Activity", "Resource", "Timestamp", "Complete Timestamp"]
keep = [c for c in keep if c in result.columns]
df = result[keep].copy()

if "Resource" not in df.columns:
    df["Resource"] = None

# ── Format timestamps as strings ──────────────────────────────────────────────
for col in ["Timestamp", "Complete Timestamp"]:
    if col in df.columns:
        df[col] = pd.to_datetime(df[col], errors="coerce").dt.strftime(TS_FMT)

print(f"\nFinal columns: {df.columns.tolist()}")
print(f"Final shape:   {df.shape}")
print(df.head(3).to_string())

out_path = os.path.join(OUT_DIR, "BPIChallenge2012.csv")
df.to_csv(out_path, index=False)
print(f"\nSaved: {out_path}")
