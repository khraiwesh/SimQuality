"""
generate_synthetic_logs.py
--------------------------
Generates clean and noisy synthetic event logs for testing the quality assessment tool.

Output structure:
  Clean:    datasetwith{N}cases_clean.csv
  Separate: datasetwith{N}cases_{ErrorType}_{Dimension}_{pct}%.csv
  Combined: datasetwith{N}cases_Combined_{pct}%.csv

Error types   : Timestamp, ActivityLabel, CaseID, Resource
Dimensions    : Accuracy, Completeness, Consistency
Sizes         : 1000, 5000, 10000 cases
Ratios        : 5%, 10%, 20%
"""

import os
import random
from datetime import datetime, timedelta
import pandas as pd

# Save generated logs in the same Datasets/ folder as this script
OUTPUT_DIR = os.path.dirname(os.path.abspath(__file__))
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ── Process definition ────────────────────────────────────────────────────────

EXPECTED_RESOURCES = {
    "Patient Registration": "Receptionist 1",
    "Diagnosis":            "Doctor 1",
    "Lab Test":             "Lab Technician 1",
    "X-Ray Scan":           "Radiologist 1",
    "Treatment Evaluation": "Nurse 1",
}

DURATIONS = {                     # (mean_min, std_min)
    "Patient Registration": (20,  5),
    "Diagnosis":            (40, 10),
    "Lab Test":             (60, 15),
    "X-Ray Scan":           (45, 12),
    "Treatment Evaluation": (30,  8),
}

WRONG_ACTIVITIES = ["Surgery", "Consultation", "Triage", "Discharge", "Observation"]

TYPO_ACTIVITIES = {
    "Patient Registration": ["Patient Registrtion", "Patient Registeration", "Patiant Registration"],
    "Diagnosis":            ["Diagnosiss", "Diagnosi", "Diagonsis"],
    "Lab Test":             ["Lab test", "Lab Tset", "Labtest"],
    "X-Ray Scan":           ["X-ray Scan", "XRay Scan", "X-Ray scan"],
    "Treatment Evaluation": ["Treatment Evaluaton", "Treatement Evaluation", "Treatment Evalution"],
}

TYPO_RESOURCES = {
    "Receptionist 1":  ["Receptionist1", "receptionist 1", "Receptonist 1"],
    "Doctor 1":        ["Doctor1", "doctor 1", "Dr. 1"],
    "Lab Technician 1":["Lab Tech 1", "LabTechnician1", "Lab technician 1"],
    "Radiologist 1":   ["Radiologist1", "radiologist 1", "Radiolojist 1"],
    "Nurse 1":         ["Nurse1", "nurse 1", "Nursse 1"],
}

TS_FMT = "%Y-%m-%d %H:%M:%S"

# ── Base log generator ────────────────────────────────────────────────────────

def generate_base_log(n_cases: int, seed: int = 42) -> pd.DataFrame:
    rng = random.Random(seed)
    rows = []
    t = datetime(2025, 1, 1, 8, 0, 0)

    for case_id in range(1, n_cases + 1):
        t += timedelta(minutes=rng.uniform(5, 15))   # inter-arrival
        cur = t

        sequence = [
            "Patient Registration",
            "Diagnosis",
            rng.choice(["Lab Test", "X-Ray Scan"]),
            "Treatment Evaluation",
        ]

        for activity in sequence:
            mean_d, std_d = DURATIONS[activity]
            duration = max(5.0, rng.gauss(mean_d, std_d))
            complete = cur + timedelta(minutes=duration)

            rows.append({
                "Case":               case_id,
                "Activity":           activity,
                "Timestamp":          cur.strftime(TS_FMT),
                "Complete Timestamp": complete.strftime(TS_FMT),
                "Resource":           EXPECTED_RESOURCES[activity],
            })
            cur = complete + timedelta(minutes=rng.uniform(0, 5))

    return pd.DataFrame(rows)


# ── Noise injectors ───────────────────────────────────────────────────────────

def _sample(df, ratio, rng):
    n = max(1, int(len(df) * ratio))
    return rng.sample(range(len(df)), min(n, len(df)))


# Timestamp ───────────────────────────────────────────────────────────────────

def inject_timestamp_accuracy(df, ratio, rng):
    """Negative durations (swap start/complete) + extreme outlier start times."""
    df = df.copy()
    idx = _sample(df, ratio, rng)
    half = len(idx) // 2
    # First half: swap → negative duration
    for i in idx[:half]:
        df.at[i, "Timestamp"], df.at[i, "Complete Timestamp"] = \
            df.at[i, "Complete Timestamp"], df.at[i, "Timestamp"]
    # Second half: push start far into future → complete < start
    for i in idx[half:]:
        try:
            ts = datetime.strptime(df.at[i, "Timestamp"], TS_FMT)
            df.at[i, "Timestamp"] = (ts + timedelta(hours=rng.uniform(6, 72))).strftime(TS_FMT)
        except Exception:
            pass
    return df


def inject_timestamp_completeness(df, ratio, rng):
    """Missing start or complete timestamps (NaN)."""
    df = df.copy()
    idx = _sample(df, ratio, rng)
    half = len(idx) // 2
    for i in idx[:half]:
        df.at[i, "Timestamp"] = None
    for i in idx[half:]:
        df.at[i, "Complete Timestamp"] = None
    return df


def inject_timestamp_consistency(df, ratio, rng):
    """Mixed timestamp formats in the same column."""
    df = df.copy()
    alt_formats = ["%d/%m/%Y %H:%M:%S", "%d.%m.%Y %H:%M", "%Y/%m/%d %H:%M:%S", "%d-%m-%Y %H:%M"]
    for i in _sample(df, ratio, rng):
        try:
            ts = datetime.strptime(df.at[i, "Timestamp"], TS_FMT)
            df.at[i, "Timestamp"] = ts.strftime(rng.choice(alt_formats))
        except Exception:
            pass
    return df


# Activity Label ──────────────────────────────────────────────────────────────

def inject_activity_accuracy(df, ratio, rng):
    """Replace with completely wrong activity names."""
    df = df.copy()
    for i in _sample(df, ratio, rng):
        df.at[i, "Activity"] = rng.choice(WRONG_ACTIVITIES)
    return df


def inject_activity_completeness(df, ratio, rng):
    """Missing activity label (NaN)."""
    df = df.copy()
    for i in _sample(df, ratio, rng):
        df.at[i, "Activity"] = None
    return df


def inject_activity_consistency(df, ratio, rng):
    """Typo/variant activity names."""
    df = df.copy()
    for i in _sample(df, ratio, rng):
        act = df.at[i, "Activity"]
        if act in TYPO_ACTIVITIES:
            df.at[i, "Activity"] = rng.choice(TYPO_ACTIVITIES[act])
    return df


# Case ID ─────────────────────────────────────────────────────────────────────

def inject_caseid_accuracy(df, ratio, rng):
    """Replace case IDs with wrong/out-of-range IDs."""
    df = df.copy()
    max_id = int(df["Case"].max())
    for i in _sample(df, ratio, rng):
        df.at[i, "Case"] = rng.randint(max_id + 1000, max_id + 99999)
    return df


def inject_caseid_completeness(df, ratio, rng):
    """Missing case IDs (NaN)."""
    df = df.copy()
    for i in _sample(df, ratio, rng):
        df.at[i, "Case"] = None
    return df


def inject_caseid_consistency(df, ratio, rng):
    """Gaps in case ID sequence by renumbering some cases to non-sequential IDs."""
    df = df.copy()
    cases = sorted(df["Case"].dropna().unique().tolist())
    n_to_gap = max(1, int(len(cases) * ratio))
    cases_to_change = rng.sample(cases, n_to_gap)
    offset = int(df["Case"].max()) + 1000
    for case in cases_to_change:
        offset += rng.randint(50, 500)          # non-sequential jump
        df.loc[df["Case"] == case, "Case"] = offset
    return df


# Resource ────────────────────────────────────────────────────────────────────

def inject_resource_accuracy(df, ratio, rng):
    """Wrong resource for an activity (role violation)."""
    df = df.copy()
    all_resources = list(EXPECTED_RESOURCES.values())
    for i in _sample(df, ratio, rng):
        correct = df.at[i, "Resource"]
        wrong   = [r for r in all_resources if r != correct]
        if wrong:
            df.at[i, "Resource"] = rng.choice(wrong)
    return df


def inject_resource_completeness(df, ratio, rng):
    """Missing resource (NaN)."""
    df = df.copy()
    for i in _sample(df, ratio, rng):
        df.at[i, "Resource"] = None
    return df


def inject_resource_consistency(df, ratio, rng):
    """Same resource listed with different spelling variants."""
    df = df.copy()
    for i in _sample(df, ratio, rng):
        res = df.at[i, "Resource"]
        if res in TYPO_RESOURCES:
            df.at[i, "Resource"] = rng.choice(TYPO_RESOURCES[res])
    return df


# ── Registry ──────────────────────────────────────────────────────────────────

INJECTORS = {
    ("Timestamp",     "Accuracy"):     inject_timestamp_accuracy,
    ("Timestamp",     "Completeness"): inject_timestamp_completeness,
    ("Timestamp",     "Consistency"):  inject_timestamp_consistency,
    ("ActivityLabel", "Accuracy"):     inject_activity_accuracy,
    ("ActivityLabel", "Completeness"): inject_activity_completeness,
    ("ActivityLabel", "Consistency"):  inject_activity_consistency,
    ("CaseID",        "Accuracy"):     inject_caseid_accuracy,
    ("CaseID",        "Completeness"): inject_caseid_completeness,
    ("CaseID",        "Consistency"):  inject_caseid_consistency,
    ("Resource",      "Accuracy"):     inject_resource_accuracy,
    ("Resource",      "Completeness"): inject_resource_completeness,
    ("Resource",      "Consistency"):  inject_resource_consistency,
}

SIZES  = [1000, 5000, 10000]
RATIOS = [0.05, 0.10, 0.20]


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    total = 0

    for n in SIZES:
        print(f"\n{'='*60}")
        print(f"  Generating logs for {n:,} cases")
        print(f"{'='*60}")

        base_df = generate_base_log(n, seed=42)

        # ── Clean ──────────────────────────────────────────────────────────
        fname = f"datasetwith{n}cases_clean.csv"
        base_df.to_csv(os.path.join(OUTPUT_DIR, fname), index=False)
        print(f"  [CLEAN]    {fname}  ({len(base_df):,} rows)")
        total += 1

        # ── Separate noise ─────────────────────────────────────────────────
        for (err_type, dimension), injector in INJECTORS.items():
            for ratio in RATIOS:
                pct = int(ratio * 100)
                rng_local = random.Random(42 + n + pct + hash(err_type + dimension) % 10000)
                noisy_df = injector(base_df, ratio, rng_local)
                fname = f"datasetwith{n}cases_{err_type}_{dimension}_{pct}%.csv"
                noisy_df.to_csv(os.path.join(OUTPUT_DIR, fname), index=False)
                print(f"  [SEPARATE] {fname}")
                total += 1

        # ── Combined noise ─────────────────────────────────────────────────
        for ratio in RATIOS:
            pct = int(ratio * 100)
            rng_local = random.Random(42 + n + pct + 99999)
            combined_df = base_df.copy()
            for (_, __), injector in INJECTORS.items():
                combined_df = injector(combined_df, ratio, rng_local)
            fname = f"datasetwith{n}cases_Combined_{pct}%.csv"
            combined_df.to_csv(os.path.join(OUTPUT_DIR, fname), index=False)
            print(f"  [COMBINED] {fname}")
            total += 1

    print(f"\n{'='*60}")
    print(f"  Done!  {total} files generated.")
    print(f"  Output: {OUTPUT_DIR}")
    print(f"{'='*60}")


if __name__ == "__main__":
    main()
