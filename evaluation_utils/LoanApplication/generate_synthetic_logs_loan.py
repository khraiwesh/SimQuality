"""
generate_synthetic_logs_loan.py
--------------------------------
Generates clean and noisy synthetic event logs for the Loan Application process
for testing the quality assessment tool.

Process model : Loan Application (10 activities, 8 resources, 2 XOR gateways)
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

# Save generated logs in the same folder as this script
OUTPUT_DIR = os.path.dirname(os.path.abspath(__file__))
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ── Process definition ────────────────────────────────────────────────────────
#
# Flow:
#   Application Receipt
#       → Document Verification
#       → Credit Check
#       → Risk Assessment
#       → [XOR: 30% complex] → Collateral Evaluation (optional)
#       → Approval Decision
#       → [XOR: 70% approved] → Loan Agreement Preparation → Loan Disbursement
#                  30% rejected  → Rejection Notification
#       → Case Closure

EXPECTED_RESOURCES = {
    "Application Receipt":        "Loan Officer 1",
    "Document Verification":      "Clerk 1",
    "Credit Check":               "Credit Analyst 1",
    "Risk Assessment":            "Risk Analyst 1",
    "Collateral Evaluation":      "Appraiser 1",
    "Approval Decision":          "Manager 1",
    "Loan Agreement Preparation": "Legal Advisor 1",
    "Rejection Notification":     "Clerk 1",
    "Loan Disbursement":          "Bank Teller 1",
    "Case Closure":               "Loan Officer 1",
}

DURATIONS = {                           # (mean_min, std_min)
    "Application Receipt":        (15,  4),
    "Document Verification":      (30,  8),
    "Credit Check":               (50, 12),
    "Risk Assessment":            (40, 10),
    "Collateral Evaluation":      (90, 20),
    "Approval Decision":          (25,  6),
    "Loan Agreement Preparation": (45, 10),
    "Rejection Notification":     (10,  3),
    "Loan Disbursement":          (20,  5),
    "Case Closure":               (10,  3),
}

WRONG_ACTIVITIES = [
    "Background Check", "Property Survey", "Notarization",
    "Tax Assessment", "Insurance Verification"
]

TYPO_ACTIVITIES = {
    "Application Receipt":        ["Aplication Receipt", "Application Reciept", "Applicaton Receipt"],
    "Document Verification":      ["Document Varification", "Docuemnt Verification", "Document Verifcation"],
    "Credit Check":               ["Credut Check", "Credit Chek", "Crdit Check"],
    "Risk Assessment":            ["Risk Assesment", "Rish Assessment", "Risk Assessmnt"],
    "Collateral Evaluation":      ["Colateral Evaluation", "Collateral Evaluaton", "Colleteral Evaluation"],
    "Approval Decision":          ["Aproval Decision", "Approval Decisoin", "Approvel Decision"],
    "Loan Agreement Preparation": ["Loan Agrement Preparation", "Loan Agreement Prepartion", "Loan Agreemnt Preparation"],
    "Rejection Notification":     ["Rejecton Notification", "Rejection Notifcation", "Rejction Notification"],
    "Loan Disbursement":          ["Loan Disburesment", "Loan Disbursmnt", "Loan Disbusement"],
    "Case Closure":               ["Case Closre", "Case Cloasure", "Caes Closure"],
}

TYPO_RESOURCES = {
    "Loan Officer 1":   ["LoanOfficer1", "loan officer 1", "LOAN OFFICER 1"],
    "Clerk 1":          ["Clerk1", "clerk 1", "CLERK 1"],
    "Credit Analyst 1": ["CreditAnalyst1", "credit analyst 1", "CREDIT ANALYST 1"],
    "Risk Analyst 1":   ["RiskAnalyst1", "risk analyst 1", "RISK ANALYST 1"],
    "Appraiser 1":      ["Appraiser1", "appraiser 1", "APPRAISER 1"],
    "Manager 1":        ["Manager1", "manager 1", "MANAGER 1"],
    "Legal Advisor 1":  ["LegalAdvisor1", "legal advisor 1", "LEGAL ADVISOR 1"],
    "Bank Teller 1":    ["BankTeller1", "bank teller 1", "BANK TELLER 1"],
}

TS_FMT = "%Y-%m-%d %H:%M:%S"

# ── Base log generator ────────────────────────────────────────────────────────

def generate_base_log(n_cases: int, seed: int = 42) -> pd.DataFrame:
    rng = random.Random(seed)
    rows = []
    t = datetime(2025, 1, 1, 8, 0, 0)

    for case_id in range(1, n_cases + 1):
        t += timedelta(minutes=rng.uniform(10, 30))  # inter-arrival

        sequence = [
            "Application Receipt",
            "Document Verification",
            "Credit Check",
            "Risk Assessment",
        ]

        # XOR 1: 30% complex loans require collateral evaluation
        if rng.random() < 0.3:
            sequence.append("Collateral Evaluation")

        sequence.append("Approval Decision")

        # XOR 2: 70% approved, 30% rejected
        if rng.random() < 0.7:
            sequence += ["Loan Agreement Preparation", "Loan Disbursement"]
        else:
            sequence.append("Rejection Notification")

        sequence.append("Case Closure")

        cur = t
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
            cur = complete + timedelta(minutes=rng.uniform(0, 10))

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
    for i in idx[:half]:
        df.at[i, "Timestamp"], df.at[i, "Complete Timestamp"] = \
            df.at[i, "Complete Timestamp"], df.at[i, "Timestamp"]
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
    for i in _sample(df, ratio, rng):
        df.at[i, "Timestamp"] = None
    for i in _sample(df, ratio, rng):
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
    """Replace one activity per case with a wrong name for ratio of cases."""
    df = df.copy()
    cases = df["Case"].dropna().unique().tolist()
    n_to_affect = max(1, int(len(cases) * ratio))
    affected_cases = rng.sample(cases, min(n_to_affect, len(cases)))
    for case in affected_cases:
        case_rows = df.index[df["Case"] == case].tolist()
        if case_rows:
            i = rng.choice(case_rows)
            df.at[i, "Activity"] = rng.choice(WRONG_ACTIVITIES)
    return df


def inject_activity_completeness(df, ratio, rng):
    """Remove one activity row per case for ratio of cases."""
    df = df.copy()
    cases = df["Case"].dropna().unique().tolist()
    n_to_affect = max(1, int(len(cases) * ratio))
    affected_cases = rng.sample(cases, min(n_to_affect, len(cases)))
    rows_to_drop = []
    for case in affected_cases:
        case_rows = df.index[df["Case"] == case].tolist()
        if case_rows:
            rows_to_drop.append(rng.choice(case_rows))
    return df.drop(rows_to_drop).reset_index(drop=True)


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
    """Replace case IDs with wrong/out-of-range IDs (case-level)."""
    df = df.copy()
    max_id = int(df["Case"].max())
    all_cases = df["Case"].dropna().unique().tolist()
    n_to_change = max(1, int(len(all_cases) * ratio))
    cases_to_change = rng.sample(all_cases, n_to_change)
    for case in cases_to_change:
        new_id = rng.randint(max_id + 1000, max_id + 99999)
        df.loc[df["Case"] == case, "Case"] = new_id
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
        offset += rng.randint(50, 500)
        df.loc[df["Case"] == case, "Case"] = offset
    return df


# Resource ────────────────────────────────────────────────────────────────────

def inject_resource_accuracy(df, ratio, rng):
    """Wrong resource for an activity (role violation)."""
    df = df.copy()
    all_resources = list(set(EXPECTED_RESOURCES.values()))
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
    # CaseID Consistency omitted: R has no calculate_caseid_consistency.r script
    ("Resource",      "Accuracy"):     inject_resource_accuracy,
    ("Resource",      "Completeness"): inject_resource_completeness,
    ("Resource",      "Consistency"):  inject_resource_consistency,
}

SIZES  = [1000]
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

        # ── Combined 3000-case log (one dimension per 1000-case segment) ──
        base_3000 = generate_base_log(3000, seed=42)
        cases_sorted = sorted(base_3000["Case"].unique())
        chunk_size   = len(cases_sorted) // 3
        chunks       = [
            cases_sorted[0           : chunk_size],
            cases_sorted[chunk_size  : chunk_size * 2],
            cases_sorted[chunk_size * 2:],
        ]
        dimensions_order = ["Completeness", "Accuracy", "Consistency"]

        for ratio in RATIOS:
            for err_type in ["Timestamp", "ActivityLabel", "CaseID", "Resource"]:
                pct       = int(ratio * 100)
                result_df = base_3000.copy()

                for dim, case_chunk in zip(dimensions_order, chunks):
                    if (err_type, dim) not in INJECTORS:
                        continue
                    injector  = INJECTORS[(err_type, dim)]
                    rng_local = random.Random(42 + 3000 + pct + hash(err_type + dim) % 10000)
                    mask      = result_df["Case"].isin(case_chunk)
                    sub       = result_df[mask].reset_index(drop=True)
                    sub_noisy = injector(sub, ratio, rng_local)
                    if len(sub_noisy) != len(sub):
                        result_df = pd.concat(
                            [result_df[~mask].reset_index(drop=True), sub_noisy],
                            ignore_index=True,
                        )
                    else:
                        orig_idx  = result_df.index[mask]
                        sub_noisy.index = orig_idx
                        for col in sub_noisy.columns:
                            if sub_noisy[col].isna().any() and result_df[col].dtype == "int64":
                                result_df[col] = result_df[col].astype(float)
                            result_df.loc[orig_idx, col] = sub_noisy[col].values

                fname = f"datasetwith3000cases_{err_type}_AllDimensions_{pct}%.csv"
                result_df.to_csv(os.path.join(OUTPUT_DIR, fname), index=False)
                print(f"  [3000-COMBINED] {fname}")
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
