"""
column_detector.py
------------------
Auto-detects which CSV columns correspond to case ID, activity,
resource, start timestamp, and complete timestamp.
"""

import pandas as pd
from .config import COLUMN_CANDIDATES


def detect_columns(csv_path: str) -> dict:
    """Return best-guess column mapping {internal_key: actual_csv_column}."""
    try:
        cols = [str(c).strip() for c in pd.read_csv(csv_path, nrows=0).columns]
    except Exception:
        return {k: v[0] for k, v in COLUMN_CANDIDATES.items()}

    lower_map = {c.lower(): c for c in cols}

    def pick(candidates: list[str]) -> str:
        for c in candidates:
            if c.lower() in lower_map:
                return lower_map[c.lower()]
        return candidates[0]   # fallback to first candidate

    return {k: pick(v) for k, v in COLUMN_CANDIDATES.items()}
