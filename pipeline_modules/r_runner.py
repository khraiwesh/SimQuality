"""
r_runner.py
-----------
Handles everything related to running the R quality-assessment scripts:
  - locating Rscript.exe on the system
  - building the correct R environment (user library path)
  - writing the per-log _run_wrapper.r
  - executing the R pipeline and returning structured results
"""

import glob
import os
import re
import shutil
import subprocess
import textwrap

from .config import MAIN_R, TRUSTVALUES
from .column_detector import detect_columns
from .excel_exporter import save_excel_tables


# ── R executable discovery ────────────────────────────────────────────────────

def find_rscript() -> str:
    """Return the path to Rscript.exe, searching PATH and common Windows locations."""
    exe = shutil.which("Rscript")
    if exe:
        return exe

    r_home = os.environ.get("R_HOME", "")
    if r_home:
        for rel in (r"bin\Rscript.exe", r"bin\x64\Rscript.exe"):
            candidate = os.path.join(r_home, rel)
            if os.path.isfile(candidate):
                return candidate

    roots = [
        r"C:\Program Files\R",
        r"C:\Program Files (x86)\R",
        os.path.join(os.environ.get("LOCALAPPDATA", ""), "Programs", "R"),
    ]
    discovered: list[str] = []
    for root in roots:
        if not root or not os.path.isdir(root):
            continue
        for pat in [
            os.path.join(root, "R-*", "bin", "Rscript.exe"),
            os.path.join(root, "R-*", "bin", "x64", "Rscript.exe"),
        ]:
            discovered.extend(glob.glob(pat))

    if discovered:
        def _version_key(path: str) -> tuple:
            match = re.search(r"[\\/]R-(\d+)(?:\.(\d+))?(?:\.(\d+))?", path)
            if not match:
                return (0, 0, 0)
            return tuple(int(p or 0) for p in match.groups())

        discovered.sort(key=_version_key, reverse=True)
        return discovered[0]

    raise FileNotFoundError(
        "Rscript.exe not found. Install R and ensure Rscript is on PATH, "
        "or available under C:/Program Files/R/R-*/bin[/x64]/Rscript.exe."
    )


def build_r_environment(rscript_exe: str) -> dict:
    """Build an os.environ copy that guarantees the R user library is visible."""
    env = os.environ.copy()
    match = re.search(r"R-(\d+)\.(\d+)", rscript_exe)
    if match and env.get("USERPROFILE"):
        major, minor = match.groups()
        default_user_lib = os.path.join(
            env["USERPROFILE"], "Documents", "R", "win-library", f"{major}.{minor}"
        )
        env.setdefault("R_LIBS_USER", default_user_lib)
    return env


# ── R wrapper script ─────────────────────────────────────────────────────────

def _r_string(value: str) -> str:
    """Wrap a Python string as a properly escaped R character literal."""
    escaped = value.replace("\\", "/").replace('"', '\\"')
    return f'"{escaped}"'


def write_r_wrapper(csv_path: str, col_map: dict, work_dir: str) -> str:
    """Write _run_wrapper.r that sets variables and sources main.r."""
    wrapper_path = os.path.join(work_dir, "_run_wrapper.r")
    content = textwrap.dedent(f"""\
        # Auto-generated wrapper — do not edit manually
        data_file              <- {_r_string(csv_path)}
        case_id_col            <- {_r_string(col_map['case_id'])}
        activity_col           <- {_r_string(col_map['activity'])}
        resource_col           <- {_r_string(col_map['resource'])}
        start_timestamp_col    <- {_r_string(col_map['start'])}
        complete_timestamp_col <- {_r_string(col_map['complete'])}
        trustvalues_dir        <- {_r_string(TRUSTVALUES)}
        USE_AUTO_CONFIG        <- TRUE
        USE_CASCADING_MODE     <- FALSE
        setwd({_r_string(work_dir)})
        source({_r_string(MAIN_R)})
    """)
    with open(wrapper_path, "w", encoding="utf-8") as fh:
        fh.write(content)
    return wrapper_path


# ── Per-log pipeline ──────────────────────────────────────────────────────────

def run_single_log(csv_path: str, rscript_exe: str, log_lines: list) -> dict:
    """
    Run the full R quality-assessment pipeline for one CSV event log.

    Returns a dict with keys:
        ok, log, error, r_log, excel_attr, excel_param, excel_root_cause
    """
    log_name = os.path.splitext(os.path.basename(csv_path))[0]
    log_dir  = os.path.dirname(csv_path)

    # Dedicated working directory for this log's R output
    work_dir         = os.path.join(log_dir, f"{log_name}_quality_results")
    results_dir      = os.path.join(work_dir, "results")
    case_results_dir = os.path.join(work_dir, "results_case_id")
    os.makedirs(results_dir,      exist_ok=True)
    os.makedirs(case_results_dir, exist_ok=True)

    col_map      = detect_columns(csv_path)
    wrapper_path = write_r_wrapper(csv_path, col_map, work_dir)

    log_lines.append(f"[{log_name}] Running R tool...")

    proc = subprocess.run(
        [rscript_exe, "--vanilla", wrapper_path],
        capture_output=True,
        text=True,
        cwd=work_dir,
        env=build_r_environment(rscript_exe),
    )

    # Always save R stdout/stderr for debugging
    r_log_path = os.path.join(work_dir, "r_run.log")
    with open(r_log_path, "w", encoding="utf-8") as fh:
        fh.write("=== STDOUT ===\n")
        fh.write(proc.stdout or "")
        fh.write("\n=== STDERR ===\n")
        fh.write(proc.stderr or "")

    if proc.returncode != 0:
        return {
            "ok": False, "log": log_name,
            "error": f"R exited with code {proc.returncode}. See: {r_log_path}",
            "r_log": r_log_path,
            "excel_attr": None, "excel_param": None, "excel_root_cause": None,
        }

    summary_path = os.path.join(results_dir, "data_quality_summary.csv")
    if not os.path.isfile(summary_path):
        return {
            "ok": False, "log": log_name,
            "error": f"R finished but data_quality_summary.csv not found. See: {r_log_path}",
            "r_log": r_log_path,
            "excel_attr": None, "excel_param": None, "excel_root_cause": None,
        }

    try:
        attr_path, param_path, rc_path = save_excel_tables(results_dir, case_results_dir, log_name)
    except Exception as exc:
        import traceback
        err_detail = traceback.format_exc()
        log_lines.append(f"[{log_name}] Excel export error: {exc}")
        return {
            "ok": False, "log": log_name,
            "error": f"Excel export failed: {exc}\n{err_detail}",
            "r_log": r_log_path,
            "excel_attr": None, "excel_param": None, "excel_root_cause": None,
        }

    log_lines.append(f"[{log_name}] Attributes  → {attr_path}")
    log_lines.append(f"[{log_name}] Sim. Params → {param_path}")
    if rc_path:
        log_lines.append(f"[{log_name}] Root Cause  → {rc_path}")
    else:
        log_lines.append(f"[{log_name}] Root Cause  → (not generated — no anomalies detected)")

    return {
        "ok": True, "log": log_name, "error": "",
        "r_log": r_log_path,
        "excel_attr":       attr_path,
        "excel_param":      param_path,
        "excel_root_cause": rc_path,
    }
