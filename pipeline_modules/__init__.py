"""
functions package
-----------------
Exposes the public API of the quality-assessment tool modules.
"""

from .column_detector import detect_columns
from .excel_exporter  import build_table1, build_table2, build_table3, save_excel_tables
from .gui             import launch_gui
from .r_runner        import (
    build_r_environment,
    find_rscript,
    run_single_log,
    write_r_wrapper,
)

__all__ = [
    "detect_columns",
    "build_table1", "build_table2", "build_table3", "save_excel_tables",
    "launch_gui",
    "find_rscript", "build_r_environment", "write_r_wrapper", "run_single_log",
]
