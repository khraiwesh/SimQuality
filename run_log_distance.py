"""
run_log_distance.py
-------------------
Entry point for the Log Distance Evaluation tool.

Run this file to launch the GUI:
    python run_log_distance.py

NOTE: The log_distance/ folder contains third-party code from the
      log-distance-measures library (Camargo et al., 2021).
      See log_distance/README.md for attribution and license.

Dependencies (in addition to requirements.txt):
    pip install jellyfish scipy pulp
"""

from evaluation_utils.log_distance_gui import launch_log_distance_gui

if __name__ == "__main__":
    launch_log_distance_gui()
