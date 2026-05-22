"""
log_distance_gui.py
-------------------
Tkinter GUI for the Log Distance Evaluation tool.

Usage:
    from evaluation_utils.log_distance_gui import launch_log_distance_gui
    launch_log_distance_gui()

Lets the user pick:
  - one original (real) event log CSV
  - one or more simulated event log CSVs
Then runs all log-distance metrics and saves results to Output/.
"""

import os
import threading
import tkinter as tk
from tkinter import filedialog, messagebox, ttk

import sys
_HERE = os.path.dirname(os.path.abspath(__file__))
_ROOT = os.path.dirname(_HERE)
if _ROOT not in sys.path:
    sys.path.insert(0, _ROOT)

from evaluation_utils.log_distance_runner import run_log_distance
from pipeline_modules.config import OUTPUT_DIR


def launch_log_distance_gui() -> None:
    """Build and run the Log Distance Evaluation window."""
    root = tk.Tk()
    root.title("Log Distance Evaluation")
    root.resizable(True, True)
    root.minsize(720, 540)

    frame = tk.Frame(root, padx=14, pady=12)
    frame.pack(fill="both", expand=True)
    frame.columnconfigure(0, weight=1)

    # ── Original log (single file) ─────────────────────────────────────────
    tk.Label(frame, text="Original event log (real log):", anchor="w").grid(
        row=0, column=0, columnspan=2, sticky="w"
    )

    orig_var = tk.StringVar()
    orig_entry = tk.Entry(frame, textvariable=orig_var, state="readonly",
                          font=("Consolas", 9))
    orig_entry.grid(row=1, column=0, sticky="ew", padx=(0, 4), pady=(2, 6))

    def browse_original():
        path = filedialog.askopenfilename(
            title="Select original event log CSV",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
        )
        if path:
            orig_var.set(path)

    tk.Button(frame, text="Browse...", command=browse_original,
              width=10).grid(row=1, column=1, pady=(2, 6), sticky="e")

    # ── Simulated logs (multiple files) ───────────────────────────────────
    tk.Label(frame, text="Simulated event logs (one or more):", anchor="w").grid(
        row=2, column=0, columnspan=2, sticky="w"
    )

    sim_frame = tk.Frame(frame)
    sim_frame.grid(row=3, column=0, columnspan=2, sticky="nsew", pady=(2, 4))
    sim_frame.columnconfigure(0, weight=1)
    frame.rowconfigure(3, weight=1)

    sim_scrollbar = tk.Scrollbar(sim_frame)
    sim_scrollbar.pack(side="right", fill="y")
    sim_listbox = tk.Listbox(
        sim_frame, selectmode="extended",
        yscrollcommand=sim_scrollbar.set,
        height=6, font=("Consolas", 9),
    )
    sim_listbox.pack(side="left", fill="both", expand=True)
    sim_scrollbar.config(command=sim_listbox.yview)

    btn_frame = tk.Frame(frame)
    btn_frame.grid(row=4, column=0, columnspan=2, sticky="w", pady=(0, 8))

    def add_simulated():
        paths = filedialog.askopenfilenames(
            title="Select simulated event log CSV files",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
        )
        existing = set(sim_listbox.get(0, "end"))
        for p in paths:
            if p not in existing:
                sim_listbox.insert("end", p)

    def remove_selected():
        for idx in reversed(sim_listbox.curselection()):
            sim_listbox.delete(idx)

    def clear_all():
        sim_listbox.delete(0, "end")

    tk.Button(btn_frame, text="Add files...",    command=add_simulated,  width=14).pack(side="left", padx=2)
    tk.Button(btn_frame, text="Remove selected", command=remove_selected, width=16).pack(side="left", padx=2)
    tk.Button(btn_frame, text="Clear all",       command=clear_all,       width=10).pack(side="left", padx=2)

    # ── Progress log ──────────────────────────────────────────────────────
    tk.Label(frame, text="Progress:", anchor="w").grid(
        row=5, column=0, columnspan=2, sticky="w"
    )

    log_frame = tk.Frame(frame)
    log_frame.grid(row=6, column=0, columnspan=2, sticky="nsew", pady=(2, 8))
    log_frame.columnconfigure(0, weight=1)
    frame.rowconfigure(6, weight=2)

    log_scrollbar = tk.Scrollbar(log_frame)
    log_scrollbar.pack(side="right", fill="y")
    log_text = tk.Text(
        log_frame, height=10, state="disabled",
        yscrollcommand=log_scrollbar.set,
        font=("Consolas", 9), bg="#1e1e1e", fg="#d4d4d4",
    )
    log_text.pack(side="left", fill="both", expand=True)
    log_scrollbar.config(command=log_text.yview)

    progress_var = tk.DoubleVar(value=0)
    progress_bar = ttk.Progressbar(frame, variable=progress_var, maximum=100)
    progress_bar.grid(row=7, column=0, columnspan=2, sticky="ew", pady=(0, 6))

    run_btn = tk.Button(
        frame,
        text="Run Log Distance Analysis",
        bg="#217346", fg="white",
        font=("Segoe UI", 10, "bold"),
        padx=16, pady=6, relief="flat", cursor="hand2",
    )
    run_btn.grid(row=8, column=0, columnspan=2, pady=6)

    # ── Helpers ───────────────────────────────────────────────────────────
    def append_log(msg: str) -> None:
        log_text.config(state="normal")
        log_text.insert("end", msg + "\n")
        log_text.see("end")
        log_text.config(state="disabled")

    def set_enabled(enabled: bool) -> None:
        run_btn.config(state="normal" if enabled else "disabled")

    # ── Run handler ───────────────────────────────────────────────────────
    def on_run():
        original_path = orig_var.get().strip()
        if not original_path:
            messagebox.showwarning("No original log",
                                   "Please select the original (real) event log CSV.")
            return

        sim_paths = list(sim_listbox.get(0, "end"))
        if not sim_paths:
            messagebox.showwarning("No simulated logs",
                                   "Please add at least one simulated event log CSV.")
            return

        set_enabled(False)
        progress_var.set(0)
        log_text.config(state="normal")
        log_text.delete("1.0", "end")
        log_text.config(state="disabled")

        def worker():
            shared_log: list[str] = []
            total = len(sim_paths)

            root.after(0, lambda: append_log(
                f"Original log: {os.path.basename(original_path)}\n"
                f"Simulated logs: {total}\n"
                f"─" * 50
            ))

            result = run_log_distance(original_path, sim_paths, shared_log)

            for line in shared_log:
                root.after(0, lambda l=line: append_log(l))

            root.after(0, lambda: progress_var.set(100))

            def finish():
                set_enabled(True)
                if result["ok"] and result.get("excel"):
                    append_log(f"\n✔ Results saved → {result['excel']}")
                    messagebox.showinfo(
                        "Analysis complete",
                        f"Log distance analysis finished.\n"
                        f"Results saved in:\n{OUTPUT_DIR}",
                    )
                else:
                    err = result.get("error", "Unknown error")
                    append_log(f"\n✗ Analysis failed: {err}")
                    messagebox.showerror("Analysis failed", err)

            root.after(0, finish)

        threading.Thread(target=worker, daemon=True).start()

    run_btn.config(command=on_run)
    root.mainloop()
