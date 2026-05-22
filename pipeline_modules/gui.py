"""
gui.py
------
Tkinter GUI for the Event Log Quality Assessment tool.
Lets the user select one or more CSV event logs, shows a progress log,
and runs the R pipeline for each file in a background thread.
"""

import os
import threading
import tkinter as tk
from tkinter import filedialog, messagebox, ttk

from .config import OUTPUT_DIR
from .r_runner import find_rscript, run_single_log


def launch_gui() -> None:
    """Build and run the main application window."""
    root = tk.Tk()
    root.title("Event Log Quality Assessment")
    root.resizable(True, True)
    root.minsize(700, 460)

    frame = tk.Frame(root, padx=14, pady=12)
    frame.pack(fill="both", expand=True)
    frame.columnconfigure(0, weight=1)

    # ── File list ─────────────────────────────────────────────────────────────
    tk.Label(frame, text="Event log files to assess:", anchor="w").grid(
        row=0, column=0, columnspan=2, sticky="w"
    )

    list_frame = tk.Frame(frame)
    list_frame.grid(row=1, column=0, columnspan=2, sticky="nsew", pady=(2, 4))
    list_frame.columnconfigure(0, weight=1)
    frame.rowconfigure(1, weight=1)

    scrollbar = tk.Scrollbar(list_frame)
    scrollbar.pack(side="right", fill="y")

    listbox = tk.Listbox(
        list_frame, selectmode="extended",
        yscrollcommand=scrollbar.set,
        height=8, font=("Consolas", 9),
    )
    listbox.pack(side="left", fill="both", expand=True)
    scrollbar.config(command=listbox.yview)

    btn_frame = tk.Frame(frame)
    btn_frame.grid(row=2, column=0, columnspan=2, sticky="w", pady=(0, 8))

    def add_files():
        paths = filedialog.askopenfilenames(
            title="Select event log CSV files",
            filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
        )
        existing = set(listbox.get(0, "end"))
        for p in paths:
            if p not in existing:
                listbox.insert("end", p)

    def remove_selected():
        for idx in reversed(listbox.curselection()):
            listbox.delete(idx)

    def clear_all():
        listbox.delete(0, "end")

    tk.Button(btn_frame, text="Add files...",    command=add_files,       width=14).pack(side="left", padx=2)
    tk.Button(btn_frame, text="Remove selected", command=remove_selected, width=16).pack(side="left", padx=2)
    tk.Button(btn_frame, text="Clear all",       command=clear_all,       width=10).pack(side="left", padx=2)

    # ── Progress log ──────────────────────────────────────────────────────────
    tk.Label(frame, text="Progress:", anchor="w").grid(
        row=3, column=0, columnspan=2, sticky="w"
    )

    log_frame = tk.Frame(frame)
    log_frame.grid(row=4, column=0, columnspan=2, sticky="nsew", pady=(2, 8))
    log_frame.columnconfigure(0, weight=1)
    frame.rowconfigure(4, weight=1)

    log_scrollbar = tk.Scrollbar(log_frame)
    log_scrollbar.pack(side="right", fill="y")

    log_text = tk.Text(
        log_frame, height=8, state="disabled",
        yscrollcommand=log_scrollbar.set,
        font=("Consolas", 9), bg="#1e1e1e", fg="#d4d4d4",
    )
    log_text.pack(side="left", fill="both", expand=True)
    log_scrollbar.config(command=log_text.yview)

    progress_var = tk.DoubleVar(value=0)
    progress_bar = ttk.Progressbar(frame, variable=progress_var, maximum=100)
    progress_bar.grid(row=5, column=0, columnspan=2, sticky="ew", pady=(0, 6))

    run_btn = tk.Button(
        frame,
        text="Run Quality Assessment",
        bg="#0a66c2", fg="white",
        font=("Segoe UI", 10, "bold"),
        padx=16, pady=6, relief="flat", cursor="hand2",
    )
    run_btn.grid(row=6, column=0, columnspan=2, pady=6)

    # ── Helpers ───────────────────────────────────────────────────────────────
    def append_log(msg: str) -> None:
        log_text.config(state="normal")
        log_text.insert("end", msg + "\n")
        log_text.see("end")
        log_text.config(state="disabled")

    def set_enabled(enabled: bool) -> None:
        run_btn.config(state="normal" if enabled else "disabled")

    # ── Run handler ───────────────────────────────────────────────────────────
    def on_run():
        csv_paths = list(listbox.get(0, "end"))
        if not csv_paths:
            messagebox.showwarning("No files", "Please add at least one event log CSV file.")
            return

        try:
            rscript_exe = find_rscript()
        except FileNotFoundError as exc:
            choose = messagebox.askyesno(
                "R not found",
                f"{exc}\n\nDo you want to locate Rscript.exe manually?",
            )
            if not choose:
                return
            selected = filedialog.askopenfilename(
                title="Locate Rscript.exe",
                filetypes=[
                    ("Rscript executable", "Rscript.exe"),
                    ("Executables", "*.exe"),
                    ("All files", "*.*"),
                ],
            )
            if not selected:
                return
            if os.path.basename(selected).lower() != "rscript.exe":
                messagebox.showerror("Invalid selection", "Please select Rscript.exe.")
                return
            rscript_exe = selected

        set_enabled(False)
        progress_var.set(0)
        log_text.config(state="normal")
        log_text.delete("1.0", "end")
        log_text.config(state="disabled")

        def worker():
            total              = len(csv_paths)
            successes, failures = [], []
            shared_log         = []

            for idx, csv_path in enumerate(csv_paths):
                shared_log.clear()
                root.after(
                    0,
                    lambda p=csv_path, i=idx: append_log(
                        f"\n[{i+1}/{total}] Processing: {os.path.basename(p)}"
                    ),
                )

                result = run_single_log(csv_path, rscript_exe, shared_log)

                for line in shared_log:
                    root.after(0, lambda l=line: append_log(l))

                if result["ok"]:
                    successes.append(result)
                    def _ok_msg(r=result) -> str:
                        lines = [
                            f"  ✔ Attributes      → {r['excel_attr']}",
                            f"  ✔ Sim. Parameters → {r['excel_param']}",
                        ]
                        if r.get("excel_root_cause"):
                            lines.append(f"  ✔ Root Cause      → {r['excel_root_cause']}")
                        return "\n".join(lines)
                    root.after(0, lambda r=result: append_log(_ok_msg(r)))
                else:
                    failures.append(result)
                    root.after(
                        0, lambda r=result: append_log(f"  ✗ FAILED: {r['error']}")
                    )

                root.after(0, lambda p=(idx + 1) / total * 100: progress_var.set(p))

            def finish():
                set_enabled(True)
                lines = [f"Completed: {len(successes)} succeeded, {len(failures)} failed.\n"]
                for r in successes:
                    entry = (
                        f"  ✔ {r['log']}\n"
                        f"    Attributes      → {r['excel_attr']}\n"
                        f"    Sim. Parameters → {r['excel_param']}"
                    )
                    if r.get("excel_root_cause"):
                        entry += f"\n    Root Cause      → {r['excel_root_cause']}"
                    lines.append(entry)
                for r in failures:
                    lines.append(f"  ✗ {r['log']}\n    Error: {r['error']}")
                append_log("\n" + "\n".join(lines))

                if failures:
                    messagebox.showwarning(
                        "Some runs failed",
                        f"{len(failures)} log(s) failed. Check the progress log for details.",
                    )
                else:
                    messagebox.showinfo(
                        "Assessment complete",
                        f"All {len(successes)} log(s) processed successfully.\n"
                        f"Excel files saved in:\n{OUTPUT_DIR}",
                    )

            root.after(0, finish)

        threading.Thread(target=worker, daemon=True).start()

    run_btn.config(command=on_run)
    root.mainloop()
