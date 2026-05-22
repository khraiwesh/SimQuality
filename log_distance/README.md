# Log Distance Measures

> **This folder contains third-party code and is NOT the work of the tool authors.**

## Source

This package is a copy of the **log-distance-measures** library developed by:

> Camargo M, Dumas M, González-Rojas O.  
> *Discovering generative models from event logs: data-driven simulation vs deep learning.*  
> PeerJ Computer Science 7:e577, 2021. https://doi.org/10.7717/peerj-cs.577

Original repository: https://github.com/AutomatedProcessImprovement/log-distance-measures

## What is included

| Module | Metric |
|--------|--------|
| `n_gram_distribution.py` | N-gram distribution distance (bigram, trigram) |
| `absolute_event_distribution.py` | Absolute event distribution (EMD / Wasserstein) |
| `case_arrival_distribution.py` | Case arrival distribution (EMD / Wasserstein) |
| `circadian_event_distribution.py` | Circadian event distribution (EMD / Wasserstein) |
| `circadian_workforce_distribution.py` | Circadian workforce distribution (EMD / Wasserstein) |
| `cycle_time_distribution.py` | Cycle time distribution (Wasserstein) |
| `relative_event_distribution.py` | Relative event distribution (EMD / Wasserstein) |
| `remaining_time_distribution.py` | Remaining time distribution |
| `work_in_progress.py` | Work-in-progress distance |
| `control_flow_log_distance.py` | Control-Flow Log Distance (CFLD) |
| `earth_movers_distance.py` | Earth Mover's Distance helper |
| `config.py` | `EventLogIDs`, `DistanceMetric`, discretize helpers |

## Modifications

- Internal imports changed from `log_distance_measures.*` → `log_distance.*`  
  to fit the package location in this repository.  
  No algorithmic changes were made.

## License

See the original repository for license information.
