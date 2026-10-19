# Preserved research implementation

These modules and protocol files are copied **byte for byte** from the reviewed research baseline. Their flat layout is intentional: the original source calls, artifact paths and provenance checks are relative to this working directory. Execute them from `model/`, or use `../scripts/reproduce.py`, which sets the working directory for you.

| Layer | Primary files |
| --- | --- |
| Public-data acquisition and cleaning | `Adjusted_Plate_Disc_DC.R`, `acquire_bip_states.py`, `acquire_evaluation_bip_states.py`, `acquire_umpire_assignments.py` |
| State value foundation | `run_expectancy_models.R`, `attach_run_expectancy.R`, `take_value_models.R`, `re288_order_flags.R` |
| Swing outcomes and BIP value | `contact_models.R`, `post_contact_models.R` |
| Original strike baseline | `called_strike_models.R` |
| Support, factual validation and league policy | `score_decisions.R`, `decision_validation.R`, `evaluate_locked2025.R` |
| Catcher/umpire updates | `catcher_framing_v2.R`, `called_strike_v4.R`, `umpire_v5.R` |
| Current season aggregation and uncertainty | `hitter_stat_v6.R`, `current_hitter_stat.R`, `season_precision_v3.R` |
| Shared helpers / historical calibration experiment | `season_average_v3.R` (not promoted wholesale) |
| Audits | `review_re288_orderings.R`, `audit_umpire_v5.R`, `tests/verify_*.R` |

`*_PLAN.md` files are historical experiment protocols, not the current project landing page. Older files may refer to a locked 2025 test or a half-season release gate: they preserve the original study, whereas the current metric target and 2025 exploratory status are documented in the top-level README and docs. The historical path is retained because later reproducible artifacts depend on it. No half-season gate is added to the current v6 descriptive summary.

Rebuilt outputs are local and ignored: `data/`, `reports/`, `.R-library/`. Do not copy the original workspace's frozen manifests into this checkout: their data paths and source hashes belong to that original experiment. Rebuild manifests here from the preserved code and locally acquired inputs.
