# Baseline status and limits

Snapshot: October 19, 2026. The published source is a reproducible prototype, not a final certified hitter ranking. Aggregate numeric evidence is in `baseline_metrics.json`; full pitch-level data and trained artifacts stay outside Git.

| Quantity | Current snapshot |
| --- | ---: |
| 2025 eligible pitches | 704,987 |
| Supported comparisons | 542,384 (76.9%) |
| Qualified hitters | 339 |
| v4 → regional-umpire supported-take log loss | 0.224218 → 0.223287 |
| v4 → regional-umpire Brier score | 0.069003 → 0.068731 |
| v4 → regional-umpire take-value RMSE | 0.046605 → 0.046538 runs |
| Median absolute qualified-hitter change | 0.004786 runs per 100 supported pitches |
| Largest absolute qualified-hitter change | 0.034793 runs per 100 supported pitches |

The log-loss gain survives paired game and umpire-cluster resampling and a shared-region control. The paired factual take-value MSE interval includes zero, so an incremental run-value improvement is not established. The user requested adoption for descriptive scoring with that limitation visible.

## Known limits

- Only the action taken is observed. Factual prediction performance does not identify the accuracy of the unchosen action or prove causal decision skill.
- 2025 is no longer untouched validation. Rebuilding an old guarded `evaluate_locked2025.R` entry point does not restore independence.
- The frozen RE288 baseline has 34 flagged states with questionable count orderings; these states are excluded from primary scores. Values are not silently clamped or repaired.
- The existing BIP model's 2024 RMSE was 0.538366 versus 0.540694 for a simple state/count baseline: a supported but modest improvement.
- Known historical extrapolations in hypothetical swing-on-take BIP predictions are retained as raw audit values and excluded from primary support.
- Mean calibration can hide location errors. Side-zone called-strike error remains.
- Game-level boxscore officials may not capture rare mid-game umpire substitutions.
- The model uses frozen 2021 player profiles, ordinary action branches and a restricted analysis population.
- Model uncertainty uses first-order conditional covariance. Separate models and overlapping weekly histories use SD-sum dependence bounds. RE estimation, full training refits, variance/selection uncertainty and model misspecification are not fully covered.
- 1/7/14-game resampling is a precision sensitivity, not proof of latent skill or a requirement that two half-season estimates agree.
- Small differences in point rankings should not be interpreted as established differences between hitters.

## Validation distinction

The original workspace's v6 integration passed **9 unit checks and 52 artifact checks**. This repository separately supplies offline tests requiring no MLB data, and artifact verifiers requiring a full local rebuild. A successful code-only test suite validates implementation invariants; it does not regenerate the published historical metrics or establish predictive validity by itself.
