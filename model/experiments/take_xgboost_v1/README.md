# Isolated take-path XGBoost comparison

See [the report](reports/REPORT.md) and [prespecified protocol](PROTOCOL.md).
No active scorer or original artifact is changed.

From the project root, with the existing R `data.table`, `mgcv` and `xgboost`:

```sh
Rscript experiments/take_xgboost_v1/tests.R
Rscript experiments/take_xgboost_v1/train.R
Rscript experiments/take_xgboost_v1/fit_actors.R
Rscript experiments/take_xgboost_v1/evaluate.R
Rscript experiments/take_xgboost_v1/verify.R
Rscript experiments/take_xgboost_v1/report.R
```

Training refuses to overwrite a frozen run. To refit, copy this experiment to
a new version directory and update `TROOT`; do not delete the original freezes.
Evaluation and verification can be rerun against frozen models. Required
original inputs are listed in `reports/input_hashes.csv`; restore iCloud-only
files before running. Versions are in `reports/sessionInfo.txt`.

- `models/`: three development boosters, selected final UBJ booster, preprocessing,
  development/final actor fits with keyed offset provenance, and two freeze manifests.
- `reports/predictions_*.csv.gz`: keyed base, adjusted and calibrated probabilities,
  frozen branches, audited targets, support, fixed swing/policy values and scores.
- `metrics.csv`, `values.csv`, `boot.csv`, `valueboot.csv`: probability and value
  results with 500-draw paired whole-game conditional intervals.
- `bins.csv`, `slices.csv`, `state_errors.csv`, `boundaries.csv`, `tails.csv`:
  calibration, contextual errors, support boundaries and audit-only swing estimates.
- `hitters_*.csv`, `largest_movements_*.csv`, `hitter_games_*.csv.gz`: fixed-scale
  Decision+ effects and freshly sampled conditional hitter intervals.
- `verification.csv`, `unit_tests.csv`: independent reconstruction and focused tests.

For historical single-pitch or batch predictions, source `predict.R` and call
`predict_take_comparison(pitches, 'xgboost')` or `'gam'`. Inputs are raw cleaned
pitch/context fields consumed by `cs_transform`, pitch keys and dates. Current
outcome labels are optional and ignored. Only audited 2024/2025 dates are
supported; the saved weekly ledger always uses evidence before its block start.

2025 is exploratory, not an untouched holdout. The intervals condition on fitted
models and realized calibration histories; they do not include full fitting
uncertainty. Hypothetical take values on swings have no factual validation.
