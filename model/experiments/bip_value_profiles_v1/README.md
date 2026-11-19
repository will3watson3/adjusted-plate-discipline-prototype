# Historical BIP value profiles

Fixed three-arm experiment: frozen 2021 EV (A), prior-season EV (B), prior-season
EV + barrel/ground-ball rates (C). See [protocol](PROTOCOL.md) and
[results](reports/REPORT.md). Both evaluation seasons are exploratory.

Run from the project root, using existing R/data.table/mgcv and project inputs:

```sh
Rscript experiments/bip_value_profiles_v1/tests.R
Rscript experiments/bip_value_profiles_v1/train.R
Rscript experiments/bip_value_profiles_v1/evaluate.R
Rscript experiments/bip_value_profiles_v1/verify.R
Rscript experiments/bip_value_profiles_v1/report.R
```

Training refuses to overwrite a frozen experiment. Evaluation, verification and
reporting can be rerun. Input hashes identify the exact required files; restore
any iCloud-only inputs first. Versions are in reports/sessionInfo.txt.

- `models/`: three GAMs, dated profiles, keyed training cohort, freeze manifests.
- `reports/predictions_*.csv.gz`: identical pitch keys, three Q_BIP/Q_swing/score
  arms, frozen active components, targets, support and dated profile exposures.
- `metrics.csv`, `boot.csv`, `bins.csv`, `slices.csv`, `tails.csv`: matched results
  and 500-draw paired game intervals, including common in-range sensitivity.
- `hitters.csv`, `hitter_games_*.csv.gz`, `largest_changes_*.csv`: fixed-scale
  Decision+ effects and fresh conditional game-sampling intervals.
- `verification.csv`, `unit_tests.csv`: independent reconstruction and tests.

Source predict.R and call `predict_bip_profile(pitches, 'A'/'B'/'C')` for Q_BIP.
Inputs are CT_INPUTS plus pitch keys and season (or game_year), for 2022–2025.
Current-pitch outcome/tracking fields are unnecessary and ignored. Raw inference
does not establish support for scoring or causal/counterfactual validity.

No active scorer, pointer, source file or Git metadata is changed. Adopting a
candidate is a separate decision. Verification uses separate arithmetic within
verify.R; it does not reuse the fitting/evaluation metric or interval helpers.
