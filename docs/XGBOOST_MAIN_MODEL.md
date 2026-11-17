# XGBoost main model

The current scorer uses XGBoost for contact probability, BIP probability given contact, and the complete take path. It retains the active v6 GAMs for caught-tip probability and BIP expected runs+RE-after. The original league policy, RE288 transitions, qualification rules and exposure-weighted 2024 reference scale are unchanged. Continuous hitter surfaces and XGBoost tip/value challengers are not used.

The exact hybrid was replayed on complete 2024 and 2025 cleaned datasets without additional fitting or tuning. Results are exploratory because both seasons have already been inspected. These are factual prediction errors, not runs caused by better decisions.

## Common-supported factual accuracy

The original active v6 scorer is the baseline below. Both arms use the same supported factual pitch cohort for each comparison. This baseline differs from the updated-history population GAM control in the earlier full-swing experiment.

| year | action | arm | n | rmse | mae | bias |
| --- | --- | --- | --- | --- | --- | --- |
| 2024 | swing | baseline | 306650 | 0.339907617987306 | 0.177632589871514 | -0.000156245118938371 |
| 2024 | swing | xgboost | 306650 | 0.339666217695035 | 0.17762441464155 | 0.000537072578266395 |
| 2024 | take | baseline | 223865 | 0.0480150703823141 | 0.0179889916203845 | 8.06859628346336e-05 |
| 2024 | take | xgboost | 223865 | 0.0470018633840195 | 0.0171044440446889 | 0.000104893742780125 |
| 2025 | swing | baseline | 306240 | 0.341969206530819 | 0.17901079676241 | -0.000687096310143685 |
| 2025 | swing | xgboost | 306240 | 0.341705902521801 | 0.17933202613214 | 0.000683979767544006 |
| 2025 | take | baseline | 229317 | 0.0465959012523675 | 0.0173431809538814 | -0.00015926464890055 |
| 2025 | take | xgboost | 229317 | 0.045384647207405 | 0.0165335523806542 | -0.000104196143995951 |

On 2025 common-supported swings, RMSE improves from 0.341969 to 0.341706 runs (paired difference -0.000263, 95% interval [-0.000341, -0.000175]). Swing MAE worsens from 0.179011 to 0.179332 (difference +0.000321, interval [+0.000267, +0.000376]). The swing evidence is mixed; activation follows the requested component choice and does not imply every loss improves.

Take RMSE improves from 0.046596 to 0.045385 runs (difference -0.001211, interval [-0.001475, -0.000946]). Take MAE also improves. The take path preserves its backbone-specific historical actor offsets, joint posterior covariance and unknown-actor population variance integration, with separate strictly past-only weekly calibration.

## Coverage and scores

| year | eligible | original_supported | supported | additional_exclusions | coverage | qualified |
| --- | --- | --- | --- | --- | --- | --- |
| 2024 | 704446 | 535453 | 535411 | 42 | 0.760045482549408 | 359 |
| 2025 | 704987 | 542384 | 540298 | 2086 | 0.766394273936966 | 337 |

Primary support retains every original exclusion, including the 34 RE288 ordering-warning states, and additionally excludes extrapolations in the new contact/BIP prior-season histories. It never expands baseline support. The 2025 hybrid has 540,298 supported pitches (76.64% coverage) and 337 qualified hitters, versus 339 in v6. Qualification remains 500 supported pitches, 60 supported games and 70% coverage. The reference scale is reused without recentering.

Scores are rebuilt as `(observed_swing - league_swing_probability) * (Q_swing - Q_take)`. Every hitter receives newly sampled 500-draw whole-game conditional intervals; no old GAM model bounds or uncertainty-bearing preference labels are copied. These intervals exclude full model-fitting, hyperparameter and calibration-history uncertainty. Unchosen action values have no factual validation.

Local outputs: `reports/hitter_stat_xgboost/` contains metrics, paired intervals, all/qualified hitters, hitter game totals, largest movements and verification. Clean keyed score artifacts, the fixed reference and integration manifest live under `data/models/hitter_stat_xgboost/`. These generated artifacts and fitted weights are excluded from Git under the repository's existing source-only policy.

## Build and verification

Run the ordered stages in [REPRODUCING.md](REPRODUCING.md). The current entry point rebuilds the hybrid, independently verifies the artifacts, then updates `data/models/current_hitter_stat.rds`. A verification failure leaves the previous active pointer unchanged. The v6 directory and its original builder are preserved; the previous pointer is archived before activation.

Offline source integrity, Python/R syntax, Python extractor suites, and 10 offline R suites passed, including six new hybrid unit checks. 35 full-data hybrid artifact checks passed, covering exact cohorts, selected components, unchanged tip/value predictions, take parity, support, factual targets, branch arithmetic, singleton/order/label independence, hitter aggregation, fresh game intervals and reference scaling. The isolated take comparison separately passed 556 independent checks, 15 focused checks and 21 actor unit checks. A fresh data reacquisition/full model refit was not rerun for this source release.
