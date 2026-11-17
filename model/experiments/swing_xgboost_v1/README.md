# XGBoost swing path

An isolated four-branch swing-value comparison using the previously frozen contact XGBoost and three new stage-specific XGBoost models. [Frozen protocol](PROTOCOL.md) · [Generated report](reports/REPORT.md).

Each stage retains its population GAM's predictor information, preprocessing, targets and final training cohort. Development uses 2022→2023, including training-only 2022 run expectancy for BIP-value development. All models freeze before the new exploratory 2024/2025 comparison. The active scorer and take/policy/reference artifacts are preserved.

```sh
Rscript experiments/swing_xgboost_v1/tests.R
Rscript experiments/swing_xgboost_v1/train.R
Rscript experiments/swing_xgboost_v1/evaluate.R
Rscript experiments/swing_xgboost_v1/verify.R
Rscript experiments/swing_xgboost_v1/verify_prediction.R
Rscript experiments/swing_xgboost_v1/report.R
```

Training resumes completed stages only when their training-code/protocol hashes match and refuses an already aggregate-frozen experiment. For a new experiment, use a new version directory and update SROOT. Existing project data, GAM bundles and the original contact-XGBoost artifacts are required; input_hashes.csv records dependencies.

For raw model inference, source predict.R and call predict_swing_xgboost(pitches, arm='xgboost'); arms 'gam' and 'contact_only' are also available. Input needs pitch keys, CT_INPUTS fields and season; outcome columns are unnecessary. Estimates include branch probabilities, Q(swing) and swing run value. Callers must apply the existing support policy before constructing Decision+; raw estimates do not imply support or causal action values.

Reports retain identical-cohort component/integrated factual metrics, calibration and slices, paired game intervals, supported scores, boundary sensitivity and unsupported tails. This is exploratory 2025 evidence, not an untouched full-pipeline holdout or automatic promotion.
