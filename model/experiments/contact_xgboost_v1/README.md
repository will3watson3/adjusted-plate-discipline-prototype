# Contact XGBoost comparison

An isolated, matched-feature comparison of XGBoost and the existing population contact GAM. See [the frozen protocol](PROTOCOL.md) and [generated results](reports/REPORT.md).

The primary comparison holds the final 2022–23 training cohort, transformed predictors, missingness treatment and prior-season histories fixed. Three regularized tree depths are selected using 2022→2023 development only. The selected model is refit on 2022–23 and frozen before 2024/2025 evaluation. The original frozen-2021-history contact GAM is a secondary benchmark. This experiment does not score Decision+ or activate a model.

Run from the project root:

```sh
Rscript experiments/contact_xgboost_v1/tests.R
Rscript experiments/contact_xgboost_v1/train.R
Rscript experiments/contact_xgboost_v1/evaluate.R
Rscript experiments/contact_xgboost_v1/verify.R
Rscript experiments/contact_xgboost_v1/verify_prediction.R
Rscript experiments/contact_xgboost_v1/report.R
```

The training entry point refuses to overwrite an existing frozen experiment. For a fresh independent training run, create a new version directory and update XROOT in its functions.R. Evaluation and verification can be rerun against the existing frozen model. Required data and GAM artifacts are listed in reports/input_hashes.csv; dependency versions are recorded in reports/sessionInfo.txt.

Artifacts include native UBJ model, bundled preprocessing/history/schema, all development curves, keyed predictions, paired whole-game intervals, calibration and slice tables, tests and independent artifact verification. 2025 is exploratory because it has been inspected elsewhere in the project. Neither regularization nor later-season accuracy can prove absence of overfitting or counterfactual validity.

For batch or single-pitch inference, source `experiments/contact_xgboost_v1/predict.R` and call `predict_contact_xgboost(d)`. Supply pitch keys, the existing CT_INPUTS fields and season; outcome labels are unnecessary. The helper loads the frozen model and feature bundle.
