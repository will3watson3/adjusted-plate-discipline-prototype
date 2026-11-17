# Frozen take-path comparison

Prespecified before candidate later-season results. No active scorer changes.

Compare the adopted GAM, excluding its complete `s(catcher)` term, with XGBoost
using exactly the variables in that fitted formula except `y` and `catcher`.
Use `cs_transform` and the GAM training preprocessing: normalized vertical
location, numeric medians, existing missing indicators, and complete factor
levels. Encode each factor with all one-hot levels. No additional predictors,
actor IDs, same-pitch labels, contact outcomes, or sequencing features enter
the backbone. Record the derived allowlist and input hashes.

Develop on eligible ordinary takes from 2022; validate on 2023. Search depths
3, 4, 5 only: logistic/log loss, eta .05, min_child_weight 50, lambda 10,
alpha 1, subsample .8, colsample_bytree .9, CPU hist/max_bin 256, two threads,
seed 20261117. At most 1,200 rounds, early stopping after 60 rounds. Select
minimum 2023 log loss (ties: shallower depth), record capped candidates, refit
on the identical GAM 2022–23 cohort for the selected round count.

Reuse `u5_fit(..., 'umpire_region_model', ...)`, `u5_predict` and `u5_online`.
Both arms reestimate the same joint catcher + umpire + umpire-region random
effects and regional fixed effects against their own offsets. Development
actors use 2022 evidence and 2022-only backbone offsets, predicting 2023.
Final actors for 2024 use 2023 evidence and final 2022–23 backbone offsets;
actors for 2025 use 2024 evidence and that same final backbone. Historical
backbone/evidence overlap is disclosed; no evaluation-year actor evidence.
Unknown actors receive the existing population variance; integrate the full
joint latent covariance using the existing Normal quadrature.

Freeze in two explicit stages: freeze selection, backbone, preprocessing,
training code and protocol before reading 2024 actor evidence; then fit the
prespecified 2025 actor layers and freeze all actor models before calculating
later-season evaluation metrics. Reading 2024 labels to fit the 2025 actor
layer is required historical training, never candidate tuning. Weekly
expanding calibration is separately applied to each complete path, in fixed
seven-day Jan-1 blocks, only using valid ordinary takes before block start,
with the existing 2,000-take threshold and probability floor of 1e-8.

Primary evaluation: 2025, already inspected and exploratory. Secondary:
2024 exploratory replication, never used to revise this specification.
Base and complete probabilities are evaluated separately on identical valid
ordinary-take cohorts (ball, blocked_ball, called_strike), both all eligible
and existing supported takes. Report log loss, Brier, signed mean bias,
calibration intercept/slope/bins, and count, region, zone-edge, handedness,
pitch-family and actor-history slices. Integrated RMSE/MAE/bias uses identical
audited factual ordinary-transition targets and frozen RE288 branches.

Replay Decision+ with active v6 swing values, league probability, original
support (including all 34 RE ordering-warning states), qualification
(500 pitches, 60 games, 70% coverage), and frozen 2024 scale. No candidate
support expansion. Any nonfinite candidate on original support is an error.
Report coverage, hitter movements, state errors, support boundaries and
hypothetical take values on swings as audit-only. All intervals are fresh
500-draw paired whole-game percentile intervals, seed 20261117, conditional
on fitted models and realized calibration history. Hitter intervals resample
that hitter's games. No full fitting uncertainty or causal claim.

Verification independently reconstructs tables and resamples games from keyed
predictions, audits provenance/feature parity and full-sequence factual
transitions, checks label independence, unknown actors, chronology, batch and
row-order invariance, serialization, scaling and unchanged original hashes.
No additional model search after later-season results.
