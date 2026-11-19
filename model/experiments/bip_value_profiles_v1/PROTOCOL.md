# Frozen BIP profile experiment

Three final 2022–23 quasi-Poisson/log GAMs, using identical verified runs-on-play
+ frozen RE288-after targets and the active GAM's preprocessing and specification.
A refits the active frozen-2021 EV baseline. B changes only EV history to Y−1.
C adds four **linear additive** rates: batter/pitcher barrel and ground-ball rate.
All other history predictors remain frozen at their active 2021 definitions.
No feature, shrinkage, model, exclusion, or calibration selection is performed.
Consequently no development selection is needed; 2024 and 2025 are exploratory.

EV uses the existing split summaries and original strengths: 50 tracked balls
to league, then 30 to player. New rates use the same prespecified 50/30 strengths
in every year. League means are pooled within source season/role, as in the
active EV implementation. Splits are opponent handedness × pitch family.
Historical population is all cleaned in-play batted balls, including ineligible
prediction rows, matching the existing summary population.

Barrel numerator: launch_speed_angle == 6; denominator: labels in 1:6.
Grounder numerator: bb_type == ground_ball; denominator: known types ground_ball,
line_drive, fly_ball, popup. Missing labels never count in either denominator.
These definitions are verified against https://baseballsavant.mlb.com/csv-docs.
Finite EV is an availability proxy; Statcast can include estimated measurements.
No claim of measured-versus-imputed tracking identification is made.

Overall = (sum + 50 × league)/(n + 50).
Split = (sum + 30 × shrunken overall)/(n + 30).
Unseen players use league. Missing or zero-denominator splits use shrunken
overall; zero-denominator players use league. Only the completed Y−1 season is
joined, never earlier fallback seasons or the current pitch's outcomes.

Report B−A and C−B RMSE, MAE and signed bias with 500 paired whole-game draws
(seed 20261118). Negative RMSE/MAE differences favor the candidate. Prediction
bins are fixed [0,.25,.5,.75,1,1.5,2,3,5,Inf); no clipping or recalibration.
Show base-out, family, both-player EV and per-metric exposure and missing-history
slices, denominator/coverage audits, and prediction tails.

Replay saved, independently verified active XGBoost scorer components by key.
Only Q_BIP changes; delta Q_swing = P(BIP|swing) × delta Q_BIP. Keep take, tip,
policy, RE, support, qualification and 2024 reference frozen. Flag candidate
history values outside final-training ranges but do not silently remove rows:
primary comparisons retain identical active support; also show common in-range
sensitivity. Factual swing metrics require an observed swing and verified label.
Modeled swing values on takes are audit-only. Scores remain descriptive.

For each hitter and each comparison, draw 500 fresh whole games conditional on
fixed models/support (seed 20261118 + year), using identical sampled games for
both arms. Qualification: >=500 supported pitches, >=60 games, >=70% coverage.
Intervals omit fitting uncertainty. No automatic adoption; all writes confined
to this experiment. Verification independently reconstructs the arithmetic,
joins, targets, cohorts, metrics, intervals and active baseline.
