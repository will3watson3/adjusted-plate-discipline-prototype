# Season-average decision quality v3: calibration and precision

Goal: estimate season-to-date / full-season average modeled swing/take decision
quality, adjusted for pitch context and historical ability. Half-season correlation
is descriptive only, not a release gate for this estimand. The score remains
100 * mean((swing - reference_probability) * (Q_swing - Q_take)). Report raw
season averages, coverage and uncertainty rather than equating streaks with error.

Preserve all v1/v2 models, results, support contracts and completed 2025 test.
Create a new versioned calibration/scoring layer. Never fit to 2025 labels or
select methods using its results. 2025 is a previously inspected reanalysis;
there is no newly untouched test in this experiment.

Development: existing base fits use 2022–2023 and 2021 profiles. Fit calibration
on 2024 pitches before May 1 and evaluate May–June; independently fit before
July 1 and evaluate July onward. These evaluation periods do not overlap.
Use supported pitches and the correct factual population for each component.
The base model's fitting sample is disjoint from the calibration sample.

Fixed candidate set for contact, BIP|contact, tip|non-BIP contact, called strike
and reference swing policy: identity; intercept-only correction of base log odds;
intercept+slope correction; contextual correction adding count, pitch family and
bat/pitch handedness. Positive-valued BIP expectation: identity; multiplicative
ratio; power correction; contextual log-mean correction adding count, family,
bat side, outs and base state. No current-pitch outcomes, batter identity or
evaluation dates enter calibration predictors. Use fixed numerical link floors
only to evaluate finite log/logit bases; do not clamp raw extreme Q estimates.

Select each component by aggregated forward-period loss (binary log loss;
BIP mean squared error), preferring fewer parameters within one paired game SE
of the minimum. Reject candidates clearly worse than identity in either period
(positive lower 95% paired game interval). If no calibrated candidate qualifies,
retain identity. All candidates, periods and loss/calibration results are reported.
Refit only the chosen calibrators on all 2024 factual evidence, record that
cutoff, freeze the selection BEFORE reading the 2025 scoring sidecar, then run
the exploratory 2025 comparison. No iterative retuning using 2025 outcomes.

Calibration assessment: reliability diagrams, bias, Brier/log loss, calibration
intercept/slope, value RMSE and paired whole-game intervals. Inspect month,
count, family, catcher evidence and prediction bins. Combined factual value
checks compare observed actions only, never assert counterfactual accuracy.
Preserve every support rejection; a newly out-of-range calibrated BIP expectation
also becomes unsupported. Loss comparisons use identical rows.

Uncertainty: propagate corrected GAM covariance (Vc where available), game-cluster
robust calibration-coefficient covariance, and shared catcher uncertainty through
the combined expectation. Use sums of absolute first-order SD contributions when
cross-stage covariance is unknown. Report model uncertainty separately from
game-cluster sampling precision; neither is a proven nominal full-pipeline CI.
RE and history re-estimation and method-selection uncertainty remain unmodeled.

For full-season descriptive averages, observed choices/pitches are already known:
the game-resampling interval describes generalization across comparable games,
not uncertainty about which recorded decisions occurred. Show that distinction.
No automatic shrinkage of the raw descriptive score. Show an optional pooled
estimate separately if produced. No half-season correlation gate. No ranking
claims where conditional intervals overlap widely or coverage is poor.

Precision audit: full-season game bootstrap and analytic SE; model-only envelope;
prefix estimates at 100/250/500/1000 supported pitches; interval widths by sample
size and coverage; empirical resampling checks of the game-SE calculation with
the empirical season-game population as a known reference. Such resampling is
an internal check, not external validation of latent skill interval coverage.

Acceptance: exact keys/arithmetic/chronology; original artifacts unchanged;
finite calibrated probabilities, branches summing to one, finite uncertainty;
chronological candidate selection frozen before 2025 reads. Scientific performance
and limitations must be reported even when a candidate or overall model fails.
No model is promoted merely because an arbitrary statistic passes a threshold.

Sources: [calibration on disjoint data](https://scikit-learn.org/stable/modules/calibration.html),
[GAM posterior covariance](https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/predict.gam.html),
[GLM prediction](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/predict.glm.html).
