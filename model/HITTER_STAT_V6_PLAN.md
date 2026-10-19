# Umpire-adjusted season hitter statistic

Authorized October 19, 2026: connect the historically selected umpire-by-region
strike model to the final hitter statistic. This explicitly adopts the candidate
for descriptive scoring; it does not change the earlier inconclusive run-value
promotion test or turn the previously inspected 2025 season into a fresh holdout.

Pseudocode:
1. Verify frozen upstream artifacts. Join by the exact pitch key. Replace only
   strike probability and its downstream take value, gap and decision score.
2. Keep frozen swing branches, league swing reference and support exclusions.
3. Sum signed hitter gradients before covariance propagation. Propagate the
   joint fixed/catcher/umpire/region covariance, including unseen-group priors.
   Recompute weekly-intercept uncertainty from strictly earlier games. Sum
   standard deviations across separately fitted stages and overlapping weeks
   as a conservative dependence bound; do not assume independent pitches.
4. Report supported-pitch runs per 100, exposure and coverage. Define Decision+
   as 100 + 10 times the deviation from the qualified 2024 hitter mean divided
   by its SD, weighted by supported exposure. Freeze that reference for 2025.
   Qualification: >=500 supported pitches, >=60 games, >=70% coverage.
5. Keep descriptive season estimates unshrunk. Separate conditional model
   uncertainty from game-sampling precision and 7/14-game block sensitivity.
   No half-season reliability release requirement. Unsupported pitches do not
   contribute. Intervals are first-order conditional approximations, not proof
   of causal or counterfactual accuracy; RE288 and hyperparameter uncertainty
   are not covered. No 2026 application without geometry harmonization.
6. Verify arithmetic, key/actor preservation, frozen support, standardized index,
   repeat-group uncertainty and cancellation, and write an active local artifact
   manifest for the adopted scorer. Preserve upstream artifacts unchanged.
