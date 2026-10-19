# Decision value validation and release contract

Goal: diagnose extreme hypothetical swings, withhold unsupported comparisons,
validate observed outcomes and uncertainty, then construct an explicitly
exploratory hitter measure before a single frozen 2025 evaluation.

Existing contact, post-contact, BIP value, called-strike, and RE288 fits remain
unchanged. All rows retain their original estimates; new supported estimates
are NA on rejected rows. Grain is game_pk + at_bat_number + pitch_number.
2021 histories and 2022-2023 fits remain the historical scoring version.

## Rules declared before new validation

1. Build support exclusively from 2022-2023 training features: 0.2-foot x by
   0.2 normalized-zone-height cells, separately by batting side and pitch family.
   Require at least 30 swings, 30 contacts, 30 verified BIP targets, and 30 takes
   in the cell. Require features inside each relevant training range and no
   novel category or missing modeled input. These are conservative diagnostics,
   not proof of multivariate overlap or counterfactual identification.
2. Fit a pooled league swing policy on 2022; confirm its fixed specification on
   2023, refit on 2022-2023, then confirm on 2024. Require reference swing
   probability in [0.05, 0.95] for primary comparisons. No outcome-valued input
   enters the policy. Retain personalized historical ability in the existing
   value models and label the resulting score accordingly.
3. Exclude RE ordering warnings and nonfinite/out-of-training-target-range BIP
   predictions from primary comparisons, without changing raw estimates.
4. Propagate GAM link standard errors through all five components. Use the sum
   of absolute component standard-error contributions as a conservative
   first-order bound without assuming cross-model independence. These are
   conditional model intervals, not causal or full-pipeline intervals; the
   frozen RE table and training histories are not estimated again.
5. Compare observed-action swing value against a training count-probability /
   state-count BIP baseline. Validate ordinary take values on audited matching
   transitions. Use whole-game bootstrap intervals (1,000 draws), calibration
   by predicted-value bin, action, pitch family, month, and support status.
   Do not pretend to observe alternative-action accuracy.
6. Primary validation gates: nonempty supported populations for both actions;
   all extreme predictions withheld; arithmetic/key/chronology invariants;
   supported observed-action mean bias within 0.02 runs and its 95% game
   bootstrap interval within [-0.02, 0.02]; no clear baseline disadvantage
   (upper 95% paired MSE-difference bound <= 0); policy log loss no worse than
   a training count baseline. Failure blocks published grades and 2025 access.
7. Build exploratory hitter summaries on *all supported pitches*, retaining
   near-zero gaps. Above-reference value = (observed_swing - p_ref) * gap.
   Show unsupported coverage and game-cluster sampling uncertainty separately
   from shared model uncertainty. Estimate empirical-Bayes shrinkage across
   hitters; freeze a 2024 reference center/scale only if spread is nonzero.
   Publish no Decision+ index until model, sampling, and reliability gates pass.
   Reference hitters require >=500 supported pitches, >=60 games, and >=70%
   coverage; at least 20 must qualify. Require median conditional sampling
   reliability >=0.5 and first/second-half rate correlation >=0.5 among hitters
   with >=100 supported pitches and >=15 games in each half. These are declared
   release criteria, not established properties of the metric. The uncertainty
   envelope adds game sampling SE to a worst-correlation shared-model SE bound.
8. A 2025 evaluation requires a saved passing 2024 gate artifact and a frozen
   metric manifest first. Use the same 2022-2023 models/rules without tuning on
   2025. If gates fail, preserve the lock, save exact failures, and leave a
   runnable guarded evaluation path.

Pseudo-code: load immutable inputs -> derive training support -> trace extreme
link terms -> annotate every 2024 pitch -> propagate conditional uncertainty ->
validate factual outcomes -> aggregate with coverage -> freeze gate decisions ->
open 2025 only on pass -> report held-out results without refitting.

References: [mgcv prediction and covariance](https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/predict.gam.html);
[positivity and extrapolation](https://pmc.ncbi.nlm.nih.gov/articles/PMC4107929/).
