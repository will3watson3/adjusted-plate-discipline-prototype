# Selected continuous hitter location model

We selected the continuous surface for contact and BIP-given-contact prediction after it improved factual prediction beyond the existing categorical hitter-zone adjustment. It remains isolated from the active v6 scorer pending scoring integration and fresh model-uncertainty work.

## Controlled comparison

Train on 2022–2023 and evaluate on previously inspected exploratory 2024. All three arms use the exact same updated-history population GAM offset. Histories are prior-season for training and evaluation alike. The continuous candidate adds a six-term quadratic in existing horizontal and normalized vertical location, with one learned Gaussian variance shared across hitters and coefficients per stage. Training-only centering separates the hitter-wide intercept from location shape. The categorical benchmark already permits hitter-wide shifts through its zone effects; neither arm gets an extra personnel effect.

| Stage | Evaluation N | Population log loss | Categorical log loss | Continuous log loss | Continuous − categorical [95% interval] |
|---|---:|---:|---:|---:|---|
| Contact given swing | 337,575 | 0.441588 | 0.437937 | 0.436506 | −0.001431 [−0.001753, −0.001132] |
| BIP given contact | 258,914 | 0.668918 | 0.667345 | 0.666073 | −0.001272 [−0.001534, −0.001011] |

Brier score also improves: 0.138444 → 0.137989 for contact and 0.237499 → 0.236920 for BIP. Calibration is slightly worse: contact calibration slope moves from 1.0093 to 0.9841, and BIP mean prediction error increases from +0.00645 to +0.00706. Intervals use 500 paired game resamples and describe conditional sampling uncertainty, without training refits or model uncertainty.

The coefficient SDs are 0.192058 for contact and 0.101426 for BIP. Effective degrees of freedom are 3,121 and 2,169 out of 4,908 and 4,896 coefficients. Sparse hitters remain strongly pooled; unseen hitters receive the population prediction exactly. This compares two specific bases and penalty structures, rather than isolating continuity from every other representation choice.

## Score replay and safeguards

Caught-tip probability and BIP value are fixed at the updated-history population predictions across all arms. RE288 transitions, take/catcher/umpire values, frozen policy, reference and qualification rules remain unchanged. The common cohort has 535,129 supported pitches, with 324 additional exclusions relative to the original support mask and 359 qualified hitters.

Against categorical pooling, the median absolute qualified-hitter change is 0.303 Decision+ points; the maximum is 3.009. Unsupported extrapolations remain in local audit files and never enter primary scores. The original run passed all synthetic checks and 340 independent artifact checks, including independent reconstruction of slice metrics and bootstrap intervals. These are historical run results; release offline checks do not claim a fresh full-data rebuild.

Better factual prediction does not establish better counterfactual decision evaluation. Observed swings and contacts are selected populations, and 2024 is not an untouched holdout. No claim is made about continuous-surface performance on 2025 or 2026.

## Source and reproduction

- [Continuous surface implementation](../model/experiments/hitter_surface_v1/surface.R)
- [Frozen specification](../model/experiments/hitter_surface_v1/PROTOCOL.md)
- [Training, evaluation and score replay](../model/experiments/hitter_surface_v1/run.R)
- [Synthetic tests](../model/experiments/hitter_surface_v1/tests.R) and [artifact verification](../model/experiments/hitter_surface_v1/verify.R)
- [Categorical baseline](../model/experiments/hitter_region_v1/run.R)
- [Reproduction instructions](REPRODUCING.md#continuous-hitter-extension)

Source and a compact results summary are published. Raw data, fitted models, keyed predictions, generated reports, dependency snapshots and logs are rebuilt locally and excluded from Git.
