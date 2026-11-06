# Continuous hitter surface v1 — frozen specification

2026-11-03. Specification fixed before evaluating any new 2024 predictions. No development search. Train 2022–23; previously inspected 2024 is exploratory.

Reuse the exact hitter_region_v1 updated-history population GAMs and categorical fits. A = population; B = population + saved categorical adjustment; C = population + six-coefficient quadratic hitter surface in existing x and normalized z. Same offsets, families, predictors, targets and prior-season profiles in all arms (2022→2021, 2023→2022, 2024→2023).

The surface spans 1, x, z, x², xz, z². Standardize coordinates using training rows only. Center the five shape columns on the pooled training location distribution and whiten their training crossproduct to identity. Thus the intercept is the mean deviation over that fixed training distribution, and shape has zero mean there. The six coefficients have independent N(0,tau²) priors with ONE learned tau per stage, shared across hitters and basis coefficients. All directions, including linear and intercept, are penalized. Nearby locations share the same polynomial; no bins are used for prediction. This deliberately restrictive global quadratic is a low-dimensional smooth, not a flexible local spline.

B already permits hitter-wide shifts through its zone coefficients. Neither B nor C gets an additional overall personnel term. B can equivalently be decomposed into a hitter zone-average intercept and zero-sum zone contrasts; C explicitly separates its intercept and centered shape. Their induced covariance/prior geometry differs, necessarily, so this tests these two pooled representations, not continuity in isolation from basis/penalty choice. No change to the population backbone.

Use lme4's established sparse binomial Laplace machinery (nAGQ=1), constraining the random coefficient covariance to tau² I through its modular interface. Scalar optimize on log(tau) in [log(1e-4),log(4)], tolerance 1e-6; compare exact zero boundary, fail at upper bound. Conditional-mode tolerance 1e-9. Record penalized gradients and effective degrees of freedom trace(H (H+I/tau²)^-1). Independently compare the constrained multi-coefficient likelihood/modes with dense quadrature-free Laplace calculations on synthetic data and the scalar case with ordinary glmer. No custom production likelihood solver.

Observed bundle counts: contact 675,353 rows/818 hitters/10,181 categorical groups; BIP 515,379/816/10,036. C uses at most 4,908 coefficients, six nonzeros per row, block-diagonal by hitter. Sparse lme4 is practical on the previously documented 16 GiB machine. Current hardware sysctl is sandbox-denied; record actual sparse matrix sizes. Discrete bam can avoid a full observation-by-coefficient matrix; no claim it is infeasible. Fixed offsets and sparse lme4 isolate the requested comparison simply.

Unseen/missing hitters receive exactly zero adjustment. Nonfinite locations or invalid zone height are rejected by the existing population input contract; the surface helper returns zero adjustment with an explicit invalid-location status (not a valid population prediction). Raw finite extrapolations are retained for audit. Use existing primary support and updated feature range/BIP-target safeguards, common across arms; never promote unsupported rows.

Only contact and BIP change. Tip probability and BIP value are the same updated-history control predictions in ALL arms. Preserve take, catcher/umpire, RE288 transitions, policy, qualification and reference. Fresh 500-game bootstrap intervals are conditional sampling uncertainty, without model/offset/refit uncertainty. No old intervals or preference labels.

Report paired losses vs both controls, calibration, descriptive source-zone/history/training hitter-zone and hitter-total exposure slices, pooling/convergence/fallback, swing values and hitter scores. Slices are not selected on outcomes. Independently reconstruct every saved slice from keyed predictions. Tests cover histories, invalid inputs, label independence, smoothness, shrinkage, unseen fallback, batching/order, serialization and hashes of original files and the entire existing experiment.

Better factual prediction does not establish better counterfactual decision evaluation.

Method reference: https://lme4.github.io/lme4/reference/modular.html
