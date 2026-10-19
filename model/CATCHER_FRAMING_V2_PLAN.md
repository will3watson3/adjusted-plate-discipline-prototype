# Prior-season multilevel catcher framing, version 2

Goal: score catchers absent from the 2022–2023 vocabulary without treating their
skill as known. Decision owner: project user. Output: called-strike probability,
latent framing distribution, conditional uncertainty, and versioned decision
sidecars. Existing version 1 artifacts and its completed 2025 test stay immutable.

Model, declared before new fits:

    y_i ~ Bernoulli(logistic(eta_pitch_i + mu + u_c))
    u_c ~ Normal(0, tau^2)

`eta_pitch` is the existing called-strike GAM with its entire catcher term
excluded. Fit the intercept and catcher distribution by REML to **only the
previous season's ordinary takes**. This is a multilevel logistic offset model,
not a multiplication by cumulative framing runs. It estimates internally adjusted
framing from opportunities, not the official Savant framing statistic. Pitch
location/count/pitch characteristics adjust the opportunity quality; low-information
catchers shrink toward the estimated population mean. Remove the original catcher
term for every catcher, avoiding double counting. The population is observed MLB
catchers with prior-year takes, not all professional catchers.

For known catchers use the approximate empirical-Bayes latent posterior; for
catchers with no prior-year evidence use Normal(mu, tau^2), plus uncertainty in mu.
Integrate the logistic probability over this distribution with fixed Gaussian
quadrature. Use joint intercept/catcher covariance for known catchers. Add the
pitch-GAM and catcher standard deviations as a worst-correlation first-order
bound, since these stages are not independent. Share a catcher's uncertainty
across all their pitches; it must not disappear as a hitter sees more pitches.
Report plug-in population variance and conditional uncertainty limitations.

Chronology: 2023 development uses a 2022 pitch model and 2022 catcher evidence;
2024 historical validation uses the frozen 2022–2023 pitch model and 2023 catcher
evidence; exploratory 2025 reanalysis uses that same pitch model and 2024 catcher
evidence. Never use 2025 calls/framing to estimate a catcher effect. Historical
pitch-model fitting overlaps the catcher-evidence sample; offset uncertainty is
conditional, and nuisance/skill cross-covariance is not fully estimated. Future
skill drift is not estimated. Neither 2024 nor 2025 is a newly untouched test.

Compare the frozen scorer and zero-catcher fallback on identical ordinary-take
rows, with separate no-prior-history and previously-unseen-catcher slices. Report
log loss, Brier, calibration, and paired whole-game bootstrap loss differences.
Declare noninferiority margin 0.001 log-loss units: upper 95% paired difference
must be <=0.001 overall and among catchers absent from the frozen vocabulary
(at least 1,000 takes and 30 games); otherwise retain research-only status.
Also require convergence, finite posterior SD, no future features, exact keys,
label/batch invariance, and unchanged version 1 fingerprints. No selection among
alternative hierarchical specifications using 2025 outcomes.

In combined comparisons, remove only the catcher-novelty rejection when its
fixed categories are otherwise known; preserve all location, density, overlap,
RE and other support restrictions. Recompute take value and gap uncertainty.
Hitter summaries retain the original coverage/stability thresholds; no new
published grades or promotion merely because coverage improves.

Pseudocode: read previous-season ordinary takes -> exclude old catcher term ->
fit shared Normal catcher effects -> freeze prior-year artifact -> predict current
pitches using latent distributions -> compare factual calls -> update versioned
ordinary-take/decision sidecars -> report coverage, uncertainty and hitter gates.

Reference: [mgcv random effects](https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/random.effects.html)
and [posterior prediction covariance](https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/predict.gam.html).
