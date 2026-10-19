# Called-strike diagnosis and targeted temporal experiment

Goal: improve ordinary-take probability/value calibration without damaging the complete supported decision comparison. Preserve all v1-v3 artifacts and support masks. Season averages remain descriptive; no half-season stability gate.

Audit: all eligible ordinary takes and separately the original supported decision population, 2023-2025. Compare predicted/actual by absolute and normalized location, count, handedness, recorded zone height, season and catcher-history group. Inspect same-batter year changes in recorded bounds. Physical batter height is unavailable unless verified in cache; zone height is only a proxy. These observational checks cannot establish whether measurement changes caused residual drift.

Candidates fixed before running: v2 unchanged; intercept on all earlier current-season takes (minimum 2000); intercept on last 28 calendar days (minimum 2000); refreshed same-capacity GAM. Intercepts are refit weekly using strictly earlier dates, preserve probability ordering, and use all ordinary takes rather than selecting on supported-only outcomes. No same-day outcomes. Cold start is identity.

Historical development: 2024 May-June and July-season end, disjoint windows. Refresh model at May 1 and July 1 using 2022-2023 plus earlier 2024 ordinary takes. Keep prior-season multilevel framing distributions fixed: remove the original catcher smooth completely, fit the location/context GAM with a fixed latent-catcher-mean offset, integrate posterior catcher uncertainty when predicting. This is conditional on prior framing estimates, not a new jointly estimated latent model. Current-season takes never estimate individual catcher skills. Same-capacity refresh tests temporal fit, not new complexity.

Selection: paired whole-game loss differences; choose simplest candidate within one paired game SE of best pooled historical log loss, exclude candidates clearly worse than identity in either historical window. Order identity, expanding intercept, 28-day intercept, refreshed GAM. Freeze selection and protocol before loading 2025 outcomes. 2025 has already been inspected and is exploratory sequential replay, not an untouched holdout. A prequential replay can legitimately learn only from games earlier than the scored date.

2025 refreshes, if selected or compared: same May/July schedule, train on 2022-2024 and earlier 2025; same prior-2024 latent framing evidence. Score before May with unchanged baseline. Report matched May-on and all-season metrics separately.

Combined checks: keep swing values, reference policy and support fixed to v2; replace q_take exactly from frozen RE288 branches. Report take bias/RMSE and paired MSE CI, gap changes/sign changes, decision-score shifts, hitter-average changes. Alternative-action accuracy cannot be validated from observed outcomes. No publication promotion unless take log loss and take-value MSE improve with intervals below zero, with no changes to supported swings or policy; report conditional uncertainty limitations and do not reuse stale v2 intervals as new intervals.

Artifacts: reports/called_strike_v4 and data/models/called_strike_v4. Record code/input hashes, training endpoints, weekly coefficients, predictions, historical selection, audit tables, combined results and verification. No automatic edits to frozen model code or past reports.
