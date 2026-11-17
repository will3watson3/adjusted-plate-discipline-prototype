# Architecture of the current scorer

| Component | Inputs | Output / responsibility |
| --- | --- | --- |
| Data cleaning | Completed regular-season game schedules, Statcast pitch events, MLB player IDs | Deduplicated pitch keys, ordinary-action flags, pitch tracking, game state and past-pitch fields; full events retained for state transitions |
| Historical player profiles | Shrunken 2021 contact tendencies and contact-quality summaries, with handedness/pitch-family splits | Contact and BIP probability use immediately preceding-season histories; retained tip/value GAMs keep their v6 2021 profiles |
| RE24 / RE288 | 2022-2023 completed, uncensored half-innings; runners/outs and optionally count | Expected runs remaining. RE288 uses lambda-250 shrinkage toward RE24. Player identity is not an RE predictor |
| Contact probability | Location relative to recorded zone, pitch speed/movement/spin/extension, count, hands, game context and historical profiles | XGBoost P(contact if swing); complement is whiff |
| Post-contact distribution | Pitch characteristics, count, hands and historical profiles | XGBoost P(BIP if contact) and retained v6 GAM P(caught tip if non-BIP contact), giving coherent branches |
| BIP value | Pre-contact pitch features, base/out/count state and historical profiles | Expected runs scored plus continuation value conditional on putting the pitch in play. Current EV, launch angle and result are not predictors |
| Called-strike probability | Pitch location and recorded zone bounds, count, hands and pitch characteristics | Frozen 2022-2023 XGBoost backbone, with actor identity excluded; joint effects are reestimated against its own historical offsets |
| Joint catcher/umpire model | Prior-season called takes, catcher, home-plate official and coarse zone region | Partially pooled catcher, umpire and umpire-by-region latent effects plus common region correction. New identities receive population priors |
| Weekly calibration | Only earlier current-season ordinary takes | Expanding weekly intercept correction, with cold-start fallback |
| Swing value | Four swing-branch probabilities, ordinary branch states and predicted BIP value | Probability-weighted expected remaining-run value conditional on swing |
| Take value | Called-strike probability, ball/strike state transitions | Probability-weighted expected remaining-run value conditional on ordinary take |
| League swing policy | 2022-2023 choices, location, count, hands, pitch type, speed and movement | Contextual league P(swing), without using the current observed action as a predictor |
| Decision score | Recorded action, league P(swing), Q_swing − Q_take | Above-reference modeled decision value for that pitch |
| Support checks | Training ranges/density, modeled categories, action overlap, RE ordering and extreme-value checks | Explicit exclusions with reasons; unsupported values never enter primary hitter means |
| Hitter aggregation | All supported recorded choices | Unshrunk full-season/season-to-date mean, runs per 100, exposure and coverage, fixed-reference Decision+ |
| Uncertainty | Model coefficient covariance, shared joint effects, unknown-group priors, weekly histories and hitter game sequences | Fresh 500-draw conditional whole-game sampling intervals; full model fitting uncertainty is not estimated and no v6 model bounds are copied |

## Outcome value convention

`Q(outcome) = runs scored on play + RE(resulting state)`.

`net value(outcome) = Q(outcome) − RE(before)`.

The same starting RE cancels when comparing swinging with taking. Ordinary fouls below two strikes advance the count; with two strikes they retain the count. A caught tip can strike out. Walks force runners; an ordinary strikeout adds an out and starts the next batter at 0-0 when the inning continues. Ordinary branches do not model every extra runner action.

BIP labels use the **official immediate post-play state**, with later unrelated runner movements excluded. A BIP forecast averages possible outcomes; scoring never substitutes that pitch's actual homer/out for the pre-contact expected value.

## Time boundaries

- Contact/BIP probability profiles: immediately preceding season; retained tip/value profiles: frozen 2021.
- 2022 fit / 2023 selection / 2022-2023 refit: core outcome, value and policy models.
- 2024: historical development/forward comparisons; later upgrades also selected historically.
- Predicting 2025: catcher/umpire effects use 2024 evidence; weekly calibration uses only earlier 2025 games.
- 2024 and 2025 have already been examined repeatedly and are exploratory hybrid evaluation.
- 2026 is unsupported pending coordinate and ABS-zone harmonization.

## Historical continuous hitter extension

`experiments/hitter_surface_v1/` adds a six-term quadratic surface per hitter to the fixed updated-history GAM logits for contact and BIP-given-contact. Its centered shape terms and hitter intercept share one learned Gaussian coefficient variance per stage. Sparse hitters shrink toward the population prediction; unseen hitters receive exactly zero adjustment. The existing pitch-family predictors remain in the population GAM.

The comparison reuses `experiments/hitter_region_v1/` for the updated-history GAMs and categorical benchmark. `compare_contact_personnel.R` supplies chronological contact profiles: 2022 rows use 2021, 2023 use 2022, and 2024 use 2023; contact-quality histories follow the same chronology. The original v6 baseline's frozen-2021 history behavior above remains unchanged.

Only contact and BIP probability differ between comparison arms. Caught-tip probability and BIP expected value use the same updated-history population predictions in every arm. The replay preserves take values, RE transitions, policy, qualification and the frozen reference. It applies a common supported cohort and saves fresh conditional game-sampling intervals. Baseline model uncertainty intervals are not reused.

This historical research extension is not used by the current XGBoost scorer. No 2025 continuous-surface scoring or new model-uncertainty propagation is claimed. See [the experiment summary](CONTINUOUS_HITTER_EFFECTS.md).

## Main integration

`hitter_stat_xgboost.R` loads the frozen contact and BIP probability boosters, retains the active v6 tip/value GAMs, and uses the separately fitted complete XGBoost take path. Prediction uses cleaned pitch/context inputs, never same-pitch outcome labels. Actor evidence, Normal uncertainty integration and weekly ledgers follow the take experiment. All historical inference is restricted to audited 2024/2025 dates.

The scorer applies original support plus new-history training-range restrictions, recomputes coherent branch probabilities and both action values, and aggregates `(observed_swing - league_probability) * (Q_swing - Q_take)`. The original 2024 scale is reused without recentering. Activation occurs only after independent artifact verification succeeds. The v6 artifact directory is preserved.
