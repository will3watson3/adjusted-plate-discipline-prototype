# Architecture of the current baseline

| Component | Inputs | Output / responsibility |
| --- | --- | --- |
| Data cleaning | Completed regular-season game schedules, Statcast pitch events, MLB player IDs | Deduplicated pitch keys, ordinary-action flags, pitch tracking, game state and past-pitch fields; full events retained for state transitions |
| Historical player profiles | Shrunken 2021 contact tendencies and contact-quality summaries, with handedness/pitch-family splits | Past-only hitter/pitcher descriptors; current profiles remain frozen at 2021 |
| RE24 / RE288 | 2022-2023 completed, uncensored half-innings; runners/outs and optionally count | Expected runs remaining. RE288 uses lambda-250 shrinkage toward RE24. Player identity is not an RE predictor |
| Contact probability | Location relative to recorded zone, pitch speed/movement/spin/extension, count, hands, game context and historical profiles | P(contact if swing); complement is whiff |
| Post-contact distribution | Pitch characteristics, count, hands and historical profiles | P(BIP if contact) and P(caught tip if non-BIP contact), giving coherent BIP / ordinary-foul / caught-tip branches |
| BIP value | Pre-contact pitch features, base/out/count state and historical profiles | Expected runs scored plus continuation value conditional on putting the pitch in play. Current EV, launch angle and result are not predictors |
| Called-strike probability | Pitch location and recorded zone bounds, count, hands and pitch characteristics | Frozen 2022-2023 GAM probability baseline; original catcher smooth is removed before the replacement joint effect |
| Joint catcher/umpire model | Prior-season called takes, catcher, home-plate official and coarse zone region | Partially pooled catcher, umpire and umpire-by-region latent effects plus common region correction. New identities receive population priors |
| Weekly calibration | Only earlier current-season ordinary takes | Expanding weekly intercept correction, with cold-start fallback |
| Swing value | Four swing-branch probabilities, ordinary branch states and predicted BIP value | Probability-weighted expected remaining-run value conditional on swing |
| Take value | Called-strike probability, ball/strike state transitions | Probability-weighted expected remaining-run value conditional on ordinary take |
| League swing policy | 2022-2023 choices, location, count, hands, pitch type, speed and movement | Contextual league P(swing), without using the current observed action as a predictor |
| Decision score | Recorded action, league P(swing), Q_swing − Q_take | Above-reference modeled decision value for that pitch |
| Support checks | Training ranges/density, modeled categories, action overlap, RE ordering and extreme-value checks | Explicit exclusions with reasons; unsupported values never enter primary hitter means |
| Hitter aggregation | All supported recorded choices | Unshrunk full-season/season-to-date mean, runs per 100, exposure and coverage, fixed-reference Decision+ |
| Uncertainty | Model coefficient covariance, shared joint effects, unknown-group priors, weekly histories and hitter game sequences | Conditional model bounds and separate sampling/block precision diagnostics; do not alter the point ranking |

## Outcome value convention

`Q(outcome) = runs scored on play + RE(resulting state)`.

`net value(outcome) = Q(outcome) − RE(before)`.

The same starting RE cancels when comparing swinging with taking. Ordinary fouls below two strikes advance the count; with two strikes they retain the count. A caught tip can strike out. Walks force runners; an ordinary strikeout adds an out and starts the next batter at 0-0 when the inning continues. Ordinary branches do not model every extra runner action.

BIP labels use the **official immediate post-play state**, with later unrelated runner movements excluded. A BIP forecast averages possible outcomes; scoring never substitutes that pitch's actual homer/out for the pre-contact expected value.

## Time boundaries

- 2021: hitter/pitcher profile inputs.
- 2022 fit / 2023 selection / 2022-2023 refit: core outcome, value and policy models.
- 2024: historical development/forward comparisons; later upgrades also selected historically.
- Predicting 2025: catcher/umpire effects use 2024 evidence; weekly calibration uses only earlier 2025 games.
- 2025 has already been examined repeatedly and is exploratory reanalysis for v6.
- 2026 is unsupported pending coordinate and ABS-zone harmonization.
