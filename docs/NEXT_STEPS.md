# Next research steps (not implemented in this baseline)

1. Revisit the run-expectancy foundation. Compare coherent count-aware partial pooling with the frozen RE24/RE288 table using chronological historical evaluation. Inspect the 34 flagged states, walk/strikeout/inning-ending transitions and runner advancement. Do not silently clamp reversed state values or optimize on already inspected 2025 results and call them a fresh holdout.
2. Improve expected value conditional on a ball in play. Compare the existing direct-value GAM against interpretable out/single/double/triple/homer and runner-advancement distributions. BIP probability and BIP value are separate tasks. State-specific continuation values remain necessary.
3. Test contact-quality targets such as predicted xwOBA on contact or a contact-outcome distribution. xBA alone predicts a hit and omits hit-type value. Neither xBA nor xwOBA is already in the baseline's current-pitch scoring path.
4. Recompute complete swing/take comparisons, support and hitter uncertainty for any accepted candidate. Freeze a final candidate before genuinely new compatible evaluation data; harmonize 2026 geometry before using it.

## xBA leakage boundary

Observed same-pitch xBA uses post-contact exit velocity, launch angle and sometimes seasonal sprint speed. Inserting that observed xBA into a decision score would mix execution with the decision and leaves no equivalent value for taken pitches, whiffs or fouls.

A candidate may instead **learn to predict** contact quality from the available pitch and past-only player information, conditional on a fair ball in play. Training labels may include post-contact measurements; scoring predictors may not. Then combine that prediction with P(BIP if swing), other swing branches and context-specific run values. Verify vendor target vintages, training-data overlap and temporal construction before treating any imported expected metric as leakage-safe.

Sources: [MLB xBA definition](https://www.mlb.com/glossary/statcast/expected-batting-average), [MLB xwOBA definition](https://www.mlb.com/glossary/statcast/expected-woba).
