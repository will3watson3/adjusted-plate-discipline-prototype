# Adjusted Plate Discipline +
### Prototype With Next Game State Valuation

A research prototype that estimates the value of swinging versus taking each pitch, then summarizes a hitter's recorded choices relative to a contextual league policy.

```text
Pitch + game state + past player information
  ├─ Swing: whiff / foul / caught tip / ball-in-play probabilities and values
  └─ Take: called-strike probability, adjusted for catcher and umpire
       ↓
Outcome value = runs scored + RE(resulting runners, outs and count)
       ↓
Pitch score = (actual swing − league swing probability) × (swing value − take value)
       ↓
Supported-pitch season average → runs per 100 → Decision+
```

Actual swing is 1 for a swing and 0 for a take. Current-pitch home runs, exit velocity and launch angle do not directly reward the decision. Decision+ uses a fixed 2024 reference: 100 is the qualified hitter average; 10 points equal one reference standard deviation. Qualification requires 500 supported pitches, 60 games and 70% coverage.

## Run

Requires R >= 4.4, Python >= 3.9 and curl.

```bash
Rscript scripts/install_dependencies.R
python3 scripts/check.py
Rscript examples/decision_score_demo.R
python3 scripts/reproduce.py --plan
```

The checks and synthetic example need no MLB data. A full rebuild downloads multiple seasons and runs substantial fits; see [reproduction instructions](docs/REPRODUCING.md).

## Code guide

- [Architecture](docs/ARCHITECTURE.md): inputs and responsibility of each model.
- [model/](model/): acquisition, state values, swing/take models, support checks and hitter aggregation.
- [model/current_hitter_stat.R](model/current_hitter_stat.R): current hitter-score entry point after rebuilding artifacts.
- [scripts/](scripts/): dependency setup, offline checks and staged rebuilds.
- [examples/](examples/): synthetic scoring example.
- [Source manifest](docs/source_manifest.csv): hashes of preserved source, protocols and tests.

## Current model

The main scorer uses **XGBoost for contact probability, ball-in-play probability given contact, and the full take path**. The take backbone has its own prior-season joint catcher/umpire adjustments and past-only weekly calibration. Caught-tip probability and BIP value retain the active v6 GAMs. RE288, league swing policy, qualification and the frozen 2024 Decision+ scale stay fixed.

The hybrid is implemented in [model/hitter_stat_xgboost.R](model/hitter_stat_xgboost.R), with [model/current_hitter_stat.R](model/current_hitter_stat.R) as the entry point. It withholds original unsupported pitches and any added extrapolations in the new prior-season histories. Hitter intervals are freshly sampled conditional game intervals; earlier GAM model intervals are not reused.

See [the integration results](docs/XGBOOST_MAIN_MODEL.md) and [reproduction instructions](docs/REPRODUCING.md). The isolated contact, swing and take comparisons are included under `model/experiments/`. The full-swing comparison is historical evidence; its XGBoost tip and value models are **not used** by the current scorer. Continuous hitter surfaces remain a historical research experiment.

## Results and limits

The isolated 2025 complete take comparison reduced supported-take log loss from 0.223287 to 0.214115 and factual take-value RMSE from 0.046538 to 0.045328 runs. Both paired game intervals favored XGBoost. Contact and BIP-given-contact probability also improved in their isolated comparisons. The exact hybrid uses unchanged v6 tip/value GAMs and is evaluated separately in the integration summary.

These are descriptive model-based rankings. 2024 and 2025 are already inspected exploratory data; unchosen actions remain unobserved. Game intervals condition on fitted models and realized weekly histories and exclude full fitting uncertainty. The frozen RE288 baseline has 34 ordering-warning states excluded from primary scores. Applying the model to 2026 requires zone/coordinate harmonization.

Raw data, fitted models, libraries and generated reports are excluded from Git. The source release rebuilds these artifacts locally. The original v6 builder is preserved as `model/current_hitter_stat_v6.R` for the historical prerequisite stage.

## Sources

[baseballr](https://billpetti.github.io/baseballr/) · [Baseball Savant](https://baseballsavant.mlb.com/) · [MLB game feeds](https://statsapi.mlb.com/api/v1/game/745444/boxscore) · [mgcv](https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/mgcv-package.html)

Third-party data and dependencies retain their own terms and licenses. No distribution license has been selected for this prototype.
