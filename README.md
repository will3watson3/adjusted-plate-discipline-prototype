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

## Results and limits

The exploratory 2025 snapshot contains **542,384 supported pitches and 339 qualified hitters**. Regional umpire effects improved supported-take log loss from **0.224218 to 0.223287**. Take-value RMSE changed from **0.046605 to 0.046538 runs**, but its paired error interval included zero. The original integration passed 9 unit and 52 artifact checks; a fresh clone must rebuild its own artifacts for artifact verification.

These are provisional descriptive rankings. Unchosen actions are unobserved, 2025 is no longer an untouched test, and uncertainty is conditional. The frozen RE288 baseline has 34 flagged states excluded from primary scores; unsupported BIP extrapolations are also excluded. Small ranking differences should not be treated as established skill differences. The next research priorities are improving state valuations and BIP predictions. Same-pitch observed xBA is not a scoring input. Applying the model to 2026 requires zone/coordinate harmonization.

Raw data, fitted models, libraries and generated reports are excluded. The original model source and historical protocols remain unchanged; run individual modules from `model/` or use the rebuild runner.

## Sources

[baseballr](https://billpetti.github.io/baseballr/) · [Baseball Savant](https://baseballsavant.mlb.com/) · [MLB game feeds](https://statsapi.mlb.com/api/v1/game/745444/boxscore) · [mgcv](https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/mgcv-package.html)

Third-party data and dependencies retain their own terms and licenses. No distribution license has been selected for this prototype.
