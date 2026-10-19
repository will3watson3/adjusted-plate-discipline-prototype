# Adjusted Plate Discipline +
### Prototype With Next Game State Valuation

A research baseline for describing a hitter's season-average swing/take decision quality. It estimates the value of **both actions on each supported pitch**, compares the recorded choice with a contextual league policy, and aggregates those comparisons into a descriptive hitter score.

**Status:** working prototype with exploratory 2025 results. These are provisional model-based rankings, not established causal measures of plate discipline. Current-pitch hits, home runs, exit velocity and launch angle do not directly reward a decision.

## Start here

```bash
# Requires R >= 4.4, Python >= 3.9, and curl.
Rscript scripts/install_dependencies.R
python3 scripts/check.py
Rscript examples/decision_score_demo.R
python3 scripts/reproduce.py --plan
```

An optional GitHub Actions workflow is provided at [`docs/ci/offline-checks.yml`](docs/ci/offline-checks.yml); copy it to `.github/workflows/check.yml` to enable hosted CI. Local checks run independently of GitHub Actions.

The example is synthetic and works without MLB downloads or fitted artifacts. `check.py` runs offline tests; it does not download data or refit the full MLB models. For a complete historical rebuild, follow [reproduction instructions](docs/REPRODUCING.md). That process downloads several seasons and runs substantial model fits; it is not a quickstart.

## How the score works

```text
Pitch + game state + past player information
  ├─ Swing model → P(whiff / foul / caught tip / ball in play) + expected BIP value
  └─ Take model  → P(called strike), adjusted for catcher, umpire and calibration
                        ↓
          runs scored + RE(resulting runners / outs / count)
                        ↓
          gap = expected swing value − expected take value
                        ↓
      pitch score = (actual swing − league swing probability) × gap
                        ↓
          supported-pitch season mean × 100 = runs per 100
                        ↓
          Decision+ = 100 + 10 × standardized 2024-reference deviation
```

Actual swing is 1 for a swing and 0 for a take. The league swing probability is a contextual benchmark, not a claim that league choices are optimal. Higher values mean better modeled choices relative to that policy on the pitches faced.

**Decision+ is a display scale:** 100 is the exposure-weighted qualified 2024 average; 10 points equal one 2024 reference standard deviation, not 10% improvement. Qualification requires 500 supported pitches, 60 games with supported pitches and 70% coverage. Full-season averages are unshrunk; half-season stability is not a current release criterion.

## Repository map

| Location | Purpose |
| --- | --- |
| [model/](model/README.md) | Preserved model modules, public-data acquisition, frozen experiment protocols and associated tests |
| [scripts/](scripts/) | Dependency installation, offline checks, staged rebuild orchestration and source-integrity checks |
| [examples/decision_score_demo.R](examples/decision_score_demo.R) | Small, explicitly synthetic valuation/decision example |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | Inputs and responsibility of each component |
| [docs/REPRODUCING.md](docs/REPRODUCING.md) | Environment, data requirements, execution order and reproducibility limits |
| [docs/BASELINE_STATUS.md](docs/BASELINE_STATUS.md) | Measured results, exclusions, uncertainty and unfinished work |
| [docs/NEXT_STEPS.md](docs/NEXT_STEPS.md) | Planned RE/BIP work and the xBA leakage boundary |
| [docs/source_manifest.csv](docs/source_manifest.csv) | SHA-256 provenance of every preserved source/protocol/test file |

## Baseline evidence

The existing 2025 exploratory snapshot contains **704,987 eligible pitches, 542,384 supported comparisons and 339 qualified hitters**. Adding regional umpire effects improved supported-take log loss from **0.224218 to 0.223287**. Factual take-value RMSE moved from **0.046605 to 0.046538 runs**, but the paired MSE interval included zero. The requested descriptive integration passed 9 unit and 52 artifact checks in the original workspace. Those counts describe that snapshot; a code-only clone must rebuild its own artifacts before running artifact verification.

See [aggregate evidence](docs/baseline_metrics.json) and [limitations](docs/BASELINE_STATUS.md). Raw pitch exports, game-response caches, fitted model RDS files, full hitter tables, local libraries and exploratory/discarded runs are intentionally not distributed. This repository has no bundled MLB data or pretrained scoring endpoint.

## Data sources and attribution

Data acquisition uses [baseballr](https://billpetti.github.io/baseballr/), [Baseball Savant](https://baseballsavant.mlb.com/), and public [MLB game feeds](https://statsapi.mlb.com/api/v1/game/745444/boxscore). Fitting uses R's [mgcv](https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/mgcv-package.html), data.table and a glmnet reference implementation in solver tests. Data providers' terms and software licenses remain their own; this repository does not relicense their data or dependencies. No software distribution license has been selected for the prototype.
