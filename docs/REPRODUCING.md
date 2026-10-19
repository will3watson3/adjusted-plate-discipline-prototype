# Reproducing the baseline

## Environment and scope

Use R >= 4.4, Python >= 3.9 and curl. R dependencies: data.table, mgcv, glmnet (reference solver/tests) and baseballr >= 2.0.0 (public-data acquisition). `scripts/install_dependencies.R` checks versions and installs missing/older packages to `model/.R-library/`. It also records the resolved session locally. Python acquisition and orchestration use only the standard library.

The source snapshot was exercised with R 4.4.2, data.table 1.18.2.1, mgcv 1.9-1, glmnet 4.1-10 and project-local baseballr 2.0.0; see `environment_tested.json`. The installer uses compatible available releases, not a fabricated exact lockfile. This is **source/method reproducibility**, not a claim of byte-identical output from every OS, dependency version or later MLB export. Save your resolved environment, raw response hashes and generated manifests for exact local replay. Full historical reacquisition/refitting is not executed as part of this publication task.

## Offline quickstart

From the repository root:

```bash
Rscript scripts/install_dependencies.R
python3 scripts/check.py
Rscript examples/decision_score_demo.R
```

Tests use generated fixtures and make no network requests. Acquisition extractors are tested with synthetic API payloads. Offline tests intentionally exclude checks that read actual RE tables, player histories or production fits. After running a test, ignored directories under `model/reports/` are normal.

## Complete historical rebuild

First review the plan without running downloads:

```bash
python3 scripts/reproduce.py --plan
```

Then run stages in order, or use `--all` to explicitly opt into all downloads and expensive fits:

```bash
python3 scripts/reproduce.py --stage data
python3 scripts/reproduce.py --stage expectancy
python3 scripts/reproduce.py --stage strike
python3 scripts/reproduce.py --stage swing
python3 scripts/reproduce.py --stage validation
python3 scripts/reproduce.py --stage evaluation
python3 scripts/reproduce.py --stage framing
python3 scripts/reproduce.py --stage calibration
python3 scripts/reproduce.py --stage umpires
python3 scripts/reproduce.py --stage hitters
```

Each command runs from `model/`, creates ignored local output folders and stops on the first failing command. The runner records commands, execution timestamps and exit codes in `model/reports/reproduction/`. Inspect failure diagnostics before retrying. Never edit code during a frozen artifact run. Reruns use the scripts' own caches and guards; some historic entry points require an explicit resume path rather than blind repetition. Use `--plan` to see exact commands.

Data stage downloads 2021-2025 completed regular seasons in cached three-day windows, verifies one row per `game_pk + at_bat_number + pitch_number`, and creates clean/full and ordinary-analysis tables, player splits, game cohorts and audit files. Expect several GB of data and model artifacts and substantial runtime. Public APIs can change, requests can fail, raw exports can be revised, and missing games must not be silently ignored. Downloading current historical feeds is not guaranteed to reproduce old input hashes or exact old cohort counts; the snapshot artifact verifiers deliberately stop on such changes for review.

The historical evaluation stage retains original 2024 release checks and the guarded 2025-opening mechanism because subsequent artifacts depend on it. Its original half-season gate belongs to that legacy experiment, not the current v6 season-average target. In today's project, 2025 is already inspected exploratory data, regardless of the legacy function name. Do not bypass a failed historical guard merely to obtain v6 output.

The calibration stage reconstructs the historically selected v4 correction. The v3 whole-stack calibration experiment is not promoted or applied to the v6 swing values; its module supplies shared helper functions. Regional umpire effects replace original framing rather than being counted twice.

## Read rebuilt hitter scores

After the final artifact verifier and activation succeed:

```r
setwd('model')
.libPaths(c(normalizePath('.R-library', mustWork=FALSE), .libPaths()))
source('current_hitter_stat.R')
current_hitter_stat(2025)
current_hitter_stat(2025, qualified_only=FALSE)
```

Primary files:

- `model/reports/hitter_stat_v6/qualified_hitters_2025.csv`
- `model/reports/hitter_stat_v6/hitters_2025.csv`
- `model/reports/hitter_stat_v6/uncertainty_components_2025.csv`
- `model/reports/hitter_stat_v6/sampling_sensitivity_2025.csv`
- `model/data/models/current_hitter_stat.rds`

Artifact tests are distinct from offline CI. They require locally rebuilt data, models and manifests and include expected snapshot counts. They cannot run from a code-only clone before rebuilding. No data or model download happens just by importing the current-score reader.

## Optional hosted CI

`docs/ci/offline-checks.yml` is a ready-to-use GitHub Actions template. Copy it to `.github/workflows/check.yml` in a commit made with workflow-write permission to enable it. Hosted CI is not active in this baseline; local offline checks are the verified publication checks.
