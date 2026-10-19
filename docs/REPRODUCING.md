# Reproducing the baseline

## Environment

Use R >= 4.4, Python >= 3.9 and curl. `scripts/install_dependencies.R` installs missing/older R dependencies to `model/.R-library/`: data.table, mgcv, glmnet and baseballr >= 2.0.0. Python scripts use only the standard library.

The source snapshot was tested with R 4.4.2, data.table 1.18.2.1, mgcv 1.9-1, glmnet 4.1-10 and baseballr 2.0.0. The installer resolves compatible available releases; it is not an exact dependency lock. Save the resolved session, raw responses and generated manifests for local replay. A later MLB export or different environment may change results or trigger a snapshot verification failure.

## Offline checks

From the repository root:

```bash
Rscript scripts/install_dependencies.R
python3 scripts/check.py
Rscript examples/decision_score_demo.R
```

The offline suite uses generated fixtures and makes no network requests. Other tests under `model/tests/` require rebuilt RE tables, player profiles or fitted models. No hosted CI workflow is enabled.

## Historical rebuild

Preview exact commands with `python3 scripts/reproduce.py --plan`. Run stages in this order:

```text
data → expectancy → strike → swing → validation → evaluation
     → framing → calibration → umpires → hitters
```

Execute an individual stage with `python3 scripts/reproduce.py --stage data`, replacing `data` with the desired stage. `python3 scripts/reproduce.py --all` explicitly runs every stage, including downloads and expensive fits. Expect several GB of local data/artifacts and substantial runtime. Full historical reacquisition was not rerun for this publication.

The runner works from `model/`, records commands and exit codes under `model/reports/reproduction/`, and stops on failure. Scripts retain their own caches and guards; inspect failures before retrying or choosing a documented resume path. Never edit code during a frozen run. Artifact verifiers deliberately check snapshot counts and source/data hashes; do not bypass a mismatch.

The data stage acquires 2021-2025 completed regular seasons, cleans/deduplicates pitch keys and saves complete event tables, ordinary-pitch subsets, profiles and audits. Downloads can change or fail; missing games must remain explicit. Data, reports, caches, libraries and model RDS files are ignored by Git.

`model/*_PLAN.md` files preserve historical experiment protocols. Original 2024 gates and the guarded 2025 opening remain because later artifacts depend on them. Their half-season gate belongs to that historical experiment, not the current season-average target. **2025 is already inspected exploratory data.** Rebuilding it does not create an independent holdout.

The v3 calibration module supplies shared helpers; its complete experimental correction stack is not applied to v6. The current scorer uses the selected umpire-by-region model, replacing original framing rather than counting it twice. Rebuild manifests in this checkout; do not import frozen manifests from another working directory.

## Read the rebuilt results

```r
setwd('model')
.libPaths(c(normalizePath('.R-library', mustWork=FALSE), .libPaths()))
source('current_hitter_stat.R')
current_hitter_stat(2025)
```

Outputs are under `model/reports/hitter_stat_v6/`: qualified/all hitter tables, uncertainty components and game-block precision diagnostics. Activation is recorded in `model/data/models/current_hitter_stat.rds` after artifact checks pass. Importing the reader does not download data.
