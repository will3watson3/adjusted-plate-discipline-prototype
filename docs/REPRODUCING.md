# Reproducing the baseline

## Environment

Use R >= 4.4, Python >= 3.9 and curl. `scripts/install_dependencies.R` installs missing/older R dependencies to `model/.R-library/`: data.table, mgcv, glmnet, baseballr >= 2.0.0, lme4 >= 2.0.1, digest >= 0.6.33 and xgboost >= 3.2.1.1. Python scripts use only the standard library.

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

The v3 calibration module supplies shared helpers; its complete experimental correction stack is not applied to v6. The v6 prerequisite uses the GAM-based umpire-by-region model. The current XGBoost scorer refits the identical actor structure against XGBoost offsets, replacing the entire take path. Rebuild manifests in this checkout; do not import frozen manifests from another working directory.

## Read the rebuilt results

```r
setwd('model')
.libPaths(c(normalizePath('.R-library', mustWork=FALSE), .libPaths()))
source('current_hitter_stat.R')
current_hitter_stat(2025)
```

After the XGBoost stages below, outputs are under `model/reports/hitter_stat_xgboost/`: qualified/all hitter tables, common-cohort value metrics, paired game intervals and hitter sampling intervals. Activation is recorded in `model/data/models/current_hitter_stat.rds` after artifact checks pass. Importing the reader does not download data.

## Continuous hitter extension

The extension was tested with lme4 2.0.1 and digest 0.6.37. The installer requires lme4 >= 2.0.1 because the constrained fit uses its modular Laplace interface. The offline check command also runs the continuous surface synthetic suite; it needs no downloaded data or fitted artifacts.

After the historical rebuild through `hitters`, run:

```bash
python3 scripts/reproduce.py --stage hitter-regions
python3 scripts/reproduce.py --stage hitter-surface
```

The stages are also appended, in that order, to `--all`. An individual `--stage` runs only that stage; prerequisites must already exist. The regional stage creates the four updated-history control bundles, categorical fits, histories, keyed predictions and hashes required by the surface stage. It creates the dependency-snapshot directory before calling the preserved regional runner. No precomputed model bundles are distributed.

Each stage runs tests, fitting/evaluation, artifact verification and report generation. The surface results are written under `model/experiments/hitter_surface_v1/reports/`; its models and preprocessing are under `models/` beside that directory. Both are ignored by Git. All stages execute with `model/` as the working directory. The existing regional source and the continuous source are copied without changes from the verified local experiment.

The surface stage fingerprints the complete regional experiment directory. Finish regional verification/reporting first, and do not rerun or edit that directory while reproducing the surface stage. Manifests must be regenerated locally rather than imported from the research workspace. Existing cache/hash checks deliberately fail when frozen inputs or fitting code change.

2024 remains previously inspected exploratory evaluation. The published summary records the original run; exact reacquisition and a full clean-clone rebuild of these added stages have not been rerun for this release preparation. The report scripts preserve the original study narrative, including some fixed numerical statements; after a different-data rebuild, validate that narrative against the newly generated CSV tables before publishing it. The continuous extension is retained as historical research, but these stages do not activate it in the v6 scorer or reuse v6 uncertainty intervals.

## XGBoost main scorer

After the historical rebuild, `hitter-regions` and `hitter-surface`, run these stages in order:

```bash
python3 scripts/reproduce.py --stage contact-profiles
python3 scripts/reproduce.py --stage contact-xgboost
python3 scripts/reproduce.py --stage swing-xgboost
python3 scripts/reproduce.py --stage take-xgboost
python3 scripts/reproduce.py --stage current-xgboost
```

`--all` runs these after the earlier stages. The `hitters` prerequisite explicitly invokes `current_hitter_stat.R --baseline`; only `current-xgboost` activates the new main model. For direct integration after prerequisites, run `Rscript current_hitter_stat.R` from `model/`. Verification failure leaves the active pointer unchanged.

The full-swing historical comparison fits tip/value challengers for evaluation, but the main hybrid uses only its BIP-probability booster. Contact uses its own frozen booster. Tip and BIP value retain the original v6 GAMs. No new tuning is performed during integration.

All experiment tests, fitting, evaluation and verification must finish **before activation**, because their frozen input manifests include the previous active pointer. Activation archives that pointer under `data/models/hitter_stat_xgboost/previous_active.rds`; original v6 artifacts remain untouched. The main scorer freezes its own inputs, verifies exact selected components and factual arithmetic, and writes fresh 500-draw game intervals. Do not interpret these as full training/model uncertainty.

Frozen experiment trainers refuse overwrites; use a new version directory for a genuinely fresh refit. Re-running a completed historical study after activating another scorer requires its original baseline pointer and source snapshot. A full reacquisition/refit was not performed for this release; integration was tested against existing frozen models and both complete 2024/2025 datasets.
