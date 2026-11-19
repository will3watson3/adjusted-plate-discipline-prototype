"""Inspect or execute the staged historical rebuild. Default is plan-only."""
import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
MODEL = ROOT / 'model'
def R(name, *args): return ['Rscript', name, *args]
def P(name, *args): return [sys.executable, name, *args]
STAGES = {
    'data': [R('Adjusted_Plate_Disc_DC.R', f'--season={y}') for y in range(2021, 2026)],
    'expectancy': [R('run_expectancy_models.R'), R('attach_run_expectancy.R')],
    'strike': [['Rscript', '-e', 'source("called_strike_models.R"); run_called_strike_experiment()'], R('take_value_models.R')],
    'swing': [R('contact_models.R'), P('acquire_bip_states.py'), R('post_contact_models.R'), R('score_decisions.R')],
    'validation': [R('decision_validation.R', '--2024'), R('tests/verify_decision_validation_artifacts.R')],
    'evaluation': [R('evaluate_locked2025.R', '--evaluate'), R('tests/verify_locked2025_artifacts.R')],
    'framing': [R('catcher_framing_v2.R', '--run')],
    'calibration': [R('called_strike_v4.R')],
    'umpires': [R('umpire_v5.R', '--prepare'), P('acquire_umpire_assignments.py'), P('tests/verify_umpire_acquisition.py'),
                R('umpire_v5.R'), R('audit_umpire_v5.R'), R('tests/verify_umpire_v5.R')],
    'hitters': [R('tests/test_hitter_stat_v6.R'), R('current_hitter_stat.R', '--baseline'), P('write_hitter_stat_v6_report.py')],
    'hitter-regions': [R('experiments/hitter_region_v1/tests.R'),
                       R('experiments/hitter_region_v1/run.R'),
                       R('experiments/hitter_region_v1/verify.R'),
                       R('experiments/hitter_region_v1/report.R')],
    'hitter-surface': [R('experiments/hitter_surface_v1/tests.R'),
                       R('experiments/hitter_surface_v1/run.R'),
                       R('experiments/hitter_surface_v1/verify.R'),
                       R('experiments/hitter_surface_v1/report.R')],
    'contact-profiles': [R('compare_contact_personnel.R')],
    'contact-xgboost': [R('experiments/contact_xgboost_v1/tests.R'),
                        R('experiments/contact_xgboost_v1/train.R'),
                        R('experiments/contact_xgboost_v1/evaluate.R'),
                        R('experiments/contact_xgboost_v1/verify.R'),
                        R('experiments/contact_xgboost_v1/verify_prediction.R'),
                        R('experiments/contact_xgboost_v1/report.R')],
    'swing-xgboost': [R('experiments/swing_xgboost_v1/tests.R'),
                      R('experiments/swing_xgboost_v1/train.R'),
                      R('experiments/swing_xgboost_v1/evaluate.R'),
                      R('experiments/swing_xgboost_v1/verify.R'),
                      R('experiments/swing_xgboost_v1/verify_prediction.R'),
                      R('experiments/swing_xgboost_v1/report.R')],
    'take-xgboost': [R('experiments/take_xgboost_v1/tests.R'),
                     R('experiments/take_xgboost_v1/train.R'),
                     R('experiments/take_xgboost_v1/fit_actors.R'),
                     R('experiments/take_xgboost_v1/evaluate.R'),
                     R('experiments/take_xgboost_v1/verify.R'),
                     R('experiments/take_xgboost_v1/report.R')],
    'current-xgboost': [R('tests/test_hitter_stat_xgboost.R'), R('current_hitter_stat_v7.R')],
    'bip-value-profiles': [R('experiments/bip_value_profiles_v1/' + script + '.R')
                           for script in ('tests', 'train', 'evaluate', 'verify', 'report')],
    'current-profiles': [R('current_hitter_stat.R'), R('tests/verify_hitter_stat_v8.R')],
}

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--plan', action='store_true')
    group.add_argument('--stage', choices=STAGES)
    group.add_argument('--all', action='store_true')
    args = parser.parse_args()
    stages = [args.stage] if args.stage else list(STAGES)
    for stage in stages:
        print(stage + ':', flush=True)
        for cmd in STAGES[stage]:
            print('  ' + shlex.join(cmd), flush=True)
    if not (args.stage or args.all):
        print('\nPlan only. --stage or --all explicitly executes downloads and/or fits.')
        return
    env = dict(os.environ)
    local = MODEL / '.R-library'
    if local.is_dir():
        env['R_LIBS_USER'] = str(local)
    out = MODEL / 'reports/reproduction'
    out.mkdir(parents=True, exist_ok=True)
    (MODEL / 'reports/hitter_stat_v6').mkdir(parents=True, exist_ok=True)
    for name in ('contact', 'swing', 'take'):
        (MODEL / f'experiments/{name}_xgboost_v1/reports').mkdir(parents=True, exist_ok=True)
    # The preserved regional runner snapshots its dependencies into this directory.
    if 'hitter-regions' in stages:
        (MODEL / 'experiments/hitter_region_v1/source').mkdir(parents=True, exist_ok=True)
    ledger = out / 'commands.jsonl'
    for stage in stages:
        for command in STAGES[stage]:
            record = dict(stage=stage, command=command, start_utc=datetime.now(timezone.utc).isoformat())
            try:
                subprocess.run(command, cwd=MODEL, env=env, check=True)
                record['exit_code'] = 0
            except (subprocess.CalledProcessError, OSError) as error:
                record.update(exit_code=getattr(error, 'returncode', None), error=str(error))
                raise
            finally:
                record['end_utc'] = datetime.now(timezone.utc).isoformat()
                with ledger.open('a') as f:
                    f.write(json.dumps(record) + '\n')

if __name__ == '__main__':
    main()
