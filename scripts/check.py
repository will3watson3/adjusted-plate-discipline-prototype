"""Offline source and invariant checks. No MLB data or network required."""
import argparse
import ast
import csv
import hashlib
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
MODEL = ROOT / 'model'
OFFLINE_R = ('test_run_expectancy_models.R', 'test_decision_validation.R',
             'test_called_strike_v4.R', 'test_umpire_v5.R',
             'test_season_average_v3.R', 'test_season_precision_v3.R',
             'test_hitter_stat_v6.R', 'test_called_strike_models.R')

def environment():
    env = dict(os.environ)
    local = MODEL / '.R-library'
    if local.is_dir():
        env['R_LIBS_USER'] = str(local)
    env['PYTHONPYCACHEPREFIX'] = str(Path(os.environ.get('TMPDIR', '/tmp')) / 'decision-baseline-pyc')
    return env

def check_manifest():
    with (ROOT / 'docs/source_manifest.csv').open() as f:
        rows = list(csv.DictReader(f))
    assert rows and len({r['published'] for r in rows}) == len(rows)
    for row in rows:
        p = ROOT / row['published']
        assert p.is_file(), 'Missing preserved source: ' + row['published']
        assert hashlib.sha256(p.read_bytes()).hexdigest() == row['sha256'], 'Preserved source changed: ' + row['published']
    print(f'{len(rows)} preserved source/protocol/test hashes verified', flush=True)

def run(command, cwd=MODEL):
    print('+ ' + ' '.join(command), flush=True)
    subprocess.run(command, cwd=cwd, env=environment(), check=True)

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--static-only', action='store_true')
    args = parser.parse_args()
    check_manifest()
    paths = [p for folder in ('scripts', 'examples', 'model') for p in (ROOT / folder).rglob('*')
             if p.is_file() and not any(x in ('data', 'reports', '.R-library', '__pycache__') for x in p.relative_to(ROOT).parts)]
    for p in paths:
        if p.suffix == '.py':
            ast.parse(p.read_text(), filename=str(p))
    rpaths = [str(p.relative_to(ROOT)) for p in paths if p.suffix == '.R']
    run(['Rscript', '-e', 'for(p in commandArgs(TRUE)) invisible(parse(file=p)); cat("All R files parsed\\n")'] + rpaths, ROOT)
    if args.static_only:
        return
    (MODEL / 'reports/hitter_stat_v6').mkdir(parents=True, exist_ok=True)
    run([sys.executable, '-m', 'unittest', 'discover', '-s', 'tests', '-p', 'test_acquire_*.py'])
    for name in OFFLINE_R:
        run(['Rscript', 'tests/' + name])
    print(f'PASS: source integrity, Python/R syntax, Python extractor suites and {len(OFFLINE_R)} offline R suites', flush=True)

if __name__ == '__main__':
    main()
