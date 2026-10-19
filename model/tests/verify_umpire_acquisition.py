import csv,hashlib,pathlib,sys
sys.path.insert(0,str(pathlib.Path(__file__).resolve().parents[1]))
from acquire_umpire_assignments import ROOT,REPORT,extract,write
with (REPORT/'umpire_assignments.csv').open() as f:rows=list(csv.DictReader(f))
cohort=set()
for year in range(2022,2026):
 with (ROOT/f'reports/game_coverage_{year}.csv').open() as f:cohort.update((str(year),r['game_pk']) for r in csv.DictReader(f))
checks=[]
def check(name,value):
 if not value:raise AssertionError(name)
 checks.append(dict(check=name,passed=True))
check('exact historical game cohort',set((r['season'],r['game_pk']) for r in rows)==cohort)
check('unique game assignment rows',len(rows)==len({r['game_pk'] for r in rows}))
import json
for r in rows:
 blob=(ROOT/r['cache']).read_bytes()
 if hashlib.md5(blob).hexdigest()!=r['md5']:raise AssertionError('cache hash '+r['game_pk'])
 payload=json.loads(blob)
 if not set(payload).issubset({'officials'}):raise AssertionError('nonmetadata response '+r['game_pk'])
 parsed=extract(payload)
 if any(str(parsed[k])!=r[k] for k in parsed):raise AssertionError('extraction '+r['game_pk'])
check('every cached response matches its manifest hash',True)
check('every assignment reconstructs from raw official metadata',True)
check('acquisition contains official metadata only and no outcomes',True)
check('all unique assignments have valid identities',all(r['umpire_id'] and r['umpire_name'] for r in rows if r['status']=='unique_game_assignment'))
check('ambiguous assignments explicitly withheld',all(not r['umpire_id'] for r in rows if r['status']!='unique_game_assignment'))
write(REPORT/'acquisition_verification.csv',checks)
print(len(checks),'acquisition artifact checks passed')
