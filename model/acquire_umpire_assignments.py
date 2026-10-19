"""Cache only public game-official metadata; no pitch outcomes or model fitting."""
import argparse, concurrent.futures, csv, datetime, hashlib, json, pathlib, time, urllib.request
ROOT=pathlib.Path(__file__).resolve().parent
CACHE=ROOT/'data/raw/umpire_boxscores'
REPORT=ROOT/'reports/umpire_v5'
FIELDS='officials,official,id,fullName,officialType'
def extract(payload):
 officials=payload.get('officials',[])
 home=[o for o in officials if o.get('officialType')=='Home Plate']
 if len(home)!=1:return dict(umpire_id='',umpire_name='',status='missing_home_plate' if not home else 'multiple_home_plate',home_plate_records=len(home),official_records=len(officials))
 person=home[0].get('official',{})
 if not person.get('id') or not person.get('fullName'):raise ValueError('Incomplete home-plate official')
 return dict(umpire_id=int(person['id']),umpire_name=person['fullName'],status='unique_game_assignment',home_plate_records=1,official_records=len(officials))
def fetch(item):
 year,game=item;path=CACHE/f'{game}.json'
 url=f'https://statsapi.mlb.com/api/v1/game/{game}/boxscore?fields={FIELDS}'
 if path.exists():blob=path.read_bytes();payload=json.loads(blob)
 else:
  for attempt in range(4):
   try:
    with urllib.request.urlopen(url,timeout=30) as response:blob=response.read()
    payload=json.loads(blob);extract(payload);break
   except Exception:
    if attempt==3:raise
    time.sleep(attempt+1)
  temp=path.with_suffix('.tmp');temp.write_bytes(blob);temp.replace(path)
 return dict(season=year,game_pk=game,**extract(payload),source_url=url,md5=hashlib.md5(blob).hexdigest(),cache=str(path.relative_to(ROOT)))
def write(path,rows,fields=None):
 if fields is None:fields=list(rows[0])
 temp=path.with_suffix('.tmp')
 with temp.open('w',newline='') as f:
  w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(rows)
 temp.replace(path)
def main():
 parser=argparse.ArgumentParser();parser.add_argument('--limit',type=int,default=0);parser.add_argument('--workers',type=int,default=6);args=parser.parse_args()
 if not 1<=args.workers<=8:parser.error('Use 1-8 workers')
 CACHE.mkdir(parents=True,exist_ok=True);REPORT.mkdir(parents=True,exist_ok=True)
 cohort=[]
 for year in range(2022,2026):
  with (ROOT/f'reports/game_coverage_{year}.csv').open() as f:cohort.extend((year,int(r['game_pk'])) for r in csv.DictReader(f))
 if len(cohort)!=len(set(cohort)):raise ValueError('Duplicate cohort keys')
 if args.limit:cohort=cohort[:args.limit]
 rows=[];errors=[]
 with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
  futures={pool.submit(fetch,x):x for x in cohort}
  for n,future in enumerate(concurrent.futures.as_completed(futures),1):
   try:rows.append(future.result())
   except Exception as e:errors.append(dict(season=futures[future][0],game_pk=futures[future][1],error=str(e)))
   if n%250==0 or n==len(cohort):print(f'{n}/{len(cohort)} official records; {len(errors)} failures',flush=True)
 rows.sort(key=lambda x:(x['season'],x['game_pk']))
 write(REPORT/('umpire_assignments_pilot.csv' if args.limit else 'umpire_assignments.csv'),rows)
 write(REPORT/'acquisition_failures.csv',errors,['season','game_pk','error'])
 if errors:raise RuntimeError('Incomplete acquisition: explicit failures saved, rerun uses cache')
if __name__=='__main__':main()
