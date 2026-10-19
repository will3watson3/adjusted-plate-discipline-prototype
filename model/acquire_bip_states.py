"""Cache public MLB post-play states for the existing 2022-2024 game cohort.

Pseudocode: read audited game IDs; download/cache a narrow play-by-play response;
extract terminal batted-ball pitch and official post-PA bases/outs; flag later
runner movements and incomplete final halves; export pitch-keyed observations.
No 2025 data is requested. Run: python3 acquire_bip_states.py [--limit 5]
"""
import argparse
import concurrent.futures
import csv
import datetime as dt
import gzip
import hashlib
import json
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parent
CACHE = ROOT / "data/raw/bip_play_by_play"
OUT = ROOT / "reports/post_contact"
FIELDS = ("allPlays,atBatIndex,about,inning,halfInning,result,eventType,awayScore,homeScore,"
          "count,outs,matchup,batter,id,postOnFirst,postOnSecond,postOnThird,playEvents,"
          "index,pitchNumber,isPitch,details,code,isInPlay,runners,movement,start,end,"
          "outBase,isOut,outNumber,playIndex")


def extract(game, year, payload):
    plays = payload.get("allPlays", [])
    if not plays:
        raise ValueError(f"No completed plays: {game}")
    last = plays[-1]
    last_half = (last["about"]["inning"], last["about"]["halfInning"])
    incomplete = last.get("count", {}).get("outs", -1) != 3
    rows = []
    for play in plays:
        hits = [e for e in play.get("playEvents", [])
                if e.get("isPitch") and e.get("details", {}).get("isInPlay")]
        if not hits:
            continue
        if len(hits) != 1:
            raise ValueError(f"Ambiguous batted-ball event: {game}/{play['atBatIndex']}")
        event = hits[0]
        about, result, matchup = play["about"], play["result"], play["matchup"]
        later = [r for r in play.get("runners", [])
                 if r.get("details", {}).get("playIndex", -1) > event["index"]]
        same_event = [r for r in play.get("runners", [])
                      if r.get("details", {}).get("playIndex", -1) == event["index"]]
        outs = play["count"]["outs"]
        bases = [matchup.get(n, {}).get("id", "")
                 for n in ("postOnFirst", "postOnSecond", "postOnThird")]
        rows.append(dict(
            game_pk=game, at_bat_number=play["atBatIndex"]+1,
            pitch_number=event["pitchNumber"], season=year,
            api_batter=matchup["batter"]["id"], api_inning=about["inning"],
            api_half=about["halfInning"], api_event=result.get("eventType", ""),
            api_outs_after=outs, api_on_1b_after=bases[0], api_on_2b_after=bases[1],
            api_on_3b_after=bases[2],
            api_post_bat_score=result["awayScore" if about["halfInning"] == "top" else "homeScore"],
            api_scoring_runners=sum(bool(r.get("details", {}).get("isScoringEvent")) for r in same_event),
            api_later_runner_moves=len(later),
            api_incomplete_final_half=int(incomplete and
                (about["inning"], about["halfInning"]) == last_half)))
    return rows


def fetch(item):
    year, game = item
    path = CACHE / f"{game}.json.gz"
    url = f"https://statsapi.mlb.com/api/v1/game/{game}/playByPlay?fields={FIELDS}"
    if path.exists():
        with gzip.open(path, "rt") as handle:
            payload = json.load(handle)
    else:
        response = subprocess.run(["curl", "--fail", "--silent", "--show-error", "--location",
            "--max-time", "45", "--retry", "3", "--retry-delay", "2", url],
            check=True, capture_output=True)
        payload = json.loads(response.stdout)
        extract(game, year, payload)  # Validate before committing the cache.
        temporary = path.with_suffix(".tmp")
        with gzip.open(temporary, "wb", compresslevel=1) as handle:
            handle.write(response.stdout)
        temporary.replace(path)
    rows = extract(game, year, payload)
    manifest = dict(season=year, game_pk=game, bip_events=len(rows),
        cache=str(path.relative_to(ROOT)), md5=hashlib.md5(path.read_bytes()).hexdigest(),
        source_url=url, checked_utc=dt.datetime.now(dt.timezone.utc).isoformat())
    return rows, manifest


def write_csv(path, rows):
    if not rows:
        raise ValueError(f"Refusing empty export: {path}")
    temporary = path.with_suffix(".tmp")
    with temporary.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    temporary.replace(path)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--limit", type=int, default=0)
    parser.add_argument("--workers", type=int, default=4)
    args = parser.parse_args()
    if not 1 <= args.workers <= 6:
        parser.error("Use 1-6 workers")
    CACHE.mkdir(parents=True, exist_ok=True)
    OUT.mkdir(parents=True, exist_ok=True)
    games = []
    for year in (2022, 2023, 2024):
        with (ROOT / f"reports/game_coverage_{year}.csv").open() as handle:
            games.extend((year, int(row["game_pk"])) for row in csv.DictReader(handle))
    if len(games) != len(set(games)):
        raise ValueError("Duplicate game IDs")
    if args.limit:
        games = games[:args.limit]
    rows, manifest, failures = [], [], []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.workers) as pool:
        futures = {pool.submit(fetch, item): item for item in games}
        for i, future in enumerate(concurrent.futures.as_completed(futures), 1):
            try:
                r, m = future.result()
                rows.extend(r)
                manifest.append(m)
            except Exception as exc:
                failures.append(dict(season=futures[future][0], game_pk=futures[future][1], error=str(exc)))
            if i % 100 == 0 or i == len(games):
                print(f"{i}/{len(games)} games; {len(rows)} BIP records; {len(failures)} failures", flush=True)
    if failures:
        write_csv(OUT / "acquisition_failures.csv", failures)
        raise RuntimeError(f"{len(failures)} failed games; rerun to reuse cache")
    rows.sort(key=lambda row: (row["game_pk"], row["at_bat_number"], row["pitch_number"]))
    suffix = "_pilot" if args.limit else ""
    write_csv(OUT / f"bip_post_states{suffix}.csv", rows)
    write_csv(OUT / f"pbp_manifest{suffix}.csv", sorted(manifest, key=lambda row: row["game_pk"]))


if __name__ == "__main__":
    main()
