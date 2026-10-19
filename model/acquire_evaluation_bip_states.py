"""Acquire only the 2025 test cohort after R verifies the frozen 2024 release.

Reuses the unchanged official-state extractor and cache implementation.
No fitting, thresholds, or 2025 reads occur before the release check succeeds.
"""
import concurrent.futures
import csv
import subprocess

import acquire_bip_states as historical


def main():
    subprocess.run(["Rscript", "evaluate_locked2025.R", "--check-lock"], check=True,
                   cwd=historical.ROOT)
    out = historical.ROOT / "reports/decision_validation"
    with (historical.ROOT / "reports/game_coverage_2025.csv").open() as handle:
        games = [(2025, int(row["game_pk"])) for row in csv.DictReader(handle)]
    if not games or len(games) != len(set(games)):
        raise ValueError("Empty or duplicate test game cohort")
    rows, manifest, failures = [], [], []
    historical.CACHE.mkdir(parents=True, exist_ok=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        futures = {pool.submit(historical.fetch, item): item for item in games}
        for i, future in enumerate(concurrent.futures.as_completed(futures), 1):
            try:
                records, audit = future.result()
                rows.extend(records)
                manifest.append(audit)
            except Exception as exc:
                failures.append(dict(game_pk=futures[future][1], error=str(exc)))
            if i % 100 == 0 or i == len(games):
                print(f"2025: {i}/{len(games)} games; {len(failures)} failures", flush=True)
    if failures:
        historical.write_csv(out / "acquisition_failures_2025.csv", failures)
        raise RuntimeError("Incomplete test acquisition; no evaluation targets exported")
    rows.sort(key=lambda row: (row["game_pk"], row["at_bat_number"], row["pitch_number"]))
    historical.write_csv(out / "bip_post_states_2025.csv", rows)
    historical.write_csv(out / "pbp_manifest_2025.csv", sorted(manifest, key=lambda row: row["game_pk"]))


if __name__ == "__main__":
    main()
