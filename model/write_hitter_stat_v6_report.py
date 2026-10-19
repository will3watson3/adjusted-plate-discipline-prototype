"""Write a compact report from the verified hitter artifacts (stdlib only)."""
import csv
from pathlib import Path
import statistics

root = Path('reports/hitter_stat_v6')
def rows(name):
    with (root / name).open() as f:
        return list(csv.DictReader(f))
overview = rows('overview.csv')
q = rows('qualified_hitters_2025.csv')
comparison = [r for r in rows('comparison_v4_2025.csv') if r['qualified'] == 'TRUE']
checks = rows('artifact_verification.csv') + rows('unit_verification.csv')
assert all(r['passed'] == 'TRUE' for r in checks)
median_change = statistics.median(abs(float(r['change_runs100'])) for r in comparison)
max_change = max(abs(float(r['change_runs100'])) for r in comparison)
lines = [
    '# Umpire-adjusted Decision+ hitter statistic', '',
    'The umpire-by-region strike model is connected to the adopted descriptive '
    'season hitter statistic. The user authorized this adoption on October 19, '
    '2026. It does not change the earlier inconclusive test of incremental '
    'run-value accuracy. The 2025 results remain exploratory reanalysis.', '',
    'The scoring path is: umpire-adjusted P(called strike if taken) → count/state '
    'take value → swing-minus-take value → league-relative pitch score → '
    'supported-pitch season mean. Frozen swing values and league reference '
    'probabilities are retained. Catcher framing is jointly estimated with umpire '
    'effects; the previous catcher effect is replaced, never added twice.', '',
    '## Interpretation', '',
    '`runs_per100 = 100 × mean((observed_swing − league_swing_probability) × '
    '(swing_value − take_value))`, using supported pitches only. Higher is better '
    'relative to the frozen league decision policy for the pitches faced. This is '
    'a contextual descriptive comparison, not actual runs produced or a causal '
    'estimate of latent talent.', '',
    '`Decision+ = 100 + 10 × (runs_per100 − 2024 reference mean) / 2024 reference SD`. '
    'The reference uses exposure-weighted qualified 2024 hitters and is frozen '
    'for 2025. Ten points equal one reference SD, not ten percent improvement. '
    'The scale is a display convention, not a separately validated grade model.', '',
    'Qualification requires at least 500 supported pitches, 60 games with '
    'supported pitches and 70% supported coverage. Estimates are unshrunk '
    'season averages. All hitters retain exposure and coverage information; '
    'qualified CSVs provide the comparison cohort.', '',
    '## Cohorts and sensitivity', '',
    '| Season | Eligible pitches | Supported pitches | Qualified hitters |',
    '| --- | ---: | ---: | ---: |',
]
for r in overview:
    lines.append(f"| {r['year']} | {int(r['eligible']):,} | {int(r['supported']):,} | {r['qualified']} |")
lines += ['', f'The median absolute qualified-hitter change from v4 is '
          f'**{median_change:.4f} runs per 100 pitches**; the largest absolute '
          f'change is **{max_change:.4f}**. Support is unchanged.', '',
          'The strike model previously improved supported-take log loss from '
          '0.224218 to 0.223287. Factual take RMSE decreased from 0.046605 to '
          '0.046538 runs, but the paired MSE interval included zero. Adoption '
          'here follows the requested descriptive integration, not a newly '
          'passed run-value promotion gate.', '',
          '## Example 2025 outputs', '',
          'These are the top descriptive point estimates among qualified hitters. '
          'Point ordering alone does not establish distinguishable talent.', '',
          '| Hitter | Decision+ | Runs / 100 | Coverage | Supported pitches |',
          '| --- | ---: | ---: | ---: | ---: |']
for r in q[:10]:
    lines.append(f"| {r['batter_name']} | {float(r['decision_plus']):.1f} | "
                 f"{float(r['runs_per100']):.3f} | {100*float(r['coverage']):.1f}% | {r['supported']} |")
lines += ['', '## Updated uncertainty', '',
          'Signed gradients are summed by hitter before multiplying the full '
          'joint posterior covariance. This preserves cancellation, coefficient '
          'cross-covariance and repeated-catcher/umpire dependence. Unseen '
          'catchers, umpires and umpire-region combinations use their population '
          'skill distributions; repeat observations share the same unknown effect.', '',
          'Weekly intercept SEs use game-cluster sandwich estimates on strictly '
          'earlier games. Overlapping weekly histories are handled with an '
          'SD-sum dependence bound. Separate model stages also use an SD-sum '
          'bound rather than an independence assumption. The frozen base GAM, '
          'swing branches and league policy are propagated using the new score '
          'derivatives. No earlier catcher-only intervals are reused.', '',
          'Model-only intervals describe first-order conditional model uncertainty. '
          'Game-sampling SEs and 1/7/14-game circular block bootstrap intervals '
          'are separate precision diagnostics. The combined envelope sums model '
          'and game-sampling SEs; it is not a proven nominal 95% confidence '
          'interval. Reference scaling, RE288 estimates, full training refits, '
          'hyperparameters, posterior-variance derivatives, model misspecification '
          'and selection uncertainty are not covered. All index intervals treat '
          'the descriptive 2024 reference as fixed. Half-season stability is '
          'not a release criterion.', '',
          f'**{len(checks)} integration checks passed.** Frozen upstream artifacts, '
          'exact pitch cohorts, selected probabilities, branch arithmetic, support, '
          'index standardization, covariance propagation and uncertainty scope '
          'were verified.', '',
          '## Files and use', '',
          '- [Qualified 2025 hitters](qualified_hitters_2025.csv)',
          '- [All 2025 hitters, coverage and intervals](hitters_2025.csv)',
          '- [Changes versus v4](comparison_v4_2025.csv)',
          '- [Uncertainty components](uncertainty_components_2025.csv)',
          '- [Game block sampling sensitivity](sampling_sensitivity_2025.csv)', '',
          'From the project root:', '', '```r',
          'source("current_hitter_stat.R")',
          'current_hitter_stat(2025) # verified qualified hitters',
          'current_hitter_stat(2025, qualified_only=FALSE)', '```', '',
          'Rebuild and verify before activating:', '', '```sh',
          'Rscript tests/test_hitter_stat_v6.R',
          'Rscript current_hitter_stat.R',
          'python3 write_hitter_stat_v6_report.py', '```', '',
          'The active local manifest is `data/models/current_hitter_stat.rds`. '
          'Upstream v1–v5 artifacts remain preserved. The scorer currently serves '
          'audited 2024/2025 season artifacts; this change does not add live '
          'data ingestion or a 2026 scoring path.']
(root / 'README.md').write_text('\n'.join(lines) + '\n')
print(f'Wrote report: {len(q)} qualified 2025 hitters; {len(checks)} checks')
