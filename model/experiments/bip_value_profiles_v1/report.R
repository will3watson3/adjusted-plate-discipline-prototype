source('experiments/bip_value_profiles_v1/functions.R')
read_table <- function(n)fread(file.path(BR,paste0(n,'.csv')))
v <- read_table('verification');u <- read_table('unit_tests')
stopifnot(all(v$passed),all(u$passed))
freeze <- readRDS(file.path(BM,'frozen.rds'));bp_check_hash(freeze$inputs);bp_check_hash(freeze$code);bp_check_hash(freeze$models)
m <- read_table('metrics');b <- read_table('boot');cov <- read_table('coverage')
h <- read_table('hitters');tails <- read_table('tails');tracking <- read_table('historical_tracking')
md <- function(d) {
  x <- copy(d)
  for(n in names(x))if(is.numeric(x[[n]]))set(x,j=n,value=if(is.integer(x[[n]]))as.character(x[[n]])else sprintf('%.6f',x[[n]]))
  c(paste0('| ',paste(names(x),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(x)),collapse=' | '),' |'),
    apply(as.matrix(x),1,function(z)paste0('| ',paste(z,collapse=' | '),' |')))
}
verdict <- function(comparison_name,pop) {
  z <- b[comparison==comparison_name&population==pop&metric=='rmse']
  improved <- all(z$difference<0);clear <- all(z$ci_high<0)
  if(clear)'RMSE improves in both seasons, with both paired 95% intervals below zero.' else
    if(improved)'RMSE improves in both seasons, but at least one paired 95% interval includes zero.' else
      'There is no consistent two-season RMSE improvement.'
}
main <- m[population%in%c('bip_all','swing_supported'),.(year,population,arm,n,rmse,mae,bias)]
intervals <- merge(b[population%in%c('bip_all','swing_supported')&metric=='rmse',
    .(year,population,comparison,rmse=difference,rmse_low=ci_low,rmse_high=ci_high)],
  b[population%in%c('bip_all','swing_supported')&metric=='mae',
    .(year,population,comparison,mae=difference,mae_low=ci_low,mae_high=ci_high)],by=c('year','population','comparison'))
hist <- tracking[,.(batted_balls=sum(batted_balls),ev_available=sum(ev_n),barrel_classifiable=sum(barrel_n),
  type_known=sum(gb_n),barrels=sum(barrels),grounders=sum(grounders)),by=season]
hist[,`:=`(ev_coverage=ev_available/batted_balls,barrel_coverage=barrel_classifiable/batted_balls,type_coverage=type_known/batted_balls)]
large <- h[qualified==TRUE&comparison%in%c('B-A','C-B')][order(-abs(delta_plus)),head(.SD,2),by=.(year,comparison)]
large <- large[,.(year,comparison,batter_name,delta_plus,delta_plus_low,delta_plus_high)]
setorder(large,year,comparison)
tailtable <- merge(tails[group=='factual_bip'&arm=='C',.(year,C_factual_BIP_max=max)],
  dcast(tails[group=='observed_take_audit_only'&arm%in%c('A','C')],year~arm,value.var='max'),by='year')
setnames(tailtable,c('A','C'),c('A_all_takes_BIP_max','C_all_takes_BIP_max'))
tailtable <- merge(tailtable,read_table('supported_take_audit')[arm=='C',.(year,C_supported_takes_swing_max=q_swing_max)],by='year')
lines <- c('# Historical contact-quality profiles: BIP value',
  '',paste0('**Does fresher exit velocity help? Mixed.** ',verdict('B-A','bip_all'),' B worsens MAE and positive bias in both seasons for both BIP and complete swing value. Neither complete-swing RMSE interval excludes zero.'),
  '',paste0('**Do barrel and ground-ball rates help beyond fresher EV? Small benefits.** ',verdict('C-B','bip_all'),' C improves MAE and reduces positive bias versus B in both seasons and outcomes. The 2025 BIP and complete-swing RMSE intervals exclude zero; the 2024 intervals include zero. C still has higher MAE and positive bias than A in both years.'),
  '', 'Both 2024 and 2025 are exploratory. Changes are small predictive comparisons; they do not establish causal decision skill. No candidate was activated.',
  '', '## Fixed design and verification',
  '',paste0('All arms fit the same ',read_table('fits')$n[1],' verified 2022–23 BIP rows. A exactly refits the active frozen-2021 EV GAM; B changes only EV history to the immediately preceding season; C adds four linear rate terms. All other histories, preprocessing, GAM basis/penalty specification and fitting procedure are fixed; smoothing parameters are reestimated per fit. There was no tuning or development selection.'),
  '', 'Each rate uses its own observed-label denominator. Barrel = official launch_speed_angle 6 among labels 1–6; grounder = ground_ball among four known bb_type labels. [Official field definitions](https://baseballsavant.mlb.com/csv-docs). Both fields are present in cleaned data and checked against keyed raw samples. No hard-hit proxy is used. EV availability does not distinguish measured from Statcast-estimated values.',
  '', 'For EV and both rates: shrink overall to the source-season pooled league with 50 observations, then hand × family splits to the shrunken player with 30. Unseen/zero-denominator players use league; absent/zero-denominator splits use player. Only Y−1 evidence enters the prediction row. Same-pitch tracking and outcomes are ignored.',
  '',paste0(nrow(v),' independent reconstruction checks and ',nrow(u),' focused tests passed. These check baseline parity, raw labels, denominators, shrinkage/fallbacks, dates, feature invariance, keyed targets/cohorts, swing arithmetic and the reported metrics/intervals. Source, active artifacts, and RE hashes remain unchanged. Verification is separate arithmetic, not an independent scientific replication.'),
  '', '## Matched factual performance',
  '', 'Values are runs + frozen RE288-after. Signed bias is prediction minus observation. Swing results use the unchanged active support policy and observed swings with verified factual targets.',
  '',md(main),
  '', 'Candidate minus comparator; negative RMSE/MAE favors candidate. Intervals use 500 paired whole-game draws conditional on the fitted models.',
  '',md(intervals),
  '', 'The subgroup pattern is heterogeneous: C worsens offspeed BIP RMSE and MAE versus B in both years, and some base-out cells worsen. About 23% of BIP rows lack at least one player’s prior-season profile and therefore use a league fallback for that player. Calibration above predicted Q_BIP = 3 has only seven C rows per season; those bins are too sparse for a firm conclusion. No subgroup selection or recalibration was performed.',
  '', '## Coverage and historical availability',
  '',md(cov),
  '', 'No additional primary exclusions. Qualification and the frozen 2024 reference scale are unchanged. Exposure bands use the minimum of batter/pitcher prior-season metric denominators; missing-history status flags any zero-denominator overall history.',
  '',md(hist[,.(season,batted_balls,ev_coverage,barrel_coverage,type_coverage)]),
  '', 'Profile values outside their final-training ranges are retained and flagged. The matched common in-range comparisons retain the overall RMSE pattern; see the *_common_inrange populations in [paired intervals](boot.csv).',
  '', '## Decision+ and tails',
  '', 'Only Q_BIP changes. The verified identity is ΔQ_swing = P(BIP|swing) × ΔQ_BIP. Contact/BIP XGBoost probabilities, caught-tip GAM, complete take path, policy, RE, support and reference scale remain fixed. Decision+ uses all supported actions, so these descriptive score changes include audit-only swing estimates on observed takes. Largest qualified-hitter changes for each isolated comparison:',
  '',md(large),
  '', 'Hitter intervals use fresh paired whole-game samples (500 draws per hitter/comparison), conditional on fixed models and support; fitting uncertainty is omitted. They are descriptive score changes, not evidence that a hitter changed behavior.',
  '',md(tailtable),
  '', 'The extreme all-take values are excluded by the existing support policy and do not enter the reported hitter changes. Modeled swings on observed takes are audit-only; BIP factual accuracy does not validate those hypothetical values. No predictions are clipped.',
  '', '## Detailed outputs',
  '', '[Calibration bins](bins.csv), [base-out/family/exposure/missing-history slices](slices.csv), [all paired intervals, including signed bias](boot.csv), [historical denominator audit](historical_tracking.csv), [all prediction tails](tails.csv), [supported-take audit](supported_take_audit.csv), [all hitter changes](hitters.csv), [verification](verification.csv). Keyed predictions and top-tail rows are alongside these tables. The [README](../README.md) gives rerun commands; frozen fits and manifests are in ../models/.')
writeLines(lines,file.path(BR,'REPORT.md'))
paths <- list.files(BP,recursive=TRUE,full.names=TRUE)
paths <- paths[!grepl('artifact_manifest[.]csv$|[.]log$',paths)]
bp_write(bp_hash(paths),'artifact_manifest')
message('Report and artifact manifest written')
