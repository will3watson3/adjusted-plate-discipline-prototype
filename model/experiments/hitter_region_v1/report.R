source('experiments/hitter_region_v1/run.R')
setDTthreads(2)
fmt<-function(x,digits=6)formatC(x,format='f',digits=digits)
metrics<-fread(file.path(HR_REPORT,'metrics_2024.csv'))
boot<-fread(file.path(HR_REPORT,'bootstrap_2024.csv'))
diag<-fread(file.path(HR_REPORT,'fit_diagnostics.csv'))
# Use floating point before multiplying row and group counts.
diag[,dense_full_design_GiB:=as.double(train_n)*groups*8/1024^3]
fwrite(diag,file.path(HR_REPORT,'fit_diagnostics.csv'))
support<-fread(file.path(HR_REPORT,'score_support_2024.csv'))
h<-fread(file.path(HR_REPORT,'hitter_changes_2024.csv'))
sw<-fread(file.path(HR_REPORT,'swing_value_changes_2024.csv'))
fall<-fread(file.path(HR_REPORT,'scoring_fallback_2024.csv'))
slices<-fread(file.path(HR_REPORT,'slices_2024.csv'))
comparison<-merge(metrics[arm=='control'],metrics[arm=='regional'],by='stage',suffixes=c('_control','_regional'))
lines<-c('# Hitter-by-region pooling: exploratory 2024 results','',
'**Regional pooling adds factual predictive value beyond updated histories for contact and BIP probability. It does not improve every component, and this run does not establish better counterfactual decision evaluation. The active Decision+ scorer is unchanged.**','',
'Both arms fit 2022–2023 with chronological player histories: 2022→2021, 2023→2022, 2024→2023. Every stage retains its existing formula, continuous location smooths, family, link, and target. Regional effects are a shared-Gaussian residual adjustment to each fixed updated-history GAM, without extra overall hitter/pitcher effects. See [the exact protocol](../PROTOCOL.md). 2024 was previously inspected and is exploratory evaluation; its outcomes were not used to tune these fits.','',
'## Factual component results','',
'Lower loss, Brier, RMSE and MAE are better. Mean error is prediction minus outcome. Differences below are regional minus control. Intervals resample 2,429 whole games 500 times, conditional on the fitted models.','',
'| Stage | Evaluation N | Metric | Updated histories | + Region pooling | Difference [95% game-bootstrap interval] |',
'|---|---:|---|---:|---:|---|')
for(st in HR_STAGES) {
  row<-comparison[which(comparison$stage==st)]
  met<-if(st=='value')c('rmse','mae','mean_error') else c('log_loss','brier','mean_error')
  for(mm in met) {
    bb<-boot[which(boot$stage==st & boot$metric==mm)]
    lines<-c(lines,sprintf('| %s | %s | %s | %s | %s | %s [%s, %s] |',st,format(row$n_control,big.mark=','),mm,
      fmt(row[[paste0(mm,'_control')]]),fmt(row[[paste0(mm,'_regional')]]),fmt(bb$difference),fmt(bb$ci_low),fmt(bb$ci_high)))
  }
}
lines<-c(lines,'',
'Contact and BIP probability show clear paired improvements in log loss and Brier. Caught-tip intervals cross zero. BIP-value RMSE decreases by only about 0.000179 runs (0.033%); MAE increases, and positive mean error increases. BIP probability also becomes more positively biased despite better proper scoring losses. These mixed results argue against claiming a broad improvement in all swing components.','',
'Calibration intercept is the intercept-only logistic recalibration diagnostic with predicted logit as offset; slope comes from a joint intercept/slope regression. Diagnostics are descriptive and never applied to predictions. Ideal values are 0 and 1.','',
'| Binary stage | Control intercept | Regional intercept | Control slope | Regional slope |',
'|---|---:|---:|---:|---:|')
for(st in HR_STAGES[1:3]) {
  row<-comparison[which(comparison$stage==st)]
  lines<-c(lines,sprintf('| %s | %.4f | %.4f | %.4f | %.4f |',st,row$calibration_intercept_control,row$calibration_intercept_regional,row$calibration_slope_control,row$calibration_slope_regional))
}
lines<-c(lines,'','Calibration-bin tables are saved separately for all binary stages.','',
'## Pooling, convergence and fallback','',
'| Stage | Training N | Hitter-region groups | Learned link-scale variance | SD | Quasi dispersion | Full dense design GiB |',
'|---|---:|---:|---:|---:|---:|---:|',
 sprintf('| %s | %d | %d | %.6f | %.6f | %.6f | %.1f |',diag$stage,diag$train_n,diag$groups,diag$variance,diag$tau,diag$dispersion,diag$dense_full_design_GiB),'',
'All control GAMs and pooled conditional-mode solvers converged. The largest final penalized-score gradient was below 1e-10. The only control-fit warning was that this R build lacks OpenMP, so computation ran single-threaded. A dense contact design alone would exceed the 16 GiB machine memory; the chosen sparse offset fit avoids constructing it. Full dense-design sizes are explanatory estimates, not measured peak allocations.','',
'A joint bam random-effect fit was not attempted. Discrete bam can avoid the full design, but dense coefficient algebra for roughly 10,000 groups remains costly. The offset method is an explicit computational tradeoff, not a demonstrated failure of joint fitting.','',
'The sparse binomial solver agrees with an independent lme4 Laplace fit; the quasi-Poisson solver agrees with a dense mgcv ML random-effect fit at fixed dispersion on synthetic data. Sparse groups shrink more strongly. The variance is learned separately by stage, with one shared Gaussian distribution across all hitter-region combinations in that stage.','',
'Fallback on all 704,446 eligible 2024 decision pitches:','',
'| Stage | Unseen hitter-region N | Frequency | Missing/invalid N |',
'|---|---:|---:|---:|')
for(st in HR_STAGES) {
  z<-fall[which(fall$stage==st)];unseen<-sum(z[fallback=='unseen_hitter_region',n]);invalid<-sum(z[fallback=='missing_invalid_region_or_identity',n])
  lines<-c(lines,sprintf('| %s | %d | %.2f%% | %d |',st,unseen,100*unseen/sum(z$n),invalid))
}
lines<-c(lines,'',
'Fallback is exactly the control prediction, without a global intercept shift. Missing or invalid zones are explicitly supported by the code and synthetic tests; none occur in the eligible real evaluation population. Stage-specific factual fallback counts are in slices_2024.csv.','',
'## Regions and history/exposure slices','',
'The [official Statcast documentation](https://baseballsavant.mlb.com/csv-docs) and its [zone diagram](https://baseballsavant.mlb.com/sections/statcast-search-new/images/zones.png) establish catcher-view source categories: 1–3 high, 4–6 middle, 7–9 low, and 11–14 outside quadrants. The source zone field is used as a category in the hitter-region identity, never a numeric trend. No attack-zone thresholds are inferred. Raw yearly audits show 2,054/3,011/2,672 missing zones in 2022/2023/2024, all already outside the eligible population; no invalid nonmissing codes were found. Continuous location features remain in each GAM.','',
'The linked tables give evaluation N and regional-minus-control log loss (or RMSE for value). Full Brier/MAE/bias slices and pitcher-history slices are in slices_2024.csv. Slice results are descriptive; no subgroup tuning or multiple-comparison claims.','',
'Contact log loss improves in 10 of 13 regions; BIP-probability log loss improves in all 13. Both improve in the available and unavailable prior-season contact-history groups. Zero-exposure hitter-region groups have exactly zero change in every stage, as required.','')
main_lines<-lines
lines<-c('# Region, history and exposure slices','',
  'Descriptive exploratory 2024 comparisons. Differences are regional minus control; negative favors regional pooling. See REPORT.md for limitations.','')
for(dim in c('region','history','exposure_band')) {
  lines<-c(lines,paste0('### ',dim),'','| Stage | Slice | N | Loss difference |','|---|---|---:|---:|')
  for(st in HR_STAGES) {
    z<-slices[which(slices$dimension==dim & slices$stage==st)]
    zz<-merge(z[arm=='control'],z[arm=='regional'],by='slice',suffixes=c('_control','_regional'))
    metric<-if(st=='value')'rmse' else 'log_loss'
    setorder(zz,slice)
    for(i in seq_len(nrow(zz)))lines<-c(lines,sprintf('| %s | %s | %d | %.6f |',st,zz$slice[i],zz$n_control[i],zz[[paste0(metric,'_regional')]][i]-zz[[paste0(metric,'_control')]][i]))
  }
  lines<-c(lines,'')
}
writeLines(lines,file.path(HR_REPORT,'SLICE_SUMMARY.md'))
lines<-c(main_lines,'See [the compact slice tables](SLICE_SUMMARY.md) for region, history and exposure results. All metrics and sample sizes remain in the machine-readable CSV.','')
qualified<-h[qualified==TRUE]
lines<-c(lines,'## Exploratory swing values and hitter scores','',
 sprintf('The original active-v6 support mask contained %s pitches. Applying the existing range/target safeguards to the changed features and both models leaves %s common supported pitches (%s additional exclusions). Both arms use this identical comparison cohort and the unchanged qualification thresholds; %d hitters qualify.',
  format(support$original_supported,big.mark=','),format(support$common_supported,big.mark=','),format(support$additional_range_or_value_exclusions,big.mark=','),support$qualified),'',
 sprintf('The frozen reference center is %.9f runs/100 and scale is %.9f; neither is re-estimated. Among qualified hitters, the median absolute change is %.4f runs/100 (%.3f Decision+ points), with a maximum absolute change of %.3f Decision+ points.',support$reference_center,support$reference_scale,
 median(abs(qualified$delta_runs100)),median(abs(qualified$delta_decision_plus)),max(abs(qualified$delta_decision_plus))),'',
'| Primary support | Observed action | N | Mean change Q(swing) | Mean absolute change Q(swing) | Change in score runs/100 |',
'|---|---|---:|---:|---:|---:|',
sprintf('| %s | %s | %d | %.6f | %.6f | %.6f |',sw$primary_supported,sw$observed_action,sw$n,sw$mean_delta_q,sw$mean_absolute_delta_q,sw$change_runs_per100),'',
'Unsupported rows are shown only to audit extrapolation. They are excluded from primary hitter scores; their raw values are not clamped. The take path, catcher/umpire predictions, frozen policy and deterministic branch-state transitions are reused exactly. Scores are still (observed swing − league policy) × (Q_swing − Q_take).','',
'Individual hitter changes, fresh paired game-bootstrap sampling intervals, qualifications and underlying game totals are in hitter_changes_2024.csv and hitter_game_scores_2024.csv.gz. These sampling intervals condition on fitted models; they are not replacement model-uncertainty intervals or evidence of player talent. No previous model intervals, preference labels or action recommendations are reused.','',
'## Limits and interpretation','',
'- The two-stage fit holds the updated GAM fixed and estimates residual regional effects on the same training observations. It does not jointly optimize smooths and regional effects, and does not propagate offset estimation uncertainty. The value stage uses a quasi-marginal Laplace approximation with control dispersion fixed.',
'- One shared variance per stage and fixed effects across both training seasons are a deliberately simple baseline. No current-season updating, hitter-region-pitch-type interaction, or player-specific variance is fitted.',
'- Observed swings and BIP are selected populations. Better factual prediction does not identify how a taken pitch would have performed if swung at, or prove the resulting Decision+ score is better.',
'- 2024 is previously inspected exploratory evaluation, and also underlies the frozen reference scale. Bootstrap intervals describe game-sampling variation conditional on fits, not training/model/RE/reference uncertainty. Subgroup comparisons are not adjusted for multiplicity.',
'- The small BIP RMSE improvement comes with worse MAE and bias. There is no compelling caught-tip improvement. Do not present all four changes as uniformly beneficial.','',
'**Conclusion:** the regional extension adds measurable signal beyond updated histories for contact and BIP probability. The evidence supports further evaluation of those components, while the full four-stage extension remains experimental. Nothing is activated in the main scorer.','',
'## Files and reproduction','',
'- `../run.R`: complete training, evaluation, scoring, and basic verification; cached stage models are reused only within this version directory.',
'- `../tests.R`: independent numerical solver checks, sparse/unseen behavior, and chronological profile checks.',
'- `../verify.R`: independent saved-artifact checks, original formulas/families/cohorts, branch and score reconstruction, reference and input hashes.',
'- `../models/`: four bundles containing both control GAM and regional pooling effects, prior-season histories, experiment manifest.',
'- `predictions_*_2024.csv.gz`, `branch_predictions_*_2024.csv.gz`, `decision_predictions_2024.csv.gz`: keyed factual and scoring predictions.',
'- `metrics_2024.csv`, `bootstrap_2024.csv`, `slices_2024.csv`, `calibration_*`, `fit_diagnostics.csv`, `effects_*`: numerical evidence.',
'- `input_hashes.csv`, `verification.csv`, `independent_verification.csv`, `unit_tests.log`, `sessionInfo.txt`: audit evidence.',
'- `../source/`: snapshots of original project dependencies. The originals were preserved.','',
'From the project root:','',
'```sh','Rscript experiments/hitter_region_v1/tests.R','Rscript experiments/hitter_region_v1/run.R','Rscript experiments/hitter_region_v1/verify.R','Rscript experiments/hitter_region_v1/report.R','```','',
'For an entirely fresh fit, copy the experiment to a new version directory, update HR_ROOT there, and use an empty models directory. Required input files and hashes are listed in input_hashes.csv; the project-local raw data and original model artifacts are required. This experiment does not claim a standalone data-free reproduction package.')
writeLines(lines,file.path(HR_REPORT,'REPORT.md'))
cat('Report written\n')
