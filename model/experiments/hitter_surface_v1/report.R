source('experiments/hitter_surface_v1/surface.R')
read<-function(n)fread(file.path(HR_REPORT,n))
metrics<-read('metrics_2024.csv');boot<-read('bootstrap_2024.csv');diag<-read('fit_diagnostics.csv');support<-read('score_support_2024.csv');hit<-read('hitter_changes_2024.csv');slices<-read('slices_2024.csv')
lines<-c('# Continuous hitter location surface: exploratory 2024', '',
 'The prespecified continuous surface improves log loss and Brier score beyond categorical pooling for both stages; paired game-bootstrap intervals for both losses are below zero. Calibration is slightly worse: contact slope moves from 1.0093 to 0.9841, and BIP positive mean error increases from 0.00645 to 0.00706. This is predictive evidence for this representation on exploratory 2024, not counterfactual validation.', '',
 'The primary comparison is continuous versus categorical pooling. Both use exactly the same frozen updated-history population GAM offset. Only contact and BIP-given-contact change. The active scorer is unchanged.', '',
 '## Factual results', '',
 '| Stage | Arm | N | Log loss | Brier | Calibration intercept | Calibration slope |',
 '|---|---|---:|---:|---:|---:|---:|')
for(i in seq_len(nrow(metrics))){r<-metrics[i];lines<-c(lines,sprintf('| %s | %s | %d | %.6f | %.6f | %.5f | %.5f |',r$stage,r$arm,r$n,r$log_loss,r$brier,r$calibration_intercept,r$calibration_slope))}
lines<-c(lines,'','Calibration intercept is intercept-only recalibration with logit prediction as offset; slope is from a joint intercept/slope logistic regression. These diagnostics do not alter predictions. Calibration bins are in calibration_2024.csv.','',
 '| Stage | Comparison | Metric | Difference | 95% game-bootstrap interval |','|---|---|---|---:|---|')
for(i in seq_len(nrow(boot[metric%in%c('log_loss','brier')]))){r<-boot[metric%in%c('log_loss','brier')][i];lines<-c(lines,sprintf('| %s | %s minus %s | %s | %.6f | [%.6f, %.6f] |',r$stage,r$candidate,r$reference,r$metric,r$difference,r$ci_low,r$ci_high))}
lines<-c(lines,'','Lower losses are better. Intervals use 500 paired whole-game resamples and describe conditional sampling uncertainty. They omit training refits, model selection, offset estimation, RE and reference uncertainty. 2024 was previously inspected and is not an untouched holdout.','', '## Pooling and implementation', '',
 'A six-term quadratic (1, x, z, x², xz, z²) gives each hitter a smooth location deviation. Training-only centering and whitening separate the mean hitter intercept from zero-mean shape over the pooled training location distribution. All six directions share one learned Gaussian variance per stage. No player-specific smoothing parameters or pitch-type interactions were added.', '',
 'The categorical arm already permits a hitter-wide mean through its zone effects; no extra overall personnel effect was added to either arm. Its zone-average intercept and contrasts have a different induced prior covariance from the continuous basis. Therefore this tests these two specific pooled representations, not continuity independently of all basis and penalty choices.', '',
 '| Stage | Training N | Hitters | Coefficients | SD | Variance | Effective df | Maximum mode gradient | Fit seconds | Sparse design MiB |',
 '|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|')
for(i in seq_len(nrow(diag))){r<-diag[i];lines<-c(lines,sprintf('| %s | %d | %d | %d | %.6f | %.6f | %.1f | %.3g | %.1f | %.1f |',r$stage,r$n,r$hitters,r$coefficients,r$tau,r$variance,r$edf,r$max_gradient,r$seconds,r$sparse_design_MiB))}
lines<-c(lines,'','Effective df is the conditional trace of H(H + I/tau²)^−1, summed over hitters; it excludes variance estimation and the fixed GAM backbone. Categorical effective df and SD are in fit_diagnostics.csv. Per-hitter coefficients, exposure and df are in effects_contact.csv and effects_bip.csv.', '',
 'The production fit uses lme4 sparse Laplace likelihood, constrained to a spherical random-coefficient covariance, with scalar variance optimization and an explicit zero-variance boundary. Conditional modes use tolerance 1e-9. No custom production likelihood solver is used. The independent synthetic six-dimensional calculation agrees in modes to <1e-5 and deviance to <1e-4 (a numerical tolerance on a deviance around 2,400). The scalar case agrees with ordinary glmer. See unit_tests.log and [the frozen protocol](../PROTOCOL.md).', '',
 'Sparse hitters remain strongly pooled: among hitters with 1–9 training observations, median effective df is 0.156 for contact and 0.055 for BIP (out of six). See pooling_by_exposure.csv for all bands.', '',
 'The prior run documented a 16 GiB machine; current hardware and process queries were sandbox-denied. Actual sparse design sizes above are measured R object sizes, not peak process memory. Discrete bam can avoid the full observation-by-coefficient matrix; it was not declared infeasible or benchmarked here.', '',
 '## Exposure, calibration and fallback', '',
 'slices_2024.csv reports every arm by source zone, hitter/pitcher prior-history availability, training hitter-zone exposure, total hitter exposure and fallback. These use predictors/training counts, not favorable evaluation outcomes. The separate slice summary gives paired changes. Every saved slice is independently reconstructed from keyed predictions.', '',
 '| Stage | Fallback on all eligible pitches | N | Percent |','|---|---|---:|---:|')
fallback<-read('scoring_fallback_2024.csv')
for(i in seq_len(nrow(fallback))){r<-fallback[i];lines<-c(lines,sprintf('| %s | %s | %d | %.3f%% |',r$stage,r$fallback,r$n,100*r$n/support$eligible))}
lines<-c(lines,'','Unseen hitters have exactly the population prediction. A known hitter can borrow its smooth across locations even with no training observations in that source zone. Missing/invalid core locations are rejected by the population feature contract; the surface helper marks them and returns zero adjustment, not a valid prediction. None of the eligible evaluation rows bypass this contract.', '',
 '## Exploratory swing values and hitter scores', '',
 sprintf('The common comparison cohort contains %s of %s originally supported pitches (%s additional exclusions); %s hitters qualify. All %s eligible rows, including unsupported extrapolations, are retained in decision_predictions_2024.csv.gz.',support$common_supported,support$original_supported,support$additional_exclusions,support$qualified,support$eligible), '',
 'Tip probability and BIP value are fixed at updated-history population predictions in all three arms. The categorical scoring arm here therefore intentionally differs from hitter_region_v1’s four-stage scoring arm. RE288 transitions, the take/catcher/umpire path, policy, support safeguards and qualification rules remain fixed.', '',
 sprintf('The frozen reference center is %.9f runs/100 and scale %.9f. Scores remain (observed swing − frozen policy) × (Q_swing − Q_take), averaged per 100 supported pitches; Decision+ is 100 + 10 × (rate − center)/scale.',support$reference_center,support$reference_scale), '',
 '| Comparison among qualified hitters | Median absolute runs/100 change | Median absolute Decision+ change | Maximum absolute Decision+ change |',
 '|---|---:|---:|---:|')
for(reference in c('control','regional')){h<-hit[qualified==TRUE];dr<-h$continuous_runs100-h[[paste0(reference,'_runs100')]];dp<-h$continuous_decision_plus-h[[paste0(reference,'_decision_plus')]]
 lines<-c(lines,sprintf('| continuous minus %s | %.6f | %.3f | %.3f |',reference,median(abs(dr)),median(abs(dp)),max(abs(dp))))}
lines<-c(lines,'','On the common supported cohort, continuous minus categorical mean Q(swing) changes are −0.000556 runs on observed takes and −0.000093 on observed swings. Corresponding score changes are +0.02755 and −0.00611 runs/100. The largest absolute Q(swing) difference on unsupported takes is 14.662 runs; these extrapolations are retained only for audit and excluded from all primary hitter scores. Full values are in swing_value_changes_2024.csv.', '',
 'Hitter-level paired game-bootstrap intervals versus both references are fresh conditional sampling intervals in runs/100, with no reused model intervals or preference labels. Per-game scores and swing-value summaries are saved separately. Unsupported values are audit-only, not primary hitter scores.','', '## Interpretation and limitations','')
for(stage_name in c('contact','bip')){r<-boot[stage==stage_name&candidate=='continuous'&reference=='regional'&metric=='log_loss'];verdict<-if(r$ci_high<0)'improves on categorical pooling' else if(r$ci_low>0)'performs worse than categorical pooling' else 'does not show a clear improvement over categorical pooling'
 lines<-c(lines,sprintf('- **%s:** the continuous surface %s: paired log-loss change %.6f [%.6f, %.6f].',stage_name,verdict,r$difference,r$ci_low,r$ci_high))}
lines<-c(lines,'',
 'This is one prespecified heavily pooled quadratic representation. A negative result would not rule out other continuous surfaces, and a positive result would not establish that every smooth representation is superior. There was no search against 2024 outcomes.', '',
 'The two-stage approximation conditions on GAM offsets fitted on the same training observations. Histories remain chronological and unchanged. Global quadratics can extrapolate; established support exclusions remain essential, and raw unsupported predictions are retained. Observed swings/contacts are selected populations. **Better factual prediction does not establish better counterfactual decision evaluation.**', '',
 '## Reproduction and artifacts', '',
 'From the project root:', '', '```sh',
 'Rscript experiments/hitter_surface_v1/tests.R',
 'Rscript experiments/hitter_surface_v1/run.R',
 'Rscript experiments/hitter_surface_v1/verify.R',
 'Rscript experiments/hitter_surface_v1/report.R','```','',
 'The run reuses fitted surface bundles only after checking their fitting-code/protocol fingerprints. A fresh fit should use a new version directory with empty models. Original data, prior experiment bundles and dependencies must be present; this is not a data-free package.', '',
 '- surface.R: reusable basis, constrained lme4 fitting and prediction.',
 '- models/: contact/BIP bundles containing unchanged GAMs/categorical effects, new surfaces/preprocessing, histories and manifest.',
 '- predictions_* and branch_predictions_*: keyed factual and branch predictions.',
 '- metrics, bootstrap, calibration, slices, effects and fit_diagnostics: numerical evidence.',
 '- decision_predictions, hitter_changes, hitter_game_scores, swing_value_changes and score_support: score replay and audit.',
 '- input_hashes, code_hashes, independent_verification, unit_tests.log, run.log and sessionInfo: verification evidence.',
 '', 'Original inputs and the entire hitter_region_v1 directory are fingerprinted and preserved. No candidate was activated.')
pooling<-rbindlist(lapply(c('contact','bip'),function(stage_name){
 e<-read(paste0('effects_',stage_name,'.csv'));e[,exposure_band:=hs_band(n)]
 e[,coefficient_rms:=sqrt(rowMeans(as.matrix(.SD)^2)),.SDcols=paste0('b',0:5)]
 e[,.(hitters=.N,total_rows=sum(n),median_edf=median(edf),median_coefficient_rms=median(coefficient_rms)),by=exposure_band][,stage:=stage_name]
}))
fwrite(pooling,file.path(HR_REPORT,'pooling_by_exposure.csv'))
checks<-read('independent_verification.csv');stopifnot(all(checks$passed))
lines<-c(lines,'',sprintf('Verification: all %d independent artifact checks passed; synthetic solver, pooling, continuity and fallback tests also passed (unit_tests.log).',nrow(checks)))
writeLines(lines,file.path(HR_REPORT,'REPORT.md'))
sl<-c('# Descriptive exposure and region comparisons','','All slices are prespecified, descriptive and unadjusted for multiplicity. Differences below are continuous minus reference. Full counts and losses for every arm are in slices_2024.csv.','',
 '| Stage | Dimension | Slice | N | Δ log loss vs control | Δ log loss vs categorical | Δ Brier vs categorical |','|---|---|---|---:|---:|---:|---:|')
for(stage_name in c('contact','bip'))for(dim_name in c('region','history','exposure_band','hitter_exposure_band')){
 a<-slices[stage==stage_name&dimension==dim_name&arm=='control'];b<-slices[stage==stage_name&dimension==dim_name&arm=='regional'];c<-slices[stage==stage_name&dimension==dim_name&arm=='continuous']
 a<-a[match(c$slice,a$slice)];b<-b[match(c$slice,b$slice)]
 for(i in seq_len(nrow(c)))sl<-c(sl,sprintf('| %s | %s | %s | %d | %.6f | %.6f | %.6f |',stage_name,dim_name,c$slice[i],c$n[i],c$log_loss[i]-a$log_loss[i],c$log_loss[i]-b$log_loss[i],c$brier[i]-b$brier[i]))
}
writeLines(sl,file.path(HR_REPORT,'SLICE_SUMMARY.md'))
fwrite(vd_hash(list.files(HS_ROOT,pattern='[.]R$|[.]md$',full.names=TRUE)),file.path(HR_REPORT,'code_hashes.csv'))
cat('Report written\n')
