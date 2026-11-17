source('experiments/take_xgboost_v1/functions.R')
read <- function(n) fread(file.path(TR,paste0(n,'.csv')))
metrics <- read('metrics'); values <- read('values'); boot <- read('boot'); vb <- read('valueboot')
coverage <- read('coverage'); selection <- read('selection'); checks <- read('verification'); units <- read('unit_tests')
stopifnot(all(checks$passed),all(units$passed))
bundle <- readRDS(file.path(TM,'bundle.rds'))
raw <- c(count='balls and strikes',stand='stand',throws='p_throws',x='plate_x',z='plate_z, sz_bot, sz_top',
  family='pitch_family',speed='release_speed',movx='pfx_x',movz='pfx_z',zone_height='sz_top, sz_bot')
transform <- c(count='paste(balls, strikes, sep="-")',stand='factor',throws='factor',x='unchanged',
  z='(plate_z - sz_bot) / (sz_top - sz_bot)',family='factor',speed='unchanged',movx='unchanged',movz='unchanged',zone_height='sz_top - sz_bot')
features <- rbindlist(lapply(bundle$schema$features,function(nm)data.table(feature=nm,raw_fields=raw[[nm]],
  transform=transform[[nm]],levels=if(nm%in%names(bundle$schema$levels))paste(bundle$schema$levels[[nm]],collapse=';')else'',
  training_median=if(nm%in%names(bundle$prep$medians))bundle$prep$medians[[nm]]else NA_real_,
  novel_factor_reference=if(nm%in%names(bundle$schema$levels))bundle$prep$references[[nm]]else'',
  timing='current pitch, before call/outcome; no same-pitch outcome input')))
t_write(features,'feature_specification')
md <- function(d,digits=6) {
  f <- copy(d)
  for(n in names(f)) if(is.numeric(f[[n]])) set(f,j=n,value=formatC(f[[n]],format='f',digits=if(all(f[[n]]==floor(f[[n]]),na.rm=TRUE))0 else digits))
  paste(c(paste0('| ',paste(names(f),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(f)),collapse=' | '),' |'),
    apply(f,1,function(row)paste0('| ',paste(row,collapse=' | '),' |'))),collapse='\n')
}
verdict <- function(d,metric_name) {
  x <- d[year==2025 & population=='supported_takes' & stage=='complete' & metric==metric_name]
  if(x$ci_high<0)'improves'else if(x$ci_low>0)'worsens'else'has an inconclusive change in'
}
prob <- verdict(boot,'log_loss'); value <- verdict(vb,'rmse')
base <- boot[year==2025 & population=='supported_takes' & stage=='base' & metric=='log_loss']
base_verdict <- if(base$ci_high<0)'improves'else if(base$ci_low>0)'worsens'else'has an inconclusive change in'
both <- prob=='improves'&&value=='improves'
movements <- read('largest_movements_2025')[1:10,.(batter_name,supported,gam_decision_plus,xgboost_decision_plus,delta_plus,delta_plus_low,delta_plus_high)]
state <- read('state_errors')[year==2025 & supported==TRUE & n>=100]
state <- merge(state[arm=='gam',.(re288_state,n,mean_branch_gap,gam_rmse=rmse)],
  state[arm=='xgboost',.(re288_state,xgboost_rmse=rmse)],by='re288_state')
setorder(state,-mean_branch_gap); state <- head(state,10); state[,delta_rmse:=xgboost_rmse-gam_rmse]
t_write(state,'consequential_states_2025')
paragraphs <- c('# Take-path XGBoost comparison',
  sprintf('**Result:** On exploratory 2025 supported takes, the XGBoost backbone %s called-strike log loss. The complete adjusted XGBoost path %s log loss and %s integrated take-value RMSE. %s',
    base_verdict,prob,value,if(both)'Both primary conditional intervals favor the candidate; this does not establish causal decision skill.' else 'This does not establish a complete take-path improvement; retain the active scorer.'),
  'The active scorer is unchanged. 2025 was previously inspected; these are exploratory later-season results, and 2024 is secondary exploratory replication. No specification was revised using either evaluation season.',
  '## Frozen design',
  'The actual GAM has **10 context features**: count, batter stance, pitcher handedness, pitch family, horizontal location, normalized vertical location, speed, horizontal movement, vertical movement, and recorded zone height. The original GAM catcher smooth is completely excluded from its backbone. XGBoost uses the same transformed numeric values and all levels of each factor; no actor identity or new feature enters its backbone. The adopted formula uses no sequencing or player-history features and no numeric missing indicators (the preprocessor has unused previous-location indicators). Missing numeric values and novel fixed categories follow the original training medians/reference levels; novel fixed categories are excluded identically from probability evaluation.',
  md(selection),
  'Depth and round count were selected using only 2022 training / 2023 validation, then refit on the original 741,296-pitch 2022–23 GAM cohort. The cap is disclosed above; no rounds or search were extended. `models/backbone_frozen.rds` precedes 2024 actor evidence; `models/frozen.rds` locks all actor models before evaluation. Native models, preprocessing, curves and hashes are retained.',
  'Both complete paths reestimate the existing joint catcher/umpire/umpire-region structure against their own backbone offsets. Development offsets use 2022-only backbones and 2022 actor evidence. Final offsets use 2022–23 backbones: 2023 evidence predicts 2024, and 2024 evidence predicts 2025. Historical backbone/evidence overlap remains; evaluation-year actor labels are excluded. Unknown actors receive population variance, with full joint latent covariance integrated. Separate expanding weekly corrections use only ordinary takes before each seven-day block, with the unchanged 2,000-observation threshold.',
  '## Probability accuracy',
  md(metrics[year==2025,.(population,stage,arm,n,log_loss,brier,bias,calibration_intercept,calibration_slope)]),
  'Calibration intercept is calibration-in-the-large (slope fixed at one); slope comes from a joint intercept/slope logistic fit. Bias is predicted minus observed probability. Bins and count, location, edge, handedness, pitch-family and actor-history slices are in `bins.csv` and `slices.csv`. Edge means within 0.10 feet of either horizontal boundary or 0.10 normalized units of a vertical boundary.',
  'Probability gains do not improve every calibration summary: the supported complete-path slope is 1.0499 for XGBoost versus 1.0472 for GAM, while mean bias improves from +0.1243 to +0.0410 percentage points. Both slopes remain above one. The uncalibrated backbone mean bias is slightly worse for XGBoost.',
  md(boot[year==2025,.(population,stage,metric,difference,ci_low,ci_high)]),
  '## Integrated ordinary-take value',
  'Both arms use the identical frozen RE288 ball/strike branches. Targets require the next observed state and runs in the full pitch sequence to match the ordinary transition. Exceptional runner actions, sequence gaps and unverified terminal records are excluded identically. These are factual expected-future-value targets, not observed causal effects.',
  md(values[year==2025,.(population,stage,arm,n,rmse,mae,bias)]),
  md(vb[year==2025 & stage=='complete',.(population,metric,difference,ci_low,ci_high)]),
  '## Decision+ consequences',
  md(coverage),
  'Swing values, league swing probabilities, support exclusions (including 34 ordering-warning states), qualification and the original 2024 reference scale are fixed. There are no added candidate exclusions. Hitter intervals are newly sampled conditional game intervals, never copied from old model intervals. A change in grade is sensitivity to the take model, not verified improvement in decision skill.',
  md(read('tails')[year==2025]),md(movements,3),
  'The swing rows in `tails.csv` are **audit-only hypothetical take predictions**; no factual take outcome exists for those pitches. `state_errors.csv` reports errors by count/base-out state, support and ordering-warning status. `boundaries.csv` reports behavior near density and action-overlap thresholds and under stricter support cuts.',
  'The following supported states have the largest ball–strike value gaps among states with at least 100 verified takes. State digits indicate occupied first/second/third base, then outs, then count.',md(state),
  'Some consequential states worsen despite the aggregate gain. For example, with runners on first and second, two outs and a full count (`110_2_3-2`), RMSE increases by 0.00585 runs across 165 verified supported takes. These state comparisons are descriptive and sparse; no state-specific model adjustment was made.',
  '## Secondary 2024 replication',
  md(metrics[year==2024 & stage=='complete',.(population,arm,n,log_loss,brier,bias)]),
  md(values[year==2024 & stage=='complete',.(population,arm,n,rmse,mae,bias)]),
  '## Verification and limits',
  sprintf('%d independent artifact checks and %d focused checks passed, plus the existing actor-layer unit tests. Verification covers exact cohort/features, training-only preprocessing, historical offsets, original hashes, full-sequence factual targets, weekly chronology, unseen actors/covariance, outcome independence, serialization, singleton/batch/order invariance, value arithmetic, independently reconstructed metrics/calibration/slices and 500 paired game intervals, hitter aggregation and frozen scaling.',nrow(checks),nrow(units)),
  'All reported differences are XGBoost minus GAM. Every interval uses 500 paired whole-game resamples and is conditional on fitted models and the realized weekly calibration history. Hitter intervals resample each hitter’s games. They do not propagate full XGBoost fitting, hyperparameter, actor-fit or weekly-history uncertainty. Slice comparisons are exploratory and pointwise. No untouched holdout or causal/counterfactual validation is claimed.',
  '![2025 supported-take comparison](comparison.png)',
  '![2025 complete-path calibration](calibration.png)',
  'Run instructions and artifact map: [README](../README.md).')
writeLines(paste(paragraphs,collapse='\n\n'),file.path(TR,'REPORT.md'))
png(file.path(TR,'comparison.png'),width=1500,height=650,res=140)
par(mfrow=c(1,2),mar=c(5,5,3,1))
for(spec in list(list(boot,'log_loss','Log-loss difference'),list(vb,'rmse','Take-value RMSE difference'))) {
  x <- spec[[1]][year==2025 & population=='supported_takes' & metric==spec[[2]]]
  x <- x[match(c('base','complete'),stage)]
  plot(1:2,x$difference,ylim=range(c(0,x$ci_low,x$ci_high)),xlim=c(.5,2.5),xaxt='n',xlab='',ylab=spec[[3]],pch=19,col='#236b8e',main='2025 supported takes')
  axis(1,1:2,c('Backbone','Complete path'));abline(h=0,lty=2,col='gray50')
  arrows(1:2,x$ci_low,1:2,x$ci_high,angle=90,code=3,length=.08,col='#236b8e')
}
dev.off()
png(file.path(TR,'calibration.png'),width=1000,height=750,res=140)
cal <- read('bins')[year==2025 & population=='supported_takes' & stage=='complete']
plot(c(0,1),c(0,1),type='n',xlab='Mean predicted strike probability',ylab='Observed strike fraction',main='2025 supported takes: complete paths')
abline(0,1,lty=2,col='gray50')
for(arm in c('gam','xgboost')) {
  keep_arm <- cal$arm==arm; z <- cal[keep_arm][order(predicted)]
  lines(z$predicted,z$observed,type='b',pch=if(arm=='gam')16 else 17,col=if(arm=='gam')'#236b8e'else'#b45309')
}
legend('topleft',c('GAM','XGBoost'),col=c('#236b8e','#b45309'),pch=c(16,17),lty=1,bty='n')
dev.off()
t_write(t_hash(list.files(TROOT,pattern='[.]R$|[.]md$',full.names=TRUE)),'final_code_hashes')
message('Report complete')
