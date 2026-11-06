source('experiments/hitter_surface_v1/surface.R')
checks<-list();check<-function(name,ok){if(!isTRUE(ok))stop(name);checks[[name]]<<-data.table(check=name,passed=TRUE)}
vd_check_hash(fread(file.path(HR_REPORT,'input_hashes.csv')));check('all_original_inputs_and_entire_regional_experiment_unchanged',TRUE)
m<-readRDS(file.path(HR_MODELS,'manifest.rds'));vd_check_hash(m$code_hashes);check('not_activated',identical(m$activated,FALSE))
histories<-readRDS(file.path(HR_MODELS,'histories.rds'))
check('contact_histories_exact',isTRUE(all.equal(histories$contact,setNames(lapply(2021:2023,cp_profiles),2021:2023))))
check('power_histories_exact',isTRUE(all.equal(histories$power,setNames(lapply(2021:2023,hr_power),2021:2023))))
metrics<-fread(file.path(HR_REPORT,'metrics_2024.csv'));slices<-fread(file.path(HR_REPORT,'slices_2024.csv'))
cal<-fread(file.path(HR_REPORT,'calibration_2024.csv'));boots<-fread(file.path(HR_REPORT,'bootstrap_2024.csv'))
for(stage_name in c('contact','bip')){
 b<-readRDS(file.path(HR_MODELS,paste0(stage_name,'.rds')));old<-readRDS(file.path(HS_OLD,paste0(stage_name,'.rds')))
 check(paste(stage_name,'backbone_identical'),identical(b$model[setdiff(names(b$model),'family')],old$model[setdiff(names(old$model),'family')])&&isTRUE(all.equal(b$model$family,old$model$family))&&identical(b$prep,old$prep)&&identical(b$pool,old$pool))
 check(paste(stage_name,'chronology'),identical(b$history_years,c('2022'=2021L,'2023'=2022L,'2024'=2023L)))
 check(paste(stage_name,'converged'),b$surface$converged&&b$surface$max_gradient<.01)
 vd_check_hash(b$surface_code_hash)
 # Verify the actual learned surfaces across approximate old geometric grid edges.
 grid<-CJ(x=c(-.83,-.28,.28,.83),z=c(0,1/3,2/3,1))
 dd<-data.table(batter=rep(as.integer(rownames(b$surface$effects)[1]),nrow(grid)))
 left<-copy(grid);right<-copy(grid);left[,x:=x-1e-8];right[,x:=x+1e-8]
 check(paste(stage_name,'actual_horizontal_continuity'),max(abs(hs_adjust(b,dd,left)$effect-hs_adjust(b,dd,right)$effect))<1e-5)
 left<-copy(grid);right<-copy(grid);left[,z:=z-1e-8];right[,z:=z+1e-8]
 check(paste(stage_name,'actual_vertical_continuity'),max(abs(hs_adjust(b,dd,left)$effect-hs_adjust(b,dd,right)$effect))<1e-5)
 missing<-copy(dd);missing[,batter:=NA_integer_]
 check(paste(stage_name,'missing_identity_fallback'),all(hs_adjust(b,missing,grid)$effect==0))
 invalid<-copy(grid);invalid[,z:=Inf]
 check(paste(stage_name,'nonfinite_location_fallback'),all(hs_adjust(b,dd,invalid)$effect==0))
 dd[,`:=`(sz_top=1,sz_bot=2)]
 check(paste(stage_name,'invalid_zone_height_fallback'),all(hs_adjust(b,dd,grid)$effect==0))
 p<-hr_fread(file.path(HR_REPORT,paste0('predictions_',stage_name,'_2024.csv.gz')))
 prior<-hr_fread(file.path('experiments/hitter_region_v1/reports',paste0('predictions_',stage_name,'_2024.csv.gz')))
 i<-match(dv_key(p),dv_key(prior));check(paste(stage_name,'same_factual_cohort'),!anyNA(i)&&!anyDuplicated(p,by=CT_KEYS)&&nrow(p)==nrow(prior)&&identical(p$y,prior$y[i]))
 for(arm_name in HS_ARMS){
  if(arm_name!='continuous')check(paste(stage_name,arm_name,'benchmark_reproduced'),max(abs(p[[arm_name]]-prior[[arm_name]][i]))<1e-12)
  actual<-metrics[stage==stage_name&arm==arm_name];expected<-hr_metrics(p$y,p[[arm_name]],stage_name)
  for(nm in names(expected))check(paste(stage_name,arm_name,nm),max(abs(actual[[nm]]-expected[[nm]]))<1e-9)
  selected<-slices[stage==stage_name&arm==arm_name]
  for(dim_name in unique(selected$dimension)){
   z<-data.table(slice=p[[dim_name]],y=p$y,p=p[[arm_name]])
   expected<-z[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),mean_error=mean(p-y)),by=slice]
   actual<-selected[dimension==dim_name];actual<-actual[match(expected$slice,actual$slice)]
   for(nm in setdiff(names(expected),'slice'))check(paste(stage_name,arm_name,dim_name,nm,'independent_slice'),max(abs(actual[[nm]]-expected[[nm]]))<1e-10)
  }
  z<-data.table(y=p$y,p=p[[arm_name]])[,bin:=pmin(19L,floor(p*20))][,.(n=.N,predicted=mean(p),observed=mean(y)),by=bin]
  a<-cal[stage==stage_name&arm==arm_name];a<-a[match(z$bin,a$bin)]
  for(nm in c('n','predicted','observed'))check(paste(stage_name,arm_name,nm,'calibration_bins'),max(abs(a[[nm]]-z[[nm]]))<1e-10)
 }
 check(paste(stage_name,'unseen_exact_fallback'),all(p$continuous[p$fallback=='unseen_hitter']==p$control[p$fallback=='unseen_hitter']))
 for(candidate_name in c('regional','continuous'))for(reference_name in if(candidate_name=='regional')'control' else c('control','regional')){
  z<-boots[stage==stage_name&candidate==candidate_name&reference==reference_name]
  for(metric_name in c('log_loss','brier','mean_error')){
   fn<-switch(metric_name,log_loss=function(pp)cs_loss(p$y,pp),brier=function(pp)(pp-p$y)^2,mean_error=function(pp)pp-p$y)
   delta<-fn(p[[candidate_name]])-fn(p[[reference_name]])
   actual<-z[metric==metric_name];check(paste(stage_name,candidate_name,reference_name,metric_name,'paired_difference'),abs(actual$difference-mean(delta))<1e-10)
   g<-data.table(game=p$game_pk,delta=delta)[,.(n=.N,total=sum(delta)),by=game]
   set.seed(20261102);draws<-replicate(500,{i<-sample.int(nrow(g),nrow(g),TRUE);sum(g$total[i])/sum(g$n[i])})
   check(paste(stage_name,candidate_name,reference_name,metric_name,'bootstrap_rebuilt'),max(abs(c(actual$ci_low,actual$ci_high)-quantile(draws,c(.025,.975))))<1e-10)
  }
 }
}
s<-hr_fread(file.path(HR_REPORT,'decision_predictions_2024.csv.gz'));old<-as.data.table(readRDS('data/models/hitter_stat_v6/decision_values_2024.rds'))
i<-match(dv_key(s),dv_key(old));check('unique_full_cohort',!anyNA(i)&&!anyDuplicated(s,by=CT_KEYS)&&nrow(s)==nrow(old))
check('take_frozen',max(abs(s$q_take-old$q_take_basic[i]))<1e-12);check('policy_frozen',max(abs(s$p_swing_reference-old$p_swing_reference[i]))<1e-12)
check('no_support_promotions',all(!s$primary_supported|old$primary_supported[i]));check('no_RE_order_warnings_scored',!any(s$primary_supported&old$state_value_order_warning[i]))
check('no_stale_intervals_or_preferences',!any(grepl('ci_|preference|envelope',names(s))))
control_br<-NULL
for(arm_name in HS_ARMS){
 br<-hr_fread(file.path(HR_REPORT,paste0('branch_predictions_',arm_name,'_2024.csv.gz')))
 check(paste(arm_name,'branch_keys'),identical(br[,..CT_KEYS],s[,..CT_KEYS]))
 mat<-as.matrix(br[,.(whiff,bip,foul,tip)]);check(paste(arm_name,'probability_simplex'),all(is.finite(mat))&&all(mat>=0&mat<=1)&&max(abs(rowSums(mat)-1))<1e-12)
 if(is.null(control_br)){
  control_br<-br
  previous<-hr_fread('experiments/hitter_region_v1/reports/branch_predictions_control_2024.csv.gz')
  j<-match(dv_key(br),dv_key(previous))
  check('all_control_branches_reproduce_updated_history_baseline',!anyNA(j)&&max(abs(as.matrix(br[,.(whiff,bip,foul,tip,q_bip,q_swing)])-as.matrix(previous[j,.(whiff,bip,foul,tip,q_bip,q_swing)])))<1e-10)
 } else check(paste(arm_name,'tip_and_value_fixed'),identical(br$q_bip,control_br$q_bip)&&identical(br$p_tip_given_nonbip,control_br$p_tip_given_nonbip))
 q<-br$whiff*old$q_whiff[i]+br$bip*br$q_bip+br$foul*old$q_ordinary_foul[i]+br$tip*old$q_foul_tip[i]
 check(paste(arm_name,'deterministic_transitions'),max(abs(q-br$q_swing))<1e-10)
 check(paste(arm_name,'swing_matches'),max(abs(q-s[[paste0('q_swing_',arm_name)]]))<1e-10)
 check(paste(arm_name,'score_formula'),max(abs((as.integer(s$description%chin%CT_SWINGS)-s$p_swing_reference)*(q-s$q_take)-s[[paste0('score_',arm_name)]]))<1e-10)
 for(stage_name in c('contact','bip')){
  p<-hr_fread(file.path(HR_REPORT,paste0('predictions_',stage_name,'_2024.csv.gz')));j<-match(dv_key(p),dv_key(br))
  reconstructed<-if(stage_name=='contact')1-br$whiff[j] else br$bip[j]/(1-br$whiff[j])
  check(paste(arm_name,stage_name,'factual_all_pitch_consistency'),max(abs(reconstructed-p[[arm_name]]))<1e-9)
 }
}
contract<-readRDS('data/models/decision_validation/support_contract.rds')
check('support_reconstruction',identical(s$primary_supported,s$original_supported&!s$updated_range_failure&control_br$q_bip>=contract$bip_target_range[1]&control_br$q_bip<=contract$bip_target_range[2]))
hit<-fread(file.path(HR_REPORT,'hitter_changes_2024.csv'));ref<-readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
check('qualification',identical(hit$qualified,hit$supported>=500&hit$games>=60&hit$coverage>=.7))
for(arm_name in HS_ARMS){
 calc<-s[primary_supported==TRUE,.(rate=100*mean(get(paste0('score_',arm_name)))),by=batter];j<-match(calc$batter,hit$batter)
 check(paste(arm_name,'hitter_score'),max(abs(calc$rate-hit[[paste0(arm_name,'_runs100')]][j]))<1e-10)
 check(paste(arm_name,'frozen_reference'),max(abs(100+10*(calc$rate-ref$center)/ref$scale-hit[[paste0(arm_name,'_decision_plus')]][j]))<1e-9)
}
# Rebuild saved hitter-game totals and fresh paired sampling intervals.
games<-hr_fread(file.path(HR_REPORT,'hitter_game_scores_2024.csv.gz'))
for(arm_name in HS_ARMS){
 rebuilt<-s[primary_supported==TRUE,.(n=.N,total=sum(get(paste0('score_',arm_name)))),by=.(batter,game_pk)]
 j<-match(paste(games$batter,games$game_pk),paste(rebuilt$batter,rebuilt$game_pk))
 check(paste(arm_name,'game_totals'),!anyNA(j)&&all(games$n==rebuilt$n[j])&&max(abs(games[[arm_name]]-rebuilt$total[j]))<1e-10)
}
set.seed(20261103)
intervals<-games[,{
 N<-.N;draws<-replicate(500,{j<-sample.int(N,N,TRUE);c(100*sum(continuous[j]-control[j])/sum(n[j]),100*sum(continuous[j]-regional[j])/sum(n[j]))})
 .(vs_control_low=quantile(draws[1,],.025),vs_control_high=quantile(draws[1,],.975),vs_regional_low=quantile(draws[2,],.025),vs_regional_high=quantile(draws[2,],.975))
},by=batter]
j<-match(intervals$batter,hit$batter)
for(nm in setdiff(names(intervals),'batter'))check(paste(nm,'sampling_interval_rebuilt'),max(abs(intervals[[nm]]-hit[[nm]][j]))<1e-9)
# Verify chronology on real rows from each season, independently selecting prior profiles.
for(yr in 2022:2024){
 d<-as.data.table(readRDS(sprintf('data/processed/statcast_%d_clean.rds',yr)));d<-d[analysis_eligible%in%TRUE&ct_core_valid(d)][1:40];d[,season:=yr]
 for(stage_name in c('contact','bip')){
  b<-readRDS(file.path(HR_MODELS,paste0(stage_name,'.rds')));f<-hr_features(d,b,histories$contact,histories$power)
  pp<-b$prep;pp$profiles<-histories$contact[[as.character(yr-1)]]
  expected<-ct_raw_features(d,pp$profiles)
  check(paste(yr,stage_name,'history_values'),identical(f$batter_history,expected$batter_history)&&identical(f$pitcher_history,expected$pitcher_history))
  if(stage_name=='bip'){pp$power<-histories$power[[as.character(yr-1)]];ee<-pc_transform(d,pp)$data
   check(paste(yr,'power_values'),identical(f$batter_ev_2021,ee$batter_ev_2021)&&identical(f$pitcher_ev_2021,ee$pitcher_ev_2021))}
 }
 rm(d);gc(FALSE)
}
fwrite(rbindlist(checks),file.path(HR_REPORT,'independent_verification.csv'))
cat(length(checks),'independent checks passed\n')
