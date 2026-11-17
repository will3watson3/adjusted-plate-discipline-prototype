source('experiments/swing_xgboost_v1/functions.R')
checks<-list();check<-function(name,ok){if(!isTRUE(ok))stop(name);checks[[name]]<<-data.table(check=name,passed=TRUE)}
x_check_hash(fread(file.path(SR,'input_hashes.csv')));freeze<-readRDS(file.path(SM,'frozen.rds'));x_check_hash(freeze$model_hashes);x_check_hash(freeze$code_hashes)
ev<-readRDS(file.path(SR,'evaluation_manifest.rds'));x_check_hash(ev$code_hashes)
check('frozen_and_not_activated',!freeze$activated&&!ev$activated&&freeze$frozen_at<=ev$completed)
h<-readRDS(file.path(SM,'histories.rds'));check('exact_prior_season_histories',isTRUE(all.equal(h,s_histories())))
readgz<-function(name)fread(cmd=paste('gzip -dc',shQuote(file.path(SR,name))))
# Independent bootstrap arithmetic using base-R grouped sums.
v_boot<-function(game,y,a,b,stage){
 logloss<-function(p){p<-pmin(1-1e-15,pmax(1e-15,p));-y*log(p)-(1-y)*log1p(-p)}
 loss<-if(stage=='value')list(rmse=list((a-y)^2,(b-y)^2),mae=list(abs(a-y),abs(b-y)),bias=list(a-y,b-y))else list(log_loss=list(logloss(a),logloss(b)),brier=list((a-y)^2,(b-y)^2),bias=list(a-y,b-y))
 nn<-tabulate(match(game,unique(game)))
 rbindlist(lapply(names(loss),function(metric){
  totals<-rowsum(cbind(loss[[metric]][[1]],loss[[metric]][[2]]),game,reorder=FALSE)
  calc<-function(i){p<-colSums(totals[i,,drop=FALSE])/sum(nn[i]);if(metric=='rmse')sqrt(p[1])-sqrt(p[2])else p[1]-p[2]}
  set.seed(XSEED);draw<-replicate(500,calc(sample.int(length(nn),length(nn),TRUE)))
  data.table(metric=metric,difference=calc(seq_along(nn)),ci_low=unname(quantile(draw,.025)),ci_high=unname(quantile(draw,.975)))
 }))
}

re<-read_re_baseline();api<-s_api();raw<-rbindlist(lapply(2022:2023,hr_read));devre<-list(re288=fread('reports/post_contact/RE288_development_train2022.csv'),id='train2022_only')
# Independently rebuild the development RE table from 2022 pitch states.
r22<-as.data.table(readRDS('data/processed/statcast_2022_clean.rds'));r22<-copy(r22[,..needed]);r22[,season:=2022L]
expected_re<-fit_re288(prepare_pitch_states(r22),250)$table
j<-match(devre$re288$re288_state,expected_re$re288_state)
check('development_RE_uses_2022_only',!anyNA(j)&&max(abs(devre$re288$re288-expected_re$re288[j]))<1e-10)
rm(r22,expected_re);gc(FALSE)
for(stage_name in SSTAGES){
 message('Verify fitted ',stage_name);b<-readRDS(file.path(SM,paste0(stage_name,'.rds')));base<-s_base(stage_name)
 check(paste(stage_name,'same_features'),identical(b$schema$features,setdiff(all.vars(formula(base$model)),'y')))
 check(paste(stage_name,'same_preprocessing'),identical(b$prep,base$prep))
 check(paste(stage_name,'no_outcomes_or_ids'),!any(c('batter','pitcher','zone','description','launch_speed','launch_angle','q_bip_observed')%in%b$schema$features))
 d<-s_data(raw,stage_name,api,re)
 check(paste(stage_name,'training_keys'),identical(s_key(d),s_key(b$training_keys))&&nrow(d)==base$n)
 check(paste(stage_name,'training_target'),max(abs(d$y-b$training_y))<1e-10&&max(abs(d$y-base$model$model$y))<1e-10)
 f<-s_features(d,b,h)
 for(nm in b$schema$features)check(paste(stage_name,nm,'training_parity'),isTRUE(all.equal(f[[nm]],base$model$model[[nm]],check.attributes=FALSE)))
 dd<-s_data(raw,stage_name,api,if(stage_name=='value')devre else re)[season==2022]
 pp<-ct_preprocessor(dd,h$contact[['2021']]);pp$power<-h$power[['2021']];pp$base_out_levels<-base$prep$base_out_levels
 pp$missing<-union(pp$missing,sub('_missing$','',b$schema$features[grepl('_missing$',b$schema$features)]))
 stored<-readRDS(file.path(SM,paste0(stage_name,'_development_preprocessing.rds')))
 check(paste(stage_name,'development_preprocessing_2022_only'),isTRUE(all.equal(stored$prep,pp,check.attributes=FALSE)))
 sel<-fread(file.path(SR,paste0('selection_',stage_name,'.csv')));setorder(sel,validation_loss,depth)
 check(paste(stage_name,'chronological_selection'),nrow(sel)==3&&setequal(sel$depth,3:5)&&b$selected$depth==sel$depth[1]&&b$nrounds==sel$best_rounds[1])
 for(depth_value in 3:5){
  curve<-fread(file.path(SR,paste0('development_',stage_name,'_',depth_value,'.csv')));row<-sel[sel$depth==depth_value]
  metric<-if(stage_name=='value')'validation_rmse'else'validation_logloss';k<-which.min(curve[[metric]])
  check(paste(stage_name,depth_value,'early_stopping'),row$best_rounds==k&&abs(row$validation_loss-curve[[metric]][k])<1e-10&&nrow(curve)<=1200)
 }
 check(paste(stage_name,'serialized_rounds'),xgb.get.num.boosted.rounds(xgb.load(file.path(SM,paste0(stage_name,'.ubj'))))==b$nrounds)
 rm(d,f,dd,base,b);gc(FALSE)
}
rm(raw);gc(FALSE)
metrics<-fread(file.path(SR,'component_metrics.csv'));boots<-fread(file.path(SR,'component_bootstrap.csv'));slices<-fread(file.path(SR,'slices.csv'));cal<-fread(file.path(SR,'calibration.csv'))
im<-fread(file.path(SR,'integrated_metrics.csv'));ib<-fread(file.path(SR,'integrated_bootstrap.csv'));support<-fread(file.path(SR,'support.csv'))
contract<-readRDS('data/models/decision_validation/support_contract.rds');ref<-readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
cb<-readRDS('experiments/contact_xgboost_v1/models/bundle.rds');cm<-xgb.load('experiments/contact_xgboost_v1/models/contact.ubj')
for(yr in 2024:2025){
 message('Verify full replay ',yr);raw<-hr_read(yr);out<-readgz(paste0('predictions_all_',yr,'.csv.gz'))
 old<-as.data.table(readRDS(sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',yr)));old<-old[match(s_key(out),s_key(old))]
 check(paste(yr,'all_unique_keys'),identical(s_key(raw),s_key(out))&&!anyDuplicated(out,by=CT_KEYS)&&nrow(raw)==nrow(old))
 check(paste(yr,'frozen_take_and_policy'),max(abs(out$q_take-old$q_take_basic))<1e-12&&max(abs(out$p_swing_reference-old$p_swing_reference))<1e-12)
 check(paste(yr,'no_support_promotions_or_RE_warnings'),all(!out$primary_supported|old$primary_supported)&&!any(out$primary_supported&old$state_value_order_warning))
 range_bad<-rep(FALSE,nrow(out))
 for(stage_name in HR_STAGES){
  message('Verify factual ',yr,' ',stage_name);base<-s_base(stage_name);f<-hr_features(raw,base,h$contact,h$power)
  modeled<-intersect(names(base$ranges),all.vars(formula(base$model)));range_bad<-range_bad|vd_range_flags(f,list(ranges=base$ranges[modeled]))
  d<-s_data(raw,stage_name,api,re);p<-readgz(paste0('factual_',stage_name,'_',yr,'.csv.gz'))
  check(paste(yr,stage_name,'same_factual_cohort'),identical(s_key(d),s_key(p))&&max(abs(d$y-p$y))<1e-12)
  j<-match(s_key(p),s_key(out))
  for(arm_name in c('gam','xgboost')){
   check(paste(yr,stage_name,arm_name,'all_pitch_consistency'),max(abs(p[[arm_name]]-out[[paste(stage_name,arm_name,sep='_')]][j]))<1e-10)
   pp<-p[[arm_name]];check(paste(yr,stage_name,arm_name,'finite_range'),all(is.finite(pp)&pp>=0)&&(stage_name=='value'||all(pp<=1)))
   expected<-s_metrics(p$y,pp,stage_name);actual<-metrics[year==yr&stage==stage_name&arm==arm_name]
   for(nm in names(expected))check(paste(yr,stage_name,arm_name,nm),nrow(actual)==1&&max(abs(actual[[nm]]-expected[[nm]]))<1e-8)
   for(dim_name in unique(slices$dimension)){
    zz<-data.table(slice=p[[dim_name]],y=p$y,p=pp)
    zz<-if(stage_name=='value')zz[,.(n=.N,rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),bias=mean(p-y)),by=slice]else zz[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),bias=mean(p-y)),by=slice]
    aa<-slices[year==yr&stage==stage_name&arm==arm_name&dimension==dim_name];aa<-aa[match(zz$slice,aa$slice)]
    for(nm in setdiff(names(zz),'slice'))check(paste(yr,stage_name,arm_name,dim_name,nm),max(abs(aa[[nm]]-zz[[nm]]))<1e-9)
   }
   if(stage_name!='value'){
    zz<-data.table(y=p$y,p=pp)[,bin:=if(stage_name=='tip')pmin(199L,floor(p*200))else pmin(19L,floor(p*20))][,.(n=.N,predicted=mean(p),observed=mean(y)),by=bin]
    aa<-cal[year==yr&stage==stage_name&arm==arm_name];aa<-aa[match(zz$bin,aa$bin)]
    check(paste(yr,stage_name,arm_name,'calibration_bins'),all(aa$n==zz$n)&&max(abs(aa$observed-zz$observed))<1e-10&&max(abs(aa$predicted-zz$predicted))<1e-10)
   }
  }
  expected<-v_boot(p$game_pk,p$y,p$xgboost,p$gam,stage_name);actual<-boots[year==yr&stage==stage_name];actual<-actual[match(expected$metric,actual$metric)]
  check(paste(yr,stage_name,'paired_bootstrap'),max(abs(as.matrix(actual[,.(difference,ci_low,ci_high)])-as.matrix(expected[,.(difference,ci_low,ci_high)])))<1e-10)
  ix<-unique(round(seq(1,nrow(raw),length.out=60)));dd<-copy(raw[ix]);dd[,`:=`(description='CORRUPTED',y=-999,launch_speed=999,launch_angle=999,q_bip_observed=999)]
  if(stage_name=='contact'){pred<-function(z)x_predict(cm,z,cb);single<-function(z)x_predict(cm,rbindlist(list(z,z)),cb)[1]}else{
   bb<-readRDS(file.path(SM,paste0(stage_name,'.rds')));mm<-xgb.load(file.path(SM,paste0(stage_name,'.ubj')))
   pred<-function(z)s_predict(mm,z,bb,h);single<-pred
  }
  truth<-out[[paste(stage_name,'xgboost',sep='_')]][ix]
  check(paste(yr,stage_name,'label_independence_and_native_reload'),max(abs(pred(dd)-truth))<1e-7)
  check(paste(yr,stage_name,'row_and_batch_invariance'),max(abs(rev(pred(dd[nrow(dd):1]))-truth))<1e-7&&max(abs(unlist(lapply(split(seq_len(nrow(dd)),ceiling(seq_len(nrow(dd))/7)),function(i)pred(dd[i])),use.names=FALSE)-truth))<1e-7)
  check(paste(yr,stage_name,'singleton'),abs(single(dd[1])-truth[1])<1e-7)
  bad<-copy(dd);bad[,plate_x:=NA_real_];check(paste(yr,stage_name,'invalid_core_rejected'),inherits(try(pred(bad),silent=TRUE),'try-error'))
  unseen<-copy(dd);unseen[,`:=`(batter=-999L,pitcher=-998L)];uf<-hr_features(unseen,base,h$contact,h$power)
  check(paste(yr,stage_name,'unseen_history_fallback'),all(uf$batter_unseen&uf$pitcher_unseen)&&all(is.finite(pred(unseen))))
  gg<-base$model$family$linkinv(hr_eta(base$model,f[ix]))
  check(paste(yr,stage_name,'GAM_prediction_rebuilt'),max(abs(gg-out[[paste(stage_name,'gam',sep='_')]][ix]))<1e-9)
  if(stage_name=='value'){
   fy<-rep(NA_real_,nrow(out));fy[j]<-p$y
   fy[out$description%chin%c('swinging_strike','swinging_strike_blocked')]<-old$q_whiff[out$description%chin%c('swinging_strike','swinging_strike_blocked')]
   fy[out$description=='foul']<-old$q_ordinary_foul[out$description=='foul'];fy[out$description=='foul_tip']<-old$q_foul_tip[out$description=='foul_tip']
   check(paste(yr,'factual_targets_rebuilt'),identical(is.na(fy),is.na(out$factual_target))&&max(abs(fy-out$factual_target),na.rm=TRUE)<1e-10)
  }
  rm(f,d,p,base);gc(FALSE)
 }
 check(paste(yr,'updated_range_support'),identical(range_bad,out$updated_range_failure))
 expected_support<-out$original_supported&!range_bad&out$value_gam>=contract$bip_target_range[1]&out$value_gam<=contract$bip_target_range[2]&out$value_xgboost>=contract$bip_target_range[1]&out$value_xgboost<=contract$bip_target_range[2]
 check(paste(yr,'full_common_support'),identical(expected_support,out$primary_supported))
 check(paste(yr,'GAM_support_reconstruction'),identical(out$gam_supported,out$original_supported&!range_bad&out$value_gam>=contract$bip_target_range[1]&out$value_gam<=contract$bip_target_range[2]))
 check(paste(yr,'density_for_sensitivity'),identical(out$min_cell_n,pmin(old$contact_training_cell_n,old$post_training_cell_n,old$bip_training_cell_n,old$take_training_cell_n)))
 for(arm_name in SARMS){
  br<-readgz(paste0('branches_',arm_name,'_',yr,'.csv.gz'));check(paste(yr,arm_name,'branch_keys'),identical(s_key(out),s_key(br)))
  src<-if(arm_name=='xgboost')'xgboost'else'gam';cp<-out[[if(arm_name=='gam')'contact_gam'else'contact_xgboost']]
  bp<-out[[paste0('bip_',src)]];tp<-out[[paste0('tip_',src)]];qv<-out[[paste0('value_',src)]]
  mat<-cbind(whiff=1-cp,bip=cp*bp,foul=cp*(1-bp)*(1-tp),tip=cp*(1-bp)*tp)
  expected<-as.data.table(mat);expected[,`:=`(q_bip=qv,p_tip_given_nonbip=tp,q_swing=rowSums(mat*cbind(old$q_whiff,qv,old$q_ordinary_foul,old$q_foul_tip)))]
  check(paste(yr,arm_name,'probability_simplex'),max(abs(rowSums(mat)-1))<1e-12&&all(mat>=0&mat<=1))
  check(paste(yr,arm_name,'simplex_and_branch_arithmetic'),max(abs(as.matrix(expected)-as.matrix(br[,names(expected),with=FALSE])))<1e-9)
  check(paste(yr,arm_name,'swing_and_score_arithmetic'),max(abs(br$q_swing-out[[paste0('q_swing_',arm_name)]]))<1e-9&&max(abs((as.integer(out$action=='swing')-out$p_swing_reference)*(br$q_swing-out$q_take)-out[[paste0('score_',arm_name)]]))<1e-9)
 }
 for(pop_name in c('all_factual_swings','gam_supported','common_supported')){
  keep<-out$action=='swing'&is.finite(out$factual_target)
  if(pop_name=='gam_supported')keep<-keep&out$gam_supported
  if(pop_name=='common_supported')keep<-keep&out$primary_supported
  z<-out[keep]
  for(arm_name in SARMS){
   expected<-s_metrics(z$factual_target,z[[paste0('q_swing_',arm_name)]],'value');aa<-im[year==yr&population==pop_name&arm==arm_name]
   for(nm in names(expected))check(paste(yr,pop_name,arm_name,nm),abs(expected[[nm]]-aa[[nm]])<1e-9)
   check(paste(yr,pop_name,arm_name,'branch_loss'),abs(mean(z[[paste0('branch_loss_',arm_name)]])-aa$branch_log_loss)<1e-10)
  }
  for(ref_name in c('gam','contact_only')){
   expected<-v_boot(z$game_pk,z$factual_target,z$q_swing_xgboost,z[[paste0('q_swing_',ref_name)]],'value');aa<-ib[year==yr&population==pop_name&reference==ref_name];aa<-aa[match(expected$metric,aa$metric)]
   check(paste(yr,pop_name,ref_name,'value_bootstrap'),max(abs(as.matrix(aa[,.(difference,ci_low,ci_high)])-as.matrix(expected[,.(difference,ci_low,ci_high)])))<1e-10)
  }
 }
 hit<-fread(file.path(SR,paste0('hitters_',yr,'.csv')));games<-readgz(paste0('hitter_games_',yr,'.csv.gz'))
 check(paste(yr,'unique_hitter_and_game_keys'),!anyDuplicated(hit$batter)&&!anyDuplicated(games,by=c('batter','game_pk')))
 check(paste(yr,'qualification'),identical(hit$qualified,!is.na(hit$games)&hit$supported>=500&hit$games>=60&hit$coverage>=.7))
 for(arm_name in SARMS){
  hh<-out[primary_supported==TRUE,.(rate=100*mean(get(paste0('score_',arm_name))),n=.N,games=uniqueN(game_pk)),by=batter];j<-match(hh$batter,hit$batter)
  check(paste(yr,arm_name,'hitter_scores_and_reference'),max(abs(hh$rate-hit[[paste0(arm_name,'_runs100')]][j]))<1e-9&&max(abs(100+10*(hh$rate-ref$center)/ref$scale-hit[[paste0(arm_name,'_decision_plus')]][j]))<1e-9)
  check(paste(yr,arm_name,'hitter_exposure'),all(hh$n==hit$supported[j])&&all(hh$games==hit$games[j]))
  gg<-out[primary_supported==TRUE,.(n=.N,total=sum(get(paste0('score_',arm_name)))),by=.(batter,game_pk)];jj<-match(paste(games$batter,games$game_pk),paste(gg$batter,gg$game_pk))
  check(paste(yr,arm_name,'game_totals'),!anyNA(jj)&&all(games$n==gg$n[jj])&&max(abs(games[[arm_name]]-gg$total[jj]))<1e-9)
 }
 set.seed(XSEED);intervals<-games[,{
  nn<-.N;draws<-replicate(500,{i<-sample.int(nn,nn,TRUE);c(100*sum(xgboost[i]-gam[i])/sum(n[i]),100*sum(xgboost[i]-contact_only[i])/sum(n[i]))})
  .(vs_gam_low=unname(quantile(draws[1,],.025)),vs_gam_high=unname(quantile(draws[1,],.975)),vs_contact_low=unname(quantile(draws[2,],.025)),vs_contact_high=unname(quantile(draws[2,],.975)))
 },by=batter];j<-match(intervals$batter,hit$batter)
 for(nm in setdiff(names(intervals),'batter'))check(paste(yr,nm,'fresh_interval'),max(abs(intervals[[nm]]-hit[[nm]][j]))<1e-9)
 check(paste(yr,'support_summary'),support[year==yr,common_supported]==sum(out$primary_supported))
 sh<-fread(file.path(SR,paste0('sensitivity_hitters_',yr,'.csv')));ss<-fread(file.path(SR,'sensitivity.csv'))
 for(policy_name in c('base','cells_50','overlap_10','joint')){
  keep<-out$primary_supported
  if(policy_name%in%c('cells_50','joint'))keep<-keep&out$min_cell_n>=50
  if(policy_name%in%c('overlap_10','joint'))keep<-keep&out$p_swing_reference>=.1&out$p_swing_reference<=.9
  zz<-out[keep,.(n=.N,games=uniqueN(game_pk),delta=1000*mean(score_xgboost-score_gam)/ref$scale),by=batter]
  aa<-sh[policy==policy_name];jj<-match(zz$batter,aa$batter)
  check(paste(yr,policy_name,'sensitivity_count'),sum(keep)==ss[year==yr&policy==policy_name,supported]&&all(zz$n==aa$n[jj])&&all(zz$games==aa$games[jj]))
  check(paste(yr,policy_name,'sensitivity_delta'),max(abs(zz$delta-aa$delta_plus[jj]))<1e-9)
 }

 rm(raw,out,old);gc(FALSE)
}
x_check_hash(fread(file.path(SR,'input_hashes.csv')));x_check_hash(freeze$model_hashes)
fwrite(rbindlist(checks),file.path(SR,'verification.csv'));cat(length(checks),'independent checks passed\n')
