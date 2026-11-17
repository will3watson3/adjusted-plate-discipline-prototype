source('experiments/swing_xgboost_v1/functions.R')
freeze<-readRDS(file.path(SM,'frozen.rds'));x_check_hash(freeze$model_hashes);x_check_hash(freeze$code_hashes)
x_check_hash(fread(file.path(SR,'input_hashes.csv')))
h<-readRDS(file.path(SM,'histories.rds'));re<-read_re_baseline();api<-s_api()
contact<-readRDS('experiments/contact_xgboost_v1/models/bundle.rds');cm<-xgb.load('experiments/contact_xgboost_v1/models/contact.ubj')
contract<-readRDS('data/models/decision_validation/support_contract.rds');ref<-readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
metrics<-list();boots<-list();slices<-list();cal<-list();integrated<-list();integratedboot<-list();supports<-list();tails<-list();sens<-list()
for(year in 2024:2025){
 message('Evaluate complete swing path ',year);raw<-hr_read(year)
 old<-as.data.table(readRDS(sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',year)));ix<-match(s_key(raw),s_key(old));stopifnot(!anyNA(ix),nrow(raw)==nrow(old));old<-old[ix]
 out<-raw[,c(CT_KEYS,'game_date','batter','batter_name','description'),with=FALSE]
 out[,`:=`(q_take=old$q_take_basic,p_swing_reference=old$p_swing_reference,original_supported=old$primary_supported,updated_range_failure=FALSE,
  min_cell_n=pmin(old$contact_training_cell_n,old$post_training_cell_n,old$bip_training_cell_n,old$take_training_cell_n),action=ifelse(description%chin%CT_SWINGS,'swing','take'))]
 p<-list()
 for(stage in HR_STAGES){
  message('Predict all pitches: ',stage);base<-s_base(stage);f<-hr_features(raw,base,h$contact,h$power)
  gp<-base$model$family$linkinv(hr_eta(base$model,f))
  if(stage=='contact'){xp<-x_predict(cm,raw,contact)}else{bb<-readRDS(file.path(SM,paste0(stage,'.rds')));xp<-s_predict(xgb.load(file.path(SM,paste0(stage,'.ubj'))),raw,bb,h)}
  stopifnot(all(is.finite(gp)),all(is.finite(xp)),all(gp>=0),all(xp>=0));if(stage!='value')stopifnot(all(gp<=1&xp<=1))
  p[[stage]]<-list(gam=gp,xgboost=xp)
  for(arm in c('gam','xgboost'))out[,(paste(stage,arm,sep='_')):=p[[stage]][[arm]]]
  modeled<-intersect(names(base$ranges),all.vars(formula(base$model)))
  out[,updated_range_failure:=updated_range_failure|vd_range_flags(f,list(ranges=base$ranges[modeled]))]
  d<-s_data(raw,stage,api,re);j<-match(s_key(d),s_key(raw));stopifnot(!anyNA(j),!anyDuplicated(j))
  factual<-d[,c(CT_KEYS,'game_date','batter','y'),with=FALSE]
  factual[,`:=`(gam=gp[j],xgboost=xp[j],count=as.character(f$count[j]),family=as.character(f$family[j]),region=hr_region(raw$zone[j]),
   batter_profile=ifelse(f$batter_unseen[j],'unseen','seen'),pitcher_profile=ifelse(f$pitcher_unseen[j],'unseen','seen'),history_exposure=x_bands(f$batter_history_n[j]))]
  fwrite(factual,file.path(SR,paste0('factual_',stage,'_',year,'.csv.gz')))
  tag<-paste(year,stage,sep='_')
  metrics[[tag]]<-rbindlist(lapply(c('gam','xgboost'),function(arm)cbind(data.table(year=year,stage=stage,arm=arm),s_metrics(factual$y,factual[[arm]],stage))),fill=TRUE)
  boots[[tag]]<-s_boot(factual$game_pk,factual$y,factual$xgboost,factual$gam,stage)[,`:=`(year=year,stage=stage)]
  slices[[tag]]<-rbindlist(lapply(c('count','family','region','batter_profile','pitcher_profile','history_exposure'),function(dim)rbindlist(lapply(c('gam','xgboost'),function(arm){
   z<-data.table(slice=factual[[dim]],y=factual$y,p=factual[[arm]])
   ans<-if(stage=='value')z[,.(n=.N,rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),bias=mean(p-y)),by=slice]else z[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),bias=mean(p-y)),by=slice]
   ans[,`:=`(year=year,stage=stage,arm=arm,dimension=dim)]
  }))),fill=TRUE)
  if(stage!='value')cal[[tag]]<-rbindlist(lapply(c('gam','xgboost'),function(arm)data.table(y=factual$y,p=factual[[arm]])[,bin:=if(stage=='tip')pmin(199L,floor(p*200))else pmin(19L,floor(p*20))][,.(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,`:=`(year=year,stage=stage,arm=arm)]))
  if(stage=='value'){out[,factual_target:=NA_real_];out[j,factual_target:=d$y]}
  rm(base,f,d,factual);gc(FALSE)
 }
 out[,gam_supported:=original_supported&!updated_range_failure&value_gam>=contract$bip_target_range[1]&value_gam<=contract$bip_target_range[2]]
 out[,primary_supported:=gam_supported&value_xgboost>=contract$bip_target_range[1]&value_xgboost<=contract$bip_target_range[2]]
 whiff_ix<-which(out$description%chin%c('swinging_strike','swinging_strike_blocked'));out[whiff_ix,factual_target:=old$q_whiff[whiff_ix]]
 foul_ix<-which(out$description=='foul');out[foul_ix,factual_target:=old$q_ordinary_foul[foul_ix]]
 tip_ix<-which(out$description=='foul_tip');out[tip_ix,factual_target:=old$q_foul_tip[tip_ix]]
 out[,observed_branch:=fcase(description%chin%c('swinging_strike','swinging_strike_blocked'),'whiff',description=='hit_into_play','bip',description=='foul','foul',description=='foul_tip','tip',default=NA_character_)]
 for(arm in SARMS){
  src<-if(arm=='xgboost')'xgboost'else'gam';cp<-if(arm=='gam')p$contact$gam else p$contact$xgboost
  br<-s_branches(cp,p$bip[[src]],p$tip[[src]],p$value[[src]],old)
  out[,(paste0('q_swing_',arm)):=br$q_swing]
  out[,(paste0('score_',arm)):=(as.integer(description%chin%CT_SWINGS)-p_swing_reference)*(br$q_swing-q_take)]
  br<-cbind(raw[,..CT_KEYS],br);fwrite(br,file.path(SR,paste0('branches_',arm,'_',year,'.csv.gz')))
  obs<-which(out$action=='swing');selected<-as.matrix(br[obs,.(whiff,bip,foul,tip)])[cbind(seq_along(obs),match(out$observed_branch[obs],c('whiff','bip','foul','tip')))]
  ll<-rep(NA_real_,nrow(out));ll[obs]<--log(pmax(1e-15,selected));out[,(paste0('branch_loss_',arm)):=ll]
 }
 fwrite(out,file.path(SR,paste0('predictions_all_',year,'.csv.gz')))
 for(pop in c('all_factual_swings','gam_supported','common_supported')){
  keep<-out$action=='swing'&is.finite(out$factual_target)
  if(pop=='gam_supported')keep<-keep&out$gam_supported
  if(pop=='common_supported')keep<-keep&out$primary_supported
  z<-out[keep]
  for(arm in SARMS){integrated[[paste(year,pop,arm)]]<-cbind(data.table(year=year,population=pop,arm=arm),s_metrics(z$factual_target,z[[paste0('q_swing_',arm)]],'value'),data.table(branch_log_loss=mean(z[[paste0('branch_loss_',arm)]])))}
  for(reference in c('gam','contact_only'))integratedboot[[paste(year,pop,reference)]]<-s_boot(z$game_pk,z$factual_target,z$q_swing_xgboost,z[[paste0('q_swing_',reference)]],'value')[,`:=`(year=year,population=pop,reference=reference)]
 }
 cov<-out[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=.(batter,batter_name)]
 games<-out[primary_supported==TRUE,.(n=.N,gam=sum(score_gam),contact_only=sum(score_contact_only),xgboost=sum(score_xgboost)),by=.(batter,game_pk)]
 set.seed(XSEED)
 hit<-games[,{
  nn<-.N;draws<-replicate(500,{i<-sample.int(nn,nn,TRUE);c(100*sum(xgboost[i]-gam[i])/sum(n[i]),100*sum(xgboost[i]-contact_only[i])/sum(n[i]))})
  .(games=nn,gam_runs100=100*sum(gam)/sum(n),contact_only_runs100=100*sum(contact_only)/sum(n),xgboost_runs100=100*sum(xgboost)/sum(n),
   vs_gam_low=unname(quantile(draws[1,],.025)),vs_gam_high=unname(quantile(draws[1,],.975)),vs_contact_low=unname(quantile(draws[2,],.025)),vs_contact_high=unname(quantile(draws[2,],.975)))
 },by=batter]
 hit<-merge(cov,hit,by='batter',all.x=TRUE);hit[,qualified:=!is.na(games)&supported>=500&games>=60&coverage>=.7]
 for(arm in SARMS)hit[,(paste0(arm,'_decision_plus')):=100+10*(get(paste0(arm,'_runs100'))-ref$center)/ref$scale]
 hit[,`:=`(delta_plus=xgboost_decision_plus-gam_decision_plus,delta_vs_contact=xgboost_decision_plus-contact_only_decision_plus)]
 fwrite(hit,file.path(SR,paste0('hitters_',year,'.csv')));fwrite(games,file.path(SR,paste0('hitter_games_',year,'.csv.gz')))
 fwrite(hit[qualified==TRUE][order(-abs(delta_plus))][1:20],file.path(SR,paste0('largest_movements_',year,'.csv')))
 supports[[as.character(year)]]<-data.table(year=year,eligible=nrow(out),original_supported=sum(out$original_supported),gam_supported=sum(out$gam_supported),common_supported=sum(out$primary_supported),qualified=sum(hit$qualified),coverage=mean(out$primary_supported))
 tails[[as.character(year)]]<-out[,.(n=.N,mean_delta_q=mean(q_swing_xgboost-q_swing_gam),p99_abs_delta_q=quantile(abs(q_swing_xgboost-q_swing_gam),.99),max_abs_delta_q=max(abs(q_swing_xgboost-q_swing_gam)),change_runs100=100*mean(score_xgboost-score_gam)),by=.(action,primary_supported,gam_supported)][,year:=year]
 policies<-list(base=out$primary_supported,cells_50=out$primary_supported&out$min_cell_n>=50,overlap_10=out$primary_supported&out$p_swing_reference>=.1&out$p_swing_reference<=.9,joint=out$primary_supported&out$min_cell_n>=50&out$p_swing_reference>=.1&out$p_swing_reference<=.9)
 sensitivity_hit<-list()
 for(policy in names(policies)){
  z<-out[policies[[policy]]];hh<-z[,.(n=.N,games=uniqueN(game_pk),delta_plus=100*10*mean(score_xgboost-score_gam)/ref$scale),by=batter]
  j<-match(hh$batter,hit$batter);hh[,`:=`(base_qualified=hit$qualified[j],base_delta_plus=hit$delta_plus[j],policy=policy)]
  common<-hh[base_qualified==TRUE&n>=500&games>=60]
  sensitivity_hit[[policy]]<-hh
  sens[[paste(year,policy)]]<-data.table(year=year,policy=policy,supported=nrow(z),baseline_hitters_with_exposure=nrow(common),median_abs_change_from_base=median(abs(common$delta_plus-common$base_delta_plus)),max_abs_change_from_base=if(nrow(common))max(abs(common$delta_plus-common$base_delta_plus))else NA_real_)
 }
 fwrite(rbindlist(sensitivity_hit),file.path(SR,paste0('sensitivity_hitters_',year,'.csv')))
 rm(raw,old,out,p);gc(FALSE)
}
for(pair in list(c('metrics', 'component_metrics.csv'),c('boots','component_bootstrap.csv'),c('slices','slices.csv'),c('cal','calibration.csv'),c('integrated','integrated_metrics.csv'),c('integratedboot','integrated_bootstrap.csv'),c('supports','support.csv'),c('tails','tails.csv'),c('sens','sensitivity.csv')))fwrite(rbindlist(get(pair[1]),fill=TRUE),file.path(SR,pair[2]))
x_check_hash(freeze$model_hashes);x_check_hash(fread(file.path(SR,'input_hashes.csv')))
saveRDS(list(activated=FALSE,code_hashes=x_hash(file.path(SROOT,c('functions.R','evaluate.R'))),completed=as.character(Sys.time())),file.path(SR,'evaluation_manifest.rds'))
message('COMPLETE SWING EVALUATION DONE')
