source('experiments/hitter_surface_v1/surface.R')
hs_train <- function() {
  dir.create(HR_REPORT,recursive=TRUE,showWarnings=FALSE);dir.create(HR_MODELS,recursive=TRUE,showWarnings=FALSE)
  oldhash<-fread('experiments/hitter_region_v1/reports/input_hashes.csv')
  paths<-unique(c(oldhash$path,list.files('experiments/hitter_region_v1',recursive=TRUE,full.names=TRUE)))
  hashes<-vd_hash(paths);vd_check_hash(hashes)
  hp<-file.path(HR_REPORT,'input_hashes.csv')
  if(file.exists(hp))vd_check_hash(fread(hp)) else fwrite(hashes,hp)
  histories<-readRDS(file.path(HS_OLD,'histories.rds'))
  stopifnot(isTRUE(all.equal(histories$contact,setNames(lapply(2021:2023,cp_profiles),2021:2023))),
    isTRUE(all.equal(histories$power,setNames(lapply(2021:2023,hr_power),2021:2023))))
  saveRDS(histories,file.path(HR_MODELS,'histories.rds'))
  train<-NULL
  for(stage in c('contact','bip')) {
    path<-file.path(HR_MODELS,paste0(stage,'.rds'));if(file.exists(path))next
    if(is.null(train))train<-rbindlist(lapply(2022:2023,hr_read))
    b<-readRDS(file.path(HS_OLD,paste0(stage,'.rds')))
    d<-hr_stage_data(train,stage,NULL,NULL);stopifnot(nrow(d)==b$n,max(d$game_date)<as.Date('2024-01-01'))
    f<-hr_features(d,b,histories$contact,histories$power);eta<-hr_eta(b$model,f)
    basis<-hs_basis(f$x,f$z);stopifnot(max(abs(crossprod(basis$X)/nrow(f)-diag(6)))<1e-8)
    message('Fit ',stage,': ',nrow(d),' rows; ',uniqueN(d$batter),' hitters; ',6*uniqueN(d$batter),' coefficients')
    start<-proc.time()[3];b$surface<-hs_pool(d$y,eta,basis$X,d$batter);b$surface$seconds<-proc.time()[3]-start;b$basis<-basis$prep
    b$surface_code_hash<-vd_hash(file.path(HS_ROOT,c('surface.R','PROTOCOL.md')))
    b$training_keys_hash<-digest::digest(d[,..CT_KEYS]);saveRDS(b,path,compress=FALSE)
    fwrite(d[,.(n=.N,history_year=unique(season)-1L,min_date=min(game_date),max_date=max(game_date)),by=season],file.path(HR_REPORT,paste0('history_chronology_',stage,'.csv')))
    fwrite(cbind(b$surface$diagnostics,as.data.table(b$surface$effects)),file.path(HR_REPORT,paste0('effects_',stage,'.csv')))
    message('Saved ',stage,': tau=',signif(b$surface$tau,6),' edf=',round(b$surface$edf,2),' seconds=',round(b$surface$seconds))
    rm(d,f,b,eta,basis);gc(FALSE)
  }
  vd_check_hash(hashes)
}
hs_evaluate <- function(d,b,histories) {
  p<-hs_predict(b,d,histories)
  out<-d[,c(CT_KEYS,'game_date','batter','pitcher','zone','y'),with=FALSE]
  for(arm in HS_ARMS)out[,(arm):=p[[arm]]]
  out[,`:=`(region=hr_region(zone),history=ifelse(p$features$batter_unseen,'unavailable','available'),
    pitcher_history=ifelse(p$features$pitcher_unseen,'unavailable','available'),
    training_exposure=p$categorical$n,exposure_band=hs_band(p$categorical$n),
    hitter_training_exposure=p$smooth$n,hitter_exposure_band=hs_band(p$smooth$n),
    fallback=p$smooth$fallback,categorical_fallback=p$categorical$fallback)]
  fwrite(out,file.path(HR_REPORT,paste0('predictions_',b$stage,'_2024.csv.gz')))
  metrics<-rbindlist(lapply(HS_ARMS,function(arm)cbind(data.table(stage=b$stage,arm=arm),hr_metrics(out$y,out[[arm]],b$stage))))
  dims<-c('region','history','pitcher_history','exposure_band','hitter_exposure_band','fallback','categorical_fallback')
  slices<-rbindlist(lapply(dims,function(dim)rbindlist(lapply(HS_ARMS,function(arm){
    z<-data.table(slice=out[[dim]],y=out$y,p=out[[arm]])
    z[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),mean_error=mean(p-y)),by=slice][,`:=`(stage=b$stage,arm=arm,dimension=dim)]
  }))))
  cal<-rbindlist(lapply(HS_ARMS,function(arm)data.table(y=out$y,p=out[[arm]])[,bin:=pmin(19L,floor(p*20))][,
    .(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,`:=`(stage=b$stage,arm=arm)]))
  boot<-rbindlist(lapply(list(c('regional','control'),c('continuous','control'),c('continuous','regional')),function(pair){
    z<-out[,.(game_pk,y,control=get(pair[2]),regional=get(pair[1]))]
    hr_boot(z,b$stage)[,`:=`(candidate=pair[1],reference=pair[2],uncertainty='conditional_game_sampling')]
  }))
  # Real-row prediction contract: labels, row order, batches and serialization.
  ix<-unique(round(seq(1,nrow(d),length.out=60)));dd<-copy(d[ix]);dd[,`:=`(y=-999,description='corrupted',launch_speed=999,launch_angle=999,estimated_ba_using_speedangle=999,estimated_woba_using_speedangle=999)]
  reload<-readRDS(file.path(HR_MODELS,paste0(b$stage,'.rds')));pp<-hs_predict(reload,dd,histories)
  rr<-hs_predict(reload,dd[nrow(dd):1],histories)
  chunks<-lapply(split(seq_len(nrow(dd)),ceiling(seq_len(nrow(dd))/7)),function(i)hs_predict(reload,dd[i],histories))
  for(arm in HS_ARMS)stopifnot(max(abs(pp[[arm]]-out[[arm]][ix]))<1e-9,max(abs(rev(rr[[arm]])-pp[[arm]]))<1e-9,
    max(abs(unlist(lapply(chunks,`[[`,arm),use.names=FALSE)-pp[[arm]]))<1e-9,all(is.finite(out[[arm]])),all(out[[arm]]>=0&out[[arm]]<=1))
  unseen<-copy(dd);unseen[,batter:=-999L];u<-hs_predict(reload,unseen,histories);stopifnot(identical(u$control,u$continuous))
  invalid<-copy(dd);invalid[,plate_x:=NA_real_];stopifnot(inherits(try(hs_predict(reload,invalid,histories),silent=TRUE),'try-error'))
  list(metrics=metrics,slices=slices,calibration=cal,bootstrap=boot)
}
hs_scores <- function(val,histories) {
  old<-as.data.table(readRDS('data/models/hitter_stat_v6/decision_values_2024.rds'))
  k<-match(dv_key(val),dv_key(old));stopifnot(!anyNA(k),!anyDuplicated(k),nrow(val)==nrow(old));old<-old[k]
  out<-val[,c(CT_KEYS,'batter','batter_name','zone','description'),with=FALSE]
  out[,`:=`(q_take=old$q_take_basic,p_swing_reference=old$p_swing_reference,original_supported=old$primary_supported,updated_range_failure=FALSE)]
  p<-list();fallback<-list()
  for(stage in HR_STAGES){
    b<-readRDS(file.path(if(stage%in%c('contact','bip'))HR_MODELS else HS_OLD,paste0(stage,'.rds')))
    f<-hr_features(val,b,histories$contact,histories$power)
    if(stage%in%c('contact','bip')){
      pp<-hs_predict(b,val,histories);p[[stage]]<-pp[HS_ARMS]
      fallback[[stage]]<-data.table(fallback=pp$smooth$fallback)[,.(n=.N),by=fallback][,stage:=stage]
    }else{a<-b$model$family$linkinv(hr_eta(b$model,f));p[[stage]]<-setNames(rep(list(a),3),HS_ARMS)}
    modeled<-intersect(names(b$ranges),all.vars(formula(b$model)))
    out[,updated_range_failure:=updated_range_failure|vd_range_flags(f,list(ranges=b$ranges[modeled]))]
    rm(b,f);gc(FALSE)
  }
  contract<-readRDS('data/models/decision_validation/support_contract.rds')
  out[,primary_supported:=original_supported & !updated_range_failure & p$value$control>=contract$bip_target_range[1] & p$value$control<=contract$bip_target_range[2]]
  for(arm in HS_ARMS){
    c<-p$contact[[arm]];b<-p$bip[[arm]];t<-p$tip[[arm]];q<-p$value[[arm]]
    mat<-cbind(whiff=1-c,bip=c*b,foul=c*(1-b)*(1-t),tip=c*(1-b)*t)
    stopifnot(all(is.finite(mat)),all(mat>=0&mat<=1),max(abs(rowSums(mat)-1))<1e-12,all(is.finite(q)))
    swing<-mat[,'whiff']*old$q_whiff+mat[,'bip']*q+mat[,'foul']*old$q_ordinary_foul+mat[,'tip']*old$q_foul_tip
    out[,(paste0('q_swing_',arm)):=swing];out[,(paste0('score_',arm)):=(as.integer(description%chin%CT_SWINGS)-p_swing_reference)*(swing-q_take)]
    br<-cbind(val[,..CT_KEYS],as.data.table(mat));br[,`:=`(q_bip=q,p_tip_given_nonbip=t,q_swing=swing)]
    fwrite(br,file.path(HR_REPORT,paste0('branch_predictions_',arm,'_2024.csv.gz')))
  }
  fwrite(out,file.path(HR_REPORT,'decision_predictions_2024.csv.gz'));fwrite(rbindlist(fallback),file.path(HR_REPORT,'scoring_fallback_2024.csv'))
  coverage<-out[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=.(batter,batter_name)]
  games<-out[primary_supported==TRUE,.(n=.N,control=sum(score_control),regional=sum(score_regional),continuous=sum(score_continuous)),by=.(batter,game_pk)]
  ref<-readRDS('data/models/hitter_stat_v6/season_reference_2024.rds');set.seed(20261103)
  hitters<-games[,{
    N<-.N;draws<-replicate(500,{i<-sample.int(N,N,TRUE);c(100*sum(continuous[i]-control[i])/sum(n[i]),100*sum(continuous[i]-regional[i])/sum(n[i]))})
    .(games=N,control_runs100=100*sum(control)/sum(n),regional_runs100=100*sum(regional)/sum(n),continuous_runs100=100*sum(continuous)/sum(n),
      vs_control_low=quantile(draws[1,],.025),vs_control_high=quantile(draws[1,],.975),vs_regional_low=quantile(draws[2,],.025),vs_regional_high=quantile(draws[2,],.975))
  },by=batter]
  hitters<-merge(coverage,hitters,by='batter',all.x=TRUE);hitters[,qualified:=supported>=500&games>=60&coverage>=.7]
  for(arm in HS_ARMS)hitters[,(paste0(arm,'_decision_plus')):=100+10*(get(paste0(arm,'_runs100'))-ref$center)/ref$scale]
  hitters[,uncertainty:='conditional_game_sampling_no_model_refits']
  fwrite(hitters,file.path(HR_REPORT,'hitter_changes_2024.csv'));fwrite(games,file.path(HR_REPORT,'hitter_game_scores_2024.csv.gz'))
  summary<-rbindlist(lapply(c('control','regional'),function(reference){
    z<-copy(out);z[,`:=`(dq=q_swing_continuous-get(paste0('q_swing_',reference)),ds=score_continuous-get(paste0('score_',reference)))]
    z[,.(n=.N,mean_delta_q=mean(dq),mean_absolute_delta_q=mean(abs(dq)),max_absolute_delta_q=max(abs(dq)),change_runs100=100*mean(ds)),by=.(primary_supported,action=ifelse(description%chin%CT_SWINGS,'swing','take'))][,reference:=reference]
  }))
  fwrite(summary,file.path(HR_REPORT,'swing_value_changes_2024.csv'))
  fwrite(data.table(original_supported=sum(out$original_supported),common_supported=sum(out$primary_supported),additional_exclusions=sum(out$original_supported&!out$primary_supported),eligible=nrow(out),qualified=sum(hitters$qualified,na.rm=TRUE),reference_center=ref$center,reference_scale=ref$scale),file.path(HR_REPORT,'score_support_2024.csv'))
}
hs_run <- function(){
  hs_train();histories<-readRDS(file.path(HR_MODELS,'histories.rds'))
  message('Models frozen. Evaluate exploratory 2024.');val<-hr_read(2024);results<-list();diagnostics<-list()
  for(stage in c('contact','bip')){
    b<-readRDS(file.path(HR_MODELS,paste0(stage,'.rds')));vd_check_hash(b$surface_code_hash)
    message('Evaluate ',stage);d<-hr_stage_data(val,stage,NULL,NULL);results[[stage]]<-hs_evaluate(d,b,histories)
    s<-b$surface;diagnostics[[stage]]<-data.table(stage=stage,n=b$n,hitters=nrow(s$effects),coefficients=s$coefficient_count,tau=s$tau,variance=s$variance,edf=s$edf,max_gradient=s$max_gradient,converged=s$converged,seconds=s$seconds,sparse_design_MiB=s$sparse_design_bytes/1024^2,categorical_tau=b$pool$tau,categorical_edf=sum(b$pool$effects$information_fraction))
  }
  for(nm in c('metrics','slices','calibration','bootstrap'))fwrite(rbindlist(lapply(results,`[[`,nm)),file.path(HR_REPORT,paste0(nm,'_2024.csv')))
  fwrite(rbindlist(diagnostics),file.path(HR_REPORT,'fit_diagnostics.csv'))
  message('Score identical support cohort');hs_scores(val,histories)
  vd_check_hash(fread(file.path(HR_REPORT,'input_hashes.csv')))
  capture.output(sessionInfo(),file=file.path(HR_REPORT,'sessionInfo.txt'))
  saveRDS(list(version='hitter_surface_v1',activated=FALSE,training=2022:2023,evaluation='previously_inspected_exploratory_2024',code_hashes=vd_hash(file.path(HS_ROOT,c('surface.R','run.R','PROTOCOL.md')))),file.path(HR_MODELS,'manifest.rds'))
  message('DONE')
}
if(sys.nframe()==0L)hs_run()
