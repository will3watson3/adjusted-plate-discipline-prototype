source('experiments/contact_xgboost_v1/functions.R')
options(error=function(){traceback(8);quit(status=1)})
checks<-list();check<-function(name,ok){if(!isTRUE(ok))stop(name);checks[[name]]<<-data.table(check=name,passed=TRUE)}
x_check_hash(fread(file.path(XR,'input_hashes.csv')));check('all_original_inputs_and_active_artifacts_unchanged',TRUE)
freeze<-readRDS(file.path(XM,'frozen.rds'));bundle<-readRDS(file.path(XM,'bundle.rds'));x_check_hash(freeze$model_hashes);x_check_hash(bundle$code_hashes)
evalmanifest<-readRDS(file.path(XR,'evaluation_manifest.rds'));x_check_hash(evalmanifest$evaluation_code)
check('no_activation_and_frozen_before_evaluation',identical(freeze$activated,FALSE)&&identical(freeze$evaluation_read_before_freeze,FALSE)&&freeze$frozen_at<=evalmanifest$finished)
check('all_profile_years_exact',isTRUE(all.equal(bundle$profiles,setNames(lapply(2021:2024,cp_profiles),2021:2024))))
base<-readRDS('data/models/contact_personnel_comparison/updated_profiles.rds')
check('exact_GAM_feature_allowlist',identical(bundle$schema$features,setdiff(all.vars(formula(base$model)),'y')))
check('same_final_preprocessing',identical(bundle$prep,base$prep))
check('no_outcome_or_identity_predictors',!any(grepl('batter$|pitcher$|zone$|description|launch|estimated_|^y$',bundle$schema$features)))
selection<-fread(file.path(XR,'selection.csv'));setorder(selection,validation_log_loss,depth)
check('small_prespecified_search',setequal(selection$depth,3:5)&&nrow(selection)==3)
check('selection_matches_development_only',identical(as.integer(bundle$selected$depth),as.integer(selection$depth[1]))&&bundle$nrounds==selection$best_rounds[1])
for(depth_value in 3:5){
 z<-fread(file.path(XR,paste0('development_depth_',depth_value,'.csv')))
 # Avoid data.table name masking in the selection row.
 row<-selection[selection$depth==depth_value]
 k<-which.min(z$validation_logloss)
 check(paste('depth',depth_value,'best_round'),row$best_rounds==k&&abs(row$validation_log_loss-z$validation_logloss[k])<1e-6)
 check(paste('depth',depth_value,'bounded_rounds'),nrow(z)<=1200&&nrow(z)>=k)
}
model<-xgb.load(file.path(XM,'contact.ubj'));check('saved_boosting_rounds',xgb.get.num.boosted.rounds(model)==bundle$nrounds)
train<-rbindlist(lapply(2022:2023,ct_read,report_dir=XR));check('same_training_keys_and_count',all(vapply(CT_KEYS,function(nm)identical(train[[nm]],bundle$training_keys[[nm]]),logical(1)))&&nrow(train)==nrow(base$model$model))
check('training_precedes_all_evaluation',max(train$game_date)<as.Date('2024-01-01'))
dev<-readRDS(file.path(XM,'development_preprocessing.rds'));a<-train[season==2022]
expectedprep<-ct_preprocessor(a,bundle$profiles[['2021']]);expectedprep$missing<-union(expectedprep$missing,sub('_missing$','',bundle$schema$features[grepl('_missing$',bundle$schema$features)]))
check('development_preprocessing_2022_only',isTRUE(all.equal(dev$prep,expectedprep,check.attributes=FALSE)))
counts<-train[,.(n=.N),by=batter]
# Independently compare transformed model inputs with the actual GAM training frame.
f<-x_features(train,bundle$prep,bundle$profiles,bundle$schema$features)
for(nm in bundle$schema$features)check(paste('GAM_training_feature',nm),isTRUE(all.equal(f[[nm]],base$model$model[[nm]],check.attributes=FALSE)))
rm(f,a,train);gc(FALSE)
metrics<-fread(file.path(XR,'metrics.csv'));boots<-fread(file.path(XR,'paired_game_bootstrap.csv'));slices<-fread(file.path(XR,'slices.csv'));cal<-fread(file.path(XR,'calibration.csv'))
readpred<-function(year)fread(cmd=paste('gzip -dc',shQuote(file.path(XR,paste0('predictions_',year,'.csv.gz')))))
arms<-c('gam_prior_season','xgboost_prior_season','gam_frozen_2021')
for(yr in 2024:2025){
 d<-ct_read(yr,XR);p<-readpred(yr)
 key<-function(z)do.call(paste,c(z[,..CT_KEYS],sep='|'))
 check(paste(yr,'identical_unique_full_cohort'),identical(key(d),key(p))&&!anyDuplicated(p,by=CT_KEYS)&&identical(p$y,d$y))
 check(paste(yr,'no_training_game_overlap'),!any(p$game_pk%in%bundle$training_keys$game_pk))
 for(arm in arms){
  message('Verify metrics/slices ',yr,' ',arm)
  pp<-p[[arm]];yy<-p$y;check(paste(yr,arm,'finite_probabilities'),all(is.finite(pp)&pp>0&pp<1))
  expected<-cs_metrics(yy,pp);actual<-metrics[year==yr&model==arm]
  for(nm in names(expected))check(paste(yr,arm,nm),max(abs(actual[[nm]]-expected[[nm]]))<1e-8)
  # Explicit independent loss/Brier arithmetic.
  check(paste(yr,arm,'independent_losses'),abs(mean(-yy*log(pp)-(1-yy)*log1p(-pp))-actual$log_loss)<1e-10&&abs(mean((pp-yy)^2)-actual$brier)<1e-10)
  for(dim in unique(slices$dimension)){
   zz<-data.table(slice=p[[dim]],y=yy,p=pp)[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),bias=mean(p-y)),by=slice]
   aa<-slices[year==yr&model==arm&dimension==dim];aa<-aa[match(zz$slice,aa$slice)]
   for(nm in c('n','log_loss','brier','bias'))check(paste(yr,arm,dim,nm),max(abs(aa[[nm]]-zz[[nm]]))<1e-10)
  }
  zz<-data.table(y=yy,p=pp)[,bin:=pmin(19L,floor(p*20))][,.(n=.N,predicted=mean(p),observed=mean(y)),by=bin]
  aa<-cal[year==yr&model==arm];aa<-aa[match(zz$bin,aa$bin)]
  check(paste(yr,arm,'calibration_bins'),all(aa$n==zz$n)&&max(abs(aa$predicted-zz$predicted))<1e-10&&max(abs(aa$observed-zz$observed))<1e-10)
 }
 for(reference_name in c('gam_prior_season','gam_frozen_2021'))for(metric_name in c('log_loss','brier','mean_error')){
  delta<-switch(metric_name,log_loss=cs_loss(p$y,p$xgboost_prior_season)-cs_loss(p$y,p[[reference_name]]),brier=(p$xgboost_prior_season-p$y)^2-(p[[reference_name]]-p$y)^2,mean_error=p$xgboost_prior_season-p[[reference_name]])
  g<-data.table(game=p$game_pk,delta=delta)[,.(n=.N,total=sum(delta)),by=game]
  set.seed(XSEED);draws<-replicate(500,{i<-sample.int(nrow(g),nrow(g),TRUE);sum(g$total[i])/sum(g$n[i])})
  aa<-boots[year==yr&boots$reference==reference_name&boots$metric==metric_name]
  check(paste(yr,reference_name,metric_name,'paired_bootstrap'),nrow(aa)==1&&abs(aa$difference-mean(delta))<1e-10&&max(abs(c(aa$ci_low,aa$ci_high)-quantile(draws,c(.025,.975))))<1e-10)
 }
 ix<-unique(round(seq(1,nrow(d),length.out=100)));dd<-copy(d[ix]);truth<-p$xgboost_prior_season[ix]
 check(paste(yr,'saved_prediction_rebuilt'),max(abs(x_predict(model,dd,bundle)-truth))<1e-7)
 dd[,`:=`(y=1L-y,description='CORRUPTED',launch_speed=999,launch_angle=-999,estimated_ba_using_speedangle=999)]
 check(paste(yr,'same_pitch_labels_and_outcomes_ignored'),max(abs(x_predict(model,dd,bundle)-truth))<1e-7)
 check(paste(yr,'row_order'),max(abs(rev(x_predict(model,dd[nrow(dd):1],bundle))-truth))<1e-7)
 chunks<-split(seq_len(nrow(dd)),ceiling(seq_len(nrow(dd))/13))
 check(paste(yr,'batch_invariance'),max(abs(unlist(lapply(chunks,function(i)x_predict(model,dd[i],bundle)),use.names=FALSE)-truth))<1e-7)
 unseen<-copy(dd);unseen[,`:=`(batter=-999L,pitcher=-998L)]
 ff<-x_features(unseen,bundle$prep,bundle$profiles,bundle$schema$features)
 check(paste(yr,'unseen_history_fallback'),all(ff$batter_unseen&ff$pitcher_unseen)&&all(is.finite(x_predict(model,unseen,bundle))))
 invalid<-copy(dd);invalid[,plate_x:=NA_real_]
 check(paste(yr,'invalid_core_rejected'),inherits(try(x_predict(model,invalid,bundle),silent=TRUE),'try-error'))
 # Independently reproduce both GAMs on the same sampled real rows.
 bb<-base;bb$profiles<-bundle$profiles
 check(paste(yr,'updated_GAM_reproduced'),max(abs(cp_predict_updated(bb,d[ix])-p$gam_prior_season[ix]))<1e-10)
 active<-readRDS('data/models/contact/contact_gam_fit_2022_2023.rds')
 check(paste(yr,'active_GAM_reproduced'),max(abs(predict_contact(active,d[ix])$p_contact_given_swing-p$gam_frozen_2021[ix]))<1e-10)
 expn<-counts$n[match(d$batter,counts$batter)];expn[is.na(expn)]<-0L
 check(paste(yr,'training_exposure_slices'),identical(p$training_exposure,x_bands(expn)))
 rm(d,p);gc(FALSE)
}
x_check_hash(fread(file.path(XR,'input_hashes.csv')));x_check_hash(freeze$model_hashes)
fwrite(rbindlist(checks),file.path(XR,'independent_verification.csv'));cat(length(checks),'independent checks passed\n')
