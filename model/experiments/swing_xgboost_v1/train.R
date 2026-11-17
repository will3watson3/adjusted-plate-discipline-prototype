source('experiments/swing_xgboost_v1/functions.R')
stopifnot(!file.exists(file.path(SM,'frozen.rds')))
re<-read_re_baseline();api<-s_api()
inputs<-unique(c(list.files('experiments/contact_xgboost_v1/models',full.names=TRUE),
 file.path('experiments/contact_xgboost_v1',c('functions.R','PROTOCOL.md')),file.path('experiments/hitter_region_v1/models',paste0(HR_STAGES,'.rds')),
 re$files$file,'reports/post_contact/RE288_development_train2022.csv','reports/post_contact/bip_post_states.csv','reports/decision_validation/bip_post_states_2025.csv',
 'data/models/current_hitter_stat.rds','data/models/decision_validation/league_policy_2022_2023.rds','data/models/decision_validation/support_contract.rds',
 'data/models/hitter_stat_v6/season_reference_2024.rds',sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',2024:2025),
 sprintf('data/processed/statcast_%d_clean.rds',2022:2025),unlist(lapply(c('batter','pitcher'),function(role)sprintf('data/processed/%s_splits_%d.csv',role,2021:2024))),
 'experiments/hitter_region_v1/run.R','compare_contact_personnel.R','post_contact_models.R','contact_models.R','decision_validation.R','take_value_models.R','re288_order_flags.R','attach_run_expectancy.R','run_expectancy_models.R','called_strike_models.R'))
stopifnot(all(file.exists(inputs)));hash<-x_hash(inputs);fwrite(hash,file.path(SR,'input_hashes.csv'))
code<-x_hash(file.path(SROOT,c('functions.R','train.R','PROTOCOL.md')))
h<-s_histories();saveRDS(h,file.path(SM,'histories.rds'),compress=FALSE)
raw<-rbindlist(lapply(2022:2023,hr_read));devre<-list(re288=fread('reports/post_contact/RE288_development_train2022.csv'),id='train2022_only')
for(stage in SSTAGES){
 target<-file.path(SM,paste0(stage,'.rds'))
 if(file.exists(target)){bb<-readRDS(target);x_check_hash(bb$code_hashes);message('Reuse frozen stage ',stage);next}
 base<-s_base(stage);features<-setdiff(all.vars(formula(base$model)),'y')
 stopifnot(!any(c('batter','pitcher','zone','description','launch_speed','launch_angle','q_bip_observed','y')%in%features))
 ddev<-s_data(raw,stage,api,if(stage=='value')devre else re);a<-ddev[season==2022];v<-ddev[season==2023]
 prep<-ct_preprocessor(a,h$contact[['2021']]);prep$power<-h$power[['2021']];prep$base_out_levels<-base$prep$base_out_levels
 prep$missing<-union(prep$missing,sub('_missing$','',features[grepl('_missing$',features)]))
 db<-list(stage=stage,prep=prep,schema=s_schema(features,prep))
 da<-xgb.DMatrix(s_matrix(s_features(a,db,h),db$schema),label=a$y,nthread=2)
 dv<-xgb.DMatrix(s_matrix(s_features(v,db,h),db$schema),label=v$y,nthread=2)
 saveRDS(db,file.path(SM,paste0(stage,'_development_preprocessing.rds')))
 candidates<-list();metric<-if(stage=='value')'rmse'else'logloss'
 for(depth in 3:5){
  message('Develop ',stage,' depth ',depth);set.seed(XSEED)
  fit<-xgb.train(params=s_params(depth,stage),data=da,nrounds=1200,evals=list(train=da,validation=dv),callbacks=list(xgb.cb.early.stop(60,metric_name=paste0('validation_',metric),save_best=TRUE,verbose=FALSE)),verbose=0)
  log<-as.data.table(attr(fit,'evaluation_log'));k<-which.min(log[[paste0('validation_',metric)]])
  stopifnot(attr(fit,'early_stop')$best_iteration==k,xgb.get.num.boosted.rounds(fit)==k)
  fwrite(log,file.path(SR,paste0('development_',stage,'_',depth,'.csv')))
  candidates[[as.character(depth)]]<-data.table(stage=stage,depth=depth,best_rounds=k,trained_rounds=nrow(log),cap_reached=nrow(log)==1200,validation_loss=log[[paste0('validation_',metric)]][k])
  message(stage,' depth ',depth,' best ',k,' validation ',candidates[[as.character(depth)]]$validation_loss)
  rm(fit);gc(FALSE)
 }
 selection<-rbindlist(candidates);setorder(selection,validation_loss,depth);chosen<-selection[1];fwrite(selection,file.path(SR,paste0('selection_',stage,'.csv')))
 df<-s_data(raw,stage,api,re);stopifnot(nrow(df)==base$n)
 b<-list(stage=stage,prep=base$prep,schema=s_schema(features,base$prep),params=s_params(chosen$depth,stage),selected=chosen,nrounds=chosen$best_rounds,
  train_n=nrow(df),training_keys=df[,..CT_KEYS],training_y=df$y,code_hashes=code,train_end=max(df$game_date))
 ff<-s_features(df,b,h)
 for(nm in features)stopifnot(isTRUE(all.equal(ff[[nm]],base$model$model[[nm]],check.attributes=FALSE)))
 stopifnot(max(abs(df$y-base$model$model$y))<1e-10)
 dm<-xgb.DMatrix(s_matrix(ff,b$schema),label=df$y,nthread=2)
 rm(da,dv,ddev,a,v);gc(FALSE)
 message('Final refit ',stage,' depth ',chosen$depth,' rounds ',chosen$best_rounds);set.seed(XSEED)
 m<-xgb.train(params=b$params,data=dm,nrounds=b$nrounds,verbose=0)
 xgb.save(m,file.path(SM,paste0(stage,'.ubj')));saveRDS(b,target,compress=FALSE)
 fwrite(data.table(stage=stage,feature=features),file.path(SR,paste0('features_',stage,'.csv')))
 rm(m,dm,ff,df,b,base);gc(FALSE)
}
x_check_hash(hash);x_check_hash(code)
saveRDS(list(activated=FALSE,training=2022:2023,frozen_at=as.character(Sys.time()),code_hashes=code,model_hashes=x_hash(c(file.path(SM,c('histories.rds',paste0(SSTAGES,'.rds'),paste0(SSTAGES,'.ubj')))))),file.path(SM,'frozen.rds'))
capture.output(sessionInfo(),file=file.path(SR,'sessionInfo.txt'));message('ALL SWING MODELS FROZEN')
