source('experiments/contact_xgboost_v1/functions.R')
stopifnot(!file.exists(file.path(XM,'frozen.rds')))
dir.create(XM,recursive=TRUE,showWarnings=FALSE);dir.create(XR,recursive=TRUE,showWarnings=FALSE)
protected<-c('data/models/current_hitter_stat.rds','data/models/decision_validation/league_policy_2022_2023.rds',
 'data/models/hitter_stat_v6/season_reference_2024.rds','data/models/contact/contact_gam_fit_2022_2023.rds',
 'data/models/contact_personnel_comparison/updated_profiles.rds',
 'experiments/hitter_surface_v1/models/contact.rds')
inputs<-c(protected,sprintf('data/processed/statcast_%d_clean.rds',2022:2025),
 unlist(lapply(c('batter','pitcher'),function(role)sprintf('data/processed/%s_splits_%d.csv',role,2021:2024))),
 'compare_contact_personnel.R','contact_models.R','called_strike_models.R')
stopifnot(all(file.exists(inputs)));hashes<-x_hash(inputs);fwrite(hashes,file.path(XR,'input_hashes.csv'))
codehash<-x_hash(file.path(XROOT,c('functions.R','train.R','PROTOCOL.md')))
baseline<-readRDS('data/models/contact_personnel_comparison/updated_profiles.rds')
features<-setdiff(all.vars(formula(baseline$model)),'y')
stopifnot(!any(c('batter','pitcher','zone','description','y','launch_speed','launch_angle')%in%features))
fwrite(data.table(feature=features),file.path(XR,'feature_allowlist.csv'))
profiles<-setNames(lapply(2021:2024,cp_profiles),2021:2024)
a<-ct_read(2022,XR);b<-ct_read(2023,XR)
stopifnot(max(a$game_date)<min(b$game_date),!any(a$game_pk%in%b$game_pk))
prep<-ct_preprocessor(a,profiles[['2021']]);prep$missing<-union(prep$missing,sub('_missing$','',features[grepl('_missing$',features)]))
schema<-x_schema(features,prep)
fa<-x_features(a,prep,profiles,features);fb<-x_features(b,prep,profiles,features)
dtrain<-xgb.DMatrix(x_matrix(fa,schema),label=a$y,nthread=2);dval<-xgb.DMatrix(x_matrix(fb,schema),label=b$y,nthread=2)
saveRDS(list(prep=prep,schema=schema,training=2022L,validation=2023L),file.path(XM,'development_preprocessing.rds'))
rows<-list()
for(depth in 3:5){
 set.seed(XSEED);message('Develop depth ',depth)
 fit<-xgb.train(params=x_params(depth),data=dtrain,nrounds=1200,evals=list(train=dtrain,validation=dval),
  callbacks=list(xgb.cb.early.stop(60,maximize=FALSE,metric_name='validation_logloss',save_best=TRUE,verbose=FALSE)),verbose=0)
 log<-as.data.table(attr(fit,'evaluation_log'));best<-which.min(log$validation_logloss)
 stopifnot(log$iter[best]==best,attr(fit,'early_stop')$best_iteration==best,xgb.get.num.boosted.rounds(fit)==best)
 pv<-as.numeric(predict(fit,dval));stopifnot(abs(mean(cs_loss(b$y,pv))-log$validation_logloss[best])<1e-6)
 fwrite(log,file.path(XR,paste0('development_depth_',depth,'.csv')))
 rows[[as.character(depth)]]<-data.table(depth=depth,best_rounds=best,trained_rounds=nrow(log),cap_reached=nrow(log)==1200,
  train_log_loss=log$train_logloss[best],validation_log_loss=mean(cs_loss(b$y,pv)),validation_brier=mean((pv-b$y)^2))
 message('depth ',depth,' best rounds ',best,' validation ',rows[[as.character(depth)]]$validation_log_loss)
 rm(fit);gc(FALSE)
}
selection<-rbindlist(rows);setorder(selection,validation_log_loss,depth);fwrite(selection,file.path(XR,'selection.csv'))
chosen<-selection[1];params<-x_params(chosen$depth)
# Refit exactly the same rows and feature values used for the updated-history GAM.
train<-rbindlist(list(a,b));stopifnot(nrow(train)==nrow(baseline$model$model),all(train$season%in%2022:2023))
final_schema<-x_schema(features,baseline$prep);f<-x_features(train,baseline$prep,profiles,features)
dfinal<-xgb.DMatrix(x_matrix(f,final_schema),label=train$y,nthread=2)
rm(dtrain,dval,fa,fb);gc(FALSE)
message('Final refit: depth ',chosen$depth,', rounds ',chosen$best_rounds)
set.seed(XSEED);start<-proc.time()[3]
final<-xgb.train(params=params,data=dfinal,nrounds=chosen$best_rounds,evals=list(train=dfinal),verbose=0)
xgb.save(final,file.path(XM,'contact.ubj'))
bundle<-list(prep=baseline$prep,profiles=profiles,schema=final_schema,params=params,nrounds=chosen$best_rounds,
 train_years=2022:2023,train_end=max(train$game_date),train_n=nrow(train),training_keys=train[,..CT_KEYS],
 training_log_loss=mean(cs_loss(train$y,as.numeric(predict(final,dfinal)))),seconds=proc.time()[3]-start,
 selected=chosen,code_hashes=codehash,activated=FALSE,xgboost_version=as.character(packageVersion('xgboost')))
saveRDS(bundle,file.path(XM,'bundle.rds'),compress=FALSE)
fwrite(train[,.(n=.N,history_year=unique(season)-1L,min_date=min(game_date),max_date=max(game_date)),by=season],file.path(XR,'chronology.csv'))
fwrite(xgb.importance(model=final),file.path(XR,'feature_importance.csv'))
x_check_hash(hashes);x_check_hash(codehash)
saveRDS(list(frozen_at=as.character(Sys.time()),model_hashes=x_hash(file.path(XM,c('contact.ubj','bundle.rds'))),selection=chosen,
 evaluation_read_before_freeze=FALSE,activated=FALSE),file.path(XM,'frozen.rds'))
capture.output(sessionInfo(),file=file.path(XR,'sessionInfo.txt'));message('FROZEN: evaluation may begin')
