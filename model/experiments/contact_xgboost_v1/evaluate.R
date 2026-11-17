source('experiments/contact_xgboost_v1/functions.R')
freeze<-readRDS(file.path(XM,'frozen.rds'));x_check_hash(freeze$model_hashes)
bundle<-readRDS(file.path(XM,'bundle.rds'));x_check_hash(bundle$code_hashes)
x_check_hash(fread(file.path(XR,'input_hashes.csv')))
model<-xgb.load(file.path(XM,'contact.ubj'))
gam<-readRDS('data/models/contact_personnel_comparison/updated_profiles.rds');gam$profiles<-bundle$profiles
active<-readRDS('data/models/contact/contact_gam_fit_2022_2023.rds')
counts<-rbindlist(lapply(2022:2023,function(year)ct_read(year,XR)[,.(n=.N),by=batter]))[,.(training_n=sum(n)),by=batter]
stopifnot(sum(counts$training_n)==bundle$train_n)
metrics<-list();boot<-list();slices<-list();cal<-list()
for(year in 2024:2025){
 message('Frozen comparison: ',year);d<-ct_read(year,XR)
 f<-x_features(d,bundle$prep,bundle$profiles,bundle$schema$features)
 preds<-list(gam_prior_season=cp_predict_updated(gam,d),xgboost_prior_season=x_predict(model,d,bundle),gam_frozen_2021=predict_contact(active,d)$p_contact_given_swing)
 out<-d[,c(CT_KEYS,'game_date','batter','pitcher','y'),with=FALSE]
 for(arm in names(preds))out[,(arm):=preds[[arm]]]
 exposure<-counts$training_n[match(d$batter,counts$batter)];exposure[is.na(exposure)]<-0L
 out[,`:=`(count=as.character(f$count),family=as.character(f$family),stand=as.character(f$stand),
  batter_prior_profile=ifelse(f$batter_unseen,'unseen','seen'),pitcher_prior_profile=ifelse(f$pitcher_unseen,'unseen','seen'),
  training_exposure=x_bands(exposure))]
 fwrite(out,file.path(XR,paste0('predictions_',year,'.csv.gz')))
 metrics[[as.character(year)]]<-rbindlist(lapply(names(preds),function(arm)cbind(data.table(year=year,model=arm),cs_metrics(d$y,preds[[arm]]))))
 boot[[as.character(year)]]<-rbindlist(lapply(c('gam_prior_season','gam_frozen_2021'),function(reference)x_boot(out,'xgboost_prior_season',reference)))[,year:=year]
 dims<-c('count','family','stand','batter_prior_profile','pitcher_prior_profile','training_exposure')
 slices[[as.character(year)]]<-rbindlist(lapply(dims,function(dim)rbindlist(lapply(names(preds),function(arm){
  z<-data.table(slice=out[[dim]],y=d$y,p=preds[[arm]])
  z[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),bias=mean(p-y)),by=slice][,`:=`(year=year,dimension=dim,model=arm)]
 }))))
 cal[[as.character(year)]]<-rbindlist(lapply(names(preds),function(arm)data.table(y=d$y,p=preds[[arm]])[,bin:=pmin(19L,floor(p*20))][,.(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,`:=`(year=year,model=arm)]))
 rm(d,f,out,preds);gc(FALSE)
}
fwrite(rbindlist(metrics),file.path(XR,'metrics.csv'));fwrite(rbindlist(boot),file.path(XR,'paired_game_bootstrap.csv'))
fwrite(rbindlist(slices),file.path(XR,'slices.csv'));fwrite(rbindlist(cal),file.path(XR,'calibration.csv'))
x_check_hash(freeze$model_hashes);x_check_hash(fread(file.path(XR,'input_hashes.csv')))
saveRDS(list(frozen_model_hashes=freeze$model_hashes,evaluation_code=x_hash(file.path(XROOT,c('evaluate.R','functions.R'))),
 finished=as.character(Sys.time()),evaluation_years=2024:2025,activated=FALSE),file.path(XR,'evaluation_manifest.rds'))
message('EVALUATION COMPLETE')
