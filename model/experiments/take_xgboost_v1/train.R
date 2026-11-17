source('experiments/take_xgboost_v1/functions.R')
dir.create(TM,recursive=TRUE,showWarnings=FALSE); dir.create(TR,recursive=TRUE,showWarnings=FALSE)
stopifnot(!file.exists(file.path(TM,'backbone_frozen.rds')))
active <- readRDS('data/models/current_hitter_stat.rds')$artifacts
vd_check_hash(active)
inputs <- unique(c(active$path,'data/models/current_hitter_stat.rds',t_base_path(TRUE),
  sprintf('data/processed/statcast_%d_clean.rds',2022:2025),
  'reports/umpire_v5/umpire_assignments.csv','umpire_v5.R','called_strike_v4.R','catcher_framing_v2.R',
  'decision_validation.R','season_average_v3.R','data/models/decision_validation/support_contract.rds',
  read_re_baseline()$files$file,
  sprintf('data/models/umpire_v5/predictions_%d.rds',2024:2025),
  sprintf('data/models/umpire_v5/fit_umpire_region_model_for_%d.rds',2024:2025)))
hashes <- t_hash(inputs); vd_check_hash(hashes); t_write(hashes,'input_hashes')
code <- t_hash(file.path(TROOT,c('functions.R','train.R','fit_actors.R','PROTOCOL.md')))
gam <- readRDS(t_base_path()); dev <- readRDS(t_base_path(TRUE))
stopifnot(identical(gam$train_years,2022:2023),identical(dev$train_years,2022L))
schema <- t_schema(dev); final_schema <- t_schema(gam)
stopifnot(identical(schema$features,final_schema$features),
  !any(c('catcher','umpire','batter','pitcher','y','description','launch_speed','launch_angle')%in%schema$features))
features <- data.table(feature=schema$features,encoding=ifelse(schema$features%in%names(schema$levels),'all factor levels one-hot','unchanged cs_transform numeric'))
t_write(features,'feature_allowlist')
a <- t_read(2022,TRUE); b <- t_read(2023,TRUE)
stopifnot(nrow(a)==dev$train_n, nrow(a)+nrow(b)==gam$train_n, max(a$game_date)<min(b$game_date))
stopifnot(isTRUE(all.equal(cs_preprocessor(a),dev$prep)),
  isTRUE(all.equal(cs_preprocessor(rbindlist(list(a,b))),gam$prep)))
# Exact equality to the actual GAM training frames, including labels and factors.
for(pair in list(list(a,dev),list(rbindlist(list(a,b)),gam))) {
  f <- cs_transform(pair[[1]],pair[[2]]$prep)$data; f[,y:=pair[[1]]$y]
  expected <- as.data.table(pair[[2]]$model$model)
  stopifnot(all(vapply(names(expected),function(n) identical(f[[n]],expected[[n]]),logical(1))))
}
bundle <- list(prep=dev$prep,schema=schema,train_years=2022L,training_keys=a[,..CS_KEYS])
saveRDS(bundle,file.path(TM,'development_bundle.rds'))
da <- xgb.DMatrix(t_matrix(a,dev$prep,schema),label=a$y,nthread=2)
db <- xgb.DMatrix(t_matrix(b,dev$prep,schema),label=b$y,nthread=2)
selection <- list()
for(depth in 3:5) {
  message('Develop depth ',depth); set.seed(TSEED)
  fit <- xgb.train(params=t_params(depth),data=da,nrounds=1200,evals=list(train=da,validation=db),
    callbacks=list(xgb.cb.early.stop(60,maximize=FALSE,metric_name='validation_logloss',save_best=TRUE,verbose=FALSE)),verbose=0)
  curve <- as.data.table(attr(fit,'evaluation_log')); best <- which.min(curve$validation_logloss)
  stopifnot(xgb.get.num.boosted.rounds(fit)==best,attr(fit,'early_stop')$best_iteration==best)
  p <- as.numeric(predict(fit,db))
  stopifnot(abs(mean(cs_loss(b$y,p))-curve$validation_logloss[best])<1e-6)
  t_write(curve,paste0('development_depth_',depth)); xgb.save(fit,file.path(TM,paste0('development_depth_',depth,'.ubj')))
  selection[[as.character(depth)]] <- data.table(depth,best_rounds=best,trained_rounds=nrow(curve),
    cap_reached=nrow(curve)==1200,train_log_loss=curve$train_logloss[best],validation_log_loss=mean(cs_loss(b$y,p)))
  message('Depth ',depth,': ',best,' rounds; validation loss ',mean(cs_loss(b$y,p)))
}
selection <- rbindlist(selection); setorder(selection,validation_log_loss,depth); t_write(selection,'selection')
chosen <- selection[1]
devpath <- file.path(TM,paste0('development_depth_',chosen$depth,'.ubj'))
model <- xgb.load(devpath)
# Development actor offsets are deliberately 2022-only, never final-fit offsets.
development <- list()
for(arm in c('gam','xgboost')) {
  message('Development actor fit ',arm)
  ea <- if(arm=='gam') fr_base(dev,a,se=FALSE)$eta else t_eta(model,a,bundle)
  actor <- t_fit_actor(a,ea,dev$prep,arm,2023L,'development',if(arm=='gam')t_base_path(TRUE)else devpath)
  eb <- if(arm=='gam') fr_base(dev,b,se=FALSE)$eta else t_eta(model,b,bundle)
  d <- t_actor_data(b,eb,dev$prep); pp <- u5_predict(actor,d); online <- u5_online(d,pp$p)
  t_write(online$ledger,paste0('development_weekly_',arm))
  development[[arm]] <- rbindlist(list(cbind(data.table(arm,stage='base'),t_probability_metrics(b$y,plogis(eb))),
    cbind(data.table(arm,stage='complete'),t_probability_metrics(b$y,online$p))))
}
t_write(rbindlist(development),'development_metrics')
train <- rbindlist(list(a,b)); bundle <- list(prep=gam$prep,schema=final_schema,train_years=2022:2023,
  train_end=max(train$game_date),training_keys=train[,..CS_KEYS],selection=chosen)
df <- xgb.DMatrix(t_matrix(train,bundle$prep,bundle$schema),label=train$y,nthread=2)
message('Final refit'); set.seed(TSEED)
fit <- xgb.train(params=t_params(chosen$depth),data=df,nrounds=chosen$best_rounds,verbose=0)
xgb.save(fit,file.path(TM,'take.ubj')); saveRDS(bundle,file.path(TM,'bundle.rds'))
t_write(xgb.importance(model=fit),'feature_importance')
vd_check_hash(hashes); vd_check_hash(code)
saveRDS(list(frozen_at=as.character(Sys.time()),selection=chosen,code_hashes=code,
  model_hashes=t_hash(list.files(TM,full.names=TRUE)),later_seasons_read=FALSE,activated=FALSE),file.path(TM,'backbone_frozen.rds'))
capture.output(sessionInfo(),file=file.path(TR,'sessionInfo.txt'))
message('BACKBONE FROZEN; historical actor fitting may begin')
