source('experiments/take_xgboost_v1/functions.R')
freeze <- readRDS(file.path(TM,'backbone_frozen.rds'))
vd_check_hash(freeze$model_hashes); vd_check_hash(freeze$code_hashes)
vd_check_hash(fread(file.path(TR,'input_hashes.csv')))
stopifnot(!file.exists(file.path(TM,'frozen.rds')))
gam <- readRDS(t_base_path()); bundle <- readRDS(file.path(TM,'bundle.rds'))
model <- xgb.load(file.path(TM,'take.ubj')); rows <- list()
for(year in 2024:2025) {
  d <- t_read(year-1L,TRUE)
  for(arm in c('gam','xgboost')) {
    message('Final ',arm,' actors for ',year,', evidence ',year-1L)
    eta <- if(arm=='gam') fr_base(gam,d,se=FALSE)$eta else t_eta(model,d,bundle)
    fit <- t_fit_actor(d,eta,bundle$prep,arm,year,'final',if(arm=='gam')t_base_path()else file.path(TM,'take.ubj'))
    rows[[paste(year,arm)]] <- data.table(prediction_year=year,history_year=year-1L,arm,
      train_n=fit$train_n,excluded=fit$excluded,train_end=fit$train_end,
      train_loss=fit$quality$training_loss,null_loss=fit$quality$null_loss)
  }
}
t_write(rbindlist(rows),'actor_fits')
vd_check_hash(freeze$model_hashes); vd_check_hash(freeze$code_hashes)
vd_check_hash(fread(file.path(TR,'input_hashes.csv')))
saveRDS(list(frozen_at=as.character(Sys.time()),model_hashes=t_hash(list.files(TM,full.names=TRUE)),
  code_hashes=freeze$code_hashes,selection=freeze$selection,evaluation_metrics_computed=FALSE,
  actor_evidence_note='2024 used only to fit prespecified 2025 actor layer',activated=FALSE),file.path(TM,'frozen.rds'))
message('ALL MODELS FROZEN; later-season evaluation may begin')
