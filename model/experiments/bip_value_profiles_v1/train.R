source('experiments/bip_value_profiles_v1/functions.R')
stopifnot(!file.exists(file.path(BM,'frozen.rds')))
dir.create(BM,recursive=TRUE,showWarnings=FALSE);dir.create(BR,recursive=TRUE,showWarnings=FALSE)
inputs <- unique(c(list.files('.',pattern='[.]R$',full.names=TRUE),
  'data/models/post_contact/bip_value_fit_2022_2023.rds','data/models/current_hitter_stat.rds',
  'data/models/hitter_stat_xgboost/manifest.rds','data/models/hitter_stat_v6/season_reference_2024.rds',
  sprintf('data/models/hitter_stat_xgboost/decision_values_%d.rds',2024:2025),
  sprintf('data/processed/statcast_%d_clean.rds',2021:2025),
  unname(CT_PROFILE_FILES),unname(PC_POWER_FILES),
  unlist(lapply(c('batter','pitcher'),function(role)sprintf('data/processed/%s_splits_%d.csv',role,2021:2024))),
  read_re_baseline()$files$file,'reports/post_contact/bip_post_states.csv',
  'reports/decision_validation/bip_post_states_2025.csv'))
stopifnot(all(file.exists(inputs)))
hashes <- bp_hash(inputs);bp_check_hash(hashes);bp_write(hashes,'input_hashes')
active <- readRDS('data/models/current_hitter_stat.rds')
stopifnot(active$version=='decision_plus_xgboost_v7')
bp_check_hash(active$artifacts)
profiles <- bp_profiles();saveRDS(profiles,file.path(BM,'profiles.rds'),compress=FALSE)
raw <- rbindlist(lapply(2022:2023,bp_read),use.names=TRUE,fill=TRUE)
d <- bp_targets(raw);rm(raw);gc(FALSE)
original <- readRDS('data/models/post_contact/bip_value_fit_2022_2023.rds')
stopifnot(nrow(d)==original$n,max(abs(d$q_bip_observed-original$model$model$y))<1e-12)
f <- pc_transform(d,original$prep)$data
for(nm in setdiff(all.vars(as.formula(original$formula)),'y'))
  stopifnot(isTRUE(all.equal(f[[nm]],original$model$model[[nm]],check.attributes=FALSE)))
saveRDS(d[,c(CT_KEYS,'season','game_date','q_bip_observed','runs_on_play','re_after','target_status'),with=FALSE],
  file.path(BM,'training_cohort.rds'),compress=FALSE)
parity <- list()
for(arm in c('A','B','C')) {
  message('Fit arm ',arm,' on ',nrow(d),' identical verified BIP')
  fit <- bp_fit(d,original$prep,profiles,arm)
  fit$arm <- arm
  ff <- bp_features(d,fit$prep,profiles,arm)
  vars <- setdiff(all.vars(as.formula(fit$formula)),'y')
  fit$ranges <- lapply(ff[,intersect(vars,names(ff)[vapply(ff,is.numeric,logical(1))]),with=FALSE],range)
  saveRDS(fit,file.path(BM,paste0(arm,'.rds')),compress=FALSE)
  parity[[arm]] <- data.table(arm,n=fit$n,converged=fit$model$converged,
    edf=sum(fit$model$edf),formula=fit$formula,warnings=paste(fit$warnings,collapse=';'))
  if(arm=='A') {
    err <- max(abs(bp_predict(fit,d,profiles,arm)-pc_predict_one(original,d)))
    stopifnot(err<1e-8);bp_write(data.table(max_prediction_error=err,
      max_coefficient_error=max(abs(coef(fit$model)-coef(original$model)))),'baseline_refit_parity')
  }
  rm(fit,ff);gc(FALSE)
}
bp_write(rbindlist(parity),'fits')
bp_check_hash(hashes)
code <- bp_hash(c(file.path(BP,c('functions.R','train.R','PROTOCOL.md'))))
saveRDS(list(activated=FALSE,training=2022:2023,specification='fixed_no_selection',
  frozen_at=as.character(Sys.time()),inputs=hashes,code=code,
  models=bp_hash(list.files(BM,pattern='[.]rds$',full.names=TRUE))),file.path(BM,'frozen.rds'))
capture.output(sessionInfo(),file=file.path(BR,'sessionInfo.txt'))
message('Three models frozen; active inputs and Git unchanged')
