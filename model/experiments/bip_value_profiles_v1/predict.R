source('experiments/bip_value_profiles_v1/functions.R')
predict_bip_profile <- function(pitches,arm='C') {
  d <- copy(as.data.table(pitches))
  if(!'season'%in%names(d))d[,season:=game_year]
  freeze <- readRDS(file.path(BM,'frozen.rds'));bp_check_hash(freeze$code);bp_check_hash(freeze$models)
  fit <- readRDS(file.path(BM,paste0(arm,'.rds')))
  profiles <- readRDS(file.path(BM,'profiles.rds'))
  data.table(d[,..CT_KEYS],q_bip=bp_predict(fit,d,profiles,arm))
}
