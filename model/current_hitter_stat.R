# Current historical scorer. From model/: Rscript current_hitter_stat.R
source('hitter_stat_xgboost.R')
current_hitter_stat <- function(year=2025L,qualified_only=TRUE) {
  if(!year%in%2024:2025)stop('2026 needs coordinate/zone harmonization; audited years are 2024 and 2025')
  manifest <- readRDS('data/models/current_hitter_stat.rds')
  if(manifest$version!=H7_VERSION)stop('Build the current XGBoost scorer with Rscript current_hitter_stat.R')
  vd_check_hash(manifest$artifacts)
  h <- fread(file.path(manifest$report_directory,sprintf('hitters_%d.csv',year)))
  if(qualified_only)h<-h[qualified==TRUE]
  h[order(-decision_plus)]
}
activate_hitter_xgboost <- function() {
  checks <- fread(file.path(H7_REPORT,'artifact_verification.csv'))
  stopifnot(nrow(checks)>=35L,all(checks$passed))
  manifest <- readRDS(file.path(H7_MODELS,'manifest.rds'));vd_check_hash(manifest$inputs)
  old <- readRDS('data/models/current_hitter_stat.rds')
  if(old$version!=H7_VERSION)cs_save(old,file.path(H7_MODELS,'previous_active.rds'))
  paths <- c('current_hitter_stat.R','hitter_stat_xgboost.R',file.path(H7_MODELS,'manifest.rds'),
    file.path(H7_REPORT,'artifact_verification.csv'),file.path(H7_MODELS,'season_reference_2024.rds'),
    file.path(H7_REPORT,sprintf('hitters_%d.csv',2024:2025)),
    file.path(H7_MODELS,sprintf('decision_values_%d.rds',2024:2025)))
  cs_save(list(version=H7_VERSION,report_directory=H7_REPORT,model_directory=H7_MODELS,
    artifacts=vd_hash(paths),scope='descriptive_historical_2024_2025_conditional_game_sampling',
    components=manifest$components), 'data/models/current_hitter_stat.rds')
}
if(sys.nframe()==0L) {
  if('--baseline'%in%commandArgs(TRUE)) {
    # Historical prerequisites are rebuilt before fitting the isolated comparisons.
    status<-system2(file.path(R.home('bin'),'Rscript'),'current_hitter_stat_v6.R')
    if(status!=0L)stop('Baseline rebuild failed')
  } else {
    h7_run()
    status<-system2(file.path(R.home('bin'),'Rscript'),'tests/verify_hitter_stat_xgboost.R')
    if(status!=0L)stop('Hybrid verification failed; active pointer unchanged')
    activate_hitter_xgboost()
  }
}
