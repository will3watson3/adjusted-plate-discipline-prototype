# Public local entry point. Run Rscript current_hitter_stat.R to rebuild, or
# source this file and call current_hitter_stat(2025) for qualified hitter rows.
source('hitter_stat_v6.R')
current_hitter_stat <- function(year=2025L,qualified_only=TRUE) {
  if(!year%in%2024:2025) stop('Only audited 2024/2025 artifacts are available; 2026 needs geometry harmonization')
  manifest<-readRDS('data/models/current_hitter_stat.rds')
  vd_check_hash(manifest$artifacts)
  h<-fread(file.path(manifest$report_directory,sprintf('hitters_%d.csv',year)))
  if(qualified_only) h<-h[qualified==TRUE]
  h[order(-decision_plus)]
}
activate_hitter_stat <- function() {
  checks<-fread(file.path(H6_REPORT,'artifact_verification.csv'))
  stopifnot(nrow(checks)>=49L,all(checks$passed))
  manifest<-readRDS(file.path(H6_MODELS,'manifest.rds'));vd_check_hash(manifest$inputs)
  extra<-fread(file.path(H6_REPORT,'additional_input_hashes.csv'));vd_check_hash(extra)
  paths<-c(file.path(H6_MODELS,'manifest.rds'),file.path(H6_REPORT,'artifact_verification.csv'),
    file.path(H6_REPORT,'additional_input_hashes.csv'),extra$path,
    file.path(H6_MODELS,'season_reference_2024.rds'),
    file.path(H6_REPORT,sprintf('hitters_%d.csv',2024:2025)),
    file.path(H6_MODELS,sprintf('decision_values_%d.rds',2024:2025)))
  cs_save(list(version=H6_VERSION,report_directory=H6_REPORT,model_directory=H6_MODELS,
    artifacts=vd_hash(paths),scope=manifest$status), 'data/models/current_hitter_stat.rds')
}
if(sys.nframe()==0L) {
  h6_run()
  result<-system2(file.path(R.home('bin'),'Rscript'),'tests/verify_hitter_stat_v6.R')
  if(result!=0L) stop('Artifact verification failed; active manifest not updated')
  activate_hitter_stat()
}
