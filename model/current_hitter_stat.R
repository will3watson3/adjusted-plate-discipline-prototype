# Build the active 2024–25 historical scorer with Rscript current_hitter_stat.R.
source('hitter_stat_xgboost.R')

current_hitter_stat <- function(year=2025L,qualified_only=TRUE) {
  if(!year%in%2024:2025)stop('Scoring is validated for 2024 and 2025')
  manifest <- readRDS('data/models/current_hitter_stat.rds')
  stopifnot(manifest$version==H8_VERSION)
  vd_check_hash(manifest$artifacts)
  h <- fread(file.path(manifest$report_directory,sprintf('hitters_%d.csv',year)))
  if(qualified_only)h <- h[qualified==TRUE]
  h[order(-decision_plus)]
}

activate_hitter_profiles <- function() {
  pointer <- 'data/models/current_hitter_stat.rds'
  active <- readRDS(pointer)
  if(active$version==H8_VERSION) {
    vd_check_hash(active$artifacts)
    message('Verified existing active v8 release')
    return(invisible(active))
  }
  if(active$version!=H7_VERSION)stop('Build the current-xgboost (v7) prerequisite before promoting v8')
  stopifnot(active$version==H7_VERSION)
  oldfiles <- as.data.table(active$artifacts)
  vd_check_hash(oldfiles[!path%in%c('current_hitter_stat.R','hitter_stat_xgboost.R')])
  dir.create(H8_REPORT,recursive=TRUE,showWarnings=FALSE)
  dir.create(H8_MODELS,recursive=TRUE,showWarnings=FALSE)
  m <- h8_load()
  ref <- readRDS(file.path(H7_MODELS,'season_reference_2024.rds'))
  cs_save(ref,file.path(H8_MODELS,'season_reference_2024.rds'))
  evidence <- fread(file.path(BR,'artifact_manifest.csv'))
  checks <- list()
  near <- function(x,y) stopifnot(length(x)==length(y),max(abs(x-y),na.rm=TRUE)<1e-8,identical(is.na(x),is.na(y)))
  for(yr in 2024:2025) {
    message('Scoring and verifying ',yr)
    raw <- t_read(yr)
    d <- as.data.table(readRDS(file.path(H7_MODELS,sprintf('decision_values_%d.rds',yr))))
    k <- match(dv_key(d),dv_key(raw));stopifnot(!anyNA(k))
    raw <- raw[k]
    p <- predict_hitter_xgboost(raw,m)
    stopifnot(identical(dv_key(p),dv_key(d)),identical(p$new_history_outside_range,d$new_history_outside_range))
    for(nm in setdiff(names(p),c(CT_KEYS,'q_bip','q_swing_basic','new_history_outside_range')))near(p[[nm]],d[[nm]])
    near(p$q_swing_basic-d$q_swing_basic,d$p_bip_given_swing*(p$q_bip-d$q_bip))
    d[,`:=`(baseline_q_swing=q_swing_basic,baseline_q_take=q_take_basic,baseline_score=score,
      q_bip=p$q_bip,q_swing_basic=p$q_swing_basic,statistic_version=H8_VERSION)]
    d[,swing_minus_take:=q_swing_basic-q_take_basic]
    d[,score:=(as.integer(observed_action=='swing')-p_swing_reference)*swing_minus_take]
    prediction_file <- file.path(BR,sprintf('predictions_%d.csv.gz',yr))
    bp_check_hash(evidence[match(c(prediction_file,file.path(BR,'hitters.csv')),path)])
    expected <- hr_fread(prediction_file)
    stopifnot(identical(dv_key(expected),dv_key(d)),identical(expected$primary_supported,d$primary_supported))
    near(expected$q_bip_C,d$q_bip);near(expected$q_swing_C,d$q_swing_basic);near(expected$score_C,d$score)
    result <- h7_summary(d,ref)
    h <- result$hitters;h[,statistic_version:=H8_VERSION]
    control <- fread(file.path(BR,'hitters.csv'))[year==yr & comparison=='C-A']
    j <- match(h$batter,control$batter);stopifnot(!anyNA(j),identical(h$qualified,control$qualified[j]))
    near(h$decision_plus,control$decision_plus_a[j])
    cs_save(d,file.path(H8_MODELS,sprintf('decision_values_%d.rds',yr)))
    fwrite(h,file.path(H8_REPORT,sprintf('hitters_%d.csv',yr)))
    fwrite(h[qualified==TRUE][order(-decision_plus)],file.path(H8_REPORT,sprintf('qualified_hitters_%d.csv',yr)))
    checks[[as.character(yr)]] <- data.table(year=yr,pitches=nrow(d),supported=sum(d$primary_supported),
      qualified=sum(h$qualified),max_q_bip_error=max(abs(expected$q_bip_C-d$q_bip)),
      max_score_error=max(abs(expected$score_C-d$score)),max_hitter_error=max(abs(h$decision_plus-control$decision_plus_a[j]),na.rm=TRUE))
    rm(raw,d,p,expected);gc(FALSE)
  }
  fwrite(rbindlist(checks),file.path(H8_REPORT,'verification.csv'))
  active$components['bip_value'] <- 'prior_season_ev_barrel_ground_ball_gam'
  paths <- c('current_hitter_stat.R','hitter_stat_xgboost.R',file.path(BP,'functions.R'),
    file.path(BM,c('frozen.rds','C.rds','profiles.rds')),list.files(H8_REPORT,full.names=TRUE),
    file.path(H8_MODELS,c('season_reference_2024.rds',sprintf('decision_values_%d.rds',2024:2025))))
  release <- list(version=H8_VERSION,report_directory=H8_REPORT,model_directory=H8_MODELS,
    components=active$components,artifacts=vd_hash(paths),scope=active$scope)
  vd_check_hash(release$artifacts)
  cs_save(release,file.path(H8_MODELS,'manifest.rds'))
  cs_save(release,pointer)
}
activate_hitter_xgboost <- activate_hitter_profiles

if(sys.nframe()==0L) {
  if('--baseline'%in%commandArgs(TRUE)) {
    status <- system2(file.path(R.home('bin'),'Rscript'),'current_hitter_stat_v6.R')
    if(status!=0L)stop('Historical baseline rebuild failed')
  } else activate_hitter_profiles()
}
