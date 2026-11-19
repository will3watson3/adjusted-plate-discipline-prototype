# Main historical scorer: XGBoost contact, BIP|contact and complete take path.
# The current BIP-value GAM uses prior-season EV, barrel and ground-ball profiles.
source('experiments/swing_xgboost_v1/functions.R')
source('experiments/take_xgboost_v1/predict.R')
source('experiments/bip_value_profiles_v1/functions.R')
H8_MODELS <- 'data/models/hitter_stat_v8'
H8_REPORT <- 'reports/hitter_stat_v8'
H8_VERSION <- 'decision_plus_bip_profiles_v8'

h8_load <- function() {
  models <- h7_load(legacy=list(post=readRDS(file.path(PC_MODELS,'post_contact_fit_2022_2023.rds'))))
  frozen <- readRDS(file.path(BM,'frozen.rds'))
  paths <- file.path(BM,c('C.rds','profiles.rds'))
  bp_check_hash(frozen$code)
  bp_check_hash(frozen$models[match(paths,path)])
  models$profile_value <- readRDS(paths[1])
  models$value_histories <- readRDS(paths[2])
  models
}

H7_MODELS <- 'data/models/hitter_stat_xgboost'
H7_REPORT <- 'reports/hitter_stat_xgboost'
H7_VERSION <- 'decision_plus_xgboost_v7'

h7_load <- function(legacy=load_decision_models()) {
  for(path in c('experiments/contact_xgboost_v1/models/frozen.rds',
    'experiments/swing_xgboost_v1/models/frozen.rds','experiments/take_xgboost_v1/models/frozen.rds')) {
    f <- readRDS(path); vd_check_hash(f$model_hashes)
  }
  list(contact=readRDS(file.path(XM,'bundle.rds')),contact_model=xgb.load(file.path(XM,'contact.ubj')),
    bip=readRDS(file.path(SM,'bip.rds')),bip_model=xgb.load(file.path(SM,'bip.ubj')),
    histories=readRDS(file.path(SM,'histories.rds')),legacy=legacy,
    contact_control=readRDS('data/models/contact_personnel_comparison/updated_profiles.rds'),bip_control=s_base('bip'))
}

h7_branches <- function(contact,bip,tip,value,states) {
  p <- cbind(whiff=1-contact,bip=contact*bip,foul=contact*(1-bip)*(1-tip),tip=contact*(1-bip)*tip)
  stopifnot(all(is.finite(p)),all(p>=0&p<=1),max(abs(rowSums(p)-1))<1e-12)
  data.table(p_contact_given_swing=contact,p_in_play_given_contact=bip,p_tip_given_nonbip=tip,
    p_whiff_given_swing=p[,'whiff'],p_bip_given_swing=p[,'bip'],p_foul_given_swing=p[,'foul'],
    p_tip_given_swing=p[,'tip'],q_bip=value,
    q_swing_basic=p[,'whiff']*states$q_whiff+p[,'bip']*value+p[,'foul']*states$q_ordinary_foul+p[,'tip']*states$q_foul_tip)
}

predict_hitter_xgboost <- function(pitches,models=h8_load(),frozen_legacy=NULL) {
  d <- copy(as.data.table(pitches)); n <- nrow(d)
  stopifnot(n>0L,length(unique(d$game_year))==1L,all(d$game_year%in%2024:2025))
  d[,season:=game_year]
  # The historical contact encoder needs two rows for singleton factor matrices.
  z <- if(n==1L)rbindlist(list(d,d))else d
  contact <- x_predict(models$contact_model,z,models$contact)[seq_len(n)]
  bip <- s_predict(models$bip_model,d,models$bip,models$histories)
  if(is.null(frozen_legacy)) {
    tip_features <- pc_transform(d,models$legacy$post$tip$prep)$data
    tip <- plogis(hr_eta(models$legacy$post$tip$model,tip_features))
  } else {
    stopifnot(identical(dv_key(d),dv_key(frozen_legacy)))
    tip <- frozen_legacy$p_foul_tip_given_contact/(1-frozen_legacy$p_in_play_given_contact)
  }
  value <- if(!is.null(models$profile_value)) {
    bp_predict(models$profile_value,d,models$value_histories,'C')
  } else if(!is.null(frozen_legacy)) frozen_legacy$q_bip else {
    exp(hr_eta(models$legacy$value$model,pc_transform(d,models$legacy$value$prep)$data))
  }
  states <- pc_branch_states(read_re_baseline()); k <- match(pc_state(d),states$re288_state);stopifnot(!anyNA(k))
  out <- h7_branches(contact,bip,tip,value,states[k])
  p <- predict_take_comparison(d,'xgboost')$complete
  take_states <- build_take_state_values(); j <- match(pc_state(d),take_states$re288_state)
  out[,`:=`(p_called_strike_if_ordinary_take=p,
    q_take_basic=(1-p)*take_states$ball_value[j]+p*take_states$called_strike_value[j])]
  # Preserve original support and also withhold extrapolations in the new histories.
  cf <- x_features(d,models$contact$prep,models$contact$profiles,models$contact$schema$features)
  bf <- s_features(d,models$bip,models$histories)
  numeric <- intersect(models$contact$schema$numeric,names(models$contact_control$model$model))
  ranges <- lapply(as.data.table(models$contact_control$model$model)[,..numeric],range)
  br <- models$bip_control$ranges[intersect(names(models$bip_control$ranges),models$bip$schema$features)]
  out[,new_history_outside_range:=vd_range_flags(cf,list(ranges=ranges))|vd_range_flags(bf,list(ranges=br))]
  cbind(d[,..CT_KEYS],out)
}

h7_summary <- function(d,reference) {
  cov <- d[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=.(batter,batter_name)]
  g <- d[primary_supported==TRUE,.(n=.N,total=sum(score),baseline=sum(baseline_score)),by=.(batter,game_pk)]
  setorder(g,batter,game_pk);set.seed(20261117L)
  h <- g[,{
    draws <- replicate(500,{i<-sample.int(.N,.N,TRUE);100*c(sum(total[i]),sum(total[i]-baseline[i]))/sum(n[i])})
    .(games=.N,runs_per100=100*sum(total)/sum(n),baseline_runs_per100=100*sum(baseline)/sum(n),
      sampling_low=unname(quantile(draws[1,],.025)),sampling_high=unname(quantile(draws[1,],.975)),
      delta_low=unname(quantile(draws[2,],.025)),delta_high=unname(quantile(draws[2,],.975)))
  },by=batter]
  h <- merge(cov,h,by='batter',all.x=TRUE)
  h[,`:=`(qualified=!is.na(games)&supported>=500&games>=60&coverage>=.7,
    decision_plus=100+10*(runs_per100-reference$center)/reference$scale,
    baseline_decision_plus=100+10*(baseline_runs_per100-reference$center)/reference$scale,
    decision_plus_sampling_low=100+10*(sampling_low-reference$center)/reference$scale,
    decision_plus_sampling_high=100+10*(sampling_high-reference$center)/reference$scale,
    delta_plus=10*(runs_per100-baseline_runs_per100)/reference$scale,
    delta_plus_low=10*delta_low/reference$scale,delta_plus_high=10*delta_high/reference$scale,
    statistic_version=H7_VERSION,uncertainty_status='conditional_game_sampling_only_no_fitting_uncertainty')]
  list(hitters=h,games=g)
}

h7_run <- function() {
  dir.create(H7_MODELS,recursive=TRUE,showWarnings=FALSE);dir.create(H7_REPORT,recursive=TRUE,showWarnings=FALSE)
  baseline <- readRDS('data/models/current_hitter_stat.rds')
  if(baseline$version==H7_VERSION)baseline<-readRDS(file.path(H7_MODELS,'previous_active.rds'))
  stopifnot(baseline$version!=H7_VERSION)
  paths <- unique(c('hitter_stat_xgboost.R','tests/verify_hitter_stat_xgboost.R',
    'data/models/hitter_stat_v6/season_reference_2024.rds',
    sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',2024:2025),
    sprintf('data/processed/statcast_%d_clean.rds',2024:2025),
    baseline$artifacts$path,
    unlist(lapply(c('contact','swing','take'),function(stage) {
      root<-file.path('experiments',paste0(stage,'_xgboost_v1'))
      c(list.files(file.path(root,'models'),full.names=TRUE),list.files(root,pattern='[.]R$',full.names=TRUE))
    })),read_re_baseline()$files$file,
    'data/models/contact_personnel_comparison/updated_profiles.rds',
    file.path('experiments/hitter_region_v1/models',paste0(HR_STAGES,'.rds')),
    'reports/umpire_v5/umpire_assignments.csv',
    sprintf('experiments/take_xgboost_v1/reports/weekly_xgboost_%d.csv',2024:2025)))
  # The active pointer and reader are replaced only after independent verification.
  paths <- setdiff(paths,c('data/models/current_hitter_stat.rds','current_hitter_stat.R'))
  hashes <- vd_hash(paths);vd_check_hash(hashes);fwrite(hashes,file.path(H7_REPORT,'input_hashes.csv'))
  m <- h7_load();reference <- readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
  metrics <- boot <- coverage <- list()
  for(year in 2024:2025) {
    message('Hybrid scorer ',year)
    raw <- t_read(year)
    old <- as.data.table(readRDS(sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',year)))
    k <- match(dv_key(old),dv_key(raw));stopifnot(!anyNA(k));d <- raw[k];rm(raw);gc(FALSE)
    p <- predict_hitter_xgboost(d,m,old)
    # Persist a clean artifact; no v6 model-uncertainty columns are inherited.
    cols <- c(CT_KEYS,'game_date','batter','batter_name','description','observed_action',
      're288_state','re_before','p_swing_reference','support_reason','re_order_warning','factual_q','factual_target_status')
    out <- copy(old[,..cols]);out[,game_year:=year];out <- cbind(out,p[,setdiff(names(p),CT_KEYS),with=FALSE])
    out[,`:=`(original_supported=old$primary_supported,primary_supported=old$primary_supported&!new_history_outside_range,
      baseline_q_swing=old$q_swing_basic,baseline_q_take=old$q_take_basic,
      baseline_score=(as.integer(observed_action=='swing')-p_swing_reference)*(old$q_swing_basic-old$q_take_basic))]
    out[new_history_outside_range==TRUE,support_reason:=ifelse(nzchar(support_reason),paste0(support_reason,';new_history_outside_range'),'new_history_outside_range')]
    out[,`:=`(swing_minus_take=q_swing_basic-q_take_basic,
      score=(as.integer(observed_action=='swing')-p_swing_reference)*(q_swing_basic-q_take_basic),
      statistic_version=H7_VERSION,uncertainty_status='conditional_game_sampling_only_no_fitting_uncertainty')]
    stopifnot(max(abs(out$q_bip-old$q_bip))<1e-8,
      max(abs(out$p_tip_given_nonbip-old$p_foul_tip_given_contact/(1-old$p_in_play_given_contact)))<1e-8)
    cs_save(out,file.path(H7_MODELS,sprintf('decision_values_%d.rds',year)))
    result <- h7_summary(out,reference); h<-result$hitters
    fwrite(h,file.path(H7_REPORT,sprintf('hitters_%d.csv',year)))
    fwrite(h[qualified==TRUE][order(-decision_plus)],file.path(H7_REPORT,sprintf('qualified_hitters_%d.csv',year)))
    fwrite(h[qualified==TRUE][order(-abs(delta_plus))][1:20],file.path(H7_REPORT,sprintf('largest_movements_%d.csv',year)))
    fwrite(result$games,file.path(H7_REPORT,sprintf('hitter_games_%d.csv',year)))
    for(action in c('swing','take')) {
      z <- out[primary_supported==TRUE&observed_action==action&is.finite(factual_q)]
      a <- if(action=='swing')z$q_swing_basic else z$q_take_basic
      b <- if(action=='swing')z$baseline_q_swing else z$baseline_q_take
      for(arm in c('baseline','xgboost')) metrics[[paste(year,action,arm)]] <- cbind(data.table(year,action,arm),t_value_metrics(z$factual_q,if(arm=='baseline')b else a))
      boot[[paste(year,action)]] <- t_boot(z$game_pk,z$factual_q,a,b,TRUE)[,`:=`(year=year,action=action)]
    }
    coverage[[as.character(year)]] <- data.table(year,eligible=nrow(out),original_supported=sum(out$original_supported),
      supported=sum(out$primary_supported),additional_exclusions=sum(out$original_supported&!out$primary_supported),
      coverage=mean(out$primary_supported),qualified=sum(h$qualified))
    rm(old,d,out,p);gc(FALSE)
  }
  fwrite(rbindlist(metrics),file.path(H7_REPORT,'metrics.csv'));fwrite(rbindlist(boot),file.path(H7_REPORT,'paired_intervals.csv'))
  fwrite(rbindlist(coverage),file.path(H7_REPORT,'coverage.csv'))
  cs_save(reference,file.path(H7_MODELS,'season_reference_2024.rds'))
  vd_check_hash(hashes)
  cs_save(list(version=H7_VERSION,inputs=hashes,reference=reference,
    components=c(contact='xgboost',bip_given_contact='xgboost',take='xgboost_joint_actors_weekly',tip='active_v6_gam',bip_value='active_v6_gam'),
    limitation='exploratory_2024_2025; conditional_game_sampling_only'),file.path(H7_MODELS,'manifest.rds'))
}
if(sys.nframe()==0L) {
  source('current_hitter_stat.R')
  activate_hitter_profiles()
}
