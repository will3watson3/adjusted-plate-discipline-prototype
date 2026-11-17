source('experiments/take_xgboost_v1/functions.R')
freeze <- readRDS(file.path(TM,'frozen.rds'))
vd_check_hash(freeze$model_hashes); vd_check_hash(freeze$code_hashes)
vd_check_hash(fread(file.path(TR,'input_hashes.csv')))
evaluation_code <- t_hash(file.path(TROOT,c('evaluate.R','functions.R')))
saveRDS(list(started=as.character(Sys.time()),code_hashes=evaluation_code),file.path(TR,'evaluation_started.rds'))
gam <- readRDS(t_base_path()); bundle <- readRDS(file.path(TM,'bundle.rds'))
model <- xgb.load(file.path(TM,'take.ubj')); re <- read_re_baseline(); states <- build_take_state_values(re)
ref <- readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
stopifnot(sum(states$state_value_order_warning)==34L)
metrics <- boot <- bins <- slices <- values <- valueboot <- coverage <- tails <- state_errors <- boundaries <- list()
for(year in 2024:2025) {
  message('Evaluate ',year)
  raw <- t_read(year)
  old <- as.data.table(readRDS(sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',year)))
  k <- match(dv_key(old),dv_key(raw)); stopifnot(!anyNA(k))
  # Audit the full sequence before selecting pitches; exceptional takes remain unverified.
  audit <- audit_take_transitions(raw,attach_re_to_pitches(raw,year,re),states,year)
  t_write(audit[,.(n=.N),by=audit_status],paste0('target_audit_',year))
  d <- raw[k]; rm(raw); gc(FALSE)
  t <- cs_transform(d,bundle$prep); stopifnot(!any(t$invalid))
  out <- d[,c(CS_KEYS,'game_date','game_year','batter','description','balls','strikes','stand','p_throws','pitch_family','plate_x','plate_z','sz_bot','sz_top'),with=FALSE]
  out[,`:=`(batter_name=old$batter_name,take=description%chin%c('ball','blocked_ball','called_strike'),
    y=as.integer(description=='called_strike'),valid=!t$invalid&!t$novel_category,
    supported=old$primary_supported,support_reason=old$support_reason,action=old$observed_action,
    re288_state=old$re288_state,re_order_warning=old$re_order_warning,
    q_swing=old$q_swing_basic,reference=old$p_swing_reference,
    min_cell_n=pmin(old$contact_training_cell_n,old$post_training_cell_n,old$bip_training_cell_n,old$take_training_cell_n))]
  j <- match(out$re288_state,states$re288_state); stopifnot(!anyNA(j))
  out[,`:=`(q_ball=states$ball_value[j],q_strike=states$called_strike_value[j],
    state_warning=states$state_value_order_warning[j],target=NA_real_)]
  j <- match(dv_key(out),dv_key(audit))
  good <- which(out$take & !is.na(j) & audit$audit_status[j]=='matches_basic_transition')
  out[good,target:=ifelse(y==1L,q_strike,q_ball)]
  stopifnot(identical(is.finite(out$target[out$take]),is.finite(old$factual_q[out$take])),
    max(abs(out$target[good]-old$factual_q[good]))<1e-12)
  for(arm in c('gam','xgboost')) {
    message('Predict ',year,' ',arm)
    eta <- if(arm=='gam') fr_base(gam,d,se=FALSE)$eta else t_eta(model,d,bundle)
    actor <- readRDS(file.path(TM,sprintf('final_%s_actors_%d.rds',arm,year)))
    ad <- t_actor_data(d,eta,bundle$prep); p <- u5_predict(actor,ad); online <- u5_online(ad,p$p)
    t_write(online$ledger,paste0('weekly_',arm,'_',year))
    for(nm in c('catcher','umpire','ump_region','region')) set(out,j=nm,value=ad[[nm]])
    for(nm in names(actor$groups)) {
      tab <- actor$groups[[nm]]$table
      n <- tab$opportunities[match(ad[[nm]],tab$id)]; n[is.na(n)] <- 0L
      set(out,j=paste0(nm,'_history_',arm),value=n)
    }
    out[,(paste0('eta_',arm)):=eta]
    out[,(paste0('base_',arm)):=plogis(eta)]
    out[,(paste0('actor_',arm)):=p$p]
    out[,(paste0('effect_',arm)):=p$effect]
    out[,(paste0('latent_sd_',arm)):=p$latent_sd]
    out[,(paste0('complete_',arm)):=online$p]
    qt <- (1-online$p)*out$q_ball+online$p*out$q_strike
    out[,(paste0('q_take_',arm)):=qt]
    out[,(paste0('score_',arm)):=(as.integer(action=='swing')-reference)*(q_swing-qt)]
  }
  # Reestimated GAM control must reproduce the active complete path.
  pdiff <- max(abs(out$complete_gam-old$p_called_strike_if_ordinary_take))
  stopifnot(pdiff<1e-6,max(abs(out$q_take_gam-old$q_take_basic))<1e-6,
    all(is.finite(out$q_take_xgboost[out$supported])),!any(out$supported&out$state_warning))
  out[,`:=`(count=paste(balls,strikes,sep='-'),hands=paste(stand,p_throws,sep='/'),
    edge=ifelse(pmin(abs(abs(plate_x)-.83),abs((plate_z-sz_bot)/(sz_top-sz_bot)),
      abs((plate_z-sz_bot)/(sz_top-sz_bot)-1))<=.10,'near_edge','away_from_edge'),
    catcher_history=ifelse(catcher_history_gam==0,'unseen','seen'),
    umpire_history=ifelse(umpire_history_gam==0,'unseen','seen'))]
  fwrite(out,file.path(TR,paste0('predictions_',year,'.csv.gz')))
  for(pop in c('all_takes','supported_takes')) {
    z <- out[valid&take&if(pop=='supported_takes')supported else TRUE]
    for(stage in c('base','complete')) {
      tag <- paste(year,pop,stage)
      for(arm in c('gam','xgboost')) {
        p <- z[[paste(stage,arm,sep='_')]]
        metrics[[paste(tag,arm)]] <- cbind(data.table(year,population=pop,stage,arm),t_probability_metrics(z$y,p))
        bins[[paste(tag,arm)]] <- data.table(y=z$y,p)[,bin:=pmin(19L,floor(p*20))][,
          .(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,`:=`(year=year,population=pop,stage=stage,arm=arm)]
        for(dim in c('count','region','edge','hands','pitch_family','catcher_history','umpire_history')) {
          slices[[paste(tag,arm,dim)]] <- data.table(slice=z[[dim]],y=z$y,p)[,
            .(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),bias=mean(p-y)),by=slice][,
            `:=`(year=year,population=pop,stage=stage,arm=arm,dimension=dim)]
        }
      }
      boot[[tag]] <- t_boot(z$game_pk,z$y,z[[paste0(stage,'_xgboost')]],z[[paste0(stage,'_gam')]])[,
        `:=`(year=year,population=pop,stage=stage)]
      factual <- z[is.finite(target)]
      qa <- (1-factual[[paste0(stage,'_xgboost')]])*factual$q_ball+factual[[paste0(stage,'_xgboost')]]*factual$q_strike
      qb <- (1-factual[[paste0(stage,'_gam')]])*factual$q_ball+factual[[paste0(stage,'_gam')]]*factual$q_strike
      for(arm in c('gam','xgboost')) values[[paste(tag,arm)]] <- cbind(data.table(year,population=pop,stage,arm),
        t_value_metrics(factual$target,if(arm=='gam')qb else qa))
      valueboot[[tag]] <- t_boot(factual$game_pk,factual$target,qa,qb,TRUE)[,`:=`(year=year,population=pop,stage=stage)]
    }
  }
  cov <- out[,.(eligible=.N,supported=sum(supported),coverage=mean(supported)),by=.(batter,batter_name)]
  games <- out[supported==TRUE,.(n=.N,gam=sum(score_gam),xgboost=sum(score_xgboost)),by=.(batter,game_pk)]
  setorder(games,batter,game_pk)
  set.seed(TSEED)
  hit <- games[,{
    draws <- replicate(500,{ix<-sample.int(.N,.N,TRUE);100*c(sum(gam[ix]),sum(xgboost[ix]),sum(xgboost[ix]-gam[ix]))/sum(n[ix])})
    .(games=.N,gam_runs100=100*sum(gam)/sum(n),xgboost_runs100=100*sum(xgboost)/sum(n),
      gam_low=unname(quantile(draws[1,],.025)),gam_high=unname(quantile(draws[1,],.975)),
      xgboost_low=unname(quantile(draws[2,],.025)),xgboost_high=unname(quantile(draws[2,],.975)),
      delta_low=unname(quantile(draws[3,],.025)),delta_high=unname(quantile(draws[3,],.975)))
  },by=batter]
  hit <- merge(cov,hit,by='batter',all.x=TRUE)
  hit[,qualified:=!is.na(games)&supported>=500&games>=60&coverage>=.7]
  for(arm in c('gam','xgboost')) {
    hit[,(paste0(arm,'_decision_plus')):=100+10*(get(paste0(arm,'_runs100'))-ref$center)/ref$scale]
    for(bound in c('low','high')) hit[,(paste(arm,'plus',bound,sep='_')):=100+10*(get(paste(arm,bound,sep='_'))-ref$center)/ref$scale]
  }
  hit[,`:=`(delta_plus=xgboost_decision_plus-gam_decision_plus,delta_plus_low=10*delta_low/ref$scale,delta_plus_high=10*delta_high/ref$scale)]
  t_write(hit,paste0('hitters_',year)); t_write(hit[qualified==TRUE][order(-abs(delta_plus))][1:20],paste0('largest_movements_',year))
  fwrite(games,file.path(TR,paste0('hitter_games_',year,'.csv.gz')))
  coverage[[as.character(year)]] <- data.table(year,eligible=nrow(out),supported=sum(out$supported),coverage=mean(out$supported),
    qualified=sum(hit$qualified),additional_exclusions=0L,gam_active_max_probability_difference=pdiff)
  tails[[as.character(year)]] <- out[,.(n=.N,mean_take_change=mean(q_take_xgboost-q_take_gam),
    mean_absolute_take_change=mean(abs(q_take_xgboost-q_take_gam)),p99_absolute_take_change=quantile(abs(q_take_xgboost-q_take_gam),.99),
    max_absolute_take_change=max(abs(q_take_xgboost-q_take_gam)),delta_runs100=100*mean(score_xgboost-score_gam),
    preference_flip=mean((q_swing>q_take_xgboost)!=(q_swing>q_take_gam))),by=.(action,supported)][,year:=year]
  for(arm in c('gam','xgboost')) state_errors[[paste(year,arm)]] <- out[take&valid&is.finite(target),
    .(n=.N,rmse=sqrt(mean((get(paste0('q_take_',arm))-target)^2)),mae=mean(abs(get(paste0('q_take_',arm))-target)),
      bias=mean(get(paste0('q_take_',arm))-target),mean_branch_gap=mean(q_ball-q_strike)),
    by=.(re288_state,supported,state_warning)][,`:=`(year=year,arm=arm)]
  policies <- list(original=out$supported,cells_50=out$supported&out$min_cell_n>=50,
    overlap_10=out$supported&between(out$reference,.1,.9),
    near_cell_boundary=out$supported&out$min_cell_n<50,
    near_overlap_boundary=out$supported&(out$reference<.1|out$reference>.9))
  for(policy in names(policies)) {
    z <- out[policies[[policy]]]
    boundaries[[paste(year,policy)]] <- data.table(year,policy,n=nrow(z),
      mean_absolute_take_change=mean(abs(z$q_take_xgboost-z$q_take_gam)),
      delta_runs100=100*mean(z$score_xgboost-z$score_gam))
  }
  rm(d,old,out,audit); gc(FALSE)
}
for(nm in c('metrics','boot','bins','slices','values','valueboot','coverage','tails','state_errors','boundaries')) t_write(rbindlist(get(nm)),nm)
vd_check_hash(freeze$model_hashes); vd_check_hash(freeze$code_hashes); vd_check_hash(evaluation_code)
vd_check_hash(fread(file.path(TR,'input_hashes.csv')))
saveRDS(list(completed=as.character(Sys.time()),code_hashes=evaluation_code,activated=FALSE),file.path(TR,'evaluation_manifest.rds'))
message('EVALUATION COMPLETE')
