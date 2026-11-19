source('experiments/bip_value_profiles_v1/functions.R')
frozen <- readRDS(file.path(BM,'frozen.rds'));bp_check_hash(frozen$inputs);bp_check_hash(frozen$code);bp_check_hash(frozen$models)
profiles <- readRDS(file.path(BM,'profiles.rds'))
fits <- setNames(lapply(c('A','B','C'),function(a)readRDS(file.path(BM,paste0(a,'.rds')))),c('A','B','C'))
ref <- readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
metrics <- boot <- bins <- slices <- tails <- coverage <- hitters <- list()
for(year in 2024:2025) {
  message('Evaluate ',year)
  raw <- bp_read(year); target <- bp_targets(raw)
  old <- as.data.table(readRDS(sprintf('data/models/hitter_stat_xgboost/decision_values_%d.rds',year)))
  j <- match(bp_key(old),bp_key(raw));stopifnot(!anyNA(j))
  cols <- unique(c(CT_KEYS,CT_INPUTS,'season','game_date','game_year','description','batter_name'))
  d <- raw[j,..cols];rm(raw);gc(FALSE)
  out <- copy(old[,c(CT_KEYS,'game_date','batter','batter_name','description','observed_action',
    're288_state','re_before','p_swing_reference','primary_supported','support_reason','re_order_warning',
    'factual_q','factual_target_status','p_contact_given_swing','p_in_play_given_contact','p_tip_given_nonbip',
    'p_bip_given_swing','p_whiff_given_swing','p_foul_given_swing','p_tip_given_swing','q_take_basic'),with=FALSE])
  out[,`:=`(year=year,pitch_family=d$pitch_family,base_out=sub('_[0-3]-[0-2]$','',re288_state))]
  out <- cbind(out,bp_history_audit(d,profiles))
  k <- match(bp_key(out),bp_key(target));out[,bip_y:=target$q_bip_observed[k]]
  stopifnot(all(!is.finite(out$bip_y)|out$description=='hit_into_play'))
  for(arm in c('A','B','C')) {
    message('Predict ',year,' arm ',arm)
    q <- bp_predict(fits[[arm]],d,profiles,arm)
    if(arm=='A')stopifnot(max(abs(q-old$q_bip))<1e-8)
    out[,(paste0('q_bip_',arm)):=q]
    out[,(paste0('q_swing_',arm)):=old$q_swing_basic+old$p_bip_given_swing*(q-old$q_bip)]
    out[,(paste0('score_',arm)):=(as.integer(observed_action=='swing')-p_swing_reference)*
      (get(paste0('q_swing_',arm))-q_take_basic)]
    ff <- bp_features(d,fits[[arm]]$prep,profiles,arm)
    names_check <- intersect(names(fits[[arm]]$ranges),c('batter_ev_2021','pitcher_ev_2021',
      'batter_barrel','pitcher_barrel','batter_gb','pitcher_gb'))
    outside <- rep(FALSE,nrow(d))
    for(nm in names_check)outside <- outside|ff[[nm]]<fits[[arm]]$ranges[[nm]][1]|ff[[nm]]>fits[[arm]]$ranges[[nm]][2]
    out[,(paste0('profile_outside_',arm)):=outside]
    rm(ff);gc(FALSE)
  }
  stopifnot(max(abs(out$score_A-old$score))<1e-8)
  out[,common_inrange:=!profile_outside_B&!profile_outside_C]
  # Save targets, all three arms and fixed scorer components under identical keys.
  fwrite(out,file.path(BR,sprintf('predictions_%d.csv.gz',year)))
  bip <- out[is.finite(bip_y)]
  swing <- out[primary_supported & observed_action=='swing' & is.finite(factual_q)]
  populations <- list(bip_all=bip,bip_supported=bip[primary_supported==TRUE],
    bip_common_inrange=bip[common_inrange==TRUE],swing_supported=swing,
    swing_common_inrange=swing[common_inrange==TRUE])
  for(pop in names(populations)) {
    z <- populations[[pop]];is_bip <- startsWith(pop,'bip');y <- if(is_bip)'bip_y'else'factual_q'
    prefix <- if(is_bip)'q_bip_'else'q_swing_'
    for(arm in c('A','B','C'))metrics[[paste(year,pop,arm)]] <- cbind(data.table(year,population=pop,arm),bp_metrics(z[[y]],z[[paste0(prefix,arm)]]))
    for(pair in list(c('B','A'),c('C','B')))boot[[paste(year,pop,pair[1])]] <- cbind(
      data.table(year,population=pop,comparison=paste(pair,collapse='-')),
      bp_boot(z,paste0(prefix,pair[1]),paste0(prefix,pair[2]),y))
    if(pop%in%c('bip_all','swing_supported'))for(arm in c('A','B','C')) {
      zz <- copy(z);zz[,`:=`(p=get(paste0(prefix,arm)),y=get(y))]
      zz[,bin:=as.character(cut(p,c(-Inf,0,.25,.5,.75,1,1.5,2,3,5,Inf),right=FALSE))]
      bins[[paste(year,pop,arm)]] <- zz[,.(n=.N,predicted=mean(p),observed=mean(y),bias=mean(p-y)),by=bin][,`:=`(year=year,population=pop,arm=arm)]
      for(dim in c('base_out','pitch_family','ev_exposure','barrel_exposure','gb_exposure','missing_history')) {
        sl <- zz[,.(n=.N,rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),bias=mean(p-y)),by=dim]
        setnames(sl,dim,'slice');sl[,`:=`(year=year,population=pop,arm=arm,dimension=dim)]
        slices[[paste(year,pop,arm,dim)]] <- sl
      }
    }
  }
  for(group in c('factual_bip','supported_swing','observed_take_audit_only'))for(arm in c('A','B','C')) {
    z <- switch(group,factual_bip=bip,supported_swing=swing,observed_take_audit_only=out[observed_action=='take'])
    p <- z[[paste0('q_bip_',arm)]]
    tails[[paste(year,group,arm)]] <- data.table(year,group,arm,n=nrow(z),
      min=min(p),p50=quantile(p,.5),p95=quantile(p,.95),p99=quantile(p,.99),p999=quantile(p,.999),max=max(p),
      q_over5=sum(p>5),q_over10=sum(p>10),outside_profile_range=sum(z[[paste0('profile_outside_',arm)]]))
    extreme <- copy(z[order(-get(paste0('q_bip_',arm)))][1:min(20,.N)])
    fwrite(extreme,file.path(BR,paste0('tails_',year,'_',group,'_',arm,'.csv')))
  }
  cov <- out[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=.(batter,batter_name)]
  g <- out[primary_supported==TRUE,.(n=.N,A=sum(score_A),B=sum(score_B),C=sum(score_C)),by=.(batter,game_pk)]
  setorder(g,batter,game_pk);fwrite(g,file.path(BR,paste0('hitter_games_',year,'.csv.gz')))
  for(pair in list(c('B','A'),c('C','B'),c('C','A'))) {
    a <- pair[1];b <- pair[2];set.seed(BSEED+year)
    h <- g[,{
      av <- get(a);bv <- get(b)
      draws <- replicate(500,{i<-sample.int(.N,.N,TRUE);100*sum(av[i]-bv[i])/sum(n[i])})
      .(games=.N,rate_a=100*sum(av)/sum(n),rate_b=100*sum(bv)/sum(n),
        delta_low=unname(quantile(draws,.025)),delta_high=unname(quantile(draws,.975)))
    },by=batter]
    h <- merge(cov,h,by='batter',all.x=TRUE)
    h[,`:=`(year=year,comparison=paste(pair,collapse='-'),
      qualified=!is.na(games)&supported>=500&games>=60&coverage>=.7,
      decision_plus_a=100+10*(rate_a-ref$center)/ref$scale,
      decision_plus_b=100+10*(rate_b-ref$center)/ref$scale,
      delta_plus=10*(rate_a-rate_b)/ref$scale,delta_plus_low=10*delta_low/ref$scale,
      delta_plus_high=10*delta_high/ref$scale)]
    hitters[[paste(year,a,b)]] <- h
    bp_write(h[qualified==TRUE][order(-abs(delta_plus))][1:15],paste0('largest_changes_',year,'_',a,'_',b))
  }
  coverage[[as.character(year)]] <- data.table(year,eligible=nrow(out),supported=sum(out$primary_supported),
    coverage=mean(out$primary_supported),verified_bip=nrow(bip),factual_swing=nrow(swing),
    additional_exclusions=0L,inrange_supported=sum(out$primary_supported&out$common_inrange),
    qualified=sum(merge(cov,g[,.(games=.N),by=batter],by='batter')[supported>=500&games>=60&coverage>=.7,.N]))
  rm(d,out,old,target,bip,swing,populations);gc(FALSE)
}
for(nm in c('metrics','boot','bins','slices','tails','coverage','hitters'))bp_write(rbindlist(get(nm),fill=TRUE),nm)
bp_check_hash(frozen$inputs);bp_check_hash(frozen$models)
saveRDS(bp_hash(file.path(BP,'evaluate.R')),file.path(BM,'evaluation_code.rds'))
message('Evaluation complete')
