source('post_contact_models.R')
BP <- 'experiments/bip_value_profiles_v1'
BM <- file.path(BP,'models'); BR <- file.path(BP,'reports')
BSEED <- 20261118L
setDTthreads(2)
bp_write <- function(x,name) fwrite(x,file.path(BR,paste0(name,'.csv')))
bp_hash <- function(paths) data.table(path=paths,md5=unname(tools::md5sum(paths)))
bp_check_hash <- function(h) stopifnot(!anyNA(h$md5),identical(unname(tools::md5sum(h$path)),h$md5))
bp_key <- function(d) do.call(paste,c(d[,..CT_KEYS],sep='|'))
bp_read <- function(year) {
  d <- as.data.table(readRDS(sprintf('data/processed/statcast_%d_clean.rds',year)))
  stopifnot(!anyDuplicated(d,by=CT_KEYS),!anyNA(d[,..CT_KEYS]),all(d$game_year==year),
    all(format(d$game_date,'%Y')==as.character(year)))
  d[,season:=year];d[,re288_state:=pc_state(d)];setorderv(d,c('game_date',CT_KEYS)); d
}
bp_targets <- function(d) {
  z <- pc_bip_targets(d[analysis_eligible%in%TRUE & ct_core_valid(d)],
    rbindlist(list(fread('reports/post_contact/bip_post_states.csv'),
      fread('reports/decision_validation/bip_post_states_2025.csv')),fill=TRUE),read_re_baseline())
  bp_write(z[,.(n=.N),by=.(season,target_status)],paste0('targets_',paste(unique(d$season),collapse='_')))
  z[target_status=='verified']
}
bp_rates <- function(d,role,metric) {
  hand <- if(role=='batter')'p_throws'else'stand'
  z <- d[in_play%in%TRUE]
  known <- if(metric=='barrel')z$launch_speed_angle%in%1:6 else z$bb_type%chin%c('ground_ball','line_drive','fly_ball','popup')
  positive <- if(metric=='barrel')z$launch_speed_angle%in%6L else z$bb_type%chin%'ground_ball'
  z[,`:=`(den=as.integer(known),num=as.integer(known&positive))]
  x <- z[,.(n=sum(den),total=sum(num),batted_balls=.N),by=c(role,hand,'pitch_family')]
  league <- sum(x$total)/sum(x$n)
  overall <- x[,.(n=sum(n),total=sum(total),batted_balls=sum(batted_balls)),by=role]
  overall[,rate:=(total+50*league)/(n+50)]
  parent <- overall$rate[match(x[[role]],overall[[role]])]
  split <- data.table(split_key=paste(x[[role]],x[[hand]],x$pitch_family,sep='|'),
    n=x$n,total=x$total,batted_balls=x$batted_balls,rate=(x$total+30*parent)/(x$n+30))
  list(overall=overall,split=split,league=league,hand=hand,history_year=unique(d$season))
}
bp_lookup <- function(d,p,role,value) {
  j <- match(d[[role]],p$overall[[role]])
  k <- match(paste(d[[role]],d[[p$hand]],d$pitch_family,sep='|'),p$split$split_key)
  parent <- p$overall[[value]][j]; parent[is.na(parent)] <- p$league
  v <- p$split[[value]][k]; v[is.na(v)] <- parent[is.na(v)]
  list(value=v,overall_n=ifelse(is.na(j),0,p$overall$n[j]),
    split_n=ifelse(is.na(k),0,p$split$n[k]),unseen=is.na(j),missing_split=is.na(k))
}
bp_profiles <- function() {
  out <- list(); audits <- list()
  for(year in 2021:2024) {
    message('Historical profiles ',year)
    d <- bp_read(year)
    stopifnot(all(c('bb_type','launch_speed_angle','launch_speed','launch_angle','in_play')%in%names(d)))
    z <- d[in_play%in%TRUE]
    stopifnot(all(is.na(z$launch_speed_angle)|z$launch_speed_angle%in%1:6),
      all(is.na(z$bb_type)|z$bb_type%chin%c('ground_ball','line_drive','fly_ball','popup')))
    audits[[as.character(year)]] <- z[,.(batted_balls=.N,ev_n=sum(is.finite(launch_speed)),
      barrel_n=sum(launch_speed_angle%in%1:6),barrels=sum(launch_speed_angle%in%6L),
      gb_n=sum(bb_type%chin%c('ground_ball','line_drive','fly_ball','popup')),grounders=sum(bb_type%chin%'ground_ball'),
      barrel_label_without_ev=sum(launch_speed_angle%in%1:6&!is.finite(launch_speed)),
      barrel_label_without_la=sum(launch_speed_angle%in%1:6&!is.finite(launch_angle))),by=.(season,pitch_family)]
    ev <- lapply(c('batter','pitcher'),function(role) {
      x <- fread(sprintf('data/processed/%s_splits_%d.csv',role,year))
      hand <- if(role=='batter')'p_throws'else'stand'
      x[,n:=fifelse(is.finite(mean_exit_velocity_mph)&is.finite(tracked_balls_in_play),tracked_balls_in_play,0)]
      x[,total:=fifelse(n>0,n*mean_exit_velocity_mph,0)]
      league <- sum(x$total)/sum(x$n)
      overall <- x[,.(n=sum(n),total=sum(total)),by=role]
      overall[,ev:=(total+50*league)/(n+50)]
      parent <- overall$ev[match(x[[role]],overall[[role]])]
      list(overall=overall,split=data.table(split_key=paste(x[[role]],x[[hand]],x$pitch_family,sep='|'),
        n=x$n,ev=(x$total+30*parent)/(x$n+30)),league=league,hand=hand,history_year=year)
    }); names(ev) <- c('batter','pitcher')
    rates <- lapply(c('batter','pitcher'),function(role)setNames(lapply(c('barrel','gb'),function(metric)bp_rates(d,role,metric)),c('barrel','gb')))
    names(rates) <- names(ev)
    out[[as.character(year)]] <- list(ev=ev,rates=rates)
    rm(d,z);gc(FALSE)
  }
  bp_write(rbindlist(audits),'historical_tracking');out
}
bp_features <- function(d,prep,profiles,arm) {
  stopifnot(arm%in%c('A','B','C'),all(d$season%in%2022:2025))
  # pc_transform freezes all other player histories and preprocessing.
  ans <- pc_transform(d,prep); stopifnot(!any(ans$invalid)); f <- ans$data
  if(arm=='A') return(f)
  for(year in unique(d$season)) {
    ix <- which(d$season==year); h <- profiles[[as.character(year-1L)]]
    stopifnot(!is.null(h),all(vapply(h$ev,function(p)p$history_year,numeric(1))==year-1L))
    for(role in c('batter','pitcher')) {
      set(f,ix,paste0(role,'_ev_2021'),bp_lookup(d[ix],h$ev[[role]],role,'ev')$value)
      if(arm=='C')for(metric in c('barrel','gb'))
        set(f,ix,paste0(role,'_',metric),bp_lookup(d[ix],h$rates[[role]][[metric]],role,'rate')$value)
    }
  }; f
}
bp_history_audit <- function(d,profiles) {
  out <- data.table(profile_season=d$season-1L)
  for(role in c('batter','pitcher'))for(metric in c('ev','barrel','gb')) {
    v <- vector('list',length(unique(d$season)))
    for(year in unique(d$season)) {
      ix <- which(d$season==year);h <- profiles[[as.character(year-1L)]]
      p <- if(metric=='ev')h$ev[[role]]else h$rates[[role]][[metric]]
      z <- as.data.table(bp_lookup(d[ix],p,role,if(metric=='ev')'ev'else'rate')); z[,ix:=ix]
      v[[as.character(year)]] <- z
    }
    z <- rbindlist(v);setorder(z,ix)
    for(nm in c('overall_n','split_n','unseen','missing_split'))out[,(paste(role,metric,nm,sep='_')):=z[[nm]]]
  }
  out[,missing_history:=ifelse(batter_ev_overall_n==0|pitcher_ev_overall_n==0|
    batter_barrel_overall_n==0|pitcher_barrel_overall_n==0|batter_gb_overall_n==0|pitcher_gb_overall_n==0,'missing','available')]
  for(metric in c('ev','barrel','gb')) {
    n <- pmin(out[[paste0('batter_',metric,'_overall_n')]],out[[paste0('pitcher_',metric,'_overall_n')]])
    out[,(paste0(metric,'_exposure')):=as.character(cut(n,c(-1,0,29,99,299,Inf),labels=c('0','1-29','30-99','100-299','300+')))]
  }
  out
}
bp_fit <- function(d,prep,profiles,arm) {
  if(arm=='A') return(pc_fit(d,'value','gam',prep))
  stopifnot(all(d$season%in%2022:2023),all(d$target_status=='verified'))
  f <- bp_features(d,prep,profiles,arm); f[,y:=d$q_bip_observed]
  form <- as.formula(readRDS('data/models/post_contact/bip_value_fit_2022_2023.rds')$formula,env=globalenv())
  if(arm=='C')form <- update(form,.~.+batter_barrel+pitcher_barrel+batter_gb+pitcher_gb)
  warnings <- character();set.seed(20261004)
  fit <- withCallingHandlers(bam(form,data=f,family=quasipoisson(link='log'),method='fREML',
    discrete=TRUE,select=TRUE,nthreads=1,na.action=na.fail,control=gam.control(maxit=100)),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
  stopifnot(isTRUE(fit$converged),isTRUE(fit$mgcv.conv),all(is.finite(coef(fit))))
  list(model=fit,prep=prep,stage='value',kind='gam',arm=arm,n=nrow(d),train_years=2022:2023,
    formula=paste(deparse(form,width.cutoff=500),collapse=' '),warnings=unique(warnings))
}
bp_predict <- function(b,d,profiles,arm) {
  f <- bp_features(d,b$prep,profiles,arm)
  p <- unlist(lapply(split(seq_len(nrow(f)),ceiling(seq_len(nrow(f))/25000)),function(i)
    as.numeric(predict(b$model,newdata=f[i],type='response',discrete=FALSE))),use.names=FALSE)
  stopifnot(all(is.finite(p)),all(p>=0));p
}
bp_metrics <- function(y,p) data.table(n=length(y),rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),bias=mean(p-y))
bp_boot <- function(d,a,b,y,seed=BSEED) {
  g <- data.table(game=d$game_pk,ea=d[[a]]-d[[y]],eb=d[[b]]-d[[y]])[,
    .(n=.N,sa=sum(ea^2),sb=sum(eb^2),aa=sum(abs(ea)),ab=sum(abs(eb)),ba=sum(ea),bb=sum(eb)),by=game]
  calc <- function(i) {
    n <- sum(g$n[i]);c(rmse=sqrt(sum(g$sa[i])/n)-sqrt(sum(g$sb[i])/n),
      mae=(sum(g$aa[i])-sum(g$ab[i]))/n,bias=(sum(g$ba[i])-sum(g$bb[i]))/n)
  }
  set.seed(seed);draw <- replicate(500,calc(sample.int(nrow(g),nrow(g),TRUE)))
  data.table(metric=rownames(draw),difference=calc(seq_len(nrow(g))),
    ci_low=apply(draw,1,quantile,.025),ci_high=apply(draw,1,quantile,.975),games=nrow(g),replicates=500)
}
