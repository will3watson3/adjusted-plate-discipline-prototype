# Independent calculations from saved keyed predictions, not evaluation helpers.
source('experiments/take_xgboost_v1/predict.R')
checks <- list()
check <- function(name,ok) {
  checks[[length(checks)+1L]] <<- data.table(check=name,passed=isTRUE(ok))
  if(!isTRUE(ok)) {t_write(rbindlist(checks),'verification');stop(name)}
}
equal <- function(a,b,tol=1e-9) isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE))
freeze <- readRDS(file.path(TM,'frozen.rds')); vd_check_hash(freeze$model_hashes); vd_check_hash(freeze$code_hashes)
vd_check_hash(fread(file.path(TR,'input_hashes.csv')))
vd_check_hash(readRDS('data/models/current_hitter_stat.rds')$artifacts)
check('Frozen inputs, code, models and original active artifacts unchanged',TRUE)
dev <- readRDS(t_base_path(TRUE)); gam <- readRDS(t_base_path()); bundle <- readRDS(file.path(TM,'bundle.rds'))
dbundle <- readRDS(file.path(TM,'development_bundle.rds'))
check('Feature allowlist exactly equals actual GAM formula minus outcome and catcher',
  identical(bundle$schema$features,setdiff(all.vars(formula(gam$model)),c('y','catcher'))))
check('No extra backbone actor or same-pitch outcome information',
  identical(bundle$schema$features,c('count','stand','throws','x','z','family','speed','movx','movz','zone_height')) ||
  setequal(bundle$schema$features,c('count','stand','throws','x','z','family','speed','movx','movz','zone_height')))
selection <- fread(file.path(TR,'selection.csv')); setorder(selection,validation_log_loss,depth)
check('Selection only uses prescribed three depths and historical log loss',setequal(selection$depth,3:5)&&equal(selection[1],bundle$selection))
for(depth in 3:5) {
  curve <- fread(file.path(TR,paste0('development_depth_',depth,'.csv')))
  keep_depth <- selection$depth==depth; chosen <- selection[keep_depth]
  check(paste('Depth',depth,'round selection and cap reporting'),chosen$best_rounds==which.min(curve$validation_logloss)&&
    chosen$trained_rounds==nrow(curve)&&chosen$cap_reached==(nrow(curve)==1200)&&nrow(curve)<=1200)
}
history <- list()
for(year in 2022:2024) {
  d <- t_read(year,TRUE); history[[as.character(year)]] <- d
  if(year==2022L) check('Development preprocessing fitted only on 2022',equal(cs_preprocessor(d),dev$prep)&&equal(dbundle$prep,dev$prep))
  if(year==2023L) {
    train <- rbindlist(history)
    check('Final preprocessing fitted only on 2022-23',equal(cs_preprocessor(train),bundle$prep))
    check('Final training cohort exactly matches original GAM',identical(dv_key(train),dv_key(bundle$training_keys))&&nrow(train)==gam$train_n)
    f <- cs_transform(train,bundle$prep)$data; f[,y:=train$y]
    check('Every final GAM training feature and label matches',all(vapply(names(gam$model$model),function(n)identical(f[[n]],gam$model$model[[n]]),logical(1))))
    rm(train,f); gc(FALSE)
  }
}
for(phase in c('development','final')) for(year in if(phase=='development')2023L else 2024:2025) for(arm in c('gam','xgboost')) {
  b <- readRDS(file.path(TM,sprintf('%s_%s_actors_%d.rds',phase,arm,year)))
  tag <- paste(phase,year,arm)
  d <- history[[as.character(year-1L)]]; j <- match(dv_key(b$provenance$offset_keys),dv_key(d))
  check(paste(tag,'actor evidence is exactly the preceding eligible season'),!anyNA(j)&&length(j)==nrow(d)&&b$history_year==year-1L&&as.Date(b$train_end)<as.Date(sprintf('%d-01-01',year)))
  check(paste(tag,'same joint structure and converged fit'),setequal(names(b$groups),c('catcher','umpire','ump_region'))&&b$fit$converged&&b$fit$mgcv.conv)
  vd_check_hash(b$provenance$backbone)
  expected_backbone <- if(arm=='gam')t_base_path(phase=='development')else if(phase=='development')
    file.path(TM,paste0('development_depth_',bundle$selection$depth,'.ubj')) else file.path(TM,'take.ubj')
  check(paste(tag,'offset source is the mandated development or final fit'),identical(b$provenance$backbone$path,expected_backbone))
  ix <- unique(round(seq(1,nrow(d),length.out=250)))
  bb <- if(phase=='development')dbundle else bundle
  eta <- if(arm=='gam')fr_base(if(phase=='development')dev else gam,d[j[ix]],se=FALSE)$eta else
    t_eta(xgb.load(b$provenance$backbone$path),d[j[ix]],bb)
  check(paste(tag,'offsets reconstruct from correct historical backbone'),equal(eta,b$provenance$offsets[ix],1e-10))
  check(paste(tag,'preprocessing provenance'),equal(b$provenance$prep,bb$prep))
  ad <- t_actor_data(d,b$provenance$offsets,bb$prep)
  keep <- ad$valid&ad$take&is.finite(ad$base_eta)&!is.na(ad$catcher)&!is.na(ad$umpire)
  check(paste(tag,'fitted actor offsets and labels match provenance'),equal(b$fit$model[['offset(base_eta)']],ad$base_eta[keep])&&equal(b$fit$model$y,ad$y[keep]))
}
rm(history); gc(FALSE)
ind_prob <- function(y,p) {
  clipped <- pmin(1-1e-15,pmax(1e-15,p)); lp <- qlogis(pmin(1-1e-10,pmax(1e-10,p)))
  fit <- suppressWarnings(glm.fit(cbind(1,lp),y,family=binomial()))
  one <- suppressWarnings(glm.fit(matrix(1,length(y),1),y,offset=lp,family=binomial()))
  c(n=length(y),log_loss=-mean(y*log(clipped)+(1-y)*log1p(-clipped)),brier=mean((y-p)^2),bias=mean(p-y),
    calibration_intercept=one$coefficients[1],calibration_slope=fit$coefficients[2],joint_intercept=fit$coefficients[1])
}
ind_boot <- function(d,a,b,value) {
  y <- if(value)d$target else d$y
  ca <- pmin(1-1e-15,pmax(1e-15,a)); cb <- pmin(1-1e-15,pmax(1e-15,b))
  losses <- if(value)list(rmse=cbind((a-y)^2,(b-y)^2),mae=cbind(abs(a-y),abs(b-y)),bias=cbind(a-y,b-y)) else
    list(log_loss=cbind(-y*log(ca)-(1-y)*log1p(-ca),
      -y*log(cb)-(1-y)*log1p(-cb)),brier=cbind((a-y)^2,(b-y)^2),bias=cbind(a-y,b-y))
  rbindlist(lapply(names(losses),function(metric) {
    sums <- rowsum(cbind(n=1,losses[[metric]]),d$game_pk,reorder=FALSE)
    calc <- function(i) {v<-colSums(sums[i,,drop=FALSE]);v<-v[2:3]/v[1];if(metric=='rmse')v<-sqrt(v);v[1]-v[2]}
    set.seed(TSEED); draws <- replicate(500,calc(sample.int(nrow(sums),nrow(sums),replace=TRUE)))
    data.table(metric,difference=calc(seq_len(nrow(sums))),ci_low=unname(quantile(draws,.025)),ci_high=unname(quantile(draws,.975)))
  }))
}
tables <- setNames(lapply(c('metrics','values','boot','valueboot','bins','slices'),function(n)fread(file.path(TR,paste0(n,'.csv')))),c('metrics','values','boot','valueboot','bins','slices'))
select_rows <- function(tab,yr,pop,st,ar=NULL) {
  keep <- tab$year==yr & tab$population==pop & tab$stage==st
  if(!is.null(ar)) keep <- keep & tab$arm==ar
  tab[keep]
}
ref <- readRDS('data/models/hitter_stat_v6/season_reference_2024.rds'); re <- read_re_baseline()
states <- build_take_state_values(re)
for(year in 2024:2025) {
  message('Independent verification ',year)
  out <- fread(cmd=paste('gzip -dc',shQuote(file.path(TR,paste0('predictions_',year,'.csv.gz')))))
  old <- as.data.table(readRDS(sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',year)))
  raw <- t_read(year); j <- match(dv_key(out),dv_key(raw)); stopifnot(!anyNA(j))
  check(paste(year,'exact original pitch cohort and order'),identical(dv_key(out),dv_key(old))&&!anyDuplicated(out,by=CS_KEYS))
  check(paste(year,'ordinary-take cohort and validity parity'),identical(out$take,out$description%in%c('ball','blocked_ball','called_strike'))&&
    identical(out$valid,!cs_transform(raw[j],bundle$prep)$invalid&!cs_transform(raw[j],bundle$prep)$novel_category))
  check(paste(year,'original swing, policy and support fixed'),equal(out$q_swing,old$q_swing_basic)&&equal(out$reference,old$p_swing_reference)&&identical(out$supported,old$primary_supported))
  check(paste(year,'GAM control reproduces active probability and take value'),
    max(abs(out$complete_gam-old$p_called_strike_if_ordinary_take))<1e-6&&max(abs(out$q_take_gam-old$q_take_basic))<1e-6)
  check(paste(year,'actor history opportunities identical across arms'),
    identical(out$catcher_history_gam,out$catcher_history_xgboost)&&identical(out$umpire_history_gam,out$umpire_history_xgboost))
  check(paste(year,'all 34 RE ordering states remain excluded'),sum(states$state_value_order_warning)==34&&!any(out$supported&out$state_warning)&&identical(out$re_order_warning,old$re_order_warning))
  rz <- (raw$plate_z[j]-raw$sz_bot[j])/(raw$sz_top[j]-raw$sz_bot[j])
  region <- ifelse(rz<0,'below',ifelse(rz>1,'above',ifelse(abs(raw$plate_x[j])>.83,'side','inside_rectangle')))
  edge <- ifelse(pmin(abs(abs(raw$plate_x[j])-.83),abs(rz),abs(rz-1))<=.10,'near_edge','away_from_edge')
  check(paste(year,'slice labels reconstruct from raw inputs'),identical(out$region,region)&&identical(out$edge,edge)&&
    identical(out$count,paste(raw$balls[j],raw$strikes[j],sep='-'))&&identical(out$hands,paste(raw$stand[j],raw$p_throws[j],sep='/'))&&
    identical(out$pitch_family,raw$pitch_family[j]))
  # Independently reconstruct next observed pre-state and runs from the FULL sequence.
  seq <- copy(raw); setorderv(seq,CS_KEYS)
  seq[,pre:=paste0(as.integer(!is.na(on_1b)),as.integer(!is.na(on_2b)),as.integer(!is.na(on_3b)),'_',outs_when_up,'_',balls,'-',strikes)]
  seq[,`:=`(next_pre=shift(pre,type='lead'),next_inning=shift(inning,type='lead'),next_half=shift(inning_topbot,type='lead'),
    next_pa=shift(at_bat_number,type='lead'),next_pitch=shift(pitch_number,type='lead')),by=game_pk]
  s <- seq[match(dv_key(out),dv_key(seq))]; k <- match(out$re288_state,states$re288_state)
  ball <- out$y==0; end <- ifelse(ball,out$balls==3,out$strikes==2)
  expected <- ifelse(ball,states$ball_state[k],states$strike_state[k])
  same <- s$inning==s$next_inning&s$inning_topbot==s$next_half
  adjacent <- ifelse(end,s$next_pa>s$at_bat_number&s$next_pitch==1,s$next_pa==s$at_bat_number&s$next_pitch==s$pitch_number+1)
  good <- ifelse(expected=='INNING_END',!is.na(s$next_inning)&!same,same&adjacent&s$next_pre==expected)%in%TRUE &
    is.finite(s$post_bat_score-s$bat_score)&s$post_bat_score-s$bat_score==ifelse(ball,states$ball_runs[k],0)
  good <- out$take&good
  check(paste(year,'independent full-sequence factual target eligibility'),identical(is.finite(out$target),good))
  check(paste(year,'factual targets equal frozen branch values'),equal(out$target[good],ifelse(ball,states$ball_value[k],states$called_strike_value[k])[good]))
  check(paste(year,'saved RE ball and strike transitions'),equal(out$q_ball,states$ball_value[k])&&equal(out$q_strike,states$called_strike_value[k]))
  rm(seq,s); gc(FALSE)
  for(arm in c('gam','xgboost')) {
    tag <- paste(year,arm)
    for(stage in c('base','actor','complete')) check(paste(tag,stage,'valid probability'),all(is.finite(out[[paste(stage,arm,sep='_')]]))&&all(between(out[[paste(stage,arm,sep='_')]],0,1)))
    p <- out[[paste0('complete_',arm)]]; qt <- out$q_ball+p*(out$q_strike-out$q_ball)
    check(paste(tag,'take-value and Decision+ arithmetic'),equal(qt,out[[paste0('q_take_',arm)]])&&
      equal((as.integer(out$action=='swing')-out$reference)*(out$q_swing-qt),out[[paste0('score_',arm)]]))
    l <- fread(file.path(TR,sprintf('weekly_%s_%d.csv',arm,year)))
    anchor <- as.Date(sprintf('%d-01-01',year)); block <- anchor+7L*(as.integer(as.Date(out$game_date)-anchor)%/%7L)
    for(i in seq_len(nrow(l))) {
      ix <- which(out$valid&out$take&as.Date(out$game_date)<as.Date(l$start[i]))
      check(paste(tag,'weekly history',i),length(ix)==l$n[i]&&(length(ix)==0||as.Date(l$train_end[i])==max(as.Date(out$game_date[ix]))))
      lp <- qlogis(pmin(1-1e-8,pmax(1e-8,out[[paste0('actor_',arm)]][ix])))
      a <- if(length(ix)<2000)0 else uniroot(function(a)sum(plogis(lp+a)-out$y[ix]),c(-12,12),tol=1e-11)$root
      check(paste(tag,'independent calibration intercept',i),abs(a-l$intercept[i])<1e-8)
    }
    wk <- match(as.character(block),as.character(l$start))
    reconstructed <- plogis(qlogis(pmin(1-1e-8,pmax(1e-8,out[[paste0('actor_',arm)]])))+l$intercept[wk])
    check(paste(tag,'complete weekly replay reconstructs'),equal(reconstructed,p,1e-10))
    ix <- unique(round(seq(1,nrow(out),length.out=80))); d <- raw[j[ix]]
    pred <- predict_take_comparison(d,arm)
    if(arm=='xgboost') {
      native_booster <- xgb.load(file.path(TM,'take.ubj')); roundtrip <- tempfile(fileext='.ubj')
      xgb.save(native_booster,roundtrip)
      check(paste(tag,'native model serialization roundtrip'),identical(t_eta(native_booster,d,bundle),t_eta(xgb.load(roundtrip),d,bundle)))
      unlink(roundtrip)
    }
    check(paste(tag,'serialized full prediction parity'),equal(pred$complete,p[ix],1e-9))
    mutated <- copy(d); mutated[,`:=`(description='arbitrary',y=1-y,launch_speed=999,launch_angle=-999,events='home_run')]
    check(paste(tag,'same-pitch labels and outcomes cannot change predictions'),identical(pred,predict_take_comparison(mutated,arm)))
    check(paste(tag,'singleton prediction'),equal(pred[1],predict_take_comparison(d[1],arm),1e-10))
    check(paste(tag,'row-order invariance'),equal(pred,predict_take_comparison(d[.N:1],arm)[.N:1],1e-10))
    check(paste(tag,'batch partition invariance'),equal(pred,rbindlist(lapply(split(seq_len(nrow(d)),ceiling(seq_len(nrow(d))/19)),function(i)predict_take_comparison(d[i],arm))),1e-10))
    altered <- copy(d); altered[,fielder_2:=-999L]
    check(paste(tag,'catcher identity excluded from backbone'),equal(t_matrix(d,bundle$prep,bundle$schema),t_matrix(altered,bundle$prep,bundle$schema))&&
      equal(fr_base(gam,d,se=FALSE)$eta,fr_base(gam,altered,se=FALSE)$eta,1e-10))
    actor <- readRDS(file.path(TM,sprintf('final_%s_actors_%d.rds',arm,year)))
    for(group in names(actor$groups)) {
      tab <- actor$groups[[group]]$table
      n <- tab$opportunities[match(out[[group]],tab$id)]; n[is.na(n)] <- 0L
      check(paste(tag,group,'history availability'),identical(out[[paste0(group,'_history_',arm)]],n))
    }
    ad <- t_actor_data(d,out[[paste0('eta_',arm)]][ix],bundle$prep)
    known <- Reduce(`&`,lapply(names(actor$groups),function(g)ad[[g]]%in%actor$groups[[g]]$table$id))
    native <- mgcv::predict.gam(actor$fit,ad[known],type='link',se.fit=TRUE,unconditional=!is.null(actor$fit$Vc))
    custom <- u5_predict(actor,ad[known])
    check(paste(tag,'joint actor means and full posterior covariance match native fit'),
      max(abs(as.numeric(native$fit)-ad$base_eta[known]-custom$effect))<1e-8&&
      max(abs(as.numeric(native$se.fit)-custom$latent_sd))<1e-8)
    unseen <- copy(ad[1]); unseen[,`:=`(catcher='new',umpire='new',ump_region='new')]
    pp <- u5_predict(actor,unseen)
    X <- model.matrix(~region,data.frame(region=factor(unseen$region,levels=actor$region_levels)))
    fi <- actor$fixed_indices
    variance <- as.numeric(X%*%actor$V[fi,fi]%*%t(X))+sum(vapply(actor$groups,function(g)g$tau^2,numeric(1)))
    mu <- as.numeric(X%*%coef(actor$fit)[fi])
    integrated <- integrate(function(z)plogis(unseen$base_eta+mu+sqrt(variance)*z)*dnorm(z),-10,10,rel.tol=1e-10)$value
    check(paste(tag,'unseen population variance and uncertainty integration'),equal(pp$latent_sd^2,variance)&&equal(pp$effect,mu)&&abs(pp$p-integrated)<1e-8)
  }
  for(pop in c('all_takes','supported_takes')) for(stage in c('base','complete')) {
    z <- out[valid&take&if(pop=='supported_takes')supported else TRUE]
    tag <- paste(year,pop,stage)
    for(arm in c('gam','xgboost')) {
      p <- z[[paste(stage,arm,sep='_')]]
      m <- select_rows(tables$metrics,year,pop,stage,arm)
      expected <- ind_prob(z$y,p); names(expected) <- names(m)[match(c('n','log_loss','brier','bias','calibration_intercept','calibration_slope','joint_intercept'),names(m))]
      check(paste(tag,arm,'independent probability metrics'),equal(unlist(m[,names(expected),with=FALSE],use.names=FALSE),unname(expected)))
      b <- data.table(bin=pmin(19L,floor(p*20)),p,y=z$y)[,.(n=.N,predicted=mean(p),observed=mean(y)),by=bin]
      saved <- select_rows(tables$bins,year,pop,stage,arm)
      check(paste(tag,arm,'calibration bins'),equal(b,saved[,names(b),with=FALSE]))
      for(dim in c('count','region','edge','hands','pitch_family','catcher_history','umpire_history')) {
        v <- data.table(slice=z[[dim]],p,y=z$y)[,.(n=.N,log_loss=-mean(y*log(pmin(1-1e-15,pmax(1e-15,p)))+(1-y)*log1p(-pmin(1-1e-15,pmax(1e-15,p)))),brier=mean((p-y)^2),bias=mean(p-y)),by=slice]
        saved <- select_rows(tables$slices,year,pop,stage,arm)[dimension==dim]
        check(paste(tag,arm,dim,'independent slices'),equal(v,saved[,names(v),with=FALSE]))
      }
    }
    reconstructed <- ind_boot(z,z[[paste0(stage,'_xgboost')]],z[[paste0(stage,'_gam')]],FALSE)
    saved <- select_rows(tables$boot,year,pop,stage)
    check(paste(tag,'independent 500 paired probability intervals'),equal(reconstructed,saved[,names(reconstructed),with=FALSE]))
    z <- z[is.finite(target)]
    qa <- z$q_ball+z[[paste0(stage,'_xgboost')]]*(z$q_strike-z$q_ball)
    qb <- z$q_ball+z[[paste0(stage,'_gam')]]*(z$q_strike-z$q_ball)
    for(arm in c('gam','xgboost')) {
      e <- (if(arm=='gam')qb else qa)-z$target
      m <- select_rows(tables$values,year,pop,stage,arm)
      check(paste(tag,arm,'independent value metrics'),equal(unlist(m[,.(n,rmse,mae,bias)],use.names=FALSE),c(nrow(z),sqrt(mean(e^2)),mean(abs(e)),mean(e))))
    }
    reconstructed <- ind_boot(z,qa,qb,TRUE); saved <- select_rows(tables$valueboot,year,pop,stage)
    check(paste(tag,'independent 500 paired value intervals'),equal(reconstructed,saved[,names(reconstructed),with=FALSE]))
  }
  h <- fread(file.path(TR,paste0('hitters_',year,'.csv')))
  g <- out[supported==TRUE,.(n=.N,gam=sum(score_gam),xgboost=sum(score_xgboost)),by=.(batter,game_pk)]; setorder(g,batter,game_pk)
  cov <- out[,.(eligible=.N,supported=sum(supported),coverage=mean(supported)),by=batter]
  set.seed(TSEED)
  rebuilt <- g[,{
    draws <- replicate(500,{i<-sample.int(.N,.N,TRUE);100*c(sum(gam[i]),sum(xgboost[i]),sum(xgboost[i]-gam[i]))/sum(n[i])})
    .(games=.N,gam_runs100=100*sum(gam)/sum(n),xgboost_runs100=100*sum(xgboost)/sum(n),
      gam_low=unname(quantile(draws[1,],.025)),gam_high=unname(quantile(draws[1,],.975)),
      xgboost_low=unname(quantile(draws[2,],.025)),xgboost_high=unname(quantile(draws[2,],.975)),
      delta_low=unname(quantile(draws[3,],.025)),delta_high=unname(quantile(draws[3,],.975)))
  },by=batter]
  rebuilt <- merge(cov,rebuilt,by='batter',all.x=TRUE)
  check(paste(year,'hitter aggregation and fresh conditional intervals'),equal(rebuilt,h[,names(rebuilt),with=FALSE]))
  check(paste(year,'qualification'),identical(h$qualified,!is.na(h$games)&h$supported>=500&h$games>=60&h$coverage>=.7))
  active_hitters <- fread(sprintf('reports/hitter_stat_v6/hitters_%d.csv',year))
  check(paste(year,'qualified hitters exactly match active scorer'),setequal(h[qualified==TRUE,batter],active_hitters[qualified==TRUE,batter]))
  hk <- match(h$batter,active_hitters$batter)
  check(paste(year,'GAM Decision+ reproduces active grades'),max(abs(h$gam_decision_plus-active_hitters$decision_plus[hk]),na.rm=TRUE)<1e-4)
  for(arm in c('gam','xgboost')) check(paste(year,arm,'frozen reference scaling'),equal(h[[paste0(arm,'_decision_plus')]],100+10*(h[[paste0(arm,'_runs100')]]-ref$center)/ref$scale))
  check(paste(year,'no stale model uncertainty columns'),!any(grepl('model_se|model_only|envelope',names(h))))
  rm(raw,old,out); gc(FALSE)
}
vd_check_hash(fread(file.path(TR,'input_hashes.csv')))
vd_check_hash(readRDS(file.path(TR,'evaluation_manifest.rds'))$code_hashes)
check('Original artifacts and evaluation code unchanged after verification',TRUE)
t_write(rbindlist(checks),'verification')
message(length(checks),' independent checks passed')
