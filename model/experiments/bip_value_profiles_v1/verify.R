# Independent arithmetic reconstruction: does not call the experiment's profile
# builder, target builder, metric, bootstrap or hitter aggregation helpers.
source('experiments/bip_value_profiles_v1/functions.R')
checks <- list()
check <- function(name,ok) {
  if(!isTRUE(ok))stop('FAILED: ',name)
  checks[[name]] <<- data.table(check=name,passed=TRUE)
}
near <- function(a,b,tol=1e-10)isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE))
read_csv <- function(name)fread(file.path(BR,paste0(name,'.csv')))
read_gz <- function(path)fread(cmd=paste('gzip -dc',shQuote(path)))
freeze <- readRDS(file.path(BM,'frozen.rds'))
bp_check_hash(freeze$inputs);bp_check_hash(freeze$code);bp_check_hash(freeze$models)
bp_check_hash(readRDS(file.path(BM,'evaluation_code.rds')))
check('Original source, active artifacts, data and RE hashes unchanged',TRUE)
profiles <- readRDS(file.path(BM,'profiles.rds'))
independent_features <- function(d,prep,arm) {
  f <- pc_transform(d,prep)$data
  if(arm=='A')return(f)
  for(yr in unique(d$season))for(role in c('batter','pitcher')) {
    rows <- which(d$season==yr);h <- profiles[[as.character(yr-1L)]]
    for(metric in if(arm=='C')c('ev','barrel','gb')else'ev') {
      p <- if(metric=='ev')h$ev[[role]]else h$rates[[role]][[metric]]
      field <- if(metric=='ev')'ev'else'rate'
      hand <- if(role=='batter')'p_throws'else'stand'
      o <- match(d[[role]][rows],p$overall[[role]])
      k <- match(paste(d[[role]][rows],d[[hand]][rows],d$pitch_family[rows],sep='|'),p$split$split_key)
      v <- rep(p$league,length(rows));known <- !is.na(o);v[known] <- p$overall[[field]][o[known]]
      known <- !is.na(k);v[known] <- p$split[[field]][k[known]]
      set(f,rows,if(metric=='ev')paste0(role,'_ev_2021')else paste(role,metric,sep='_'),v)
    }
  };f
}
fits <- setNames(lapply(c('A','B','C'),function(a)readRDS(file.path(BM,paste0(a,'.rds')))),c('A','B','C'))
original <- readRDS('data/models/post_contact/bip_value_fit_2022_2023.rds')
check('Baseline refit coefficients reproduce active GAM',near(coef(fits$A$model),coef(original$model),1e-8))
check('A and B formulas identical',identical(fits$A$formula,fits$B$formula))
check('C adds exactly four linear additive terms',setequal(setdiff(attr(terms(as.formula(fits$C$formula)),'term.labels'),
  attr(terms(as.formula(fits$B$formula)),'term.labels')),c('batter_barrel','pitcher_barrel','batter_gb','pitcher_gb')))
check('All final models converged on identical target rows',all(vapply(fits,function(b)
  isTRUE(b$model$converged)&&isTRUE(b$model$mgcv.conv)&&identical(b$model$model$y,original$model$model$y),logical(1))))
common <- setdiff(names(original$model$model),c('batter_ev_2021','pitcher_ev_2021'))
for(a in c('A','B','C'))for(nm in common)check(paste(a,'unchanged training feature',nm),near(fits[[a]]$model$model[[nm]],original$model$model[[nm]]))
training <- readRDS(file.path(BM,'training_cohort.rds'))
check('Training dated 2022-23 only',all(training$season%in%2022:2023)&&max(training$game_date)<as.Date('2024-01-01'))
api <- rbindlist(list(fread('reports/post_contact/bip_post_states.csv'),fread('reports/decision_validation/bip_post_states_2025.csv')),fill=TRUE)
check('Post-play source has unique pitch keys',!anyDuplicated(api,by=CT_KEYS))
re <- read_re_baseline()$re288
# Reconstruct target eligibility and post-play RE directly from raw/API columns.
independent_targets <- function(raw) {
  z <- raw[analysis_eligible%in%TRUE & description=='hit_into_play' & ct_core_valid(raw)]
  j <- match(bp_key(z),bp_key(api));a <- api[j]
  occ <- function(x)as.integer(!is.na(x)&x!=0)
  runs <- z$post_bat_score-z$bat_score
  after <- paste0(occ(a$api_on_1b_after),occ(a$api_on_2b_after),occ(a$api_on_3b_after),'_',a$api_outs_after,'_0-0')
  before <- paste0(occ(z$on_1b),occ(z$on_2b),occ(z$on_3b),'_',z$outs_when_up,'_',z$balls,'-',z$strikes)
  ra <- re$re288[match(after,re$re288_state)];ra[a$api_outs_after==3L] <- 0
  rb <- re$re288[match(before,re$re288_state)]
  nb <- occ(z$on_1b)+occ(z$on_2b)+occ(z$on_3b)
  na <- occ(a$api_on_1b_after)+occ(a$api_on_2b_after)+occ(a$api_on_3b_after)
  outs <- a$api_outs_after-z$outs_when_up
  valid <- !is.na(j)&a$api_batter==z$batter&a$api_inning==z$inning&
    a$api_half==ifelse(z$inning_topbot=='Top','top','bottom')&a$api_event==z$events&
    a$api_post_bat_score==z$post_bat_score&a$api_later_runner_moves==0L&a$api_incomplete_final_half==0L&
    a$api_outs_after%in%0:3&outs%in%0:3&is.finite(runs)&runs>=0&runs==floor(runs)&runs<=nb+1&
    runs==a$api_scoring_runners&(a$api_outs_after==3|nb+1==na+runs+outs)&is.finite(ra)&is.finite(rb)
  valid[is.na(valid)] <- FALSE
  data.table(z[,..CT_KEYS],y=runs+ra)[valid]
}
tracking <- read_csv('historical_tracking')
for(year in 2021:2024) {
  message('Independently verify history ',year)
  raw <- bp_read(year);z <- raw[in_play%in%TRUE];h <- profiles[[as.character(year)]]
  # Check available raw-source labels by exact keys; cleaned tables retain them.
  files <- list.files('data/raw',pattern=paste0('^statcast_',year,'-.*[.]rds$'),full.names=TRUE)
  check(paste(year,'raw label source exists'),length(files)>0)
  source <- as.data.table(readRDS(files[1]));source <- unique(source[game_type=='R'&game_year==year],by=CT_KEYS)
  j <- match(bp_key(source),bp_key(raw));check(paste(year,'raw label keys match'),!anyNA(j))
  for(nm in c('launch_speed_angle','bb_type'))check(paste(year,'raw field preserved',nm),near(source[[nm]],raw[[nm]][j]))
  check(paste(year,'No current-season history'),all(vapply(c(h$ev,unlist(h$rates,recursive=FALSE)),function(p)p$history_year,numeric(1))==year))
  for(role in c('batter','pitcher'))for(metric in c('ev','barrel','gb')) {
    hand <- if(role=='batter')'p_throws'else'stand'
    n <- switch(metric,ev=as.numeric(is.finite(z$launch_speed)),barrel=as.numeric(z$launch_speed_angle%in%1:6),
      gb=as.numeric(z$bb_type%in%c('ground_ball','line_drive','fly_ball','popup')))
    total <- switch(metric,ev=ifelse(n>0,z$launch_speed,0),barrel=as.numeric(z$launch_speed_angle%in%6),gb=as.numeric(z$bb_type%in%'ground_ball'))
    zz <- data.table(id=z[[role]],hand=z[[hand]],family=z$pitch_family,n,total)
    split <- zz[,.(n=sum(n),total=sum(total)),by=.(id,hand,family)]
    overall <- zz[,.(n=sum(n),total=sum(total)),by=id]
    league <- sum(zz$total)/sum(zz$n)
    overall[,expected:=(total+50*league)/(n+50)]
    split[,expected:=(total+30*overall$expected[match(id,overall$id)])/(n+30)]
    p <- if(metric=='ev')h$ev[[role]]else h$rates[[role]][[metric]]
    value <- if(metric=='ev')'ev'else'rate'
    oi <- match(overall$id,p$overall[[role]])
    si <- match(paste(split$id,split$hand,split$family,sep='|'),p$split$split_key)
    label <- paste(year,role,metric)
    check(paste(label,'league denominator'),near(league,p$league))
    check(paste(label,'player counts and shrinkage'),!anyNA(oi)&&near(overall$n,p$overall$n[oi])&&near(overall$expected,p$overall[[value]][oi]))
    # Existing EV tables additionally retain zero-BIP splits; compare all observed BIP splits.
    check(paste(label,'split denominators and shrinkage'),!anyNA(si)&&near(split$n,p$split$n[si])&&near(split$expected,p$split[[value]][si]))
    test <- raw[1:3];test[,(role):=-999999L]
    check(paste(label,'unseen uses league'),all(bp_lookup(test,p,role,value)$value==p$league))
    test[,(role):=overall$id[which.max(overall$n)]];test[,pitch_family:='unseen_family']
    expected <- overall$expected[which.max(overall$n)]
    check(paste(label,'missing split uses player'),near(bp_lookup(test,p,role,value)$value,rep(expected,3)))
  }
  tt <- tracking[season==year]
  check(paste(year,'missing tracking excluded from denominators'),sum(tt$ev_n)==sum(is.finite(z$launch_speed))&&
    sum(tt$barrel_n)==sum(z$launch_speed_angle%in%1:6)&&sum(tt$gb_n)==sum(z$bb_type%in%c('ground_ball','line_drive','fly_ball','popup')))
  if(year%in%2022:2023) {
    independent <- independent_targets(raw);tr <- training[season==year]
    check(paste(year,'independent training target cohort'),identical(bp_key(independent),bp_key(tr)))
    check(paste(year,'independent training targets'),near(independent$y,tr$q_bip_observed))
    train_raw <- raw[match(bp_key(tr),bp_key(raw))]
    rows <- which(training$season==year)
    for(arm in c('B','C')) {
      expected <- independent_features(train_raw,fits[[arm]]$prep,arm)
      for(nm in c('batter_ev_2021','pitcher_ev_2021',if(arm=='C')c('batter_barrel','pitcher_barrel','batter_gb','pitcher_gb')))
        check(paste(year,arm,nm,'dated training join'),near(expected[[nm]],fits[[arm]]$model$model[[nm]][rows]))
    }
  }
  rm(raw,z);gc(FALSE)
}
metrics <- read_csv('metrics');boots <- read_csv('boot');bins <- read_csv('bins');slices <- read_csv('slices')
tails <- read_csv('tails');hitters <- read_csv('hitters');coverage_table <- read_csv('coverage')
ref <- readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
take_audit <- list()
for(yr in 2024:2025) {
  message('Independently verify evaluation ',yr)
  d <- read_gz(file.path(BR,paste0('predictions_',yr,'.csv.gz')))
  raw <- bp_read(yr);active <- as.data.table(readRDS(sprintf('data/models/hitter_stat_xgboost/decision_values_%d.rds',yr)))
  check(paste(yr,'keyed scorer cohort'),identical(bp_key(d),bp_key(active))&&!anyDuplicated(d,by=CT_KEYS))
  for(nm in c('primary_supported','support_reason','re_order_warning','re288_state',
    'p_contact_given_swing','p_in_play_given_contact','p_tip_given_nonbip','p_whiff_given_swing',
    'p_foul_given_swing','p_tip_given_swing','p_bip_given_swing','p_swing_reference',
    'q_take_basic','factual_q','factual_target_status','re_before'))
    check(paste(yr,'fixed component',nm),near(d[[nm]],active[[nm]]))
  check(paste(yr,'dated profile joins'),all(d$profile_season==yr-1L))
  independent <- independent_targets(raw);j <- match(bp_key(independent),bp_key(d))
  check(paste(yr,'keyed BIP cohort and target parity'),!anyNA(j)&&sum(is.finite(d$bip_y))==nrow(independent)&&near(independent$y,d$bip_y[j]))
  branch <- pc_branch_states();k <- match(d$re288_state,branch$re288_state)
  expected_y <- ifelse(d$description=='hit_into_play',d$bip_y,
    ifelse(d$description=='foul',branch$q_ordinary_foul[k],
      ifelse(d$description=='foul_tip',branch$q_foul_tip[k],branch$q_whiff[k])))
  factual <- d$observed_action=='swing'&is.finite(d$factual_q)
  check(paste(yr,'independent factual swing targets'),near(expected_y[factual],d$factual_q[factual]))
  j <- match(bp_key(d),bp_key(raw))
  ix <- unique(round(seq(1,nrow(d),length.out=100)))
  test <- copy(raw[j[ix]]);clean <- test[,unique(c(CT_KEYS,CT_INPUTS,'season','game_year','game_date')),with=FALSE]
  for(arm_name in c('A','B','C')) {
    p <- bp_predict(fits[[arm_name]],test,profiles,arm_name)
    independent_f <- independent_features(test,fits[[arm_name]]$prep,arm_name)
    independent_p <- as.numeric(predict(fits[[arm_name]]$model,newdata=independent_f,type='response',discrete=FALSE))
    check(paste(yr,arm_name,'independent dated feature joins and inference'),near(independent_p,p))
    outside <- rep(FALSE,nrow(test))
    for(nm in intersect(names(fits[[arm_name]]$ranges),c('batter_ev_2021','pitcher_ev_2021','batter_barrel','pitcher_barrel','batter_gb','pitcher_gb')))
      outside <- outside|independent_f[[nm]]<fits[[arm_name]]$ranges[[nm]][1]|independent_f[[nm]]>fits[[arm_name]]$ranges[[nm]][2]
    check(paste(yr,arm_name,'independent profile support flags'),identical(outside,d[[paste0('profile_outside_',arm_name)]][ix]))
    check(paste(yr,arm_name,'frozen prediction matches keys'),near(p,d[[paste0('q_bip_',arm_name)]][ix]))
    check(paste(yr,arm_name,'outcomes unnecessary for prediction'),near(p,bp_predict(fits[[arm_name]],clean,profiles,arm_name)))
    altered <- copy(test);altered[,`:=`(description='changed',launch_speed=-999,launch_angle=999,
      launch_speed_angle=6L,bb_type='ground_ball',events='changed',post_bat_score=999,q_bip_observed=999,runs_on_play=999)]
    check(paste(yr,arm_name,'same-pitch outcomes independent'),near(p,bp_predict(fits[[arm_name]],altered,profiles,arm_name)))
    check(paste(yr,arm_name,'single pitch and reversed order'),near(p[1],bp_predict(fits[[arm_name]],clean[1],profiles,arm_name))&&
      near(rev(p),bp_predict(fits[[arm_name]],clean[.N:1],profiles,arm_name)))
    branch <- pc_branch_states();k <- match(d$re288_state,branch$re288_state)
    qs <- d$p_bip_given_swing*d[[paste0('q_bip_',arm_name)]]+d$p_foul_given_swing*branch$q_ordinary_foul[k]+
      d$p_tip_given_swing*branch$q_foul_tip[k]+d$p_whiff_given_swing*branch$q_whiff[k]
    check(paste(yr,arm_name,'independent swing arithmetic'),near(qs,d[[paste0('q_swing_',arm_name)]]))
    score <- (as.numeric(d$observed_action=='swing')-d$p_swing_reference)*(qs-d$q_take_basic)
    check(paste(yr,arm_name,'independent score arithmetic'),near(score,d[[paste0('score_',arm_name)]]))
  }
  check(paste(yr,'active baseline prediction parity'),max(abs(d$q_bip_A-active$q_bip))<1e-8&&max(abs(d$q_swing_A-active$q_swing_basic))<1e-8)
  for(pair in list(c('B','A'),c('C','B')))check(paste(yr,paste(pair,collapse='-'),'swing delta identity'),
    near(d[[paste0('q_swing_',pair[1])]]-d[[paste0('q_swing_',pair[2])]],
      d$p_bip_given_swing*(d[[paste0('q_bip_',pair[1])]]-d[[paste0('q_bip_',pair[2])]])))
  for(pop in unique(metrics$population)) {
    is_bip <- startsWith(pop,'bip')
    z <- if(is_bip)d[is.finite(bip_y)]else d[primary_supported==TRUE&observed_action=='swing'&is.finite(factual_q)]
    if(pop=='bip_supported')z <- z[primary_supported==TRUE]
    if(grepl('common_inrange',pop))z <- z[common_inrange==TRUE]
    y <- if(is_bip)z$bip_y else z$factual_q
    for(arm_name in c('A','B','C')) {
      p <- z[[paste0(if(is_bip)'q_bip_'else'q_swing_',arm_name)]];e <- p-y
      m <- metrics[metrics$year==yr&population==pop&metrics$arm==arm_name]
      check(paste(yr,pop,arm_name,'metrics'),m$n==length(e)&&near(unlist(m[,.(rmse,mae,bias)]),c(sqrt(mean(e^2)),mean(abs(e)),mean(e))))
      if(pop%in%c('bip_all','swing_supported')) {
        bin <- as.character(cut(p,c(-Inf,0,.25,.5,.75,1,1.5,2,3,5,Inf),right=FALSE))
        reconstructed <- data.table(bin,p,y)[,.(n=.N,predicted=mean(p),observed=mean(y),bias=mean(p-y)),by=bin]
        saved <- bins[bins$year==yr&population==pop&bins$arm==arm_name];k <- match(reconstructed$bin,saved$bin)
        check(paste(yr,pop,arm_name,'calibration bins'),!anyNA(k)&&near(reconstructed[,-1],saved[k,names(reconstructed)[-1],with=FALSE]))
        for(dim in unique(slices$dimension)) {
          r <- data.table(slice=z[[dim]],e)[,.(n=.N,rmse=sqrt(mean(e^2)),mae=mean(abs(e)),bias=mean(e)),by=slice]
          s <- slices[slices$year==yr&population==pop&slices$arm==arm_name&dimension==dim];k <- match(r$slice,s$slice)
          check(paste(yr,pop,arm_name,dim,'slice metrics'),!anyNA(k)&&near(r[,-1],s[k,names(r)[-1],with=FALSE]))
        }
      }
    }
    # Independent raw-error bootstrap, with one shared sample of complete games.
    for(pair in list(c('B','A'),c('C','B'))) {
      prefix <- if(is_bip)'q_bip_'else'q_swing_'
      ea <- z[[paste0(prefix,pair[1])]]-y;eb <- z[[paste0(prefix,pair[2])]]-y
      g <- data.table(game=z$game_pk,ea,eb)[,.(n=.N,a2=sum(ea^2),b2=sum(eb^2),
        a1=sum(abs(ea)),b1=sum(abs(eb)),a0=sum(ea),b0=sum(eb)),by=game]
      set.seed(BSEED)
      draw <- replicate(500,{
        k <- sample.int(nrow(g),nrow(g),replace=TRUE);n <- sum(g$n[k])
        c(sqrt(sum(g$a2[k])/n)-sqrt(sum(g$b2[k])/n),sum(g$a1[k]-g$b1[k])/n,sum(g$a0[k]-g$b0[k])/n)
      })
      estimate <- c(sqrt(mean(ea^2))-sqrt(mean(eb^2)),mean(abs(ea))-mean(abs(eb)),mean(ea-eb))
      b <- boots[boots$year==yr&population==pop&comparison==paste(pair,collapse='-')][match(c('rmse','mae','bias'),metric)]
      check(paste(yr,pop,paste(pair,collapse='-'),'500 game intervals'),near(b$difference,estimate)&&
        near(b$ci_low,apply(draw,1,quantile,.025))&&near(b$ci_high,apply(draw,1,quantile,.975))&&all(b$replicates==500))
    }
  }
  g <- d[primary_supported==TRUE,.(n=.N,A=sum(score_A),B=sum(score_B),C=sum(score_C)),by=.(batter,game_pk)]
  setorder(g,batter,game_pk)
  hc <- d[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=batter]
  for(pair in list(c('B','A'),c('C','B'),c('C','A'))) {
    h <- hitters[hitters$year==yr&comparison==paste(pair,collapse='-')]
    set.seed(BSEED+yr)
    r <- g[,{
      a <- get(pair[1]);b <- get(pair[2]);dd <- a-b
      draw <- replicate(500,{k<-sample.int(.N,.N,TRUE);100*sum(dd[k])/sum(n[k])})
      .(games=.N,rate_a=100*sum(a)/sum(n),rate_b=100*sum(b)/sum(n),lo=quantile(draw,.025),hi=quantile(draw,.975))
    },by=batter]
    k <- match(r$batter,h$batter)
    hk <- match(h$batter,hc$batter)
    check(paste(yr,paste(pair,collapse='-'),'hitter coverage'),near(h$eligible,hc$eligible[hk])&&
      near(h$supported,hc$supported[hk])&&near(h$coverage,hc$coverage[hk]))
    check(paste(yr,paste(pair,collapse='-'),'hitter aggregation and fresh intervals'),
      near(r$rate_a,h$rate_a[k])&&near(r$rate_b,h$rate_b[k])&&near(r$lo,h$delta_low[k])&&near(r$hi,h$delta_high[k]))
    check(paste(yr,paste(pair,collapse='-'),'qualification and frozen scale'),
      identical(h$qualified,!is.na(h$games)&h$supported>=500&h$games>=60&h$coverage>=.7)&&
      near(h$delta_plus,10*(h$rate_a-h$rate_b)/ref$scale)&&near(h$decision_plus_a,100+10*(h$rate_a-ref$center)/ref$scale)&&
      near(h$delta_plus_low,10*h$delta_low/ref$scale)&&near(h$delta_plus_high,10*h$delta_high/ref$scale))
    largest <- read_csv(paste0('largest_changes_',yr,'_',pair[1],'_',pair[2]))
    expected <- h[qualified==TRUE][order(-abs(delta_plus))][1:15]
    check(paste(yr,paste(pair,collapse='-'),'reported largest hitter movements'),near(largest,expected))
  }
  cov <- coverage_table[year==yr]
  check(paste(yr,'coverage reconstruction'),cov$eligible==nrow(d)&&cov$supported==sum(d$primary_supported)&&
    near(cov$coverage,mean(d$primary_supported))&&cov$additional_exclusions==0)
  for(group_name in unique(tails$group))for(arm_name in c('A','B','C')) {
    z <- switch(group_name,factual_bip=d[is.finite(bip_y)],supported_swing=d[primary_supported==TRUE&observed_action=='swing'&is.finite(factual_q)],
      observed_take_audit_only=d[observed_action=='take'])
    p <- z[[paste0('q_bip_',arm_name)]];s <- tails[tails$year==yr&tails$arm==arm_name&tails$group==group_name]
    check(paste(yr,group_name,arm_name,'prediction tails'),s$n==nrow(z)&&near(c(s$min,s$p50,s$p95,s$p99,s$p999,s$max),
      c(min(p),quantile(p,c(.5,.95,.99,.999)),max(p)))&&s$q_over5==sum(p>5)&&s$q_over10==sum(p>10))
  }
  z <- d[primary_supported==TRUE&observed_action=='take']
  for(arm_name in c('A','B','C'))take_audit[[paste(yr,arm_name)]] <- data.table(year=yr,arm=arm_name,n=nrow(z),
    q_bip_max=max(z[[paste0('q_bip_',arm_name)]]),q_swing_max=max(z[[paste0('q_swing_',arm_name)]]),
    q_swing_p99=quantile(z[[paste0('q_swing_',arm_name)]],.99),
    profile_outside=sum(z[[paste0('profile_outside_',arm_name)]]))
  rm(d,raw,active);gc(FALSE)
}
bp_check_hash(freeze$inputs);bp_check_hash(freeze$models)
bp_write(rbindlist(checks),'verification')
bp_write(rbindlist(take_audit),'supported_take_audit')
saveRDS(bp_hash(file.path(BP,'verify.R')),file.path(BM,'verification_code.rds'))
cat(length(checks),'independent reconstruction checks passed\n')
