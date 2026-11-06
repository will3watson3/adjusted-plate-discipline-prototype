# Three contact-model variants; isolated experiment, never activates a scorer.
source('contact_models.R')
CP_REPORT <- 'reports/contact_personnel_comparison'
CP_MODELS <- 'data/models/contact_personnel_comparison'
CP_IDS <- c('current_2021_profiles','updated_prior_season_profiles','updated_profiles_personnel')

cp_profiles <- function(year) {
  stopifnot(length(year)==1L,year %in% 2021:2023)
  out <- lapply(c('batter','pitcher'),function(role) {
    hand <- if(role=='batter') 'p_throws' else 'stand'
    x <- fread(sprintf('data/processed/%s_splits_%d.csv',role,year))
    stopifnot(!anyDuplicated(x,by=c(role,hand,'pitch_family')),
      !anyNA(x[,.(swings,contacts,whiffs)]),all(x$swings>=0),
      all(x$contacts+x$whiffs==x$swings))
    league <- sum(x$contacts)/sum(x$swings)
    total <- x[,.(n=sum(swings),contacts=sum(contacts)),by=role]
    total[,rate:=(contacts+100*league)/(n+100)]
    parent <- total$rate[match(x[[role]],total[[role]])]
    split <- data.table(split_key=paste(x[[role]],x[[hand]],x$pitch_family,sep='|'),
      n=x$swings,rate=(x$contacts+50*parent)/(x$swings+50))
    list(overall=total,split=split,league=league,hand=hand,history_year=year)
  })
  names(out) <- c('batter','pitcher');out
}

# Every row uses a completed prior season, including historical training rows.
# Same-pitch labels are never accessed by this feature function.
cp_features <- function(d,prep,profiles) {
  stopifnot('season' %in% names(d),all(d$season %in% 2022:2024))
  t <- ct_transform(d,prep);f <- t$data
  if(any(t$invalid)) stop('Invalid core inputs')
  for(year in unique(d$season)) {
    ix <- which(d$season==year);p <- profiles[[as.character(year-1L)]]
    if(is.null(p)) stop('Missing prior-season profiles')
    stopifnot(all(vapply(p,function(z) z$history_year,numeric(1))==year-1L))
    fresh <- ct_raw_features(d[ix,..CT_INPUTS],p)
    for(nm in c('batter_history','pitcher_history','batter_history_n',
      'pitcher_history_n','batter_unseen','pitcher_unseen')) set(f,ix,nm,fresh[[nm]])
  }
  f
}

cp_fit_updated <- function(d,prep,profiles,baseline) {
  stopifnot(all(d$season %in% 2022:2023),max(d$game_date)<as.Date('2024-01-01'))
  f <- cp_features(d,prep,profiles);f[,y:=d$y]
  # Identical GAM formula and preprocessing to the adopted contact baseline.
  form <- formula(baseline$model);environment(form) <- globalenv()
  start <- proc.time()[[3]]
  fit <- bam(form,data=f,family=binomial(),method='fREML',discrete=TRUE,
    select=TRUE,nthreads=2,na.action=na.fail,control=gam.control(maxit=100))
  stopifnot(isTRUE(fit$converged),isTRUE(fit$mgcv.conv),all(is.finite(coef(fit))))
  list(model=fit,prep=prep,profiles=profiles,train_years=2022:2023,
    train_end=as.character(max(d$game_date)),seconds=proc.time()[[3]]-start)
}

cp_predict_updated <- function(bundle,d) {
  f <- cp_features(d,bundle$prep,bundle$profiles)
  unlist(lapply(split(seq_len(nrow(f)),ceiling(seq_len(nrow(f))/25000L)),function(i)
    as.numeric(predict(bundle$model,newdata=f[i],type='response',discrete=FALSE))),use.names=FALSE)
}

# A small second-stage mixed model keeps the updated GAM fixed. This is a
# residual personnel adjustment, not a joint refit of all GAM and player terms.
cp_fit_personnel <- function(d,p) {
  stopifnot(all(d$season %in% 2022:2023),length(p)==nrow(d),
    all(is.finite(p)),all(p>0 & p<1),!anyNA(d[,.(batter,pitcher)]))
  f <- data.frame(y=d$y,eta=qlogis(p),batter=factor(d$batter),pitcher=factor(d$pitcher))
  fit <- lme4::glmer(y~1+offset(eta)+(1|batter)+(1|pitcher),data=f,
    family=binomial(),nAGQ=0,
    control=lme4::glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=10000)))
  if(length(fit@optinfo$conv$lme4$messages)) stop(paste(fit@optinfo$conv$lme4$messages,collapse='; '))
  stopifnot(all(is.finite(lme4::fixef(fit))))
  list(model=fit,train_end=as.character(max(d$game_date)),
    method='fixed_updated_GAM_offset_plus_partially_pooled_random_intercepts',
    singular=lme4::isSingular(fit))
}

cp_predict_personnel <- function(bundle,d,p) {
  stopifnot(length(p)==nrow(d),all(is.finite(p)),all(p>0 & p<1))
  f <- data.frame(eta=qlogis(p),batter=as.character(d$batter),pitcher=as.character(d$pitcher))
  # New identities receive zero random intercept, conditional on population mean.
  # This experiment reports probability metrics, not predictive uncertainty.
  as.numeric(predict(bundle$model,newdata=f,type='response',allow.new.levels=TRUE))
}

cp_bootstrap <- function(d,p,reference,B=500L) {
  z <- data.table(game=d$game_pk,delta=cs_loss(d$y,p)-cs_loss(d$y,reference))
  g <- z[,.(n=.N,total=sum(delta)),by=game];stopifnot(nrow(g)>1L)
  set.seed(20261102)
  draws <- replicate(B,{i<-sample.int(nrow(g),nrow(g),TRUE);sum(g$total[i])/sum(g$n[i])})
  data.table(log_loss_difference=mean(z$delta),ci_low=unname(quantile(draws,.025)),
    ci_high=unname(quantile(draws,.975)),games=nrow(g),bootstrap_replicates=B)
}

cp_run <- function() {
  dir.create(CP_REPORT,recursive=TRUE,showWarnings=FALSE)
  dir.create(CP_MODELS,recursive=TRUE,showWarnings=FALSE)
  setDTthreads(2)
  protected <- c('data/models/decision_validation/league_policy_2022_2023.rds',
    'data/models/current_hitter_stat.rds')
  baseline_path <- 'data/models/contact/contact_gam_fit_2022_2023.rds'
  required <- c(protected,baseline_path,sprintf('data/processed/statcast_%d_clean.rds',2022:2024),
    unlist(lapply(c('batter','pitcher'),function(role) sprintf('data/processed/%s_splits_%d.csv',role,2021:2023))))
  absent <- required[!file.exists(required)]
  if(length(absent)) stop('Required files are not downloaded locally: ',paste(absent,collapse=', '))
  fingerprints <- data.table(path=c(required,'compare_contact_personnel.R','contact_models.R','called_strike_models.R'),
    md5=unname(tools::md5sum(c(required,'compare_contact_personnel.R','contact_models.R','called_strike_models.R'))))
  fwrite(fingerprints,file.path(CP_REPORT,'input_hashes.csv'))
  writeLines(c('Exploratory 2024 contact-model comparison; no scorer activation or league-policy refit.',
    'All variants use the identical ordinary-swing population and fixed existing GAM formula.',
    'Model 1: saved 2022-2023 contact GAM, frozen 2021 profiles.',
    'Model 2: same GAM fit on 2022-2023, each row uses previous-season profiles.',
    'Model 3: fixed model-2 logit offset plus intercept and batter/pitcher random intercepts.',
    'Model 3 uses lme4 binomial glmer, nAGQ=0 (approximate fit); no joint GAM refit.',
    'Profile shrinkage unchanged: 100 swings to league, 50 split swings to player.',
    'Training: 2022 profiles=2021; 2023 profiles=2022. Evaluation: 2024 profiles=2023.',
    'No tuning or recalibration on evaluation outcomes; 2024 is previously inspected exploratory replay.',
    'New identities get zero conditional random intercept; no uncertainty intervals for individual predictions.',
    'Paired log-loss intervals resample whole games, conditional on fitted models; 500 replicates.',
    'League reference and current active manifest must retain identical MD5 hashes.'),file.path(CP_REPORT,'PROTOCOL.txt'))
  message('Read training swings')
  train <- rbindlist(lapply(2022:2023,ct_read,report_dir=CP_REPORT))
  baseline <- readRDS(baseline_path)
  stopifnot(identical(sort(as.integer(baseline$train_years)),2022:2023),
    baseline$train_n==nrow(train),baseline$id=='gam_history')
  profiles <- setNames(lapply(2021:2023,cp_profiles),as.character(2021:2023))
  message('Fit model 2: updated prior-season profiles')
  updated <- cp_fit_updated(train,baseline$prep,profiles,baseline)
  saveRDS(updated,file.path(CP_MODELS,'updated_profiles.rds'),compress=FALSE)
  ptrain <- cp_predict_updated(updated,train)
  message('Fit model 3: residual batter/pitcher effects')
  personnel <- cp_fit_personnel(train,ptrain)
  saveRDS(personnel,file.path(CP_MODELS,'personnel.rds'),compress=FALSE)
  rm(ptrain);gc(verbose=FALSE)
  message('Evaluate all three on identical 2024 swings')
  val <- ct_read(2024L,report_dir=CP_REPORT)
  predictions <- list(predict_contact(baseline,val)$p_contact_given_swing,
    cp_predict_updated(updated,val))
  predictions[[3]] <- cp_predict_personnel(personnel,val,predictions[[2]])
  names(predictions) <- CP_IDS
  metrics <- rbindlist(lapply(CP_IDS,function(id) cbind(data.table(model=id),cs_metrics(val$y,predictions[[id]]))))
  fwrite(metrics,file.path(CP_REPORT,'metrics_2024.csv'))
  comparisons <- rbindlist(lapply(list(c(2,1),c(3,1),c(3,2)),function(pair)
    cbind(data.table(model=CP_IDS[pair[1]],reference=CP_IDS[pair[2]]),
      cp_bootstrap(val,predictions[[pair[1]]],predictions[[pair[2]]]))))
  fwrite(comparisons,file.path(CP_REPORT,'paired_game_bootstrap_2024.csv'))
  out <- val[,c(CT_KEYS,'game_date','batter','pitcher','y'),with=FALSE]
  for(id in CP_IDS) out[,(id):=predictions[[id]]]
  fwrite(out,file.path(CP_REPORT,'predictions_2024.csv.gz'))
  re <- lme4::ranef(personnel$model)
  groups <- list(all=rep('all',nrow(val)),
    batter_identity=ifelse(as.character(val$batter)%in%rownames(re$batter),'seen','new'),
    pitcher_identity=ifelse(as.character(val$pitcher)%in%rownames(re$pitcher),'seen','new'),
    batter_prior_profile=ifelse(cp_features(val,updated$prep,profiles)$batter_unseen,'new','known'))
  slices <- rbindlist(lapply(names(groups),function(dim) rbindlist(lapply(CP_IDS,function(id)
    data.table(slice=groups[[dim]],y=val$y,p=predictions[[id]])[,.(n=.N,
      log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),bias=mean(p-y)),by=slice][,`:=`(dimension=dim,model=id)]))))
  fwrite(slices,file.path(CP_REPORT,'slices_2024.csv'))
  calibration <- rbindlist(lapply(CP_IDS,function(id)
    data.table(y=val$y,p=predictions[[id]])[,bin:=pmin(19L,floor(p*20))][,
      .(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,model:=id]))
  fwrite(calibration,file.path(CP_REPORT,'calibration_2024.csv'))
  # Verify serialized prediction, label independence and ordering on real rows.
  ix <- unique(round(seq(1,nrow(val),length.out=30)))
  d <- copy(val[ix]);d[,`:=`(y=1L-y,description='changed_label',launch_speed=999)]
  p2 <- cp_predict_updated(readRDS(file.path(CP_MODELS,'updated_profiles.rds')),d)
  p3 <- cp_predict_personnel(readRDS(file.path(CP_MODELS,'personnel.rds')),d,p2)
  stopifnot(max(abs(p2-predictions[[2]][ix]))<1e-10,
    max(abs(p3-predictions[[3]][ix]))<1e-10,
    max(abs(rev(cp_predict_updated(updated,d[nrow(d):1]))-p2))<1e-10,
    !anyDuplicated(out,by=CT_KEYS),all(vapply(predictions,function(p) all(is.finite(p)&p>=0&p<=1),logical(1))),
    identical(fingerprints$md5,unname(tools::md5sum(fingerprints$path))))
  fwrite(data.table(check=c('unique_pitch_keys','finite_probabilities','same_pitch_labels_ignored',
    'serialized_predictions','row_order_invariance','all_inputs_unchanged_including_league_reference'),passed=TRUE),
    file.path(CP_REPORT,'verification.csv'))
  saveRDS(list(train_years=2022:2023,evaluation_year=2024L,models=CP_IDS,
    active_scorer_changed=FALSE,league_reference_changed=FALSE,
    personnel_method=personnel$method,personnel_singular=personnel$singular,
    inputs=fingerprints),file.path(CP_MODELS,'manifest.rds'))
  lines <- c('# Contact personnel comparison','',
    'Exploratory comparison on identical 2024 ordinary swings. Lower log loss/Brier is better.',
    'The league reference and active Decision+ scorer retain their original hashes.','',
    '| Model | N | Log loss | Brier | AUC | Calibration slope |',
    '| --- | ---: | ---: | ---: | ---: | ---: |',
    sprintf('| %s | %d | %.6f | %.6f | %.6f | %.3f |',metrics$model,metrics$n,
      metrics$log_loss,metrics$brier,metrics$auc,metrics$calibration_slope),'',
    'See PROTOCOL.txt for chronology, fixed shrinkage and the two-stage approximate personnel fit.',
    'paired_game_bootstrap_2024.csv contains conditional game-bootstrap differences; negative favors the candidate.',
    'These are contact-probability results, not evidence of full Decision+ improvement or causal talent.',
    'Run: Rscript compare_contact_personnel.R')
  writeLines(lines,file.path(CP_REPORT,'README.md'))
  print(metrics);print(comparisons);message('Comparison complete; frozen league reference verified unchanged.')
}

if(sys.nframe()==0L) cp_run()
