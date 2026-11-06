# Run from the repository root: Rscript experiments/hitter_region_v1/run.R
# Isolated exploratory replay. Never calls scorer activation or refits take/policy/RE.
source('compare_contact_personnel.R')
source('decision_validation.R')
HR_ROOT <- 'experiments/hitter_region_v1'
HR_REPORT <- file.path(HR_ROOT,'reports')
HR_MODELS <- file.path(HR_ROOT,'models')
HR_ZONES <- c(1:9,11:14)
HR_STAGES <- c('contact','bip','tip','value')
# macOS gzip is available; avoid requiring the optional R.utils package.
hr_fread <- function(path) if(endsWith(path,'.gz')) fread(cmd=paste('gzip -dc',shQuote(path))) else fread(path)
hr_region <- function(z) ifelse(!is.na(z)&z%in%HR_ZONES,as.character(z),'missing_invalid')
hr_group <- function(d) ifelse(hr_region(d$zone)=='missing_invalid'|is.na(d$batter),NA_character_,paste(d$batter,hr_region(d$zone),sep='|'))

# Reuse exactly the existing EV profile definition: 50 tracked BIP to league,
# 30 split tracked BIP to player. Only the source season changes.
hr_power <- function(year) {
  old <- PC_POWER_FILES;on.exit(assign('PC_POWER_FILES',old,envir=.GlobalEnv))
  assign('PC_POWER_FILES',setNames(sprintf('data/processed/%s_splits_%d.csv',c('batter','pitcher'),year),c('batter','pitcher')),envir=.GlobalEnv)
  p <- pc_power_profiles()
  lapply(p,function(x) {x$history_year<-year;x})
}
hr_features <- function(d,b,profiles,power) {
  if(b$stage=='contact') return(cp_features(d,b$prep,profiles))
  stopifnot(all(d$season%in%2022:2024))
  ans <- vector('list',length(unique(d$season)));names(ans)<-unique(d$season)
  for(yr in unique(d$season)) {
    ix<-which(d$season==yr);pp<-b$prep
    pp$profiles<-profiles[[as.character(yr-1L)]];pp$power<-power[[as.character(yr-1L)]]
    stopifnot(all(vapply(c(pp$profiles,pp$power),function(x)x$history_year,numeric(1))==yr-1L))
    t<-pc_transform(d[ix],pp);stopifnot(!any(t$invalid));f<-t$data;f[,row_index:=ix]
    ans[[as.character(yr)]]<-f
  }
  f<-rbindlist(ans);setorder(f,row_index);f[,row_index:=NULL];f
}
hr_read <- function(year) {
  d<-as.data.table(readRDS(sprintf('data/processed/statcast_%d_clean.rds',year)))
  stopifnot(!anyNA(d[,..CT_KEYS]),!anyDuplicated(d,by=CT_KEYS),all(d$game_year==year),all(format(d$game_date,'%Y')==as.character(year)))
  audit<-d[,.(n=.N,eligible=sum(analysis_eligible%in%TRUE),mean_x=mean(plate_x,na.rm=TRUE),
    mean_z_normalized=mean((plate_z-sz_bot)/(sz_top-sz_bot),na.rm=TRUE)),by=.(zone,region=hr_region(zone))]
  audit[,season:=year];fwrite(audit,file.path(HR_REPORT,sprintf('zone_audit_%d.csv',year)))
  cols<-unique(c(CT_KEYS,CT_INPUTS,'zone','description','events','game_date','game_year','inning_topbot','post_bat_score','batter_name','pitcher_name'))
  out<-copy(d[analysis_eligible%in%TRUE & ct_core_valid(d),..cols]);out[,season:=year]
  stopifnot(all(out$description%chin%c(CT_SWINGS,DV_TAKES)))
  out[,re288_state:=pc_state(out)];setorderv(out,c('game_date',CT_KEYS));out
}
hr_stage_data <- function(d,stage,api,re) {
  z<-switch(stage,contact=copy(d[description%chin%CT_SWINGS]),
    bip=copy(d[description%chin%c('hit_into_play','foul','foul_tip')]),
    tip=copy(d[description%chin%c('foul','foul_tip')]),
    value=pc_bip_targets(d,api,re))
  if(stage=='value') {
    fwrite(z[,.(n=.N),by=.(season,target_status)],file.path(HR_REPORT,paste0('bip_targets_',min(d$season),'_',max(d$season),'.csv')))
    z<-z[target_status=='verified']
  }
  z[,y:=switch(stage,contact=as.integer(description!='swinging_strike' & description!='swinging_strike_blocked'),
    bip=as.integer(description=='hit_into_play'),tip=as.integer(description=='foul_tip'),value=q_bip_observed)]
  stopifnot(!anyDuplicated(z,by=CT_KEYS),all(is.finite(z$y)));z
}
hr_eta <- function(model,f) unlist(lapply(split(seq_len(nrow(f)),ceiling(seq_len(nrow(f))/15000L)),function(i)
  as.numeric(predict(model,newdata=f[i],type='link',discrete=FALSE))),use.names=FALSE)

# Sparse scalar Laplace (quasi-)marginal likelihood. No global intercept or
# additional player/pitcher term: eta = fixed updated GAM eta + u[hitter,zone].
# For quasi-Poisson, use Q=(y*eta-exp(eta))/phi with phi frozen from control GAM.
# Gaussian prior u~N(0,tau^2); maximize summed group Laplace integrals for tau.
# This is an approximate two-stage empirical-Bayes fit, not joint GAM REML.
hr_pool <- function(y,eta,group,family,phi=1) {
  keep<-!is.na(group);y<-y[keep];eta<-eta[keep];group<-group[keep]
  levels<-sort(unique(group));g<-match(group,levels);G<-length(levels)
  stopifnot(G>1,all(is.finite(eta)),all(is.finite(y)),is.finite(phi),phi>0)
  sums<-function(x) as.numeric(rowsum(x,factor(g,levels=seq_len(G)),reorder=TRUE))
  sy<-sums(y);n<-tabulate(g,G);value<-identical(family,'quasipoisson')
  base<-if(value) sums(exp(eta)) else NULL
  softplus<-function(x)pmax(x,0)+log1p(exp(-abs(x)))
  evaluate<-function(tau,details=FALSE) {
    if(tau==0) {
      ll<-if(value)sum((y*eta-exp(eta))/phi) else sum(y*eta-softplus(eta))
      return(list(objective=-ll,u=rep(0,G),curvature=rep(0,G),iterations=0L,gradient=0))
    }
    variance<-tau^2;u<-numeric(G);converged<-FALSE
    for(it in 1:100) {
      if(value) {mu<-base*exp(u);score<-(sy-mu)/phi;curv<-mu/phi} else {
        p<-plogis(eta+u[g]);score<-sy-sums(p);curv<-sums(p*(1-p))
      }
      grad<-score-u/variance;step<-grad/(curv+1/variance)
      # Safeguard Newton updates on very sparse groups.
      step<-pmax(-2,pmin(2,step));u<-u+step
      if(max(abs(step))<1e-9) {converged<-TRUE;break}
    }
    if(!converged)stop('Pooling conditional modes did not converge')
    if(value) {
      mu<-base*exp(u);curv<-mu/phi;score<-(sy-mu)/phi
      ll<-sum((y*(eta+u[g])-exp(eta+u[g]))/phi)
    } else {
      e<-eta+u[g];p<-plogis(e);curv<-sums(p*(1-p));score<-sy-sums(p)
      ll<-sum(y*e-softplus(e))
    }
    objective<--ll+sum(u^2)/(2*variance)+.5*sum(log1p(variance*curv))
    list(objective=objective,u=u,curvature=curv,iterations=it,gradient=max(abs(score-u/variance)))
  }
  # Broad numerical bounds, chosen without evaluation outcomes. Fail at upper bound.
  opt<-optimize(function(logtau)evaluate(exp(logtau))$objective,c(log(1e-4),log(4)),tol=1e-6)
  tau<-exp(opt$minimum);zero<-evaluate(0)
  if(zero$objective<=opt$objective)tau<-0
  stopifnot(tau<3.99);r<-evaluate(tau,TRUE)
  effects<-data.table(group=levels,n=n,effect=r$u,
    conditional_sd=if(tau==0)rep(0,G) else 1/sqrt(r$curvature+1/tau^2),
    information_fraction=if(tau==0)rep(0,G) else r$curvature/(r$curvature+1/tau^2))
  list(tau=tau,variance=tau^2,dispersion=phi,family=family,effects=effects,
    objective=r$objective,zero_objective=zero$objective,iterations=r$iterations,max_gradient=r$gradient,
    converged=is.finite(r$objective)&&r$gradient<1e-5,
    method='fixed_GAM_offset_sparse_Laplace_quasi_likelihood',n=length(y),excluded_missing_region=sum(!keep))
}
hr_adjust <- function(pool,d,eta) {
  g<-hr_group(d);k<-match(g,pool$effects$group);u<-pool$effects$effect[k];u[is.na(k)]<-0
  n<-pool$effects$n[k];n[is.na(k)]<-0L
  list(eta=eta+u,effect=u,n=n,fallback=ifelse(is.na(g),'missing_invalid_region_or_identity',ifelse(is.na(k),'unseen_hitter_region','seen')))
}
hr_metrics <- function(y,p,stage) {
  if(stage=='value')return(data.table(n=length(y),rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),mean_error=mean(p-y)))
  m<-cs_metrics(y,p)
  m[,.(n,log_loss,brier,mean_error=mean(p-y),calibration_intercept,calibration_slope)]
}
hr_boot <- function(d,stage,B=500L) {
  y<-d$y;a<-d$control;b<-d$regional
  loss<-if(stage=='value') list(squared=list((a-y)^2,(b-y)^2),mae=list(abs(a-y),abs(b-y)),mean_error=list(a-y,b-y)) else
    list(log_loss=list(cs_loss(y,a),cs_loss(y,b)),brier=list((a-y)^2,(b-y)^2),mean_error=list(a-y,b-y))
  rbindlist(lapply(names(loss),function(metric) {
    l<-loss[[metric]];g<-data.table(game=d$game_pk,a=l[[1]],b=l[[2]])[,.(n=.N,a=sum(a),b=sum(b)),by=game]
    calc<-function(i) {aa<-sum(g$a[i])/sum(g$n[i]);bb<-sum(g$b[i])/sum(g$n[i]);if(metric=='squared')sqrt(bb)-sqrt(aa) else bb-aa}
    set.seed(20261102);draw<-replicate(B,calc(sample.int(nrow(g),nrow(g),TRUE)))
    data.table(stage=stage,metric=ifelse(metric=='squared','rmse',metric),difference=calc(seq_len(nrow(g))),
      ci_low=unname(quantile(draw,.025)),ci_high=unname(quantile(draw,.975)),games=nrow(g),replicates=B)
  }))
}
hr_evaluate <- function(d,b,profiles,power) {
  f<-hr_features(d,b,profiles,power);eta<-hr_eta(b$model,f);adjust<-hr_adjust(b$pool,d,eta)
  inv<-b$model$family$linkinv;a<-inv(eta);p<-inv(adjust$eta)
  out<-d[,c(CT_KEYS,'game_date','batter','pitcher','zone','y'),with=FALSE]
  out[,`:=`(control=a,regional=p,region=hr_region(zone),history=ifelse(f$batter_unseen,'unavailable','available'),
    pitcher_history=ifelse(f$pitcher_unseen,'unavailable','available'),training_exposure=adjust$n,fallback=adjust$fallback,
    exposure_band=as.character(cut(adjust$n,c(-1,0,9,49,199,Inf),labels=c('0','1-9','10-49','50-199','200+'))))]
  if(b$stage=='value') {
    h<-rep(FALSE,nrow(d))
    for(yr in unique(d$season)) {ix<-which(d$season==yr);pr<-power[[as.character(yr-1L)]]$batter;h[ix]<-d$batter[ix]%in%pr$overall[n>0,batter]}
    out[,power_history:=ifelse(h,'available','unavailable')]
  }
  fwrite(out,file.path(HR_REPORT,paste0('predictions_',b$stage,'_2024.csv.gz')))
  metrics<-rbindlist(lapply(c('control','regional'),function(arm)cbind(data.table(stage=b$stage,arm=arm),hr_metrics(d$y,out[[arm]],b$stage))),fill=TRUE)
  dims<-c('region','history','pitcher_history','exposure_band','fallback',if(b$stage=='value')'power_history')
  slices<-rbindlist(lapply(dims,function(dim)rbindlist(lapply(c('control','regional'),function(arm) {
    z<-data.table(slice=out[[dim]],y=d$y,p=out[[arm]])
    if(b$stage=='value') tab<-z[,.(n=.N,rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),mean_error=mean(p-y)),by=slice] else
      tab<-z[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),mean_error=mean(p-y)),by=slice]
    tab[,`:=`(dimension=dim,arm=arm,stage=b$stage)]
  }))),fill=TRUE)
  if(b$stage!='value') {
    cal<-rbindlist(lapply(c('control','regional'),function(arm)data.table(y=d$y,p=out[[arm]])[,bin:=pmin(19L,floor(p*20))][,
      .(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,`:=`(stage=b$stage,arm=arm)]))
    fwrite(cal,file.path(HR_REPORT,paste0('calibration_',b$stage,'_2024.csv')))
  }
  # Serialized bundles, row order, and same-pitch labels do not change predictions.
  ix<-unique(round(seq(1,nrow(d),length.out=40)));dd<-copy(d[ix]);dd[,`:=`(y=-999,description='corrupted_label',launch_speed=999,launch_angle=999)]
  reload<-readRDS(file.path(HR_MODELS,paste0(b$stage,'.rds')))
  ee<-hr_eta(reload$model,hr_features(dd,reload,profiles,power));rr<-hr_adjust(reload$pool,dd,ee)
  stopifnot(max(abs(inv(ee)-a[ix]))<1e-9,max(abs(inv(rr$eta)-p[ix]))<1e-9,
    max(abs(rev(hr_eta(b$model,hr_features(dd[nrow(dd):1],b,profiles,power)))-ee))<1e-9,
    all(is.finite(p)),all(p>=0),b$stage=='value'||all(p<=1))
  unknown<-copy(dd);unknown[,batter:=-999L];stopifnot(all(hr_adjust(b$pool,unknown,ee)$effect==0))
  unknown<-copy(dd);unknown[,zone:=10];stopifnot(all(hr_adjust(b$pool,unknown,ee)$effect==0))
  list(metrics=metrics,slices=slices,bootstrap=hr_boot(out,b$stage))
}

hr_run <- function() {
  setDTthreads(2);dir.create(HR_MODELS,recursive=TRUE,showWarnings=FALSE);dir.create(HR_REPORT,recursive=TRUE,showWarnings=FALSE)
  sources<-c('compare_contact_personnel.R','contact_models.R','post_contact_models.R','current_hitter_stat.R','hitter_stat_v6.R','decision_validation.R','score_decisions.R','take_value_models.R','called_strike_models.R','run_expectancy_models.R','attach_run_expectancy.R','re288_order_flags.R')
  re<-read_re_baseline()
  paths<-unique(c(sources,re$files$file,sprintf('data/processed/statcast_%d_clean.rds',2022:2024),
    unlist(lapply(c('batter','pitcher'),function(role)sprintf('data/processed/%s_splits_%d.csv',role,2021:2023))),
    'data/models/contact/contact_gam_fit_2022_2023.rds','data/models/post_contact/post_contact_fit_2022_2023.rds',
    'data/models/post_contact/bip_value_fit_2022_2023.rds','reports/post_contact/bip_post_states.csv',
    'data/models/current_hitter_stat.rds','data/models/hitter_stat_v6/decision_values_2024.rds',
    'data/models/hitter_stat_v6/season_reference_2024.rds','data/models/decision_validation/league_policy_2022_2023.rds',
    'data/models/decision_validation/support_contract.rds'))
  stopifnot(all(file.exists(paths)));hash<-vd_hash(paths);vd_check_hash(hash)
  if(file.exists(file.path(HR_REPORT,'input_hashes.csv')))stopifnot(identical(hash,fread(file.path(HR_REPORT,'input_hashes.csv'))))
  fwrite(hash,file.path(HR_REPORT,'input_hashes.csv'))
  file.copy(sources,file.path(HR_ROOT,'source'),overwrite=FALSE)
  capture.output(sessionInfo(),file=file.path(HR_REPORT,'sessionInfo.txt'))
  profiles<-setNames(lapply(2021:2023,cp_profiles),2021:2023);power<-setNames(lapply(2021:2023,hr_power),2021:2023)
  saveRDS(list(contact=profiles,power=power),file.path(HR_MODELS,'histories.rds'))
  api<-fread('reports/post_contact/bip_post_states.csv')
  need_fit<-any(!file.exists(file.path(HR_MODELS,paste0(HR_STAGES,'.rds'))))
  if(need_fit) {
    message('Read 2022-2023 training data')
    train<-rbindlist(lapply(2022:2023,hr_read))
    post<-readRDS('data/models/post_contact/post_contact_fit_2022_2023.rds')
    originals<-list(contact=readRDS('data/models/contact/contact_gam_fit_2022_2023.rds'),bip=post$bip,tip=post$tip,
      value=readRDS('data/models/post_contact/bip_value_fit_2022_2023.rds'));rm(post)
  }
  diagnostics<-list()
  for(stage in HR_STAGES) {
    path<-file.path(HR_MODELS,paste0(stage,'.rds'))
    if(file.exists(path)) {message('Resume fitted ',stage);next}
    d<-hr_stage_data(train,stage,api,re);b<-originals[[stage]];b$stage<-stage
    f<-hr_features(d,b,profiles,power);f[,y:=d$y]
    stopifnot(all(d$season%in%2022:2023),max(d$game_date)<as.Date('2024-01-01'))
    form<-formula(b$model);environment(form)<-globalenv();family<-b$model$family
    message('Fit updated-history control: ',stage,' n=',nrow(f))
    start<-proc.time()[[3]];warnings<-character()
    fit<-withCallingHandlers(bam(form,data=f,family=family,method='fREML',discrete=TRUE,select=TRUE,
      nthreads=2,na.action=na.fail,control=gam.control(maxit=100)),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
    stopifnot(isTRUE(fit$converged),isTRUE(fit$mgcv.conv),all(is.finite(coef(fit))))
    eta<-hr_eta(fit,f);message('Fit pooled region offset: ',stage)
    pool<-hr_pool(d$y,eta,hr_group(d),family$family,if(stage=='value')fit$sig2 else 1)
    stopifnot(pool$converged)
    b<-list(model=fit,prep=b$prep,stage=stage,pool=pool,train_years=2022:2023,n=nrow(d),
      formula=paste(deparse(form,width.cutoff=500),collapse=' '),warnings=unique(warnings),seconds=proc.time()[[3]]-start,
      history_years=c('2022'=2021L,'2023'=2022L,'2024'=2023L),
      ranges=lapply(f[,intersect(names(f),c('x','z',CT_NUMERIC,'batter_history','pitcher_history','batter_ev_2021','pitcher_ev_2021')),with=FALSE],range))
    saveRDS(b,path,compress=FALSE)
    fwrite(pool$effects,file.path(HR_REPORT,paste0('effects_',stage,'.csv')))
    fwrite(d[,.(n=.N,history_year=unique(season)-1L,min_date=min(game_date),max_date=max(game_date)),by=season],
      file.path(HR_REPORT,paste0('history_chronology_',stage,'.csv')))
    message(stage,' complete: tau=',signif(pool$tau,5),' seconds=',round(b$seconds))
    rm(d,f,b,fit,eta,pool);gc(verbose=FALSE)
  }
  if(need_fit)rm(train,originals)
  gc(verbose=FALSE)
  message('All models frozen. Read previously inspected exploratory 2024.')
  val<-hr_read(2024L);results<-list();diag<-list()
  for(stage in HR_STAGES) {
    message('Evaluate ',stage);b<-readRDS(file.path(HR_MODELS,paste0(stage,'.rds')))
    d<-hr_stage_data(val,stage,api,re);results[[stage]]<-hr_evaluate(d,b,profiles,power)
    diag[[stage]]<-data.table(stage=stage,train_n=b$n,groups=nrow(b$pool$effects),tau=b$pool$tau,variance=b$pool$variance,
      dispersion=b$pool$dispersion,control_converged=b$model$converged,pool_converged=b$pool$converged,
      mode_iterations=b$pool$iterations,max_gradient=b$pool$max_gradient,fit_seconds=b$seconds,
      dense_full_design_GiB=as.double(b$n)*nrow(b$pool$effects)*8/1024^3,warnings=paste(b$warnings,collapse=';'))
    rm(b,d);gc(verbose=FALSE)
  }
  for(nm in c('metrics','slices','bootstrap'))fwrite(rbindlist(lapply(results,`[[`,nm),fill=TRUE),file.path(HR_REPORT,paste0(nm,'_2024.csv')))
  fwrite(rbindlist(diag),file.path(HR_REPORT,'fit_diagnostics.csv'))
  hr_scores(val,profiles,power,re)
  vd_check_hash(hash)
  checks<-c('chronological_profiles_all_training_and_evaluation_rows','unique_pitch_keys','identical_control_candidate_cohorts',
    'finite_valid_probabilities','same_pitch_labels_ignored','save_reload_predictions','row_order_invariance',
    'unseen_group_zero_effect','missing_invalid_zone_zero_effect','branch_probabilities_sum_to_one',
    'frozen_take_policy_RE_reference_and_original_inputs_unchanged','no_old_uncertainty_reused')
  fwrite(data.table(check=checks,passed=TRUE),file.path(HR_REPORT,'verification.csv'))
  saveRDS(list(version='hitter_region_v1',training=2022:2023,evaluation=2024L,role='previously_inspected_exploratory',
    activated=FALSE,input_hashes=hash,method='fixed_updated_GAM_plus_single_Gaussian_hitter_region_intercept',
    code_md5=tools::md5sum(file.path(HR_ROOT,'run.R'))),file.path(HR_MODELS,'manifest.rds'))
  message('DONE: isolated regional experiment; active scorer unchanged.')
}

hr_scores <- function(val,profiles,power,re) {
  old<-as.data.table(readRDS('data/models/hitter_stat_v6/decision_values_2024.rds'))
  ix<-match(dv_key(val),dv_key(old));stopifnot(!anyNA(ix),nrow(val)==nrow(old),!anyDuplicated(ix));old<-old[ix]
  # Fresh narrow output: old point predictions/support only, never old intervals.
  out<-val[,c(CT_KEYS,'game_date','batter','batter_name','zone','description'),with=FALSE]
  out[,`:=`(q_take=old$q_take_basic,p_swing_reference=old$p_swing_reference,original_supported=old$primary_supported,
    state_value_order_warning=old$state_value_order_warning,updated_range_failure=FALSE)]
  fallback<-list();p<-list()
  for(stage in HR_STAGES) {
    message('Score all eligible pitches: ',stage);b<-readRDS(file.path(HR_MODELS,paste0(stage,'.rds')))
    f<-hr_features(val,b,profiles,power);eta<-hr_eta(b$model,f);adj<-hr_adjust(b$pool,val,eta)
    p[[stage]]<-list(control=b$model$family$linkinv(eta),regional=b$model$family$linkinv(adj$eta))
    # Same range safeguards as existing framework, now checked against updated features.
    modeled<-intersect(names(b$ranges),all.vars(formula(b$model)))
    bad<-vd_range_flags(f,list(ranges=b$ranges[modeled]))
    out[,updated_range_failure:=updated_range_failure|bad]
    fallback[[stage]]<-data.table(fallback=adj$fallback)[,.(n=.N),by=fallback][,stage:=stage]
    rm(b,f);gc(verbose=FALSE)
  }
  states<-pc_branch_states(re);j<-match(pc_state(val),states$re288_state);stopifnot(!anyNA(j))
  contract<-readRDS('data/models/decision_validation/support_contract.rds')
  out[,primary_supported:=original_supported & !updated_range_failure]
  branches<-list()
  for(arm in c('control','regional')) {
    c<-p$contact[[arm]];b<-p$bip[[arm]];t<-p$tip[[arm]];q<-p$value[[arm]]
    mat<-cbind(whiff=1-c,bip=c*b,foul=c*(1-b)*(1-t),tip=c*(1-b)*t)
    stopifnot(all(is.finite(mat)),all(mat>=0&mat<=1),max(abs(rowSums(mat)-1))<1e-12,all(is.finite(q)))
    swing<-mat[,'whiff']*states$q_whiff[j]+mat[,'bip']*q+mat[,'foul']*states$q_ordinary_foul[j]+mat[,'tip']*states$q_foul_tip[j]
    out[,(paste0('q_swing_',arm)):=swing]
    out[,(paste0('score_',arm)):=(as.integer(description%chin%CT_SWINGS)-p_swing_reference)*(swing-q_take)]
    out[,primary_supported:=primary_supported & q>=contract$bip_target_range[1] & q<=contract$bip_target_range[2]]
    z<-cbind(val[,..CT_KEYS],as.data.table(mat));z[,`:=`(q_bip=q,q_swing=swing)]
    fwrite(z,file.path(HR_REPORT,paste0('branch_predictions_',arm,'_2024.csv.gz')))
  }
  out[,`:=`(delta_q_swing=q_swing_regional-q_swing_control,delta_score=score_regional-score_control,
    uncertainty_status='no_model_intervals_sampling_only_new_game_bootstrap')]
  fwrite(out,file.path(HR_REPORT,'decision_predictions_2024.csv.gz'))
  fwrite(rbindlist(fallback),file.path(HR_REPORT,'scoring_fallback_2024.csv'))
  summary<-out[,.(n=.N,mean_delta_q=mean(delta_q_swing),median_delta_q=median(delta_q_swing),
    mean_absolute_delta_q=mean(abs(delta_q_swing)),max_absolute_delta_q=max(abs(delta_q_swing)),
    change_runs_per100=100*mean(delta_score)),by=.(primary_supported,observed_action=ifelse(description%chin%CT_SWINGS,'swing','take'))]
  fwrite(summary,file.path(HR_REPORT,'swing_value_changes_2024.csv'))
  coverage<-out[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=.(batter,batter_name)]
  games<-out[primary_supported==TRUE,.(n=.N,control=sum(score_control),regional=sum(score_regional)),by=.(batter,game_pk)]
  set.seed(20261102)
  hitters<-games[,{
    draws<-replicate(500L,{i<-sample.int(.N,.N,TRUE);100*sum(regional[i]-control[i])/sum(n[i])})
    .(games=.N,control_runs100=100*sum(control)/sum(n),regional_runs100=100*sum(regional)/sum(n),
      delta_sampling_low=unname(quantile(draws,.025)),delta_sampling_high=unname(quantile(draws,.975)))
  },by=batter]
  hitters<-merge(coverage,hitters,by='batter',all.x=TRUE)
  ref<-readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
  hitters[,`:=`(qualified=supported>=500&games>=60&coverage>=.7,
    control_decision_plus=100+10*(control_runs100-ref$center)/ref$scale,
    regional_decision_plus=100+10*(regional_runs100-ref$center)/ref$scale,
    delta_runs100=regional_runs100-control_runs100)]
  hitters[,delta_decision_plus:=regional_decision_plus-control_decision_plus]
  fwrite(hitters[order(-abs(delta_decision_plus))],file.path(HR_REPORT,'hitter_changes_2024.csv'))
  fwrite(games,file.path(HR_REPORT,'hitter_game_scores_2024.csv.gz'))
  fwrite(data.table(original_supported=sum(out$original_supported),common_supported=sum(out$primary_supported),
    additional_range_or_value_exclusions=sum(out$original_supported&!out$primary_supported),eligible=nrow(out),
    qualified=sum(hitters$qualified,na.rm=TRUE),reference_center=ref$center,reference_scale=ref$scale),file.path(HR_REPORT,'score_support_2024.csv'))
  stopifnot(identical(out$q_take,old$q_take_basic),identical(out$p_swing_reference,old$p_swing_reference))
}
if(sys.nframe()==0L)hr_run()
