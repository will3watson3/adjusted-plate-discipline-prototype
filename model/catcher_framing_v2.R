# Rscript catcher_framing_v2.R --run
# Versioned historical experiment; never modifies the frozen v1 scorer/artifacts.
source("decision_validation.R")
FR_VERSION <- "prior_season_multilevel_framing_v2"
FR_REPORT <- "reports/catcher_framing_v2"
FR_MODELS <- "data/models/catcher_framing_v2"

fr_read <- function(year) {
  if(length(year)!=1L || !year %in% 2022:2025) stop("Invalid framing season")
  d <- as.data.table(readRDS(sprintf("data/processed/statcast_%d_clean.rds",year)))
  stopifnot(!anyNA(d[,..CS_KEYS]),!anyDuplicated(d,by=CS_KEYS),
    all(d$game_year==year),!anyNA(d$game_date),all(format(d$game_date,"%Y")==as.character(year)))
  d
}

# PSEUDOCODE 1: remove the complete existing catcher effect, including for known
# catchers. Predict exact bases; labels never enter this transformation.
fr_base <- function(base,d,se=TRUE) {
  t <- cs_transform(d,base$prep)
  ans <- data.table(eta=rep(NA_real_,nrow(d)),se=rep(NA_real_,nrow(d)))
  valid <- which(!t$invalid)
  for(ix in split(valid,ceiling(seq_along(valid)/10000L))) {
    p <- mgcv::predict.gam(base$model,t$data[ix],exclude="s(catcher)",
      type="link",se.fit=se,unconditional=se&&!is.null(base$model$Vc))
    if(se) ans[ix,`:=`(eta=as.numeric(p$fit),se=as.numeric(p$se.fit))]
    else ans[ix,eta:=as.numeric(p)]
  }
  ans[,`:=`(invalid=t$invalid,novel_fixed_category=t$novel_category,
    original_unknown_catcher=t$unknown_catcher)]
  ans
}

# PSEUDOCODE 2: Bernoulli evidence adjusts for every opportunity's base strike
# probability. A single Gaussian population supplies partial pooling across
# catchers. No raw workload-weighted season run total is used as a skill weight.
fr_fit <- function(history,base,prediction_year) {
  stopifnot(length(prediction_year)==1L,all(history$game_year==prediction_year-1L),
    !anyNA(history$game_date),max(base$train_years)<prediction_year)
  h <- copy(history[cs_eligible(history) & !is.na(fielder_2)])
  b <- fr_base(base,h,se=FALSE)
  keep <- !b$invalid & !b$novel_fixed_category & is.finite(b$eta)
  h <- h[keep];b<-b[keep]
  f <- data.table(y=as.integer(h$description=="called_strike"),
    catcher=factor(as.character(h$fielder_2)),eta=b$eta)
  if(nlevels(f$catcher)<3L || uniqueN(f$y)!=2L) stop("Insufficient catcher evidence")
  form <- y~offset(eta)+s(catcher,bs="re")
  environment(form)<-globalenv()
  set.seed(VD_SEED)
  fit <- bam(form,data=f,family=binomial(),method="fREML",discrete=TRUE,
    nthreads=2,control=gam.control(maxit=100))
  stopifnot(isTRUE(fit$converged),isTRUE(fit$mgcv.conv),all(is.finite(coef(fit))))
  sm <- fit$smooth[[1L]]
  # mgcv variance conversion handles penalty scaling, rather than guessing 1/sp.
  vc <- mgcv::gam.vcomp(fit)
  tau <- if(is.matrix(vc)) unname(vc["s(catcher)","std.dev"]) else unname(vc["s(catcher)"])
  if(!is.finite(tau)||tau<0) stop("Invalid population framing SD")
  ids <- levels(f$catcher);idx<-sm$first.para:sm$last.para
  stopifnot(length(ids)==length(idx))
  covariance <- if(!is.null(fit$Vc)) fit$Vc else fit$Vp
  mu <- unname(coef(fit)[1]);u<-unname(coef(fit)[idx])
  v <- covariance[1,1]+diag(covariance)[idx]+2*covariance[1,idx]
  tab <- data.table(catcher=ids,random_effect_mode=u,latent_mean=mu+u,
    latent_sd=sqrt(pmax(0,v)),population_mean=mu,population_sd=tau)
  info <- f[,.(opportunities=.N,called_strikes=sum(y),
    expected_strikes_no_catcher=sum(plogis(eta)),
    information=sum(plogis(eta)*(1-plogis(eta)))),by=.(catcher=as.character(catcher))]
  tab <- merge(tab,info,by="catcher",sort=FALSE)
  tab[,`:=`(information_shrinkage_weight=information/(information+1/tau^2),
    prior_season=prediction_year-1L,prediction_year=prediction_year)]
  # The information weight is a local diagnostic, not an exact posterior weight.
  list(version=FR_VERSION,fit=fit,catchers=tab,mu=mu,tau=tau,
    mu_variance=covariance[1,1],prior_season=as.integer(prediction_year-1L),
    prediction_year=as.integer(prediction_year),evidence_end=as.character(max(h$game_date)),
    base_train_years=base$train_years,history_rows=nrow(h),history_excluded=nrow(history[cs_eligible(history)])-nrow(h))
}

# Normal quadrature via the symmetric Hermite Jacobi matrix; weights sum to one.
fr_quadrature <- function(n=21L) {
  j<-matrix(0,n,n);j[cbind(1:(n-1L),2:n)]<-sqrt(1:(n-1L));j<-j+t(j)
  e<-eigen(j,symmetric=TRUE)
  list(nodes=e$values,weights=e$vectors[1,]^2)
}
fr_integrate <- function(eta,sd) {
  stopifnot(length(eta)==length(sd),all(is.finite(sd)),all(sd>=0))
  q<-fr_quadrature();p<-deriv<-numeric(length(eta))
  for(j in seq_along(q$nodes)) {
    z<-plogis(eta+sd*q$nodes[j]);p<-p+q$weights[j]*z
    deriv<-deriv+q$weights[j]*z*(1-z)
  }
  list(p=p,derivative=deriv)
}

# PSEUDOCODE 3: no current-season outcomes are read to select effects. An unknown
# catcher receives the full population distribution, not a known zero effect.
fr_predict <- function(skill,base,d) {
  if(anyNA(d$game_year)||any(d$game_year!=skill$prediction_year) ||
    anyNA(d$game_date)||any(d$game_date<=as.Date(skill$evidence_end)) ||
    max(base$train_years)>=skill$prediction_year ||
    !identical(base$train_years,skill$base_train_years)) stop("Framing chronology mismatch")
  b<-fr_base(base,d);k<-match(as.character(d$fielder_2),skill$catchers$catcher)
  has<-!is.na(k)
  mu<-rep(skill$mu,nrow(d));sd<-rep(sqrt(skill$tau^2+skill$mu_variance),nrow(d))
  mu[has]<-skill$catchers$latent_mean[k[has]];sd[has]<-skill$catchers$latent_sd[k[has]]
  z<-fr_integrate(b$eta+mu,sd)
  # Worst-correlation SD sum: the offset and catcher evidence overlap in time.
  bound<-b$se+sd
  out<-cbind(d[,..CS_KEYS],data.table(p_called_strike=z$p,
    p_population_zero_effect=plogis(b$eta),base_eta=b$eta,base_link_se=b$se,
    framing_latent_mean=mu,framing_latent_sd=sd,framing_population_sd=skill$tau,
    probability_model_se_bound=z$derivative*bound,
    probability_ci_low=plogis(b$eta+mu-1.96*bound),
    probability_ci_high=plogis(b$eta+mu+1.96*bound),
    catcher_evidence=ifelse(has,"prior_season_posterior","population_prior_no_history"),
    original_unknown_catcher=b$original_unknown_catcher,
    invalid=b$invalid,novel_fixed_category=b$novel_fixed_category,
    prior_season=skill$prior_season,model_version=FR_VERSION))
  out
}

fr_compare <- function(pred,d,base,year) {
  ix<-which(cs_eligible(d)&!pred$invalid&!pred$novel_fixed_category)
  p<-pred[ix];h<-d[ix];old<-predict_called_strike(base,h)$p_called_strike
  y<-as.integer(h$description=="called_strike")
  cohorts<-list(all=rep(TRUE,nrow(p)),absent_from_frozen_vocabulary=p$original_unknown_catcher,
    no_prior_season_history=p$catcher_evidence=="population_prior_no_history")
  metrics<-boots<-list()
  for(nm in names(cohorts)) {
    k<-which(cohorts[[nm]]);if(!length(k)) next
    for(method in c("hierarchical","frozen","zero_catcher")) {
      pp<-switch(method,hierarchical=p$p_called_strike,frozen=old,zero_catcher=p$p_population_zero_effect)
      metrics[[length(metrics)+1L]]<-cbind(data.table(year=year,cohort=nm,method=method),cs_metrics(y[k],pp[k]))
    }
    for(method in c("frozen","zero_catcher")) {
      baseline<-if(method=="frozen") old else p$p_population_zero_effect
      delta<-cs_loss(y[k],p$p_called_strike[k])-cs_loss(y[k],baseline[k])
      boots[[length(boots)+1L]]<-cbind(data.table(year=year,cohort=nm,baseline=method,n=length(k)),
        vd_boot_mean(h$game_pk[k],delta))
    }
  }
  list(metrics=rbindlist(metrics),bootstrap=rbindlist(boots))
}

# PSEUDOCODE 4: replace only take expectations; preserve all other support rules.
fr_combine <- function(old,raw,pred,re) {
  out<-copy(old);k<-match(dv_key(out),dv_key(raw));j<-match(dv_key(out),dv_key(pred))
  stopifnot(!anyNA(k),!anyNA(j),!anyDuplicated(out,by=CS_KEYS))
  p<-pred[j];f<-raw[k];states<-build_take_state_values(re)
  tv<-combine_take_values(out$re288_state,p$p_called_strike,states)
  out[,`:=`(v1_primary_supported=primary_supported,v1_support_reason=support_reason,
    v1_q_take=q_take_basic,v1_gap=swing_minus_take)]
  out[,`:=`(p_called_strike_if_ordinary_take=p$p_called_strike,q_take_basic=tv$q_take_basic,
    take_run_value_basic=tv$take_run_value_basic,swing_minus_take=q_swing_basic-tv$q_take_basic,
    novel_take_category=p$novel_fixed_category,framing_latent_mean=p$framing_latent_mean,
    framing_latent_sd=p$framing_latent_sd,catcher_evidence=p$catcher_evidence,
    time_role=ifelse(season==2025L,"exploratory_2025_reanalysis","historical_v2_validation"),
    support_contract=FR_VERSION)]
  flags<-intersect(c("invalid_inputs",paste0("sparse_",c("contact","post","bip","take"),"_cell"),
    paste0("outside_",c("contact","post","bip","take"),"_range"),
    paste0("novel_",c("contact","post","bip","take"),"_category"),
    paste0("missing_",c("contact","post","bip","take"),"_modeled_input"),
    "outside_tip_range","limited_action_overlap","re_order_warning","extreme_bip_value","invalid_raw_comparison"),names(out))
  out[,support_reason:=vd_reasons(out[,..flags])]
  out[,primary_supported:=support_reason==""]
  out[,primary_swing_minus_take:=fifelse(primary_supported,swing_minus_take,NA_real_)]
  stopifnot(all(!out$primary_supported[out$extreme_bip_value]),
    all(out$q_swing_basic==old$q_swing_basic),all(out$primary_supported[old$primary_supported]))
  list(data=out,raw=f,pred=p)
}

fr_uncertainty <- function(combined,old,m,re) {
  out<-combined$data;raw<-combined$raw;p<-combined$pred
  ix<-which(out$primary_supported);existing<-ix[old$primary_supported[ix]]
  fresh<-setdiff(ix,existing)
  states<-build_take_state_values(re);k<-match(out$re288_state,states$re288_state)
  spread<-abs(states$called_strike_value[k]-states$ball_value[k])
  swing_bound<-rep(NA_real_,nrow(out))
  if(length(existing)) {
    oldse<-vd_link_se(m$take$model,cs_transform(raw[existing],m$take$prep)$data)$se
    oldp<-old$p_called_strike_if_ordinary_take[existing]
    swing_bound[existing]<-pmax(0,old$gap_model_se_bound[existing]-
      spread[existing]*oldp*(1-oldp)*oldse)
  }
  if(length(fresh)) {
    f<-raw[fresh];z<-out[fresh]
    bundles<-list(m$contact,m$post$bip,m$post$tip,m$value)
    es<-lapply(seq_along(bundles),function(j) {
      b<-bundles[[j]];t<-if(j==1L) ct_transform(f,b$prep) else pc_transform(f,b$prep)
      vd_link_se(b$model,t$data)
    })
    dv<-vd_gap_derivatives(z$p_contact_given_swing,z$p_in_play_given_contact,plogis(es[[3]]$eta),
      z$q_bip,z$q_ordinary_foul,z$q_foul_tip,z$q_whiff,z$p_called_strike_if_ordinary_take,
      states$ball_value[k[fresh]],states$called_strike_value[k[fresh]])[,1:4,drop=FALSE]
    swing_bound[fresh]<-rowSums(abs(dv)*do.call(cbind,lapply(es,`[[`,"se")))
  }
  for(nm in c("gap_model_se_bound","gap_ci_low","gap_ci_high","reference_probability_se","above_reference_model_se_bound"))
    set(out,j=nm,value=rep(NA_real_,nrow(out)))
  policy<-readRDS("data/models/decision_validation/league_policy_2022_2023.rds")
  # Existing policy SE is reused exactly; newly supported pitches need its SE.
  ps<-old$reference_probability_se
  if(length(fresh)) {
    e<-vd_link_se(policy$model,ct_transform(raw[fresh],policy$prep)$data)
    ps[fresh]<-out$p_swing_reference[fresh]*(1-out$p_swing_reference[fresh])*e$se
  }
  bound<-swing_bound[ix]+spread[ix]*p$probability_model_se_bound[ix]
  stopifnot(all(is.finite(bound)),all(bound>=0),all(is.finite(ps[ix])))
  out[ix,`:=`(gap_model_se_bound=bound,gap_ci_low=swing_minus_take-1.96*bound,
    gap_ci_high=swing_minus_take+1.96*bound,reference_probability_se=ps[ix],
    above_reference_model_se_bound=abs(as.integer(observed_action=="swing")-p_swing_reference)*bound+
      abs(swing_minus_take)*ps[ix])]
  out[,preference:="unsupported"]
  out[ix,preference:=fcase(gap_ci_low>0,"swing",gap_ci_high<0,"take",default="no_clear_preference")]
  out[,primary_decision_status:=ifelse(!primary_supported,"unsupported",paste0("supported_",preference))]
  out
}

fr_run <- function() {
  setDTthreads(2)
  dir.create(FR_REPORT,recursive=TRUE,showWarnings=FALSE)
  dir.create(FR_MODELS,recursive=TRUE,showWarnings=FALSE)
  # Redirect only reports from reused pure evaluation helpers, not v1 models.
  VD_REPORT <<- FR_REPORT
  original<-vd_hash(c(vd_inputs(),"data/models/decision_validation/release_manifest.rds",
    "data/models/decision_validation/supported_decisions_2024.rds",
    "data/models/decision_validation/supported_decisions_2025.rds",
    "data/models/decision_validation/evaluation_result_2025.rds",
    "reports/decision_validation/test_opened_2025.rds"))
  m<-vd_load();re<-read_re_baseline();metrics<-boots<-list();reference<-NULL
  for(year in 2023:2025) {
    message("Framing v2: prior-season latent skill for ",year)
    base<-if(year==2023L) readRDS(file.path(CS_MODELS,"candidate_gam_stronger_penalty.rds")) else m$take
    stopifnot(max(base$train_years)<year)
    h<-fr_read(year-1L);skill<-fr_fit(h,base,year);rm(h);gc(verbose=FALSE)
    cs_save(skill,file.path(FR_MODELS,sprintf("latent_framing_for_%d.rds",year)))
    fwrite(skill$catchers,file.path(FR_REPORT,sprintf("catcher_skills_for_%d.csv",year)))
    raw<-fr_read(year)
    d<-copy(raw[analysis_eligible %in% TRUE])
    pred<-fr_predict(skill,base,d)
    cmp<-fr_compare(pred,d,base,year)
    metrics[[as.character(year)]]<-cmp$metrics;boots[[as.character(year)]]<-cmp$bootstrap
    fwrite(rbindlist(metrics),file.path(FR_REPORT,"called_strike_metrics.csv"))
    fwrite(rbindlist(boots),file.path(FR_REPORT,"paired_game_bootstrap.csv"))
    cs_save(pred,file.path(FR_MODELS,sprintf("strike_predictions_%d.rds",year)))
    if(year>=2024L) {
      old<-as.data.table(readRDS(sprintf("data/models/decision_validation/supported_decisions_%d.rds",year)))
      combined<-fr_combine(old,raw,pred,re)
      message("Framing v2: gap uncertainty for ",year)
      out<-fr_uncertainty(combined,old,m,re)
      # Reuse verified factual labels; take predictions change, swing predictions do not.
      out[,factual_prediction:=ifelse(observed_action=="swing",q_swing_basic,q_take_basic)]
      out[,factual_error:=factual_prediction-factual_q]
      out[,paired_mse_difference:=factual_error^2-(factual_baseline-factual_q)^2]
      fwrite(out[is.finite(factual_q),.(n=.N,bias=mean(factual_error),rmse=sqrt(mean(factual_error^2))),
        by=.(observed_action,primary_supported)],file.path(FR_REPORT,sprintf("factual_values_%d.csv",year)))
      agg<-vd_aggregate(out,year,reference)
      if(year==2024L) {reference<-agg$reference;cs_save(reference,file.path(FR_MODELS,"hitter_reference_2024.rds"))}
      agg$hitters[,`:=`(Decision_plus=NA_real_,release_status="research_only_v2_reanalysis")]
      fwrite(agg$hitters,file.path(FR_REPORT,sprintf("hitter_research_%d.csv",year)))
      fwrite(out[,.(pitches=.N,supported=sum(primary_supported),v1_supported=sum(v1_primary_supported),
        newly_supported=sum(primary_supported&!v1_primary_supported)),by=.(observed_action,catcher_evidence)],
        file.path(FR_REPORT,sprintf("support_coverage_%d.csv",year)))
      cs_save(out,file.path(FR_MODELS,sprintf("decision_values_%d.rds",year)))
      rm(old,combined,out,agg)
    }
    vd_check_hash(original)
    rm(raw,d,pred,skill,base);gc(verbose=FALSE)
  }
  b<-rbindlist(boots)[year==2024L & baseline=="frozen" & cohort %chin% c("all","absent_from_frozen_vocabulary")]
  gates<-b[,.(gate=paste0("historical_2024_noninferiority_",cohort),
    passed=n>=1000L&games>=30L&is.finite(ci_high)&ci_high<=.001,
    detail=sprintf("n=%d games=%d delta=%.6f upper95=%.6f margin=0.001",n,games,estimate,ci_high))]
  if(nrow(gates)!=2L) stop("Missing historical validation slices")
  fwrite(gates,file.path(FR_REPORT,"historical_gates.csv"))
  cs_save(list(version=FR_VERSION,completed=as.character(Sys.time()),v1_hashes=original,
    historical_gates=gates,scope="historical_validation_and_exploratory_2025_reanalysis",
    code_hashes=vd_hash(c("catcher_framing_v2.R","CATCHER_FRAMING_V2_PLAN.md"))),
    file.path(FR_MODELS,"experiment_manifest.rds"))
  fwrite(original,file.path(FR_REPORT,"unchanged_v1_fingerprints.csv"))
  writeLines(capture.output(sessionInfo()),file.path(FR_REPORT,"session_info.txt"))
  print(gates)
}

if(sys.nframe()==0L) {
  if(!identical(commandArgs(trailingOnly=TRUE),"--run")) stop("Use --run")
  fr_run()
}
