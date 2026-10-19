# Rscript decision_validation.R --2024
# Support, factual validation, conditional uncertainty, and exploratory aggregation.
# Original models and scored artifacts are immutable. See DECISION_VALIDATION_PLAN.md.
source("score_decisions.R")
VD_REPORT <- "reports/decision_validation"
VD_MODELS <- "data/models/decision_validation"
VD_SEED <- 20261017L
vd_cell <- function(f) paste(floor(f$x/.2),floor(f$z/.2),as.character(f$stand),as.character(f$family),sep="|")
vd_hash <- function(paths) data.table(path=paths,md5=unname(tools::md5sum(paths)))
vd_check_hash <- function(h) if(anyNA(h$md5)||!identical(h$md5,unname(tools::md5sum(h$path)))) stop("Frozen input changed")

vd_load <- function() {
  m <- load_decision_models()
  m$baseline <- readRDS(file.path(PC_MODELS,"bip_value_baseline_fit_2022_2023.rds"))
  m$take <- readRDS(file.path(CS_MODELS,"gam_validation_fit_2022_2023.rds"))
  for(b in list(m$take,m$baseline)) stopifnot(identical(b$train_years,2022:2023),isTRUE(b$model$converged))
  m
}

# STEP 1. PSEUDOCODE: derive density/ranges from actual fitted training frames,
# including the verified BIP subset, rather than borrowing swing support for BIP.
vd_support_contract <- function(m) {
  bundles <- list(contact=m$contact,post=m$post$bip,bip=m$value,take=m$take)
  tabs <- lapply(bundles,function(b) {
    f <- as.data.table(b$model$model)
    nums <- setdiff(names(f)[vapply(f,is.numeric,logical(1))],c("y",names(f)[grepl("_missing$",names(f))]))
    list(cells=data.table(cell=vd_cell(f))[,.(n=.N),by=cell],
      ranges=lapply(f[,..nums],range),n=nrow(f))
  })
  tabs$tip <- list(ranges=lapply(as.data.table(m$post$tip$model$model)[,
    intersect(c("x","z","speed","movx","movz","spin","extension","batter_history","pitcher_history"),
      names(m$post$tip$model$model)),with=FALSE],range))
  list(stages=tabs,min_cell_n=30L,overlap=c(.05,.95),bip_target_range=range(m$value$model$y),
    train_years=2022:2023,version="decision_support_v1",seed=VD_SEED)
}

vd_read <- function(year,allow_test=FALSE) {
  if(!year %in% 2022:2024 && !(year==2025L&&isTRUE(allow_test))) stop("Test season is locked")
  d <- as.data.table(readRDS(sprintf("data/processed/statcast_%d_clean.rds",year)))
  stopifnot(!anyNA(d[,..CT_KEYS]),!anyDuplicated(d,by=CT_KEYS),all(d$game_year==year),
    all(format(d$game_date,"%Y")==as.character(year)))
  d
}

vd_policy_fit <- function(d) {
  stopifnot(all(d$season %in% 2022:2023))
  prep <- ct_preprocessor(d,ct_profiles());t <- ct_transform(d,prep)
  f <- t$data; f[,y:=as.integer(d$description %chin% CT_SWINGS)]
  stopifnot(!any(t$invalid))
  form <- y~count+stand*throws+family+te(x,z,k=c(6,6),bs=c("cr","cr"),by=stand)+
    s(speed,k=6,bs="cr")+s(movx,k=5,bs="cr")+s(movz,k=5,bs="cr")
  # Avoid serializing this function's training-data environment with the formula.
  environment(form)<-globalenv()
  set.seed(VD_SEED)
  fit <- bam(form,data=f,family=binomial(),method="fREML",discrete=TRUE,select=TRUE,
    nthreads=2,control=gam.control(maxit=100))
  stopifnot(isTRUE(fit$converged),isTRUE(fit$mgcv.conv),all(is.finite(coef(fit))))
  counts <- d[,.(p=(sum(description %chin% CT_SWINGS)+30*mean(f$y))/(.N+30)),by=.(balls,strikes)]
  list(model=fit,prep=prep,counts=counts,train_years=sort(unique(d$season)),
    formula=paste(deparse(form,width.cutoff=500),collapse=" "),version="pooled_pitch_policy_v1")
}
vd_policy_predict <- function(b,d) {
  f <- ct_transform(d,b$prep)$data
  unlist(lapply(split(seq_len(nrow(f)),ceiling(seq_len(nrow(f))/20000L)),function(i)
    as.numeric(predict(b$model,f[i],type="response",discrete=FALSE))),use.names=FALSE)
}
vd_policy_metrics <- function(b,d,label) {
  p <- vd_policy_predict(b,d);y <- as.integer(d$description %chin% CT_SWINGS)
  j <- match(paste(d$balls,d$strikes),paste(b$counts$balls,b$counts$strikes))
  lp<-qlogis(pmax(1e-8,pmin(1-1e-8,p)))
  calibration<-coef(glm(y~lp,family=binomial()))
  data.table(population=label,n=nrow(d),log_loss=mean(cs_loss(y,p)),
    count_log_loss=mean(cs_loss(y,b$counts$p[j])),brier=mean((p-y)^2),bias=mean(p-y),
    observed_swing_rate=mean(y),mean_prediction=mean(p),
    calibration_intercept=unname(calibration[1]),calibration_slope=unname(calibration[2]))
}
vd_prepare <- function(d) {
  cols <- unique(c(CT_KEYS,CT_INPUTS,"game_date","game_year","description","batter_name","pitcher_name"))
  out <- copy(d[analysis_eligible %in% TRUE,..cols]);out[,season:=game_year];out
}
vd_train_reference <- function() {
  path <- file.path(VD_MODELS,"league_policy_2022_2023.rds")
  if(file.exists(path)) {
    b<-readRDS(path)
    stopifnot(identical(b$train_years,2022:2023),isTRUE(b$model$converged),
      identical(b$version,"pooled_pitch_policy_v1"))
    return(b)
  }
  a <- vd_prepare(vd_read(2022L));b <- vd_prepare(vd_read(2023L))
  message("Fitting fixed league policy on 2022, checking 2023")
  dev <- vd_policy_fit(a);metric <- vd_policy_metrics(dev,b,"development_2023")
  fwrite(metric,file.path(VD_REPORT,"policy_development_2023.csv"))
  if(metric$log_loss>metric$count_log_loss) stop("Reference policy fails development baseline")
  final <- vd_policy_fit(rbindlist(list(a,b)))
  cs_save(final,path);final
}

vd_range_flags <- function(f,tab) {
  bad <- rep(FALSE,nrow(f))
  for(nm in names(tab$ranges)) bad <- bad | !is.finite(f[[nm]]) |
    f[[nm]]<tab$ranges[[nm]][1]-1e-12 | f[[nm]]>tab$ranges[[nm]][2]+1e-12
  bad
}
vd_missing_modeled <- function(f,model) {
  inds <- intersect(names(f)[grepl("_missing$",names(f))],all.vars(formula(model)))
  if(!length(inds)) return(rep(FALSE,nrow(f)))
  Reduce(`|`,lapply(inds,function(nm) f[[nm]]>0))
}
vd_reasons <- function(flags) {
  ans <- rep("",nrow(flags))
  for(nm in names(flags)) {
    i <- which(flags[[nm]] %in% TRUE)
    ans[i] <- ifelse(nzchar(ans[i]),paste(ans[i],nm,sep=";"),nm)
  }
  ans
}

# STEP 2. PSEUDOCODE: annotate every row; keep raw values; withhold an explicitly
# named primary gap if *any* support rule fails. Never clamp/rewrite a raw value.
vd_annotate <- function(d,raw,m,contract,policy) {
  out <- copy(d);k <- match(dv_key(d),dv_key(raw));stopifnot(!anyNA(k))
  features <- raw[k]
  transforms <- list(contact=ct_transform(features,m$contact$prep),post=pc_transform(features,m$post$bip$prep),
    bip=pc_transform(features,m$value$prep),take=cs_transform(features,m$take$prep))
  models <- list(contact=m$contact$model,post=m$post$bip$model,bip=m$value$model,take=m$take$model)
  flags <- data.table(invalid_inputs=rep(FALSE,nrow(out)))
  for(stage in names(transforms)) {
    t <- transforms[[stage]];f <- t$data;tab <- contract$stages[[stage]]
    flags$invalid_inputs <- flags$invalid_inputs | t$invalid
    cell_n <- tab$cells$n[match(vd_cell(f),tab$cells$cell)];cell_n[is.na(cell_n)]<-0L
    set(out,j=paste0(stage,"_training_cell_n"),value=cell_n)
    set(flags,j=paste0("sparse_",stage,"_cell"),value=cell_n<contract$min_cell_n)
    set(flags,j=paste0("outside_",stage,"_range"),value=vd_range_flags(f,tab))
    novel <- if(stage=="take") t$novel_category|t$unknown_catcher else t$novel
    set(flags,j=paste0("novel_",stage,"_category"),value=novel)
    set(flags,j=paste0("missing_",stage,"_modeled_input"),value=vd_missing_modeled(f,models[[stage]]))
  }
  tip <- pc_transform(features,m$post$tip$prep)
  flags[,outside_tip_range:=vd_range_flags(tip$data,contract$stages$tip)]
  out[,p_swing_reference:=vd_policy_predict(policy,features)]
  flags[,`:=`(limited_action_overlap=!is.finite(out$p_swing_reference)|
    out$p_swing_reference<contract$overlap[1]|out$p_swing_reference>contract$overlap[2],
    re_order_warning=out$state_value_order_warning %in% TRUE,
    extreme_bip_value=!is.finite(out$q_bip)|out$q_bip<contract$bip_target_range[1]|
      out$q_bip>contract$bip_target_range[2],
    invalid_raw_comparison=!is.finite(out$swing_minus_take))]
  out[,support_reason:=vd_reasons(flags)]
  out[,primary_supported:=support_reason==""]
  out[,`:=`(primary_swing_minus_take=fifelse(primary_supported,swing_minus_take,NA_real_),
    support_contract=contract$version)]
  out <- cbind(out,flags)
  out
}

# Prediction SEs are conditional empirical-Bayes GAM SEs. Do not assume that
# components fitted on overlapping training games have independent errors.
vd_link_se <- function(model,f) {
  unconditional <- !is.null(model$Vc)
  ans <- vector("list",ceiling(nrow(f)/10000L))
  for(i in seq_along(ans)) {
    ix <- seq.int((i-1L)*10000L+1L,min(i*10000L,nrow(f)))
    # predict.bam(discrete=FALSE) in the installed mgcv strips Vc before calling
    # predict.gam. Call the exact GAM predictor directly so an available Vc is
    # actually used for smoothing-parameter uncertainty rather than discarded.
    p <- mgcv::predict.gam(model,f[ix],type="link",se.fit=TRUE,unconditional=unconditional)
    ans[[i]] <- data.table(eta=as.numeric(p$fit),se=as.numeric(p$se.fit))
  }
  out <- rbindlist(ans);stopifnot(all(is.finite(out$eta)),all(is.finite(out$se)),all(out$se>=0));out
}
vd_gap_derivatives <- function(c,b,t,qb,qf,qt,qw,p,qball,qstrike) {
  qnf <- (1-t)*qf+t*qt;qc <- b*qb+(1-b)*qnf
  cbind(contact=c*(1-c)*(qc-qw),bip_probability=c*b*(1-b)*(qb-qnf),
    tip_probability=c*(1-b)*t*(1-t)*(qt-qf),bip_value=c*b*qb,
    take=-p*(1-p)*(qstrike-qball))
}
vd_uncertainty <- function(d,raw,m,policy,re) {
  out <- copy(d);ix <- which(out$primary_supported)
  fields <- c("gap_model_se_bound","gap_ci_low","gap_ci_high","reference_probability_se",
    "above_reference_model_se_bound")
  for(nm in fields) set(out,j=nm,value=rep(NA_real_,nrow(out)))
  out[,preference:="unsupported"]
  if(!length(ix)) return(out)
  f <- raw[match(dv_key(out[ix]),dv_key(raw))]
  bundles <- list(m$contact,m$post$bip,m$post$tip,m$value,m$take)
  eta <- lapply(seq_along(bundles),function(j) {
    message("Conditional uncertainty component ",j,"/5, ",length(ix)," supported pitches")
    b <- bundles[[j]]
    trans <- if(j==1L) ct_transform(f,b$prep) else if(j==5L) cs_transform(f,b$prep) else pc_transform(f,b$prep)
    vd_link_se(b$model,trans$data)
  })
  z <- out[ix];states <- build_take_state_values(re);k <- match(z$re288_state,states$re288_state)
  # The tip model predicts tip | non-BIP contact. The saved three-class tip
  # probability is (1 - P(BIP | contact)) * that conditional probability.
  expected <- list(z$p_contact_given_swing,z$p_in_play_given_contact,plogis(eta[[3]]$eta),
    z$q_bip,z$p_called_strike_if_ordinary_take)
  for(j in 1:5) {
    delta<-max(abs((if(j==4L) exp(eta[[j]]$eta) else plogis(eta[[j]]$eta))-expected[[j]]))
    if(!is.finite(delta)||delta>=1e-8) stop("Uncertainty prediction parity failed at component ",j,": ",delta)
  }
  stopifnot(max(abs((1-expected[[2]])*expected[[3]]-z$p_foul_tip_given_contact))<1e-8)
  deriv <- vd_gap_derivatives(expected[[1]],expected[[2]],expected[[3]],expected[[4]],
    z$q_ordinary_foul,z$q_foul_tip,z$q_whiff,expected[[5]],states$ball_value[k],states$called_strike_value[k])
  ses <- do.call(cbind,lapply(eta,`[[`,"se"))
  bound <- rowSums(abs(deriv)*ses)
  pol <- vd_link_se(policy$model,ct_transform(f,policy$prep)$data)
  ps <- z$p_swing_reference*(1-z$p_swing_reference)*pol$se
  a <- as.integer(z$observed_action=="swing")
  out[ix,`:=`(gap_model_se_bound=bound,gap_ci_low=swing_minus_take-1.96*bound,
    gap_ci_high=swing_minus_take+1.96*bound,reference_probability_se=ps,
    above_reference_model_se_bound=abs(a-p_swing_reference)*bound+abs(swing_minus_take)*ps)]
  out[ix,preference:=fcase(gap_ci_low>0,"swing",gap_ci_high<0,"take",default="no_clear_preference")]
  out
}

# STEP 3. PSEUDOCODE: trace extreme predictions back to cached source coordinates
# and additive link-scale terms. Source equality is evidence; no location repair.
vd_diagnose <- function(d,m) {
  z <- d[extreme_bip_value==TRUE]
  if(!nrow(z)) return(invisible(NULL))
  terms <- as.data.table(predict(m$value$model,pc_transform(z,m$value$prep)$data,type="terms",discrete=FALSE))
  terms[,row:=.I]
  long <- melt(terms,id.vars="row",variable.name="term",value.name="link_contribution")
  long <- cbind(z[long$row,..CT_KEYS],long)
  fwrite(long,file.path(VD_REPORT,"extreme_bip_link_terms_2024.csv"))
  z[,`:=`(raw_cache_path=NA_character_,raw_coordinate_match=FALSE)]
  paths <- list.files("data/raw",pattern="^statcast_2024.*rds$",full.names=TRUE)
  for(i in seq_len(nrow(z))) {
    date <- as.character(z$game_date[i]);bn<-basename(paths)
    candidates <- paths[substr(bn,10,19)<=date & substr(bn,21,30)>=date]
    for(path in candidates) {
      cache <- as.data.table(readRDS(path));j<-match(dv_key(z[i]),dv_key(cache))
      if(!is.na(j)) {
        z[i,`:=`(raw_cache_path=path,raw_coordinate_match=
          isTRUE(all.equal(c(plate_x,plate_z),c(cache$plate_x[j],cache$plate_z[j]),tolerance=1e-12)))]
      }
    }
  }
  fwrite(z,file.path(VD_REPORT,"extreme_predictions_2024.csv"))
}

# Baseline uses training count-only probabilities and frozen state/count BIP fit.
vd_count_rate <- function(bundle,d) {
  f <- as.data.table(bundle$model$model)
  tab <- f[,.(p=(sum(y)+30*mean(f$y))/(.N+30)),by=count]
  tab$p[match(paste(d$balls,d$strikes,sep="-"),tab$count)]
}
vd_baseline_swing <- function(d,m) {
  c <- vd_count_rate(m$contact,d);b <- vd_count_rate(m$post$bip,d);t <- vd_count_rate(m$post$tip,d)
  qb <- pc_predict_one(m$baseline,d)
  (1-c)*d$q_whiff+c*(b*qb+(1-b)*((1-t)*d$q_ordinary_foul+t*d$q_foul_tip))
}

# STEP 4. PSEUDOCODE: compare each observed action only with verified labels.
# Full sequence is shifted BEFORE filtering; quarantine nonordinary runner action.
vd_factual_targets <- function(d,raw,bip,re) {
  states <- build_take_state_values(re)
  cols <- c(CT_KEYS,"inning","inning_topbot","balls","strikes","outs_when_up","on_1b","on_2b","on_3b",
    "bat_score","post_bat_score")
  seq <- copy(raw[,..cols]);seq[,state:=pc_state(seq)];setorderv(seq,CT_KEYS)
  seq[,`:=`(next_state=shift(state,type="lead"),next_inning=shift(inning,type="lead"),
    next_half=shift(inning_topbot,type="lead"),next_pa=shift(at_bat_number,type="lead"),
    next_pitch=shift(pitch_number,type="lead")),by=game_pk]
  a <- seq[match(dv_key(d),dv_key(seq))];k <- match(d$re288_state,states$re288_state)
  isball <- d$description %chin% c("ball","blocked_ball")
  isfoul2 <- d$description=="foul" & d$strikes==2L
  expected_state <- ifelse(isball,states$ball_state[k],ifelse(isfoul2,d$re288_state,states$strike_state[k]))
  runs <- ifelse(isball,states$ball_runs[k],0)
  ends <- ifelse(isball,d$balls==3L,!isfoul2&d$strikes==2L)
  same_half <- !is.na(a$next_inning)&a$inning==a$next_inning&a$inning_topbot==a$next_half
  sequence_ok <- ifelse(ends,a$next_pa>a$at_bat_number&a$next_pitch==1L,
    a$next_pa==a$at_bat_number&a$next_pitch==a$pitch_number+1L)
  state_ok <- ifelse(expected_state=="INNING_END",!is.na(a$next_inning)&!same_half,
    same_half&sequence_ok&a$next_state==expected_state)
  good <- state_ok %in% TRUE & is.finite(a$post_bat_score-a$bat_score)&a$post_bat_score-a$bat_score==runs
  y <- ifelse(isball,states$ball_value[k],ifelse(isfoul2,d$q_ordinary_foul,states$called_strike_value[k]))
  y[!good] <- NA_real_
  status <- ifelse(good,"verified_ordinary_transition","nonordinary_or_unverified_transition")
  isbip <- d$description=="hit_into_play";j <- match(dv_key(d),dv_key(bip))
  y[isbip] <- bip$q_bip_observed[j[isbip]]
  status[isbip] <- ifelse(is.finite(y[isbip]),"verified_bip","unverified_bip")
  data.table(factual_q=y,factual_target_status=status)
}

vd_boot_mean <- function(game,value,B=1000L) {
  g <- data.table(game,value)[is.finite(value),.(n=.N,total=sum(value)),by=game]
  if(nrow(g)<2L) return(data.table(estimate=NA_real_,ci_low=NA_real_,ci_high=NA_real_,games=nrow(g)))
  set.seed(VD_SEED)
  draws <- replicate(B,{i<-sample.int(nrow(g),nrow(g),TRUE);sum(g$total[i])/sum(g$n[i])})
  data.table(estimate=sum(g$total)/sum(g$n),ci_low=unname(quantile(draws,.025)),
    ci_high=unname(quantile(draws,.975)),games=nrow(g))
}
vd_validate <- function(d,raw,m,policy,re,bip,year=2024L) {
  targets <- vd_factual_targets(d,raw,bip,re);out <- cbind(copy(d),targets)
  out[,factual_prediction:=ifelse(observed_action=="swing",q_swing_basic,q_take_basic)]
  swingbase <- vd_baseline_swing(out,m)
  takebase <- as.data.table(m$take$model$model)[,.(p=mean(y)),by=count]
  states<-build_take_state_values(re);k<-match(out$re288_state,states$re288_state)
  p<-takebase$p[match(paste(out$balls,out$strikes,sep="-"),takebase$count)]
  out[,factual_baseline:=ifelse(observed_action=="swing",swingbase,
    (1-p)*states$ball_value[k]+p*states$called_strike_value[k])]
  out[,`:=`(factual_error=factual_prediction-factual_q,
    paired_mse_difference=(factual_prediction-factual_q)^2-(factual_baseline-factual_q)^2)]
  summary <- out[is.finite(factual_q),.(n=.N,games=uniqueN(game_pk),bias=mean(factual_error),
    rmse=sqrt(mean(factual_error^2)),baseline_rmse=sqrt(mean((factual_baseline-factual_q)^2))),
    by=.(observed_action,primary_supported)]
  boot <- rbindlist(lapply(c("swing","take"),function(action) {
    z<-out[primary_supported==TRUE & observed_action==action & is.finite(factual_q)]
    rbind(cbind(data.table(action=action,metric="bias"),vd_boot_mean(z$game_pk,z$factual_error)),
      cbind(data.table(action=action,metric="paired_mse_difference"),vd_boot_mean(z$game_pk,z$paired_mse_difference)))
  }))
  strata <- list(predicted_q_bin=floor(out$factual_prediction/.1),pitch_family=out$pitch_family,
    month=format(out$game_date,"%m"),preference=out$preference)
  slices <- rbindlist(lapply(names(strata),function(dim) {
    z<-data.table(action=out$observed_action,supported=out$primary_supported,slice=as.character(strata[[dim]]),
      predicted=out$factual_prediction,observed=out$factual_q)
    z[is.finite(observed),.(n=.N,predicted=mean(predicted),observed=mean(observed),
      bias=mean(predicted-observed),rmse=sqrt(mean((predicted-observed)^2))),by=.(action,supported,slice)][,dimension:=dim]
  }))
  fwrite(summary,file.path(VD_REPORT,sprintf("factual_validation_%d.csv",year)))
  fwrite(boot,file.path(VD_REPORT,sprintf("factual_bootstrap_%d.csv",year)))
  fwrite(slices,file.path(VD_REPORT,sprintf("calibration_slices_%d.csv",year)))
  fwrite(out[,.(n=.N),by=.(observed_action,primary_supported,factual_target_status)],
    file.path(VD_REPORT,sprintf("factual_coverage_%d.csv",year)))
  metrics <- vd_policy_metrics(policy,vd_prepare(raw),paste0("confirmation_",year))
  policy_bins<-out[,.(p=p_swing_reference,y=as.integer(observed_action=="swing"),primary_supported)]
  policy_bins[,bin:=pmin(19L,floor(p*20))]
  fwrite(policy_bins[,.(n=.N,predicted=mean(p),observed=mean(y)),by=.(primary_supported,bin)],
    file.path(VD_REPORT,sprintf("policy_calibration_%d.csv",year)))
  fwrite(metrics,file.path(VD_REPORT,sprintf("policy_validation_%d.csv",year)))
  gates <- data.table(gate="reference_policy_beats_count",passed=metrics$log_loss<=metrics$count_log_loss,
    detail=sprintf("log loss %.6f vs %.6f",metrics$log_loss,metrics$count_log_loss))
  for(action in c("swing","take")) {
    action_value<-action
    z<-boot[action==action_value];bias<-z[metric=="bias"];mse<-z[metric=="paired_mse_difference"]
    gates<-rbind(gates,data.table(gate=paste0(action,c("_bias_within_0.02","_not_worse_than_baseline")),
      passed=c(is.finite(bias$ci_low)&&bias$ci_low>=-.02&&bias$ci_high<=.02,
        is.finite(mse$ci_high)&&mse$ci_high<=0),
      detail=c(sprintf("95%% bias CI [%.6f, %.6f]",bias$ci_low,bias$ci_high),
        sprintf("95%% paired MSE CI [%.6f, %.6f]",mse$ci_low,mse$ci_high))))
  }
  gates<-rbind(gates,data.table(gate=c("extremes_withheld","supported_both_actions","raw_gap_arithmetic"),
    passed=c(all(!out$primary_supported[out$extreme_bip_value]),all(c("swing","take") %in% out[primary_supported==TRUE,observed_action]),
      max(abs(out$swing_minus_take-(out$q_swing_basic-out$q_take_basic)))<1e-10),detail=""))
  fwrite(gates,file.path(VD_REPORT,sprintf("validation_gates_%d.csv",year)))
  list(data=out,gates=gates,bootstrap=boot)
}

# STEP 5. PSEUDOCODE: retain *all* supported gaps, including near-zero comparisons.
# Hitter sampling uncertainty clusters games; shared-model SE is a separate bound.
vd_cluster_se <- function(n,total) {
  if(length(n)<2L||sum(n)==0L) return(NA_real_)
  mu<-sum(total)/sum(n)
  sqrt(length(n)/(length(n)-1)*sum((total-n*mu)^2))/sum(n)
}
vd_aggregate <- function(d,year=2024L,reference=NULL) {
  coverage <- d[,.(eligible=.N,supported=sum(primary_supported),
    supported_swings=sum(primary_supported & observed_action=="swing"),
    supported_takes=sum(primary_supported & observed_action=="take")),by=.(batter,batter_name)]
  coverage[,coverage:=supported/eligible]
  z<-copy(d[primary_supported==TRUE])
  z[,`:=`(above_reference=(as.integer(observed_action=="swing")-p_swing_reference)*swing_minus_take,
    negative_regret=ifelse(observed_action=="swing",pmin(0,swing_minus_take),pmin(0,-swing_minus_take)),
    half=ifelse(game_date<as.Date(sprintf("%d-07-01",year)),"first","second"))]
  game<-z[,.(n=.N,total=sum(above_reference),model_bound_total=sum(above_reference_model_se_bound)),
    by=.(batter,game_pk,half)]
  rate<-game[,.(games=.N,runs_per_100=100*sum(total)/sum(n),sampling_se_per_100=100*vd_cluster_se(n,total),
    model_se_bound_per_100=100*sum(model_bound_total)/sum(n)),by=batter]
  regret<-z[,.(modeled_runs_forgone=-sum(negative_regret),no_clear_preference=sum(preference=="no_clear_preference")),by=batter]
  out<-merge(coverage,rate,by="batter",all.x=TRUE,sort=FALSE);out<-merge(out,regret,by="batter",all.x=TRUE,sort=FALSE)
  out[,reference_eligible:=supported>=500L & games>=60L & coverage>=.7 & is.finite(sampling_se_per_100)]
  ref<-out[reference_eligible==TRUE]
  if(is.null(reference)) {
    if(nrow(ref)>=2L) {
      w<-ref$supported/sum(ref$supported);mu<-sum(w*ref$runs_per_100);den<-1-sum(w^2)
      v<-ref$sampling_se_per_100^2
      tau2<-max(0,sum(w*(ref$runs_per_100-mu)^2)/den-sum((w-w^2)*v)/den)
    } else {mu<-NA_real_;tau2<-0}
    reference<-list(center_raw=mu,tau2=tau2,reference_season=year,minimum_pitches=500L,
      minimum_games=60L,minimum_coverage=.7,version="personalized_historical_value_policy_adjusted_v1")
  }
  out[,conditional_sampling_reliability:=ifelse(is.finite(sampling_se_per_100),
    reference$tau2/(reference$tau2+sampling_se_per_100^2),NA_real_)]
  out[!is.finite(conditional_sampling_reliability),conditional_sampling_reliability:=0]
  out[,shrunken_runs_per_100:=reference$center_raw+conditional_sampling_reliability*(runs_per_100-reference$center_raw)]
  out[,`:=`(conditional_envelope_low=runs_per_100-1.96*(sampling_se_per_100+model_se_bound_per_100),
    conditional_envelope_high=runs_per_100+1.96*(sampling_se_per_100+model_se_bound_per_100),
    estimate_scope="exploratory_supported_pitches_personalized_2021_ability",Decision_plus=NA_real_)]
  half<-game[,.(n=sum(n),games=.N,rate=100*sum(total)/sum(n)),by=.(batter,half)]
  paired<-merge(half[half=="first"],half[half=="second"],by="batter",suffixes=c("_first","_second"))
  paired<-paired[n_first>=100L & n_second>=100L & games_first>=15L & games_second>=15L]
  correlation<-if(nrow(paired)>=20L && sd(paired$rate_first)>0 && sd(paired$rate_second)>0)
    cor(paired$rate_first,paired$rate_second) else NA_real_
  corr_ci<-c(NA_real_,NA_real_)
  if(is.finite(correlation)) {
    set.seed(VD_SEED)
    draws<-replicate(1000L,{i<-sample.int(nrow(paired),nrow(paired),TRUE);cor(paired$rate_first[i],paired$rate_second[i])})
    corr_ci<-unname(quantile(draws,c(.025,.975),na.rm=TRUE))
  }
  fwrite(data.table(paired_hitters=nrow(paired),correlation=correlation,ci_low=corr_ci[1],ci_high=corr_ci[2],
    bootstrap_unit="hitter_pair",replicates=1000L),file.path(VD_REPORT,sprintf("hitter_reliability_%d.csv",year)))
  gates<-data.table(gate=c("reference_population_at_least_20","nonzero_between_hitter_spread", "median_sampling_reliability_at_least_0.5","half_year_correlation_at_least_0.5"),
    passed=c(nrow(ref)>=20L,is.finite(reference$tau2)&&reference$tau2>1e-8,
      nrow(ref)>0&&median(out[reference_eligible==TRUE,conditional_sampling_reliability])>=.5,
      is.finite(correlation)&&correlation>=.5),
    detail=c(paste("hitters",nrow(ref)),paste("tau2",signif(reference$tau2,6)),
      paste("median",signif(median(out[reference_eligible==TRUE,conditional_sampling_reliability]),6)),
      paste("paired hitters",nrow(paired),"correlation",signif(correlation,6))))
  # Exact lookup: `$center` partially matches `center_raw` before center exists.
  if(is.null(reference[["center"]])) {
    q<-out[reference_eligible==TRUE];w<-q$supported/sum(q$supported)
    reference$center<-sum(w*q$shrunken_runs_per_100)
    reference$scale<-sqrt(sum(w*(q$shrunken_runs_per_100-reference$center)^2))
  }
  fwrite(out,file.path(VD_REPORT,sprintf("hitter_exploratory_%d.csv",year)))
  fwrite(half,file.path(VD_REPORT,sprintf("hitter_half_year_%d.csv",year)))
  fwrite(gates,file.path(VD_REPORT,sprintf("hitter_release_gates_%d.csv",year)))
  list(hitters=out,reference=reference,gates=gates)
}

vd_inputs <- function() unique(c("score_decisions.R","post_contact_models.R","contact_models.R",
  "called_strike_models.R","take_value_models.R","re288_order_flags.R","attach_run_expectancy.R","run_expectancy_models.R",
  file.path(CT_MODELS,"contact_gam_fit_2022_2023.rds"),file.path(PC_MODELS,c("post_contact_fit_2022_2023.rds",
    "bip_value_fit_2022_2023.rds","bip_value_baseline_fit_2022_2023.rds","bip_value_predictions_2024.rds","swing_values_2024.rds")),
  file.path(CS_MODELS,"gam_validation_fit_2022_2023.rds"),"data/models/decision/decision_values_2024.rds",
  sprintf("data/processed/statcast_%d_clean.rds",2022:2024),read_re_baseline()$files$file,
  unname(CT_PROFILE_FILES),unname(PC_POWER_FILES)))

vd_run_2024 <- function(reuse_uncertainty=FALSE) {
  setDTthreads(2);dir.create(VD_REPORT,recursive=TRUE,showWarnings=FALSE);dir.create(VD_MODELS,recursive=TRUE,showWarnings=FALSE)
  hashes<-vd_hash(vd_inputs());vd_check_hash(hashes)
  if(!reuse_uncertainty) fwrite(hashes,file.path(VD_REPORT,"run_input_fingerprints.csv"))
  m<-vd_load();re<-read_re_baseline();contract<-vd_support_contract(m)
  components<-list(contact=m$contact,post_bip=m$post$bip,post_tip=m$post$tip,bip_value=m$value,take=m$take)
  fwrite(rbindlist(lapply(names(components),function(nm) {
    b<-components[[nm]]
    data.table(component=nm,training_rows=nrow(b$model$model),coefficients=length(coef(b$model)),
      smoothing_uncertainty_covariance_available=!is.null(b$model$Vc),family=b$model$family$family,
      link=b$model$family$link,training_years=paste(b$train_years,collapse=","))
  })),file.path(VD_REPORT,"uncertainty_component_manifest.csv"))
  cs_save(contract,file.path(VD_MODELS,"support_contract.rds"))
  policy<-vd_train_reference()
  raw<-vd_read(2024L);d<-as.data.table(readRDS("data/models/decision/decision_values_2024.rds"))
  message("Annotating support for ",nrow(d)," 2024 pitches")
  if(reuse_uncertainty) {
    previous<-fread(file.path(VD_REPORT,"run_input_fingerprints.csv"));vd_check_hash(previous)
    stopifnot(identical(previous,hashes))
    out<-as.data.table(readRDS(file.path(VD_MODELS,"uncertainty_checkpoint_2024.rds")))
    stopifnot(all(vapply(names(d),function(nm) identical(out[[nm]],d[[nm]]),logical(1))),
      all(out$support_contract==contract$version))
    drop<-intersect(c("factual_q","factual_target_status","factual_prediction","factual_baseline","factual_error","paired_mse_difference"),names(out))
    out[,(drop):=NULL]
  } else {
    out<-vd_annotate(d,raw,m,contract,policy)
    fwrite(out[,.(n=.N),by=.(observed_action,primary_supported,support_reason)],file.path(VD_REPORT,"support_coverage_2024.csv"))
    vd_diagnose(out,m)
    out<-vd_uncertainty(out,raw,m,policy,re)
  }
  out[,primary_decision_status:=ifelse(!primary_supported,"unsupported",
    ifelse(preference=="no_clear_preference","supported_no_clear_preference",paste0("supported_interval_favors_",preference)))]
  cs_save(out,file.path(VD_MODELS,"uncertainty_checkpoint_2024.rds"))
  bip<-as.data.table(readRDS(file.path(PC_MODELS,"bip_value_predictions_2024.rds")))
  val<-vd_validate(out,raw,m,policy,re,bip)
  agg<-vd_aggregate(val$data)
  gates<-rbind(val$gates,agg$gates)
  # This release is conditional/observational. No causal accuracy claim is made.
  release<-list(passed=all(gates$passed),gates=gates,reference=agg$reference,
    support_contract=contract$version,train_years=2022:2023,validation_year=2024L,
    uncertainty_scope="conditional_GAM_first_order_bound_plus_game_sampling_RE_fixed",
    input_hashes=hashes,output_hashes=vd_hash(file.path(VD_MODELS,c("league_policy_2022_2023.rds","support_contract.rds"))),
    code_hashes=vd_hash(c("decision_validation.R","DECISION_VALIDATION_PLAN.md","evaluate_locked2025.R","acquire_evaluation_bip_states.py")),
    frozen_at=format(Sys.time(),tz="UTC",usetz=TRUE))
  vd_check_hash(hashes)
  cs_save(val$data,file.path(VD_MODELS,"supported_decisions_2024.rds"))
  # Export complete keys and reasons, not just a small example of unsupported rows.
  cols<-c(CT_KEYS,"game_date","batter","batter_name","pitcher","pitcher_name","observed_action",CT_INPUTS,
    "q_bip","q_swing_basic","q_take_basic","swing_minus_take","primary_supported","support_reason", "p_swing_reference",
    "contact_training_cell_n","post_training_cell_n","bip_training_cell_n","take_training_cell_n")
  fwrite(val$data[primary_supported==FALSE,unique(cols),with=FALSE],file.path(VD_REPORT,"unsupported_pitches_2024.csv.gz"))
  fwrite(val$data[,.(n=.N),by=.(observed_action,primary_supported,preference)],file.path(VD_REPORT,"preference_coverage_2024.csv"))
  fwrite(gates,file.path(VD_REPORT,"release_gates_2024.csv"));fwrite(hashes,file.path(VD_REPORT,"frozen_input_fingerprints.csv"))
  cs_save(release,file.path(VD_MODELS,"release_manifest.rds"))
  if(release$passed) {
    h<-agg$hitters
    h[reference_eligible==TRUE,Decision_plus:=100+10*(shrunken_runs_per_100-release$reference$center)/release$reference$scale]
    fwrite(h,file.path(VD_REPORT,"hitter_validated_2024.csv"))
  }
  print(gates)
  invisible(release)
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  if(identical(args,"--2024")||!length(args)) vd_run_2024()
  else if(identical(args,"--finalize-2024")) vd_run_2024(reuse_uncertainty=TRUE)
  else stop("Use --2024 or --finalize-2024; test-season evaluation is a separate guarded entry point")
}
