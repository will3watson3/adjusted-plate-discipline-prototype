source("catcher_framing_v2.R")
SV_REPORT <- "reports/season_average_v3"
SV_MODELS <- "data/models/season_average_v3"
SV_VERSION <- "season_average_calibrated_v3"
SV_SEED <- 20261019L
SV_COMPONENTS <- c("contact","bip_probability","tip_probability","bip_value","take","policy")
SV_METHODS <- c("identity","intercept","slope","context")

sv_features <- function(d,p,kind,levels=NULL) {
  link<-if(kind=="bip_value") log(pmax(p,1e-10)) else qlogis(pmin(1-1e-8,pmax(1e-8,p)))
  f<-data.table(lp=link,count=paste(d$balls,d$strikes,sep="-"),family=as.character(d$pitch_family),
    stand=as.character(d$stand),throws=as.character(d$p_throws),outs=as.character(d$outs_when_up),
    base=substr(d$re288_state,1,3))
  factors<-setdiff(names(f),"lp")
  if(is.null(levels)) levels<-lapply(f[,..factors],function(x) sort(unique(x)))
  novel<-rep(FALSE,nrow(f))
  for(nm in factors) {
    novel<-novel | is.na(f[[nm]]) | !f[[nm]] %in% levels[[nm]]
    set(f,j=nm,value=factor(f[[nm]],levels=levels[[nm]]))
  }
  list(data=f,levels=levels,novel=novel)
}

sv_component <- function(d,kind) {
  a<-d$observed_action=="swing"
  contact<-d$description %chin% c("hit_into_play","foul","foul_tip")
  switch(kind,
    contact=list(p=d$p_contact_given_swing,y=as.numeric(contact),keep=a),
    bip_probability=list(p=d$p_in_play_given_contact,y=as.numeric(d$description=="hit_into_play"),keep=contact),
    tip_probability=list(p=d$p_foul_tip_given_contact/(1-d$p_in_play_given_contact),
      y=as.numeric(d$description=="foul_tip"),keep=d$description %chin% c("foul","foul_tip")),
    bip_value=list(p=d$q_bip,y=d$factual_q,keep=d$description=="hit_into_play"&is.finite(d$factual_q)),
    take=list(p=d$p_called_strike_if_ordinary_take,y=as.numeric(d$description=="called_strike"),keep=!a),
    policy=list(p=d$p_swing_reference,y=as.numeric(a),keep=rep(TRUE,nrow(d))),stop("Unknown component"))
}

sv_fit <- function(d,kind,method) {
  c<-sv_component(d,kind)
  ix<-which(d$primary_supported & c$keep & is.finite(c$p)&is.finite(c$y))
  z<-d[ix];p<-c$p[ix];y<-c$y[ix]
  if(!length(ix)) stop("No calibration evidence")
  common<-list(kind=kind,method=method,train_end=as.character(max(z$game_date)),
    train_n=length(ix),version=SV_VERSION)
  if(method=="identity") return(c(common,list(model=NULL,slope=1,parameters=0L)))
  f<-sv_features(z,p,kind);f$data[,y:=y]
  rhs<-if(method=="intercept") "offset(lp)" else if(method=="slope") "lp" else
    if(kind=="bip_value") "lp+count+family+stand+outs+base" else "lp+count+family+stand*throws"
  form<-as.formula(paste("y~",rhs),env=globalenv())
  fit<-glm(form,data=f$data,family=if(kind=="bip_value") quasipoisson(link="log") else binomial(),
    control=glm.control(maxit=100),na.action=na.fail,x=TRUE,y=TRUE)
  if(!isTRUE(fit$converged)||fit$rank!=length(coef(fit))||any(!is.finite(coef(fit)))) stop("Calibration did not converge")
  slope<-if(method=="intercept") 1 else unname(coef(fit)["lp"])
  if(!is.finite(slope)||slope<=0) stop("Calibration reverses base score ordering")
  x<-fit$x;mu<-fit$fitted.values
  w<-if(kind=="bip_value") mu else mu*(1-mu)
  bread<-solve(crossprod(x*sqrt(w)))
  score<-x*as.numeric(y-mu)
  sums<-rowsum(score,group=z$game_pk,reorder=FALSE)
  G<-nrow(sums);N<-nrow(x);K<-ncol(x)
  covariance<-bread%*%crossprod(sums)%*%bread * G/(G-1)*(N-1)/(N-K)
  stopifnot(all(is.finite(covariance)),all(diag(covariance)>=0))
  fit$x<-NULL;fit$y<-NULL;fit$model<-NULL
  c(common,list(model=fit,levels=f$levels,slope=slope,parameters=length(coef(fit)),
    covariance=covariance,covariance_scope="game_cluster_robust_conditional_on_base_model"))
}

# A calibrator can be scored on its own training data only for explicitly labeled
# development-reference summaries. Forward validation and deployment default safe.
sv_predict <- function(b,d,p,allow_training=FALSE) {
  if(!allow_training && any(d$game_date<=as.Date(b$train_end))) stop("Calibration prediction chronology violation")
  if(b$method=="identity") return(data.table(p=p,calibration_link_se=rep(0,length(p))))
  f<-sv_features(d,p,b$kind,b$levels)
  used<-intersect(all.vars(formula(b$model)),names(f$data))
  if(anyNA(f$data[,..used])) stop("Novel modeled calibration category; do not silently extrapolate")
  # Formula offsets must be retained in predict.glm; lpmatrix below excludes them.
  eta<-as.numeric(predict(b$model,f$data,type="link"))
  x<-model.matrix(delete.response(terms(b$model)),f$data,contrasts.arg=b$model$contrasts)
  stopifnot(identical(colnames(x),names(coef(b$model))))
  se<-sqrt(pmax(0,rowSums((x%*%b$covariance)*x)))
  v<-if(b$kind=="bip_value") exp(eta) else plogis(eta)
  if(any(!is.finite(v))||any(v<0)||(b$kind!="bip_value"&&any(v>1))) stop("Invalid calibrated mean")
  data.table(p=v,calibration_link_se=se)
}

sv_loss <- function(y,p,kind) if(kind=="bip_value") (p-y)^2 else cs_loss(y,p)
sv_boot <- function(game,value,B=1000L) {
  g<-data.table(game,value)[is.finite(value),.(n=.N,total=sum(value)),by=game]
  se<-vd_cluster_se(g$n,g$total)
  set.seed(SV_SEED)
  v<-replicate(B,{i<-sample.int(nrow(g),nrow(g),TRUE);sum(g$total[i])/sum(g$n[i])})
  data.table(estimate=sum(g$total)/sum(g$n),se=se,ci_low=unname(quantile(v,.025)),
    ci_high=unname(quantile(v,.975)),games=nrow(g))
}

sv_metrics <- function(d,p,y,kind,label) {
  loss<-sv_loss(y,p,kind)
  if(kind=="bip_value") {
    fit<-glm(y~log(pmax(p,1e-10)),family=quasipoisson(link="log"))
    summary<-data.table(n=length(y),mean_prediction=mean(p),observed=mean(y),bias=mean(p-y),
      loss=mean(loss),rmse=sqrt(mean((p-y)^2)),brier=NA_real_,
      calibration_intercept=unname(coef(fit)[1]),calibration_slope=unname(coef(fit)[2]))
  } else {
    m<-cs_metrics(y,p)
    summary<-data.table(n=length(y),mean_prediction=mean(p),observed=mean(y),bias=mean(p-y),
      loss=mean(loss),rmse=sqrt(mean((p-y)^2)),brier=m$brier,
      calibration_intercept=m$calibration_intercept,calibration_slope=m$calibration_slope)
  }
  summary[,`:=`(component=kind,population=label)]
  summary
}

sv_select <- function(d) {
  stopifnot(all(d$season==2024L))
  periods<-list(early=list(cut=as.Date("2024-05-01"),end=as.Date("2024-07-01")),
    late=list(cut=as.Date("2024-07-01"),end=as.Date("2025-01-01")))
  records<-metrics<-periods_boot<-failures<-list();parameters<-list()
  for(kind in SV_COMPONENTS) {
    message("Forward calibration search: ",kind)
    for(period in names(periods)) {
      s<-periods[[period]];train<-d[game_date<s$cut];test<-d[game_date>=s$cut & game_date<s$end]
      c<-sv_component(test,kind);ix<-which(test$primary_supported&c$keep&is.finite(c$p)&is.finite(c$y))
      z<-test[ix];p<-c$p[ix];y<-c$y[ix]
      for(method in SV_METHODS) {
        candidate<-tryCatch({b<-sv_fit(train,kind,method);list(bundle=b,p=sv_predict(b,z,p)$p)},error=function(e)e)
        if(inherits(candidate,"error")) {
          failures[[length(failures)+1L]]<-data.table(component=kind,period=period,method=method,error=conditionMessage(candidate))
          next
        }
        b<-candidate$bundle;v<-candidate$p
        rec<-data.table(component=kind,period=period,method=method,game_pk=z$game_pk,
          loss=sv_loss(y,v,kind),base_loss=sv_loss(y,p,kind),parameters=b$parameters)
        records[[length(records)+1L]]<-rec
        met<-sv_metrics(z,v,y,kind,paste0("forward_2024_",period));met[,method:=method]
        metrics[[length(metrics)+1L]]<-met
        periods_boot[[length(periods_boot)+1L]]<-cbind(data.table(component=kind,period=period,method=method),
          sv_boot(z$game_pk,rec$loss-rec$base_loss))
      }
    }
  }
  records<-rbindlist(records);metrics<-rbindlist(metrics);periods_boot<-rbindlist(periods_boot)
  choices<-candidates<-list()
  for(kind in SV_COMPONENTS) {
    z<-records[component==kind]
    valid_methods<-z[,.(folds=uniqueN(period)),by=method][folds==2L,method]
    z<-z[method %chin% valid_methods]
    ag<-z[,.(loss=mean(loss),parameters=max(parameters)),by=method]
    best<-ag$method[which.min(ag$loss)]
    # Row order for each method follows the same two disjoint evaluation periods.
    bestloss<-z[method==best,loss]
    comparisons<-rbindlist(lapply(valid_methods,function(meth) {
      x<-z[method==meth]
      cbind(data.table(method=meth),sv_boot(x$game_pk,x$loss-bestloss))
    }))
    ag<-merge(ag,comparisons,by="method")
    ag[,clearly_worse_period:=vapply(method,function(meth)
      any(periods_boot[component==kind & method==meth,ci_low]>0),logical(1))]
    ag[,within_one_paired_se:=estimate<=se+1e-15]
    admissible<-ag[within_one_paired_se==TRUE & clearly_worse_period==FALSE][order(parameters,loss)]
    chosen<-if(nrow(admissible)) admissible$method[1L] else "identity"
    ag[,`:=`(component=kind,selected=method==chosen,best_loss_method=best)]
    candidates[[kind]]<-ag
    b<-sv_fit(d,kind,chosen);parameters[[kind]]<-b
    choices[[kind]]<-data.table(component=kind,method=chosen,parameters=b$parameters,train_end=b$train_end,
      interpretation="selected_using_2024_forward_development_only")
  }
  fwrite(metrics,file.path(SV_REPORT,"forward_candidate_metrics.csv"))
  fwrite(periods_boot,file.path(SV_REPORT,"forward_candidate_paired_intervals.csv"))
  fwrite(rbindlist(candidates),file.path(SV_REPORT,"candidate_selection.csv"))
  fwrite(rbindlist(choices),file.path(SV_REPORT,"chosen_calibrators.csv"))
  fwrite(if(length(failures)) rbindlist(failures) else data.table(component=character(),period=character(),method=character(),error=character()),
    file.path(SV_REPORT,"candidate_failures.csv"))
  # Freeze choices before any 2025 reads in this entry point.
  cs_save(list(version=SV_VERSION,calibrators=parameters,selection=rbindlist(choices),
    train_end=as.Date(max(d$game_date)),code=vd_hash(c("season_average_v3.R","SEASON_AVERAGE_V3_PLAN.md")),
    frozen_utc=as.character(Sys.time()),selection_year=2024L),file.path(SV_MODELS,"frozen_calibration.rds"))
  parameters
}

sv_apply <- function(d,calibrators,allow_training=FALSE) {
  out<-copy(d);preds<-list()
  for(kind in SV_COMPONENTS) {
    c<-sv_component(d,kind)
    # Only supported rows are eligible for this calibrated scoring layer.
    ix<-which(d$primary_supported)
    z<-sv_predict(calibrators[[kind]],d[ix],c$p[ix],allow_training)
    preds[[kind]]<-z
  }
  ix<-which(d$primary_supported)
  q<-out[ix];c<-preds$contact$p;b<-preds$bip_probability$p;t<-preds$tip_probability$p
  qb<-preds$bip_value$p;p<-preds$take$p
  states<-build_take_state_values(read_re_baseline());k<-match(q$re288_state,states$re288_state)
  qs<-(1-c)*q$q_whiff+c*(b*qb+(1-b)*((1-t)*q$q_ordinary_foul+t*q$q_foul_tip))
  qt<-(1-p)*states$ball_value[k]+p*states$called_strike_value[k]
  out[,`:=`(v2_q_swing=q_swing_basic,v2_q_take=q_take_basic,v2_gap=swing_minus_take,
    v2_reference=p_swing_reference,scoring_version=SV_VERSION)]
  out[ix,`:=`(p_contact_given_swing=c,p_whiff_given_swing=1-c,p_in_play_given_contact=b,
    p_ordinary_foul_given_contact=(1-b)*(1-t),p_foul_tip_given_contact=(1-b)*t,
    p_bip_given_swing=c*b,p_foul_given_swing=c*(1-b)*(1-t),p_tip_given_swing=c*(1-b)*t,
    q_bip=qb,q_swing_basic=qs,q_take_basic=qt,swing_minus_take=qs-qt,
    swing_run_value_basic=qs-re_before,take_run_value_basic=qt-re_before,
    p_called_strike_if_ordinary_take=p,p_swing_reference=preds$policy$p)]
  out[,`:=`(factual_prediction=ifelse(observed_action=="swing",q_swing_basic,q_take_basic),
    estimate_role=if(allow_training) "development_reference_in_sample_calibration" else
      if(all(season==2025L)) "exploratory_2025_frozen_calibration" else "2024_forward_calibration_development")]
  out[,factual_error:=factual_prediction-factual_q]
  out[,paired_mse_difference:=factual_error^2-(factual_baseline-factual_q)^2]
  # New out-of-range calibrated BIP expectations get explicit rejection.
  contract<-readRDS("data/models/decision_validation/support_contract.rds")
  out[,calibrated_bip_outside_training_target:=primary_supported &
    (q_bip<contract$bip_target_range[1]|q_bip>contract$bip_target_range[2])]
  out[calibrated_bip_outside_training_target==TRUE,`:=`(primary_supported=FALSE,
    support_reason="calibrated_bip_outside_training_target")]
  out[,primary_swing_minus_take:=fifelse(primary_supported,swing_minus_take,NA_real_)]
  list(data=out,calibration_predictions=preds,scored_indices=ix)
}

sv_evaluate <- function(old,new,year,label="") {
  id<-paste0(year,if(nzchar(label)) paste0("_",label) else "")
  component_metrics<-list();comparisons<-list();bins<-list();slices<-list()
  for(kind in SV_COMPONENTS) {
    a<-sv_component(old,kind);b<-sv_component(new,kind)
    ix<-which(old$primary_supported&new$primary_supported&a$keep&is.finite(a$y))
    for(method in c("v2","v3")) {
      p<-if(method=="v2") a$p[ix] else b$p[ix]
      met<-sv_metrics(new[ix],p,a$y[ix],kind,paste0(id,if(year==2024L) "_development" else "_reanalysis"))
      met[,method:=method];component_metrics[[length(component_metrics)+1L]]<-met
      z<-data.table(pred=p,y=a$y[ix],bin=if(kind=="bip_value") pmin(19L,floor(p/.05)) else pmin(19L,floor(p*20)))
      bins[[length(bins)+1L]]<-z[,.(n=.N,prediction=mean(pred),observed=mean(y),bias=mean(pred-y)),by=bin][,
        `:=`(year=year,component=kind,method=method)]
    }
    differences<-sv_loss(a$y[ix],b$p[ix],kind)-sv_loss(a$y[ix],a$p[ix],kind)
    comparisons[[length(comparisons)+1L]]<-cbind(data.table(year=year,component=kind,metric="component_loss_difference"),
      sv_boot(new$game_pk[ix],differences))
    for(dim in c("month","count","family","catcher_evidence")) {
      groups<-switch(dim,month=format(new$game_date[ix],"%m"),count=paste(new$balls[ix],new$strikes[ix]),
        family=new$pitch_family[ix],catcher_evidence=new$catcher_evidence[ix])
      z<-data.table(slice=groups,p=b$p[ix],y=a$y[ix],difference=differences)
      slices[[length(slices)+1L]]<-z[,.(n=.N,prediction=mean(p),observed=mean(y),bias=mean(p-y),
        loss=mean(sv_loss(y,p,kind)),paired_loss_difference=mean(difference)),by=slice][,
        `:=`(year=year,component=kind,dimension=dim)]
    }
  }
  factual<-list()
  for(action in c("swing","take")) {
    ix<-which(new$primary_supported&old$primary_supported&new$observed_action==action&is.finite(new$factual_q))
    delta<-new$factual_error[ix]^2-old$factual_error[ix]^2
    comparisons[[length(comparisons)+1L]]<-cbind(data.table(year=year,component=action,metric="combined_value_mse_difference"),sv_boot(new$game_pk[ix],delta))
    comparisons[[length(comparisons)+1L]]<-cbind(data.table(year=year,component=action,metric="combined_value_bias"),sv_boot(new$game_pk[ix],new$factual_error[ix]))
    for(method in c("v2","v3")) {
      x<-if(method=="v2") old else new
      factual[[length(factual)+1L]]<-data.table(year=year,action=action,method=method,n=length(ix),
        bias=mean(x$factual_error[ix]),rmse=sqrt(mean(x$factual_error[ix]^2)))
    }
  }
  fwrite(rbindlist(component_metrics),file.path(SV_REPORT,sprintf("component_metrics_%s.csv",id)))
  fwrite(rbindlist(comparisons),file.path(SV_REPORT,sprintf("paired_game_intervals_%s.csv",id)))
  fwrite(rbindlist(factual),file.path(SV_REPORT,sprintf("factual_value_metrics_%s.csv",id)))
  fwrite(rbindlist(bins),file.path(SV_REPORT,sprintf("calibration_bins_%s.csv",id)))
  fwrite(rbindlist(slices),file.path(SV_REPORT,sprintf("calibration_slices_%s.csv",id)))
}

sv_run_calibration <- function() {
  setDTthreads(2);dir.create(SV_REPORT,recursive=TRUE,showWarnings=FALSE);dir.create(SV_MODELS,recursive=TRUE,showWarnings=FALSE)
  paths<-c(sprintf("data/models/catcher_framing_v2/decision_values_%d.rds",2024:2025),
    "data/models/catcher_framing_v2/experiment_manifest.rds","data/models/decision_validation/release_manifest.rds",
    "data/models/decision_validation/evaluation_result_2025.rds")
  hashes<-vd_hash(paths)
  old24<-as.data.table(readRDS(paths[1]));calibrators<-sv_select(old24)
  forward<-list()
  for(period in c("early","late")) {
    cut<-as.Date(if(period=="early") "2024-05-01" else "2024-07-01")
    end<-as.Date(if(period=="early") "2024-07-01" else "2025-01-01")
    train<-old24[game_date<cut];test<-old24[game_date>=cut&game_date<end]
    b<-lapply(SV_COMPONENTS,function(kind) sv_fit(train,kind,calibrators[[kind]]$method));names(b)<-SV_COMPONENTS
    forward[[period]]<-b
    message("Combined forward development check: ",period)
    out<-sv_apply(test,b)$data;sv_evaluate(test,out,2024L,paste0("forward_",period))
    rm(out,train,test)
  }
  cs_save(forward,file.path(SV_MODELS,"selected_forward_calibrators.rds"))
  applied<-sv_apply(old24,calibrators,allow_training=TRUE);sv_evaluate(old24,applied$data,2024L)
  cs_save(applied,file.path(SV_MODELS,"calibrated_checkpoint_2024.rds"));rm(old24,applied);gc(verbose=FALSE)
  # Selection is already frozen on disk. No 2025 label has been loaded above.
  frozen_md5<-unname(tools::md5sum(file.path(SV_MODELS,"frozen_calibration.rds")))
  old25<-as.data.table(readRDS(paths[2]));applied<-sv_apply(old25,calibrators)
  sv_evaluate(old25,applied$data,2025L);cs_save(applied,file.path(SV_MODELS,"calibrated_checkpoint_2025.rds"))
  stopifnot(identical(frozen_md5,unname(tools::md5sum(file.path(SV_MODELS,"frozen_calibration.rds")))))
  vd_check_hash(hashes);fwrite(hashes,file.path(SV_REPORT,"unchanged_input_hashes.csv"))
  cs_save(list(inputs=hashes,frozen_calibration_md5=frozen_md5,scope="season_average_v3_calibration_development_and_reanalysis"),
    file.path(SV_MODELS,"calibration_experiment.rds"))
}

if(sys.nframe()==0L) sv_run_calibration()
