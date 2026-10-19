source("season_average_v3.R")

# Aggregate gradients BEFORE covariance propagation. A shared coefficient error
# does not act as independent pitch noise, nor must all pitch effects have the
# same sign. Between separately fitted model stages use an SD-sum bound.
sv_gam_precision <- function(model,f,weight,batter,ids,n_by_hitter,exclude=NULL) {
  V<-if(!is.null(model$Vc)) model$Vc else model$Vp
  gradient<-matrix(0,length(ids),length(coef(model)))
  point<-eta<-numeric(nrow(f))
  for(i in split(seq_len(nrow(f)),ceiling(seq_len(nrow(f))/10000L))) {
    X<-mgcv::predict.gam(model,f[i],type="lpmatrix",exclude=exclude)
    eta[i]<-as.numeric(X%*%coef(model))
    point[i]<-sqrt(pmax(0,rowSums((X%*%V)*X)))
    g<-rowsum(X*weight[i],match(batter[i],ids),reorder=FALSE)
    h<-as.integer(rownames(g));gradient[h,]<-gradient[h,,drop=FALSE]+g
  }
  shared<-sqrt(pmax(0,rowSums((gradient%*%V)*gradient)))*100/n_by_hitter
  list(point=point,shared=shared,eta=eta)
}
sv_cal_precision <- function(b,d,p,weight,ids,n_by_hitter) {
  if(b$method=="identity") return(rep(0,length(ids)))
  f<-sv_features(d,p,b$kind,b$levels)$data
  X<-model.matrix(delete.response(terms(b$model)),f,contrasts.arg=b$model$contrasts)
  gradient<-matrix(0,length(ids),ncol(X))
  g<-rowsum(X*weight,match(d$batter,ids),reorder=FALSE)
  gradient[as.integer(rownames(g)),]<-g
  sqrt(pmax(0,rowSums((gradient%*%b$covariance)*gradient)))*100/n_by_hitter
}
sv_framing_precision <- function(skill,catcher,weight,batter,ids,n_by_hitter) {
  fit<-skill$fit;V<-if(!is.null(fit$Vc)) fit$Vc else fit$Vp
  levels<-levels(fit$model$catcher);idx<-fit$smooth[[1L]]$first.para:fit$smooth[[1L]]$last.para
  gradient<-matrix(0,length(ids),length(coef(fit)))
  h<-match(batter,ids);c<-match(as.character(catcher),levels)
  intercept<-rowsum(weight,h,reorder=FALSE)
  gradient[as.integer(rownames(intercept)),1L]<-intercept[,1L]
  known<-which(!is.na(c))
  tab<-data.table(h=h[known],c=c[known],w=weight[known])[,.(w=sum(w)),by=.(h,c)]
  gradient[cbind(tab$h,idx[tab$c])]<-tab$w
  prior<-data.table(h=h[is.na(c)],catcher=as.character(catcher[is.na(c)]),w=weight[is.na(c)])
  prior<-prior[,.(w=sum(w)),by=.(h,catcher)][,.(variance=skill$tau^2*sum(w^2)),by=h]
  variance<-pmax(0,rowSums((gradient%*%V)*gradient))
  variance[prior$h]<-variance[prior$h]+prior$variance
  sqrt(variance)*100/n_by_hitter
}

sv_propagate <- function(applied,old,raw,m,calibrators,year) {
  out<-applied$data;ix<-which(out$primary_supported);pos<-match(ix,applied$scored_indices)
  d<-out[ix];base<-old[ix]
  raw<-raw[match(dv_key(d),dv_key(raw))]
  stopifnot(!anyNA(raw$game_pk))
  ids<-sort(unique(d$batter));nn<-as.numeric(table(factor(d$batter,levels=ids)))
  a<-as.numeric(d$observed_action=="swing");choice<-a-d$p_swing_reference
  tip<-d$p_foul_tip_given_contact/(1-d$p_in_play_given_contact)
  states<-build_take_state_values(read_re_baseline());k<-match(d$re288_state,states$re288_state)
  derivatives<-vd_gap_derivatives(d$p_contact_given_swing,d$p_in_play_given_contact,tip,d$q_bip,
    d$q_ordinary_foul,d$q_foul_tip,d$q_whiff,d$p_called_strike_if_ordinary_take,
    states$ball_value[k],states$called_strike_value[k])
  shared<-list();point_link<-matrix(NA_real_,nrow(d),5)
  bundles<-list(m$contact,m$post$bip,m$post$tip,m$value)
  for(j in 1:4) {
    kind<-SV_COMPONENTS[j];b<-bundles[[j]];cal<-calibrators[[kind]]
    f<-if(j==1L) ct_transform(raw,b$prep)$data else pc_transform(raw,b$prep)$data
    message("Shared season precision: ",year," ",kind)
    weight<-choice*derivatives[,j]
    p<-sv_gam_precision(b$model,f,weight*cal$slope,d$batter,ids,nn)
    expected<-sv_component(base,kind)$p
    stopifnot(max(abs((if(j==4L) exp(p$eta) else plogis(p$eta))-expected))<1e-8)
    point_link[,j]<-abs(cal$slope)*p$point+applied$calibration_predictions[[kind]]$calibration_link_se[pos]
    shared[[paste0(kind,"_base")]]<-p$shared
    shared[[paste0(kind,"_calibration")]]<-sv_cal_precision(cal,base,sv_component(base,kind)$p,weight,ids,nn)
  }
  message("Shared season precision: ",year," take and framing")
  pred<-as.data.table(readRDS(sprintf("data/models/catcher_framing_v2/strike_predictions_%d.rds",year)))
  pred<-pred[match(dv_key(d),dv_key(pred))]
  stopifnot(max(abs(pred$p_called_strike-base$p_called_strike_if_ordinary_take))<1e-12)
  z<-fr_integrate(pred$base_eta+pred$framing_latent_mean,pred$framing_latent_sd)
  p0<-base$p_called_strike_if_ordinary_take
  chain<-z$derivative/(p0*(1-p0))
  cal<-calibrators$take;weight<-choice*derivatives[,5]
  p<-sv_gam_precision(m$take$model,cs_transform(raw,m$take$prep)$data,
    weight*cal$slope*chain,d$batter,ids,nn,exclude="s(catcher)")
  stopifnot(max(abs(p$eta-pred$base_eta))<1e-8)
  point_link[,5]<-abs(cal$slope)*chain*(p$point+pred$framing_latent_sd)+
    applied$calibration_predictions$take$calibration_link_se[pos]
  shared$take_base<-p$shared
  skill<-readRDS(sprintf("data/models/catcher_framing_v2/latent_framing_for_%d.rds",year))
  shared$catcher_framing<-sv_framing_precision(skill,raw$fielder_2,weight*cal$slope*chain,d$batter,ids,nn)
  shared$take_calibration<-sv_cal_precision(cal,base,p0,weight,ids,nn)
  message("Shared season precision: ",year," reference policy")
  policy<-readRDS("data/models/decision_validation/league_policy_2022_2023.rds")
  pp<-d$p_swing_reference;weight_policy<--d$swing_minus_take*pp*(1-pp)
  p<-sv_gam_precision(policy$model,ct_transform(raw,policy$prep)$data,
    weight_policy*calibrators$policy$slope,d$batter,ids,nn)
  stopifnot(max(abs(plogis(p$eta)-base$p_swing_reference))<1e-8)
  policy_link_se<-abs(calibrators$policy$slope)*p$point+applied$calibration_predictions$policy$calibration_link_se[pos]
  shared$policy_base<-p$shared
  shared$policy_calibration<-sv_cal_precision(calibrators$policy,base,base$p_swing_reference,weight_policy,ids,nn)
  bound<-rowSums(abs(derivatives)*point_link)
  for(nm in c("gap_model_se_bound","gap_ci_low","gap_ci_high","reference_probability_se","above_reference_model_se_bound"))
    set(out,j=nm,value=rep(NA_real_,nrow(out)))
  out[ix,`:=`(gap_model_se_bound=bound,gap_ci_low=swing_minus_take-1.96*bound,
    gap_ci_high=swing_minus_take+1.96*bound,reference_probability_se=pp*(1-pp)*policy_link_se,
    above_reference_model_se_bound=abs(choice)*bound+abs(swing_minus_take)*pp*(1-pp)*policy_link_se)]
  out[,preference:="unsupported"]
  out[ix,preference:=fcase(gap_ci_low>0,"swing",gap_ci_high<0,"take",default="no_clear_preference")]
  out[,primary_decision_status:=ifelse(!primary_supported,"unsupported",paste0("supported_",preference))]
  table<-data.table(batter=ids,supported=nn)
  for(nm in names(shared)) set(table,j=nm,value=shared[[nm]])
  table[,shared_model_se_bound_per100:=rowSums(as.matrix(.SD)),.SDcols=names(shared)]
  list(data=out,shared=table)
}

sv_boot_rate <- function(n,total,B=1000L) {
  v<-replicate(B,{i<-sample.int(length(n),length(n),TRUE);100*sum(total[i])/sum(n[i])})
  c(se=sd(v),low=unname(quantile(v,.025)),high=unname(quantile(v,.975)))
}

sv_season_summary <- function(d,shared,year,reference=NULL) {
  cov<-d[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=.(batter,batter_name)]
  d<-copy(d[primary_supported==TRUE]);d[,score:=(as.numeric(observed_action=="swing")-p_swing_reference)*swing_minus_take]
  game<-d[,.(n=.N,total=sum(score),point_model=sum(above_reference_model_se_bound),date=min(game_date)),by=.(batter,game_pk)]
  setorder(game,batter,date,game_pk)
  stats<-game[,.(games=.N,runs_per100=100*sum(total)/sum(n),
    game_sampling_se_per100=100*vd_cluster_se(n,total),
    pointwise_model_bound_per100=100*sum(point_model)/sum(n)),by=batter]
  set.seed(SV_SEED)
  boot<-game[,{
    v<-if(.N>=2L) sv_boot_rate(n,total) else rep(NA_real_,3)
    .(game_bootstrap_se=v[1],game_resampling_low=v[2],game_resampling_high=v[3])
  },by=batter]
  h<-Reduce(function(x,y) merge(x,y,by="batter",all.x=TRUE),list(cov,stats,boot,
    shared[,.(batter,shared_model_se_bound_per100)]))
  h[,reference_eligible:=supported>=500&games>=60&coverage>=.7]
  if(is.null(reference)) {
    q<-h[reference_eligible==TRUE];w<-q$supported/sum(q$supported)
    center<-sum(w*q$runs_per100);scale<-sqrt(sum(w*(q$runs_per100-center)^2))
    reference<-list(center=center,scale=scale,year=year,scope="2024_development_reference_in_sample_calibration",version=SV_VERSION)
  }
  h[,`:=`(model_only_low=runs_per100-1.96*shared_model_se_bound_per100,
    model_only_high=runs_per100+1.96*shared_model_se_bound_per100,
    conditional_envelope_low=runs_per100-1.96*(game_sampling_se_per100+shared_model_se_bound_per100),
    conditional_envelope_high=runs_per100+1.96*(game_sampling_se_per100+shared_model_se_bound_per100),
    season_decision_index_research=100+10*(runs_per100-reference$center)/reference$scale,
    publication_status="provisional_model_based_season_average",season=year,
    interpretation="supported_pitch_descriptive_mean_no_shrinkage; conditional_uncertainty_not_causal")]
  h[,envelope_width:=conditional_envelope_high-conditional_envelope_low]
  check<-h[is.finite(shared_model_se_bound_per100)&is.finite(pointwise_model_bound_per100)]
  stopifnot(all(check$shared_model_se_bound_per100<=check$pointwise_model_bound_per100+1e-8))
  prefixes<-game[,{
    cum_n<-cumsum(n);cum_t<-cumsum(total);cum_m<-cumsum(point_model)
    rbindlist(lapply(c(100,250,500,1000),function(target) {
      ix<-which(cum_n>=target)[1L]
      if(is.na(ix)||ix<2L) return(NULL)
      data.table(target=target,supported=cum_n[ix],games=ix,date=date[ix],
        runs_per100=100*cum_t[ix]/cum_n[ix],
        game_sampling_se_per100=100*vd_cluster_se(n[1:ix],total[1:ix]),
        conservative_model_bound_per100=100*cum_m[ix]/cum_n[ix])
    }))
  },by=batter]
  fwrite(h,file.path(SV_REPORT,sprintf("season_hitter_summary_%d.csv",year)))
  fwrite(shared,file.path(SV_REPORT,sprintf("shared_uncertainty_components_%d.csv",year)))
  fwrite(prefixes,file.path(SV_REPORT,sprintf("season_to_date_precision_%d.csv",year)))
  h[,sample_band:=cut(supported,c(-Inf,250,500,1000,2000,Inf),right=FALSE)]
  fwrite(h[,.(hitters=.N,median_sampling_se=median(game_sampling_se_per100,na.rm=TRUE),
    median_model_bound=median(shared_model_se_bound_per100,na.rm=TRUE),median_envelope_width=median(envelope_width,na.rm=TRUE),
    median_coverage=median(coverage)),by=sample_band],file.path(SV_REPORT,sprintf("precision_by_exposure_%d.csv",year)))
  # Empirical season-game population is a known reference for an INTERNAL
  # resampling check, not ground truth for a hitter's unobserved latent skill.
  eligible<-h[reference_eligible==TRUE,batter]
  simulations<-game[batter %in% eligible,{
    mu<-100*sum(total)/sum(n);N<-.N
    sims<-replicate(200L,{i<-sample.int(N,N,TRUE);r<-100*sum(total[i])/sum(n[i]);se<-100*vd_cluster_se(n[i],total[i])
      c(covered=abs(r-mu)<=1.96*se,width=3.92*se)})
    .(games=N,coverage=mean(sims[1,]),mean_width=mean(sims[2,]))
  },by=batter]
  fwrite(simulations,file.path(SV_REPORT,sprintf("game_resampling_self_check_%d.csv",year)))
  list(hitters=h,reference=reference,game=game)
}

sv_run_precision <- function() {
  setDTthreads(2)
  frozen<-readRDS(file.path(SV_MODELS,"frozen_calibration.rds"));vd_check_hash(frozen$code)
  experiment<-readRDS(file.path(SV_MODELS,"calibration_experiment.rds"));vd_check_hash(experiment$inputs)
  m<-vd_load();reference<-NULL
  for(year in 2024:2025) {
    message("Season-average precision: ",year)
    applied<-readRDS(file.path(SV_MODELS,sprintf("calibrated_checkpoint_%d.rds",year)))
    old<-as.data.table(readRDS(sprintf("data/models/catcher_framing_v2/decision_values_%d.rds",year)))
    raw<-fr_read(year)
    result<-sv_propagate(applied,old,raw,m,frozen$calibrators,year)
    cs_save(result$data,file.path(SV_MODELS,sprintf("decision_values_%d.rds",year)))
    summary<-sv_season_summary(result$data,result$shared,year,reference)
    if(year==2024L) {reference<-summary$reference;cs_save(reference,file.path(SV_MODELS,"season_reference_2024.rds"))}
    rm(applied,old,raw,result,summary);gc(verbose=FALSE)
  }
  vd_check_hash(experiment$inputs)
  cs_save(list(inputs=experiment$inputs,calibration_md5=experiment$frozen_calibration_md5,
    precision_code=vd_hash("season_precision_v3.R"),scope="conditional_season_average_precision_not_nominal_full_pipeline_coverage"),
    file.path(SV_MODELS,"precision_experiment.rds"))
}
if(sys.nframe()==0L) sv_run_precision()
