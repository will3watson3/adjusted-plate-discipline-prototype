# Umpire-adjusted descriptive hitter aggregation. Run Rscript hitter_stat_v6.R.
source('umpire_v5.R')
source('season_precision_v3.R')
H6_MODELS <- 'data/models/hitter_stat_v6'
H6_REPORT <- 'reports/hitter_stat_v6'
H6_VERSION <- 'decision_plus_umpire_v6'

# Sum signed gradients within hitter, including every covariance in the joint fit.
# Unseen identities remain shared latent effects across their repeated pitches.
h6_joint_precision <- function(b,d,w,ids,nn) {
  h<-match(d$batter,ids);G<-matrix(0,length(ids),length(coef(b$fit)))
  fi<-b$fixed_indices
  X<-model.matrix(~region,data.frame(region=factor(d$region,levels=b$region_levels)))
  a<-rowsum(X*w,h,reorder=FALSE);G[as.integer(rownames(a)),fi]<-a
  prior<-numeric(length(ids))
  for(g in names(b$groups)) {
    tab<-b$groups[[g]]$table;k<-match(d[[g]],tab$id);known<-!is.na(k)
    a<-data.table(h=h[known],ix=tab$index[k[known]],w=w[known])[,.(w=sum(w)),by=.(h,ix)]
    if(nrow(a)) G[cbind(a$h,a$ix)]<-a$w
    # Missing identity is conservatively one unknown identity per group.
    u<-as.character(d[[g]][!known]);u[is.na(u)]<-'__missing_identity__'
    a<-data.table(h=h[!known],id=u,w=w[!known])[,.(w=sum(w)),by=.(h,id)][,.(v=sum(w^2)*b$groups[[g]]$tau^2),by=h]
    prior[a$h]<-prior[a$h]+a$v
  }
  sqrt(pmax(0,rowSums((G%*%b$V)*G))+prior)*100/nn
}

# Intercept sandwich SE clustered by historical game. Histories overlap, so the
# hitter bound sums weekly SDs, allowing arbitrary covariance between weeks.
h6_weekly <- function(d,b,ledger) {
  p<-u5_predict(b,d)$p;out<-copy(ledger);out[,se:=0]
  for(i in seq_len(nrow(out))) if(out$n[i]>=2000L) {
    ix<-which(d$valid&d$take&d$game_date<as.Date(out$start[i]))
    stopifnot(length(ix)==out$n[i],max(d$game_date[ix])==as.Date(out$train_end[i]))
    mu<-plogis(qlogis(pmin(1-1e-8,pmax(1e-8,p[ix])))+out$intercept[i])
    g<-rowsum(d$y[ix]-mu,d$game_pk[ix]);N<-nrow(g)
    stopifnot(N>1L)
    out$se[i]<-sqrt(sum(g^2)*N/(N-1))/sum(mu*(1-mu))
  }
  out
}
h6_week_index <- function(d,l) {
  anchor<-as.Date(sprintf('%d-01-01',unique(d$game_year)))
  k<-match(as.character(anchor+7L*(as.integer(as.Date(d$game_date)-anchor)%/%7L)),as.character(l$start))
  stopifnot(!anyNA(k));k
}

h6_propagate <- function(old,pred,b,m,ledger,year) {
  out<-copy(old);k<-match(dv_key(out),dv_key(pred));stopifnot(!anyNA(k),!anyDuplicated(out,by=CS_KEYS))
  p<-pred$umpire_region_model[k];st<-build_take_state_values(read_re_baseline());sk<-match(out$re288_state,st$re288_state)
  out[,`:=`(p_called_strike_if_ordinary_take=p,
    q_take_basic=(1-p)*st$ball_value[sk]+p*st$called_strike_value[sk])]
  out[,`:=`(swing_minus_take=q_swing_basic-q_take_basic,take_run_value_basic=q_take_basic-re_before)]
  out[,primary_swing_minus_take:=fifelse(primary_supported,swing_minus_take,NA_real_)]
  out[,score:=(as.integer(observed_action=='swing')-p_swing_reference)*swing_minus_take]
  ix<-which(out$primary_supported);d<-out[ix];z<-pred[k[ix]]
  ids<-sort(unique(d$batter));nn<-as.numeric(table(factor(d$batter,levels=ids)))
  choice<-as.integer(d$observed_action=='swing')-d$p_swing_reference
  deriv<-vd_gap_derivatives(d$p_contact_given_swing,d$p_in_play_given_contact,
    d$p_foul_tip_given_contact/(1-d$p_in_play_given_contact),d$q_bip,d$q_ordinary_foul,
    d$q_foul_tip,d$q_whiff,d$p_called_strike_if_ordinary_take,st$ball_value[sk[ix]],st$called_strike_value[sk[ix]])
  shared<-list();point<-matrix(0,nrow(d),5)
  bundles<-list(m$contact,m$post$bip,m$post$tip,m$value)
  for(j in 1:4) {
    message('Hitter uncertainty ',year,' ',SV_COMPONENTS[j])
    bb<-bundles[[j]];f<-if(j==1L) ct_transform(d,bb$prep)$data else pc_transform(d,bb$prep)$data
    q<-sv_gam_precision(bb$model,f,choice*deriv[,j],d$batter,ids,nn)
    expected<-sv_component(d,SV_COMPONENTS[j])$p
    stopifnot(max(abs((if(j==4L) exp(q$eta) else plogis(q$eta))-expected))<1e-8)
    point[,j]<-q$point;shared[[SV_COMPONENTS[j]]]<-q$shared
  }
  message('Hitter uncertainty ',year,' joint catcher/umpire and strike base')
  latent<-u5_predict(b,z);integ<-fr_integrate(z$base_eta+latent$effect,latent$latent_sd)
  wk<-h6_week_index(z,ledger);pf<-d$p_called_strike_if_ordinary_take
  reconstructed<-plogis(qlogis(pmin(1-1e-8,pmax(1e-8,integ$p)))+ledger$intercept[wk])
  stopifnot(max(abs(reconstructed-pf))<1e-12)
  chain<-integ$derivative/(integ$p*(1-integ$p))
  # The explicit numerical floor is constant outside its interior.
  chain[integ$p<=1e-8|integ$p>=1-1e-8]<-0
  raw<-v4_raw(year);raw<-raw[match(dv_key(d),dv_key(raw))];stopifnot(!anyNA(raw$game_pk))
  weight<-choice*deriv[,5]
  q<-sv_gam_precision(m$take$model,cs_transform(raw,m$take$prep)$data,
    weight*chain,d$batter,ids,nn,exclude='s(catcher)')
  stopifnot(max(abs(q$eta-z$base_eta))<1e-8)
  shared$strike_base<-q$shared
  shared$catcher_umpire_joint<-h6_joint_precision(b,z,weight*chain,ids,nn)
  a<-data.table(h=match(d$batter,ids),week=wk,w=weight)[,.(w=sum(w)),by=.(h,week)]
  a[,v:=abs(w)*ledger$se[week]];a<-a[,.(v=sum(v)),by=h]
  shared$weekly_calibration<-numeric(length(ids));shared$weekly_calibration[a$h]<-100*a$v/nn[a$h]
  point[,5]<-chain*(q$point+latent$latent_sd)+ledger$se[wk]
  rm(raw,q);gc(verbose=FALSE)
  message('Hitter uncertainty ',year,' league reference')
  policy<-readRDS('data/models/decision_validation/league_policy_2022_2023.rds')
  pp<-d$p_swing_reference
  q<-sv_gam_precision(policy$model,ct_transform(d,policy$prep)$data,
    -d$swing_minus_take*pp*(1-pp),d$batter,ids,nn)
  stopifnot(max(abs(plogis(q$eta)-pp))<1e-8)
  shared$league_reference<-q$shared
  bound<-rowSums(abs(deriv)*point);policy_se<-pp*(1-pp)*q$point
  fields<-c('gap_model_se_bound','gap_ci_low','gap_ci_high','reference_probability_se','above_reference_model_se_bound')
  for(nm in fields) set(out,j=nm,value=rep(NA_real_,nrow(out)))
  out[ix,`:=`(gap_model_se_bound=bound,gap_ci_low=swing_minus_take-1.96*bound,
    gap_ci_high=swing_minus_take+1.96*bound,reference_probability_se=policy_se,
    above_reference_model_se_bound=abs(choice)*bound+abs(swing_minus_take)*policy_se)]
  out[,preference:='unsupported'];out[ix,preference:=fcase(gap_ci_low>0,'swing',gap_ci_high<0,'take',default='no_clear_preference')]
  out[,`:=`(primary_decision_status=ifelse(primary_supported,paste0('supported_',preference),'unsupported'),
    statistic_version=H6_VERSION,uncertainty_status='recomputed_conditional_first_order_sd_sum_bound',
    time_role=ifelse(year==2025L,'exploratory_2025_reanalysis','2024_development_reference'))]
  # Remove stale factual errors and old catcher-only latent diagnostics.
  drop<-intersect(c('factual_prediction','factual_error','paired_mse_difference','framing_latent_mean','framing_latent_sd','catcher_evidence'),names(out))
  out[,(drop):=NULL]
  tab<-data.table(batter=ids,supported=nn)
  for(nm in names(shared)) set(tab,j=nm,value=shared[[nm]])
  tab[,shared_model_se_bound_per100:=rowSums(as.matrix(.SD)),.SDcols=names(shared)]
  list(data=out,shared=tab)
}

h6_block <- function(n,total,L,B=500L) {
  N<-length(n)
  if(N<2L) return(c(se=NA_real_,low=NA_real_,high=NA_real_))
  draws<-replicate(B,{
    starts<-sample.int(N,ceiling(N/L),TRUE)
    ix<-as.vector(vapply(starts,function(s) (s-1L+seq_len(L)-1L)%%N+1L,integer(L)))[seq_len(N)]
    100*sum(total[ix])/sum(n[ix])
  })
  c(se=sd(draws),low=unname(quantile(draws,.025)),high=unname(quantile(draws,.975)))
}
h6_summary <- function(d,shared,year,reference=NULL) {
  cov<-d[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=.(batter,batter_name)]
  g<-d[primary_supported==TRUE,.(n=.N,total=sum(score),date=min(game_date)),by=.(batter,game_pk)]
  setorder(g,batter,date,game_pk)
  h<-g[,.(games=.N,runs_per100=100*sum(total)/sum(n),game_sampling_se_per100=100*vd_cluster_se(n,total)),by=batter]
  h<-merge(merge(cov,h,by='batter',all.x=TRUE),shared[,.(batter,shared_model_se_bound_per100)],by='batter',all.x=TRUE)
  h[,qualified:=supported>=500&games>=60&coverage>=.7]
  if(is.null(reference)) {
    z<-h[qualified==TRUE];w<-z$supported/sum(z$supported);center<-sum(w*z$runs_per100)
    reference<-list(center=center,scale=sqrt(sum(w*(z$runs_per100-center)^2)),year=2024L,version=H6_VERSION)
  }
  stopifnot(is.finite(reference$scale),reference$scale>0,reference$year==2024L)
  h[,`:=`(decision_plus=100+10*(runs_per100-reference$center)/reference$scale,
    model_only_low=runs_per100-1.96*shared_model_se_bound_per100,
    model_only_high=runs_per100+1.96*shared_model_se_bound_per100,
    conditional_envelope_low=runs_per100-1.96*(shared_model_se_bound_per100+game_sampling_se_per100),
    conditional_envelope_high=runs_per100+1.96*(shared_model_se_bound_per100+game_sampling_se_per100),
    season=year,statistic_version=H6_VERSION,publication_status='descriptive_model_based_conditional_uncertainty')]
  h[,`:=`(decision_plus_model_low=100+10*(model_only_low-reference$center)/reference$scale,
    decision_plus_model_high=100+10*(model_only_high-reference$center)/reference$scale)]
  set.seed(20261019L)
  blocks<-rbindlist(lapply(c(1L,7L,14L),function(L) g[batter%in%h[qualified==TRUE,batter],{
    b<-h6_block(n,total,L);.(block_games=L,se=b[1],low=b[2],high=b[3])
  },by=batter]))
  fwrite(h,file.path(H6_REPORT,sprintf('hitters_%d.csv',year)))
  fwrite(h[qualified==TRUE][order(-decision_plus)],file.path(H6_REPORT,sprintf('qualified_hitters_%d.csv',year)))
  fwrite(g,file.path(H6_REPORT,sprintf('hitter_games_%d.csv',year)))
  fwrite(blocks,file.path(H6_REPORT,sprintf('sampling_sensitivity_%d.csv',year)))
  fwrite(shared,file.path(H6_REPORT,sprintf('uncertainty_components_%d.csv',year)))
  list(hitters=h,reference=reference)
}
h6_run <- function() {
  setDTthreads(2);dir.create(H6_REPORT,recursive=TRUE,showWarnings=FALSE);dir.create(H6_MODELS,recursive=TRUE,showWarnings=FALSE)
  protocol<-readRDS(file.path(U5_MODELS,'frozen_protocol.rds'));vd_check_hash(protocol$hashes)
  stopifnot(protocol$selected=='umpire_region_model')
  paths<-c('hitter_stat_v6.R','HITTER_STAT_V6_PLAN.md','season_precision_v3.R',
    file.path(U5_MODELS,'frozen_protocol.rds'),protocol$hashes$path,
    unlist(lapply(2024:2025,function(y) c(sprintf('data/models/umpire_v5/predictions_%d.rds',y),
      sprintf('data/models/umpire_v5/fit_umpire_region_model_for_%d.rds',y),
      sprintf('reports/umpire_v5/weekly_umpire_region_model_%d.csv',y)))))
  hashes<-vd_hash(unique(paths));vd_check_hash(hashes);fwrite(hashes,file.path(H6_REPORT,'input_hashes.csv'))
  m<-vd_load();reference<-NULL;overview<-list()
  for(year in 2024:2025) {
    message('Connect umpire model to hitters ',year)
    old<-as.data.table(readRDS(sprintf('data/models/catcher_framing_v2/decision_values_%d.rds',year)))
    pred<-as.data.table(readRDS(sprintf('data/models/umpire_v5/predictions_%d.rds',year)))
    b<-readRDS(sprintf('data/models/umpire_v5/fit_umpire_region_model_for_%d.rds',year))
    l<-h6_weekly(pred,b,fread(sprintf('reports/umpire_v5/weekly_umpire_region_model_%d.csv',year)))
    fwrite(l,file.path(H6_REPORT,sprintf('weekly_uncertainty_%d.csv',year)))
    r<-h6_propagate(old,pred,b,m,l,year)
    cs_save(r$data,file.path(H6_MODELS,sprintf('decision_values_%d.rds',year)))
    h<-h6_summary(r$data,r$shared,year,reference);reference<-h$reference
    cs_save(reference,file.path(H6_MODELS,'season_reference_2024.rds'))
    prev<-fread(sprintf('reports/umpire_v5/selected_hitter_changes_%d.csv',year))
    comparison<-merge(h$hitters,prev[,.(batter,v4_runs100,change_runs100)],by='batter',all.x=TRUE)
    fwrite(comparison,file.path(H6_REPORT,sprintf('comparison_v4_%d.csv',year)))
    overview[[as.character(year)]]<-h$hitters[,.(year=year,eligible=sum(eligible),supported=sum(supported),qualified=sum(qualified,na.rm=TRUE),
      median_model_se=median(shared_model_se_bound_per100[qualified],na.rm=TRUE),median_sampling_se=median(game_sampling_se_per100[qualified],na.rm=TRUE))]
    rm(old,pred,b,r,h);gc(verbose=FALSE)
  }
  fwrite(rbindlist(overview),file.path(H6_REPORT,'overview.csv'));vd_check_hash(hashes)
  cs_save(list(version=H6_VERSION,strike_method=protocol$selected,reference=reference,inputs=hashes,
    years=2024:2025,status='adopted_for_descriptive_hitter_scoring',
    limitation='exploratory_2025_not_fresh_holdout; conditional_uncertainty; incremental_run_value_gain_inconclusive'),file.path(H6_MODELS,'manifest.rds'))
}
if(sys.nframe()==0L) h6_run()
