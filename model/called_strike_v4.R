# Targeted temporal audit; preserves frozen v1-v3. Run Rscript called_strike_v4.R
source('season_average_v3.R')
V4_REPORT <- 'reports/called_strike_v4'
V4_MODELS <- 'data/models/called_strike_v4'
V4_METHODS <- c('identity','expanding','recent28','refresh')
v4_methods <- function(year) {
 if(year==2025L && readRDS(file.path(V4_MODELS,'frozen_protocol.rds'))$selected!='refresh') setdiff(V4_METHODS,'refresh') else V4_METHODS
}
v4_write <- function(x,name) fwrite(x,file.path(V4_REPORT,paste0(name,'.csv')))
v4_raw <- function(year) {
 path<-sprintf('data/processed/statcast_%d_clean.rds',year)
 if(file.exists(path)) d<-as.data.table(readRDS(path)) else {
  stopifnot(year==2025L)
  d<-fread(cmd='gzip -dc data/processed/statcast_2025_analysis.csv.gz',select=CS_INPUTS)
  d[,game_date:=as.IDate(game_date)]
  old<-as.data.table(readRDS('data/models/catcher_framing_v2/decision_values_2025.rds'))
  k<-match(dv_key(old),dv_key(d));stopifnot(!anyNA(k))
  fields<-intersect(CS_INPUTS,names(old));fields<-setdiff(fields,c('game_year'))
  checks<-rbindlist(lapply(fields,function(nm) data.table(field=nm,
    matches=isTRUE(all.equal(d[[nm]][k],old[[nm]],check.attributes=FALSE)))))
  stopifnot(all(checks$matches));v4_write(checks,'csv_2025_feature_reconciliation')
 }
 stopifnot(!anyDuplicated(d,by=CS_KEYS),all(d$game_year==year))
 d[,..CS_INPUTS]
}
v4_load <- function(year) {
 r<-v4_raw(year);p<-as.data.table(readRDS(sprintf('data/models/catcher_framing_v2/strike_predictions_%d.rds',year)))
 k<-match(dv_key(p),dv_key(r));stopifnot(!anyNA(k));d<-r[k]
 d[,`:=`(p=p$p_called_strike,mu=p$framing_latent_mean,sd=p$framing_latent_sd,
   catcher_evidence=p$catcher_evidence,valid=!p$invalid&!p$novel_fixed_category,
   y=as.integer(description=='called_strike'))]
 d[,take:=description %chin% c('ball','blocked_ball','called_strike')]
 d[,z:=(plate_z-sz_bot)/(sz_top-sz_bot)]
 if(year>=2024L) {
  q<-as.data.table(readRDS(sprintf('data/models/catcher_framing_v2/decision_values_%d.rds',year)))
  j<-match(dv_key(d),dv_key(q));stopifnot(!anyNA(j));d[,supported:=q$primary_supported[j]]
 } else d[,supported:=FALSE]
 d
}
v4_intercept <- function(p,y) {
 stopifnot(length(p)==length(y),length(y)>0L,all(y %in% 0:1),all(is.finite(p)))
 lp<-qlogis(pmin(1-1e-8,pmax(1e-8,p)))
 # Bounded one-dimensional MLE; boundary hits are explicit failures.
 fit<-uniroot(function(a) sum(plogis(lp+a)-y),c(-12,12),tol=1e-10)$root
 stopifnot(abs(fit)<11.99);fit
}
v4_online <- function(d,days=Inf) {
 # Fixed seven-day blocks anchored at Jan 1, never at observed row order.
 anchor<-as.Date(sprintf('%d-01-01',unique(d$game_year)))
 block<-anchor+7L*(as.integer(as.Date(d$game_date)-anchor)%/%7L)
 out<-d$p;ledger<-list()
 for(date in sort(unique(block))) {
  date<-as.Date(date,origin='1970-01-01')
  h<-which(d$valid&d$take&d$game_date<date & (is.infinite(days)|d$game_date>=date-days))
  ix<-which(block==date);a<-if(length(h)>=2000L) v4_intercept(d$p[h],d$y[h]) else 0
  out[ix]<-plogis(qlogis(pmin(1-1e-8,pmax(1e-8,d$p[ix])))+a)
  ledger[[length(ledger)+1L]]<-data.table(start=date,n=length(h),train_end=if(length(h)) max(d$game_date[h]) else as.IDate(NA),intercept=a)
 }
 list(p=out,ledger=rbindlist(ledger))
}
v4_framing <- function(skill,d) {
 k<-match(as.character(d$fielder_2),skill$catchers$catcher);has<-!is.na(k)
 mu<-rep(skill$mu,nrow(d));sd<-rep(sqrt(skill$tau^2+skill$mu_variance),nrow(d))
 mu[has]<-skill$catchers$latent_mean[k[has]];sd[has]<-skill$catchers$latent_sd[k[has]]
 list(mu=mu,sd=sd)
}
v4_refresh_fit <- function(history,skill,cutoff) {
 stopifnot(all(history$game_date<cutoff),skill$evidence_end<as.character(cutoff))
 history<-copy(history[cs_eligible(history)]);history[,season:=game_year]
 prep<-cs_preprocessor(history);t<-cs_transform(history,prep);f<-t$data
 f[,`:=`(y=as.integer(history$description=='called_strike'),framing_offset=v4_framing(skill,history)$mu)]
 base<-readRDS('data/models/called_strike/gam_validation_fit_2022_2023.rds')
 # Use labels of the actual smooth rather than relying on exact call spelling.
 rhs<-base$spec$rhs;rhs<-gsub("s(catcher,bs='re')",'offset(framing_offset)',rhs,fixed=TRUE)
 stopifnot(!grepl('s(catcher',rhs,fixed=TRUE),grepl('framing_offset',rhs,fixed=TRUE))
 form<-as.formula(paste('y~',rhs),env=globalenv())
 set.seed(20261017);fit<-bam(form,data=f,family=binomial(),method='fREML',discrete=TRUE,
   select=TRUE,gamma=base$spec$gamma,nthreads=2,control=gam.control(maxit=100))
 stopifnot(isTRUE(fit$converged),isTRUE(fit$mgcv.conv),all(is.finite(coef(fit))))
 list(model=fit,prep=prep,train_end=as.character(max(history$game_date)),train_n=nrow(history),skill=skill,cutoff=as.character(cutoff))
}
v4_refresh_predict <- function(b,d) {
 stopifnot(all(d$game_date>as.Date(b$train_end)))
 t<-cs_transform(d,b$prep);f<-t$data;fr<-v4_framing(b$skill,d);f[,framing_offset:=fr$mu]
 # Predict mean offset, then integrate random-effect uncertainty around it.
 eta<-rep(NA_real_,nrow(d))
 for(ix in split(seq_len(nrow(d)),ceiling(seq_len(nrow(d))/10000L)))
  eta[ix]<-as.numeric(mgcv::predict.gam(b$model,f[ix],type='link'))
 p<-fr_integrate(eta,fr$sd)$p
 p[t$invalid|t$novel_category]<-d$p[t$invalid|t$novel_category]
 p
}
v4_audit <- function(d,year) {
 d<-copy(d[valid&take]);d[,`:=`(zone_height=sz_top-sz_bot,count=paste(balls,strikes,sep='-'),
   hands=paste(stand,p_throws,sep='/'),xb=floor(plate_x/.2)*.2,zb=floor(z/.1)*.1,
   absolute_z=floor(plate_z/.25)*.25,
   height_band=as.character(cut(sz_top-sz_bot,c(-Inf,1.4,1.6,1.8,Inf))),
   region=fifelse(z<0,'below',fifelse(z>1,'above',fifelse(abs(plate_x)>.83,'side','inside_rectangle'))))]
 slices<-rbindlist(lapply(c('count','hands','height_band','region','catcher_evidence'),function(nm)
  d[,.(n=.N,predicted=mean(p),actual=mean(y),bias=mean(p-y),loss=mean(cs_loss(y,p))),by=.(slice=get(nm))][,dimension:=nm]))
 v4_write(slices[,year:=year],paste0('slices_',year))
 loc<-d[,.(n=.N,predicted=mean(p),actual=mean(y),bias=mean(p-y)),by=.(xb,zb,stand)]
 v4_write(loc[,year:=year],paste0('normalized_location_',year))
 v4_write(d[,.(n=.N,predicted=mean(p),actual=mean(y),bias=mean(p-y)),by=.(xb,absolute_z,stand)][,year:=year],paste0('absolute_location_',year))
 # Same-batter medians separate cohort turnover from recorded-bound changes.
 bats<-d[,.(n=.N,bottom=median(sz_bot),top=median(sz_top),height=median(sz_top-sz_bot),
   mean_bottom=mean(sz_bot),mean_top=mean(sz_top)),by=batter][,year:=year]
 v4_write(bats,paste0('batter_zone_',year))
 summary<-d[,.(n=.N,predicted=mean(p),actual=mean(y),bias=mean(p-y),loss=mean(cs_loss(y,p)),
  bottom=mean(sz_bot),top=mean(sz_top),height=mean(zone_height)),by=.(population=fifelse(supported,'supported','unsupported'))]
 v4_write(summary[,year:=year],paste0('audit_summary_',year))
 bats
}
v4_metrics <- function(d,year,phase) {
 out<-boot<-list()
 for(pop in c('all_takes','supported_takes')) {
  keep<-d$valid&d$take & if(pop=='supported_takes') d$supported else TRUE
  for(m in v4_methods(year)) {
   ix<-which(keep);y<-d$y[ix];p<-d[[m]][ix]
   out[[length(out)+1L]]<-data.table(year,phase,population=pop,method=m,n=length(ix),bias=mean(p-y),
    loss=mean(cs_loss(y,p)),brier=mean((p-y)^2))
   boot[[length(boot)+1L]]<-cbind(data.table(year,phase,population=pop,method=m),
    sv_boot(d$game_pk[ix],cs_loss(y,p)-cs_loss(y,d$p[ix]),B=1000L))
  }
 }
 list(metrics=rbindlist(out),paired=rbindlist(boot))
}
v4_combined <- function(d,year,selected) {
 old<-as.data.table(readRDS(sprintf('data/models/catcher_framing_v2/decision_values_%d.rds',year)))
 j<-match(dv_key(old),dv_key(d));stopifnot(!anyNA(j));states<-build_take_state_values(read_re_baseline());k<-match(old$re288_state,states$re288_state)
 base<-old$q_take_basic;out<-sensitivity<-hitters<-list()
 for(m in v4_methods(year)) {
  p<-d[[m]][j];qt<-(1-p)*states$ball_value[k]+p*states$called_strike_value[k]
  gap<-old$q_swing_basic-qt;score<-(as.integer(old$observed_action=='swing')-old$p_swing_reference)*gap
  orig<-(as.integer(old$observed_action=='swing')-old$p_swing_reference)*old$swing_minus_take
  for(phase in c('all','may_on')) {
   keep<-old$primary_supported & if(phase=='may_on') old$game_date>=as.Date(sprintf('%d-05-01',year)) else TRUE
   ix<-which(keep&old$observed_action=='take'&is.finite(old$factual_q))
   err<-qt[ix]-old$factual_q[ix];prev<-base[ix]-old$factual_q[ix]
   out[[length(out)+1L]]<-cbind(data.table(year,phase,method=m,n=length(ix),bias=mean(err),rmse=sqrt(mean(err^2)),
    baseline_bias=mean(prev),baseline_rmse=sqrt(mean(prev^2))),sv_boot(old$game_pk[ix],err^2-prev^2))
   ix<-which(keep)
   sensitivity[[length(sensitivity)+1L]]<-data.table(year,phase,method=m,n=length(ix),
    mean_gap_change=mean(gap[ix]-old$swing_minus_take[ix]),mean_abs_gap_change=mean(abs(gap[ix]-old$swing_minus_take[ix])),
    sign_change_rate=mean((gap[ix]>0)!=(old$swing_minus_take[ix]>0)),score_change_runs100=100*mean(score[ix]-orig[ix]),
    unchanged_swing_value=TRUE,unchanged_reference=TRUE,unchanged_support=TRUE)
  }
  if(m==selected) {
   h<-data.table(batter=old$batter,batter_name=old$batter_name,supported=old$primary_supported,score,orig)
   hitters[[1L]]<-h[supported==TRUE,.(n=.N,raw_runs100=100*mean(score),v2_raw_runs100=100*mean(orig),
    change_runs100=100*mean(score-orig)),by=.(batter,batter_name)]
   # No stale uncertainty fields are copied into the new artifact.
   values<-old[,c(CS_KEYS,'game_date','batter','observed_action','primary_supported'),with=FALSE]
   values[,`:=`(p_called_strike=p,q_swing=old$q_swing_basic,q_take=qt,gap=gap,reference=old$p_swing_reference,
    score=score,method=selected,uncertainty_status='not_refit_do_not_reuse_v2_intervals')]
   cs_save(values,file.path(V4_MODELS,sprintf('decision_comparison_%d.rds',year)))
  }
 }
 v4_write(rbindlist(out),paste0('combined_take_',year));v4_write(rbindlist(sensitivity),paste0('comparison_sensitivity_',year))
 v4_write(rbindlist(hitters),paste0('selected_hitter_changes_',year))
}
v4_run <- function() {
 dir.create(V4_REPORT,recursive=TRUE,showWarnings=FALSE);dir.create(V4_MODELS,recursive=TRUE,showWarnings=FALSE)
 paths<-c('CALLED_STRIKE_V4_PLAN.md','called_strike_v4.R',sprintf('data/models/catcher_framing_v2/decision_values_%d.rds',2024:2025),
  sprintf('data/models/catcher_framing_v2/strike_predictions_%d.rds',2023:2025),
  sprintf('data/processed/statcast_%d_clean.rds',2022:2024),'data/processed/statcast_2025_analysis.csv.gz','data/models/called_strike/gam_validation_fit_2022_2023.rds')
 hashes<-vd_hash(paths);v4_write(hashes,'input_hashes')
 bats<-list();message('Audit 2023');d23<-v4_load(2023L);bats[[1]]<-v4_audit(d23,2023L);rm(d23);gc()
 history<-rbindlist(lapply(2022:2023,function(y){r<-v4_raw(y);r[cs_eligible(r)]}))
 runyear<-function(year,history) {
  message('Loading ',year);d<-v4_load(year);bats[[as.character(year)]]<<-v4_audit(d,year)
  d[,identity:=p]
  for(m in c('expanding','recent28')) {
   online_result<-v4_online(d,if(m=='recent28') 28L else Inf);set(d,j=m,value=online_result$p)
   v4_write(online_result$ledger[,`:=`(year=year,method=m)],paste0('weekly_',m,'_',year))
  }
  d[,refresh:=p];skill<-readRDS(sprintf('data/models/catcher_framing_v2/latent_framing_for_%d.rds',year))
  for(month in if('refresh'%in%v4_methods(year)) c('05','07') else character()) {
   cutoff<-as.Date(sprintf('%d-%s-01',year,month));end<-if(month=='05') as.Date(sprintf('%d-07-01',year)) else as.Date(sprintf('%d-12-31',year))
   path<-file.path(V4_MODELS,sprintf('refresh_%d_%s.rds',year,month))
   message('Refreshed model ',year,' ',month)
   if(file.exists(path)) {
    b<-readRDS(path)
    if(!identical(b$experiment_hash,hashes$md5[hashes$path=='called_strike_v4.R'])) {
     # Archived version differs only in data loading. Verify exact fit/predict code
     # and unchanged pre-2025 training files before reusing expensive fits.
     snap<-'data/models/called_strike_v4/experiment_source_initial.R'
     stopifnot(identical(b$experiment_hash,unname(tools::md5sum(snap))))
     oldenv<-new.env(parent=globalenv());sys.source(snap,envir=oldenv)
     stopifnot(identical(body(v4_refresh_fit),body(oldenv$v4_refresh_fit)),
       identical(body(v4_refresh_predict),body(oldenv$v4_refresh_predict)))
     previous<-fread('data/models/called_strike_v4/input_hashes_initial.csv')
     before<-previous[!is.na(md5)&nzchar(md5)&path!='called_strike_v4.R']
     vd_check_hash(before)
    }
   } else {
    train<-rbindlist(list(history,d[game_date<cutoff,..CS_INPUTS]),use.names=TRUE)
    b<-v4_refresh_fit(train,skill,cutoff);b$experiment_hash<-hashes$md5[hashes$path=='called_strike_v4.R'];cs_save(b,path);rm(train);gc()
   }
   ix<-which(d$game_date>=cutoff&d$game_date<end);d[ix,refresh:=v4_refresh_predict(b,d[ix])]
   rm(b);gc()
  }
  if(!'refresh'%in%v4_methods(year)) d[,refresh:=NA_real_]
  cs_save(d[,c(CS_KEYS,'game_date','game_year','take','valid','supported','y','p',V4_METHODS),with=FALSE],
    file.path(V4_MODELS,sprintf('predictions_%d.rds',year)))
  d
 }
 d24<-runyear(2024L,history);metrics<-paired<-list()
 for(phase in c('early','late','may_on','all')) {
  z<-switch(phase,early=d24[game_date>=as.Date('2024-05-01')&game_date<as.Date('2024-07-01')],
    late=d24[game_date>=as.Date('2024-07-01')],may_on=d24[game_date>=as.Date('2024-05-01')],all=d24)
  a<-v4_metrics(z,2024L,phase);metrics[[phase]]<-a$metrics;paired[[phase]]<-a$paired
 }
 met<-rbindlist(metrics);pa<-rbindlist(paired);v4_write(met,'metrics_2024');v4_write(pa,'paired_2024')
 z<-d24[valid&take&game_date>=as.Date('2024-05-01')];loss<-vapply(V4_METHODS,function(m) mean(cs_loss(z$y,z[[m]])),numeric(1));best<-names(which.min(loss))
 select<-rbindlist(lapply(V4_METHODS,function(m) {
  b<-sv_boot(z$game_pk,cs_loss(z$y,z[[m]])-cs_loss(z$y,z[[best]]))
  bad<-any(pa[population=='all_takes'&phase %chin% c('early','late')&method==m]$ci_low>0)
  data.table(method=m,loss=loss[[m]],best=best,delta_best=b$estimate,se_best=b$se,clearly_worse_period=bad,
    eligible=!bad & b$estimate<=b$se+1e-12)
 }))
 selected<-select[eligible==TRUE]$method[1];stopifnot(length(selected)==1L)
 select[,selected:=method==selected];v4_write(select,'selection_2024')
 cs_save(list(selected=selected,selection=select,hashes=hashes,training_labels_end='2024-09-30',
  test_role='previously_inspected_2025_prequential_reanalysis'),file.path(V4_MODELS,'frozen_protocol.rds'))
 message('Frozen historical selection: ',selected)
 v4_combined(d24,2024L,selected)
 # Only now load 2025 outcomes. History is strictly pre-2025.
 history<-rbindlist(list(history,d24[take&valid,..CS_INPUTS]));rm(d24);gc()
 d25<-runyear(2025L,history)
 for(phase in c('early','late','may_on','all')) {
  z<-switch(phase,early=d25[game_date>=as.Date('2025-05-01')&game_date<as.Date('2025-07-01')],
   late=d25[game_date>=as.Date('2025-07-01')],may_on=d25[game_date>=as.Date('2025-05-01')],all=d25)
  a<-v4_metrics(z,2025L,phase);metrics[[phase]]<-a$metrics;paired[[phase]]<-a$paired
 }
 v4_write(rbindlist(metrics),'metrics_2025');v4_write(rbindlist(paired),'paired_2025');v4_combined(d25,2025L,selected)
 bats<-rbindlist(bats)
 for(target_year in 2024:2025) {
  x<-merge(bats[year==target_year & n>=100],bats[year==target_year-1L & n>=100],by='batter',suffixes=c('_current','_prior'))
  v4_write(x,paste0('matched_batter_zone_',target_year))
 }
 vd_check_hash(hashes);message('V4 complete')
}
if(sys.nframe()==0L) v4_run()
