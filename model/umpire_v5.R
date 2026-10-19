# Rscript umpire_v5.R --prepare; acquire officials; Rscript umpire_v5.R
source('called_strike_v4.R')
U5_REPORT<-'reports/umpire_v5';U5_MODELS<-'data/models/umpire_v5'
U5_METHODS<-c('baseline','region_only','umpire_model','umpire_region_model')
stopifnot(!any(U5_METHODS%chin%c('umpire','catcher','ump_region','region','base_eta','game_pk','y')))
u5_write<-function(x,name) fwrite(x,file.path(U5_REPORT,paste0(name,'.csv')))
u5_paths<-function(year) c(if(year==2025L) 'data/processed/statcast_2025_analysis.csv.gz' else sprintf('data/processed/statcast_%d_clean.rds',year),
 sprintf('data/models/catcher_framing_v2/strike_predictions_%d.rds',year),if(year>=2024L) sprintf('data/models/called_strike_v4/predictions_%d.rds',year) else 'data/models/called_strike/gam_validation_fit_2022_2023.rds')
u5_prepare<-function(year) {
 path<-file.path(U5_MODELS,sprintf('features_%d.rds',year));h<-vd_hash(u5_paths(year))
 if(file.exists(path)) {z<-readRDS(path);vd_check_hash(z$hashes);stopifnot(identical(z$hashes$path,h$path),identical(z$hashes$md5,h$md5));return(z$data)}
 message('Prepare features ',year)
 if(year==2025L && !file.exists('data/processed/statcast_2025_analysis.csv.gz')) {
  for(attempt in seq_len(90L)) {if(file.exists('data/processed/statcast_2025_analysis.csv.gz')) break; if(attempt%%15L==1L) message('Waiting for existing 2025 iCloud dataset restoration');Sys.sleep(2)}
  stopifnot(file.exists('data/processed/statcast_2025_analysis.csv.gz'));h<-vd_hash(u5_paths(year));vd_check_hash(h)
 }
 r<-if(year==2025L) fread(cmd='gzip -dc data/processed/statcast_2025_analysis.csv.gz',select=CS_INPUTS) else as.data.table(readRDS(sprintf('data/processed/statcast_%d_clean.rds',year)))[,..CS_INPUTS]
 r[,game_date:=as.IDate(game_date)];stopifnot(!anyDuplicated(r,by=CS_KEYS),all(r$game_year==year),!anyNA(r$game_date),all(format(r$game_date,'%Y')==as.character(year)))
 p<-as.data.table(readRDS(sprintf('data/models/catcher_framing_v2/strike_predictions_%d.rds',year)))
 if(year==2023L) p<-p[dv_key(p)%in%dv_key(r[cs_eligible(r)])]
 k<-match(dv_key(p),dv_key(r));stopifnot(!anyNA(k));r<-r[k]
 eta<-if(year==2023L) {
  base<-readRDS('data/models/called_strike/gam_validation_fit_2022_2023.rds');fr_base(base,r,se=FALSE)$eta
 } else p$base_eta
 d<-r[,c(CS_KEYS,'game_date','game_year','batter','balls','strikes','stand','p_throws','pitch_family','plate_x','plate_z','sz_bot','sz_top','fielder_2','description'),with=FALSE]
 set(d,j='base_eta',value=eta);set(d,j='original_p',value=p$p_called_strike)
 set(d,j='valid',value=!p$invalid&!p$novel_fixed_category)
 d[,`:=`(catcher=as.character(fielder_2),y=as.integer(description=='called_strike'),
  take=description%chin%c('ball','blocked_ball','called_strike'),z=(plate_z-sz_bot)/(sz_top-sz_bot))]
 d[,region:=fifelse(z<0,'below',fifelse(z>1,'above',fifelse(abs(plate_x)>.83,'side','inside_rectangle')))]
 if(year>=2024L) {
  old<-as.data.table(readRDS(sprintf('data/models/called_strike_v4/predictions_%d.rds',year)))
  j<-match(dv_key(d),dv_key(old));stopifnot(!anyNA(j),all(abs(d$original_p-old$p[j])<1e-12),identical(d$y,old$y[j]))
  set(d,j='baseline',value=old$expanding[j]);set(d,j='supported',value=old$supported[j])
 } else d[,`:=`(baseline=NA_real_,supported=FALSE)]
 cs_save(list(data=d,hashes=h),path);d
}
u5_join<-function(d,assignments) {
 stopifnot(!anyDuplicated(assignments$game_pk));out<-copy(d);k<-match(d$game_pk,assignments$game_pk)
 stopifnot(!anyNA(k));u<-assignments$umpire_id[k];u[assignments$status[k]!='unique_game_assignment']<-NA_integer_
 set(out,j='umpire',value=as.character(u));set(out,j='umpire_name',value=assignments$umpire_name[k])
 out[,ump_region:=fifelse(is.na(umpire),NA_character_,paste(umpire,region,sep=':'))]
 out
}
u5_validate_fit<-function(fit,y,eta) {
 shift<-v4_intercept(plogis(eta),y)
 null_loss<-mean(cs_loss(y,plogis(eta+shift)));training_loss<-mean(cs_loss(y,fit$fitted.values))
 maximum<-max(abs(coef(fit)))
 if(!is.finite(maximum)||maximum>100||!is.finite(training_loss)||training_loss>null_loss+1e-6)
  stop('Numerically invalid fit: training objective worse than nested offset/intercept model or divergent coefficients')
 list(training_loss=training_loss,null_loss=null_loss,max_coefficient=maximum)
}
u5_fit<-function(d,method,prediction_year) {
 stopifnot(method%chin%c('catcher_only','region_only','umpire_model','umpire_region_model'),all(d$game_year==prediction_year-1L))
 groups<-c('catcher',if(method%chin%c('umpire_model','umpire_region_model')) 'umpire',if(method=='umpire_region_model') 'ump_region')
 keep<-d$valid&d$take&is.finite(d$base_eta)&!is.na(d$catcher)
 if(method%chin%c('umpire_model','umpire_region_model')) keep<-keep&!is.na(d$umpire)
 f<-copy(d[keep,c('y','base_eta',if(method!='catcher_only') 'region',groups),with=FALSE])
 for(g in groups) set(f,j=g,value=factor(f[[g]]))
 if(method!='catcher_only') f[,region:=factor(region)]
 rhs<-paste(paste0('s(',groups,',bs="re")'),collapse='+')
 form<-as.formula(paste('y~offset(base_eta)+',if(method!='catcher_only') 'region+' else '',rhs),env=globalenv())
 set.seed(20261018);fit<-bam(form,data=f,family=binomial(),method='fREML',discrete=TRUE,nthreads=2,etastart=if(method=='catcher_only') NULL else f$base_eta,control=gam.control(maxit=100))
 stopifnot(isTRUE(fit$converged),isTRUE(fit$mgcv.conv),all(is.finite(coef(fit))))
 quality<-u5_validate_fit(fit,f$y,f$base_eta)
 invisible(capture.output(vc<-mgcv::gam.vcomp(fit)))
 V<-if(!is.null(fit$Vc)) fit$Vc else fit$Vp
 structure_groups<-lapply(groups,function(g) {
  sm<-fit$smooth[[match(paste0('s(',g,')'),vapply(fit$smooth,function(x)x$label,''))]]
  ix<-sm$first.para:sm$last.para;ids<-levels(f[[g]])
  tau<-if(is.matrix(vc)) unname(vc[paste0('s(',g,')'),'std.dev']) else unname(vc[paste0('s(',g,')')])
  stopifnot(length(ix)==length(ids),is.finite(tau),tau>=0)
  tab<-data.table(id=ids,index=ix,effect=unname(coef(fit)[ix]),sd=sqrt(pmax(0,diag(V)[ix])),population_sd=tau)
  counts<-data.table(id=as.character(f[[g]]))[,.(opportunities=.N),by=id]
  tab<-merge(tab,counts,by='id',sort=FALSE)
  list(table=tab,tau=tau)
 });names(structure_groups)<-groups
 list(method=method,fit=fit,V=V,quality=quality,groups=structure_groups,fixed_indices=seq_len(fit$nsdf),region_levels=if(method!='catcher_only') levels(f$region) else NULL,train_n=nrow(f),excluded=nrow(d[d$valid&d$take])-nrow(f),
  train_end=as.character(max(d$game_date[keep])),prediction_year=prediction_year,history_year=prediction_year-1L)
}
u5_predict<-function(b,d,allow_history=FALSE) {
 if(!allow_history && (any(d$game_year!=b$prediction_year)||any(d$game_date<=as.Date(b$train_end)))) stop('Umpire prediction chronology violation')
 n<-nrow(d);co<-coef(b$fit);V<-b$V
 fi<-if(is.null(b$fixed_indices)) 1L else b$fixed_indices
 X<-if(is.null(b$region_levels)) matrix(1,n,1) else model.matrix(~region,data.frame(region=factor(d$region,levels=b$region_levels)))
 stopifnot(nrow(X)==n,ncol(X)==length(fi),!anyNA(X))
 effect<-as.numeric(X%*%co[fi]);variance<-rowSums((X%*%V[fi,fi,drop=FALSE])*X);indices<-list()
 unknown<-data.table(row=seq_len(n))
 for(g in names(b$groups)) {
  tab<-b$groups[[g]]$table;k<-match(d[[g]],tab$id);known<-!is.na(k);ix<-rep(NA_integer_,n);ix[known]<-tab$index[k[known]]
  effect[known]<-effect[known]+co[ix[known]]
  variance[known]<-variance[known]+V[cbind(ix[known],ix[known])]+2*rowSums(X[known,,drop=FALSE]*t(V[fi,ix[known],drop=FALSE]))
  variance[!known]<-variance[!known]+b$groups[[g]]$tau^2
  indices[[g]]<-ix;set(unknown,j=paste0('unknown_',g),value=!known)
 }
 if(length(indices)>1L) for(pair in combn(names(indices),2L,simplify=FALSE)) {
  a<-indices[[pair[1]]];c<-indices[[pair[2]]];keep<-!is.na(a)&!is.na(c)
  variance[keep]<-variance[keep]+2*V[cbind(a[keep],c[keep])]
 }
 stopifnot(all(is.finite(variance)),min(variance)>-1e-8)
 z<-fr_integrate(d$base_eta+effect,sqrt(pmax(0,variance)))
 list(p=z$p,effect=effect,latent_sd=sqrt(pmax(0,variance)),unknown=unknown)
}
u5_online<-function(d,p) {
 z<-copy(d[,.(game_year,game_date,take,valid,y)]);set(z,j='p',value=p)
 v4_online(z)
}
u5_boot<-function(game,value) sv_boot(game,value,B=1000L)
u5_metrics<-function(d,year,phase) {
 met<-paired<-region_paired<-list()
 for(pop in c('all_takes','supported_takes')) {
  ix<-which(d$valid&d$take&if(pop=='supported_takes') d$supported else TRUE)
  for(m in U5_METHODS) {
   y<-d$y[ix];p<-d[[m]][ix]
   met[[length(met)+1L]]<-data.table(year,phase,population=pop,method=m,n=length(ix),games=uniqueN(d$game_pk[ix]),
    bias=mean(p-y),loss=mean(cs_loss(y,p)),brier=mean((p-y)^2))
   paired[[length(paired)+1L]]<-cbind(data.table(year,phase,population=pop,method=m),u5_boot(d$game_pk[ix],cs_loss(y,p)-cs_loss(y,d$baseline[ix])))
   region_paired[[length(region_paired)+1L]]<-cbind(data.table(year,phase,population=pop,method=m),u5_boot(d$game_pk[ix],cs_loss(y,p)-cs_loss(y,d$region_only[ix])))
  }
 }
 list(metrics=rbindlist(met),paired=rbindlist(paired),region_paired=rbindlist(region_paired))
}
u5_slices<-function(d,year) {
 z<-copy(d[valid&take]);z[,`:=`(count=paste(balls,strikes,sep='-'),hands=paste(stand,p_throws,sep='/'),month=format(game_date,'%m'))]
 ans<-bins<-list()
 for(m in U5_METHODS) {
  for(nm in c('count','hands','region','month')) ans[[length(ans)+1L]]<-z[,.(n=.N,predicted=mean(get(m)),actual=mean(y),
   bias=mean(get(m)-y),loss=mean(cs_loss(y,get(m))),difference=mean(cs_loss(y,get(m))-cs_loss(y,baseline))),by=.(slice=get(nm))][,`:=`(year=year,method=m,dimension=nm)]
  z[,bin:=pmin(19L,floor(get(m)*20))]
  bins[[length(bins)+1L]]<-z[,.(n=.N,predicted=mean(get(m)),actual=mean(y)),by=bin][,`:=`(year=year,method=m)]
 }
 u5_write(rbindlist(ans),paste0('slices_',year));u5_write(rbindlist(bins),paste0('calibration_bins_',year))
}
u5_combine<-function(d,year,selected) {
 old<-as.data.table(readRDS(sprintf('data/models/catcher_framing_v2/decision_values_%d.rds',year)))
 k<-match(dv_key(old),dv_key(d));stopifnot(!anyNA(k));st<-build_take_state_values(read_re_baseline());j<-match(old$re288_state,st$re288_state)
 baseline_take<-(1-d$baseline[k])*st$ball_value[j]+d$baseline[k]*st$called_strike_value[j]
 metrics<-sensitivity<-list()
 for(m in U5_METHODS) {
  p<-d[[m]][k];qt<-(1-p)*st$ball_value[j]+p*st$called_strike_value[j];gap<-old$q_swing_basic-qt
  score<-(as.integer(old$observed_action=='swing')-old$p_swing_reference)*gap
  prevgap<-old$q_swing_basic-baseline_take;prevscore<-(as.integer(old$observed_action=='swing')-old$p_swing_reference)*prevgap
  ix<-which(old$primary_supported&old$observed_action=='take'&is.finite(old$factual_q))
  err<-qt[ix]-old$factual_q[ix];prev<-baseline_take[ix]-old$factual_q[ix]
  metrics[[length(metrics)+1L]]<-cbind(data.table(year,method=m,n=length(ix),bias=mean(err),rmse=sqrt(mean(err^2)),baseline_bias=mean(prev),
    baseline_rmse=sqrt(mean(prev^2))),u5_boot(old$game_pk[ix],err^2-prev^2))
  ix<-which(old$primary_supported)
  sensitivity[[length(sensitivity)+1L]]<-data.table(year,method=m,n=length(ix),mean_gap_change=mean(gap[ix]-prevgap[ix]),
    abs_gap_change=mean(abs(gap[ix]-prevgap[ix])),preference_change=mean((gap[ix]>0)!=(prevgap[ix]>0)),
    score_change_runs100=100*mean(score[ix]-prevscore[ix]))
  if(m==selected) {
   values<-old[,c(CS_KEYS,'game_date','batter','observed_action','primary_supported'),with=FALSE]
   set(values,j='p_called_strike',value=p);set(values,j='q_swing',value=old$q_swing_basic);set(values,j='q_take',value=qt)
   set(values,j='gap',value=gap);set(values,j='reference',value=old$p_swing_reference);set(values,j='score',value=score)
   values[,`:=`(method=selected,uncertainty_status='not_refit_do_not_reuse_earlier_intervals')]
   cs_save(values,file.path(U5_MODELS,sprintf('decision_comparison_%d.rds',year)))
   h<-data.table(batter=old$batter,batter_name=old$batter_name,keep=old$primary_supported,score,prevscore)
   u5_write(h[keep==TRUE,.(n=.N,raw_runs100=100*mean(score),v4_runs100=100*mean(prevscore),change_runs100=100*mean(score-prevscore)),by=.(batter,batter_name)],paste0('selected_hitter_changes_',year))
  }
 }
 u5_write(rbindlist(metrics),paste0('take_value_metrics_',year));u5_write(rbindlist(sensitivity),paste0('score_sensitivity_',year))
}
u5_evaluate<-function(d,year) {
 met<-paired<-region_paired<-list()
 for(phase in c('early','late','may_on','all')) {
  start<-as.Date(sprintf('%d-%s-01',year,if(phase=='early'||phase=='may_on') '05' else if(phase=='late') '07' else '01'))
  end<-as.Date(sprintf('%d-%s-01',year,if(phase=='early') '07' else '12'))
  a<-u5_metrics(d[game_date>=start&game_date<end],year,phase);met[[phase]]<-a$metrics;paired[[phase]]<-a$paired;region_paired[[phase]]<-a$region_paired
 }
 met<-rbindlist(met);paired<-rbindlist(paired);u5_write(met,paste0('metrics_',year));u5_write(paired,paste0('paired_',year));u5_write(rbindlist(region_paired),paste0('paired_region_control_',year));u5_slices(d,year)
 list(metrics=met,paired=paired)
}
u5_run<-function() {
 dir.create(U5_REPORT,recursive=TRUE,showWarnings=FALSE);dir.create(U5_MODELS,recursive=TRUE,showWarnings=FALSE);setDTthreads(2)
 assignments<-fread(file.path(U5_REPORT,'umpire_assignments.csv'));stopifnot(!anyDuplicated(assignments$game_pk))
 paths<-unique(c('UMPIRE_V5_PLAN.md','umpire_v5.R','acquire_umpire_assignments.py',file.path(U5_REPORT,'umpire_assignments.csv'),
  'data/models/called_strike/gam_validation_fit_2022_2023.rds','reports/called_strike_v4/input_hashes.csv',sprintf('data/models/catcher_framing_v2/decision_values_%d.rds',2024:2025),u5_paths(2023L),u5_paths(2024L),u5_paths(2025L)))
 hashes<-vd_hash(paths)
 missing<-which(is.na(hashes$md5))
 if(length(missing)) {
  stopifnot(length(missing)==1L,hashes$path[missing]=='data/processed/statcast_2025_analysis.csv.gz')
  previous<-fread('reports/called_strike_v4/input_hashes.csv');expected<-previous[ path=='data/processed/statcast_2025_analysis.csv.gz']$md5
  stopifnot(length(expected)==1L,!is.na(expected));hashes$md5[missing]<-expected
 }
 vd_check_hash(hashes[file.exists(path)]);u5_write(hashes,'input_hashes')
 u5_write(assignments[,.(games=.N,unique_assignment=sum(status=='unique_game_assignment'),missing=sum(status!='unique_game_assignment'),
  umpires=uniqueN(umpire_id[status=='unique_game_assignment'])),by=season],'game_coverage')
 history<-u5_join(u5_prepare(2023L),assignments)
 runyear<-function(year,history) {
  message('Umpire experiment ',year);d<-u5_join(u5_prepare(year),assignments);stats<-groups<-list()
  for(method in c('catcher_only',setdiff(U5_METHODS,'baseline'))) {
   message('Fit prior-season ',method,' for ',year)
   b<-u5_fit(history,method,year);cs_save(b,file.path(U5_MODELS,sprintf('fit_%s_for_%d.rds',method,year)))
   pred<-u5_predict(b,d)
   for(g in names(b$groups)) {
    tab<-copy(b$groups[[g]]$table);tab[,`:=`(method=method,group=g,prediction_year=year,history_year=year-1L)]
    if(g=='umpire') {k<-match(tab$id,as.character(assignments$umpire_id));tab[,name:=assignments$umpire_name[k]]}
    u5_write(tab,sprintf('%s_%s_for_%d',g,method,year))
   }
   if(method=='catcher_only') {
    # Matches the existing framing model before its weekly calibration.
    maxdiff<-max(abs(pred$p-d$original_p));stopifnot(maxdiff<1e-6)
    stats[[method]]<-data.table(year,method,train_n=b$train_n,excluded=b$excluded,train_end=b$train_end,training_loss=b$quality$training_loss,null_loss=b$quality$null_loss,max_coefficient=b$quality$max_coefficient,original_probability_max_difference=maxdiff,
      unseen_catcher=mean(pred$unknown$unknown_catcher),unseen_umpire=NA_real_)
   } else {
    online<-u5_online(d,pred$p);set(d,j=method,value=online$p)
    u5_write(online$ledger[,`:=`(year=year,method=method)],sprintf('weekly_%s_%d',method,year))
    u5_write(pred$unknown[,.(n=.N),by=setdiff(names(pred$unknown),'row')][,method:=method],sprintf('unknown_groups_%s_%d',method,year))
    stats[[method]]<-data.table(year,method,train_n=b$train_n,excluded=b$excluded,train_end=b$train_end,training_loss=b$quality$training_loss,null_loss=b$quality$null_loss,max_coefficient=b$quality$max_coefficient,original_probability_max_difference=NA_real_,
      unseen_catcher=mean(pred$unknown$unknown_catcher),unseen_umpire=if('unknown_umpire'%in%names(pred$unknown)) mean(pred$unknown$unknown_umpire) else NA_real_)
   }
   rm(b,pred);gc()
  }
  control<-u5_online(d,d$original_p);stopifnot(max(abs(control$p-d$baseline))<1e-12)
  u5_write(rbindlist(stats),paste0('fit_summary_',year))
  u5_write(d[,.(pitches=.N,takes=sum(take),supported=sum(supported),missing_assignment=sum(is.na(umpire)),
    missing_assignment_takes=sum(take&is.na(umpire)))],paste0('pitch_coverage_',year))
  cs_save(d,file.path(U5_MODELS,sprintf('predictions_%d.rds',year)));d
 }
 d24<-runyear(2024L,history);a<-u5_evaluate(d24,2024L)
 z<-d24[valid&take&game_date>=as.Date('2024-05-01')];loss<-vapply(U5_METHODS,function(m) mean(cs_loss(z$y,z[[m]])),numeric(1));best<-names(which.min(loss))
 selection<-rbindlist(lapply(U5_METHODS,function(m) {
  b<-u5_boot(z$game_pk,cs_loss(z$y,z[[m]])-cs_loss(z$y,z[[best]]))
  bad<-any(a$paired[population=='all_takes'&phase%chin%c('early','late')&method==m]$ci_low>0)
  data.table(method=m,loss=loss[[m]],best=best,delta_best=b$estimate,se_best=b$se,clearly_worse_period=bad,eligible=!bad&b$estimate<=b$se+1e-12)
 }))
 selected<-selection[eligible==TRUE]$method[1L];if(!length(selected)||is.na(selected)) selected<-'baseline'
 selection[,selected:=method==selected];u5_write(selection,'historical_selection')
 cs_save(list(selected=selected,selection=selection,hashes=hashes,labels_end='2024-09-30',role='2025_previously_inspected_exploratory_replay'),file.path(U5_MODELS,'frozen_protocol.rds'))
 message('Frozen umpire selection: ',selected);u5_combine(d24,2024L,selected)
 history<-d24;rm(d24);gc();d25<-runyear(2025L,history);u5_evaluate(d25,2025L);u5_combine(d25,2025L,selected)
 vd_check_hash(hashes);message('Umpire V5 complete')
}
if(sys.nframe()==0L) {
 dir.create(U5_MODELS,recursive=TRUE,showWarnings=FALSE);dir.create(U5_REPORT,recursive=TRUE,showWarnings=FALSE)
 if('--prepare'%in%commandArgs(TRUE)) {for(year in 2023:2024) {d<-u5_prepare(year);rm(d);gc()}} else u5_run()
}
