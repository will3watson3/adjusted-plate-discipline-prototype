source('umpire_v5.R')
checks<-list();check<-function(name,x){stopifnot(isTRUE(x));checks[[length(checks)+1L]]<<-data.table(check=name,passed=TRUE)}
f<-readRDS(file.path(U5_MODELS,'frozen_protocol.rds'));vd_check_hash(f$hashes);check('frozen source and data hashes unchanged',TRUE)
previous<-fread('reports/called_strike_v4/input_hashes.csv')
expected<-previous[path=='data/processed/statcast_2025_analysis.csv.gz']$md5
check('restored 2025 data exactly matches original v4 hash',identical(f$hashes[path=='data/processed/statcast_2025_analysis.csv.gz']$md5,expected))
a<-fread(file.path(U5_REPORT,'umpire_assignments.csv'));check('one assignment record per game',!anyDuplicated(a$game_pk))
s<-fread(file.path(U5_REPORT,'historical_selection.csv'));eligible<-s[eligible==TRUE]$method
check('historical simplest eligible selection',f$selected==if(length(eligible)) eligible[1] else 'baseline')
for(year in 2024:2025) {
 d<-as.data.table(readRDS(file.path(U5_MODELS,sprintf('predictions_%d.rds',year))))
 check(paste(year,'unique pitch keys'),!anyDuplicated(d,by=CS_KEYS))
 old<-as.data.table(readRDS(sprintf('data/models/called_strike_v4/predictions_%d.rds',year)))
 k<-match(dv_key(d),dv_key(old));check(paste(year,'exact pitch cohort'),!anyNA(k)&&nrow(d)==nrow(old))
 check(paste(year,'baseline identical to saved v4'),identical(d$baseline,old$expanding[k]))
 check(paste(year,'support unchanged'),identical(d$supported,old$supported[k]))
 expected_ids<-as.character(a$umpire_id[match(d$game_pk,a$game_pk)])
 check(paste(year,'umpire IDs preserved after scoring'),identical(d$umpire,expected_ids))
 check(paste(year,'umpire-region IDs preserved'),identical(d$ump_region,paste(d$umpire,d$region,sep=':')))
 check(paste(year,'catcher identity preserved'),identical(d$catcher,as.character(d$fielder_2)))
 rz<-(d$plate_z-d$sz_bot)/(d$sz_top-d$sz_bot)
 rr<-fifelse(rz<0,'below',fifelse(rz>1,'above',fifelse(abs(d$plate_x)>.83,'side','inside_rectangle')))
 check(paste(year,'region definitions reconstruct from locations'),identical(d$region,rr))
 for(method in U5_METHODS) check(paste(year,method,'finite valid probabilities'),all(is.finite(d[[method]][d$valid])&d[[method]][d$valid]>0&d[[method]][d$valid]<1))
 summary<-fread(file.path(U5_REPORT,paste0('fit_summary_',year,'.csv')))
 check(paste(year,'catcher-only reproduces existing framing'),summary[method=='catcher_only']$original_probability_max_difference<1e-6)
 for(method in c('region_only','umpire_model','umpire_region_model')) {
  b<-readRDS(file.path(U5_MODELS,sprintf('fit_%s_for_%d.rds',method,year)))
  check(paste(year,method,'model converged'),isTRUE(b$fit$converged)&&isTRUE(b$fit$mgcv.conv))
  check(paste(year,method,'nested training objective and finite parameters'),b$quality$training_loss<=b$quality$null_loss+1e-6&&b$quality$max_coefficient<=100)
  check(paste(year,method,'strict prior-season training'),b$history_year==year-1L&&as.Date(b$train_end)<as.Date(sprintf('%d-01-01',year)))
  check(paste(year,method,'joint catcher umpire terms'),setequal(names(b$groups),c('catcher',if(method!='region_only') 'umpire',if(method=='umpire_region_model') 'ump_region')))
  x<-copy(d[seq_len(min(1000L,.N))]);p<-u5_predict(b,x)
  x[,`:=`(y=1L-y,description=ifelse(description=='ball','called_strike','ball'))]
  check(paste(year,method,'current-label independence'),identical(p$p,u5_predict(b,x)$p))
  j<-seq_len(nrow(x))
  pieces<-unlist(lapply(split(j,ceiling(j/173)),function(i)u5_predict(b,x[i])$p))
  check(paste(year,method,'prediction batch invariance'),max(abs(p$p-pieces))<1e-12)
  l<-fread(file.path(U5_REPORT,sprintf('weekly_%s_%d.csv',method,year)))
  check(paste(year,method,'weekly update chronology'),all(as.Date(l$train_end)<as.Date(l$start),na.rm=TRUE))
  anchor<-as.Date(sprintf('%d-01-01',year));block<-anchor+7L*(as.integer(as.Date(x$game_date)-anchor)%/%7L)
  ix<-match(as.character(block),as.character(l$start));check(paste(year,method,'weekly ledger covers sample'),!anyNA(ix))
  cal<-plogis(qlogis(pmin(1-1e-8,pmax(1e-8,p$p)))+l$intercept[ix])
  check(paste(year,method,'saved calibration reconstructs'),max(abs(cal-x[[method]]))<1e-12)
  known<-rep(TRUE,nrow(d))
  for(g in names(b$groups)) known<-known&d[[g]]%chin%b$groups[[g]]$table$id
  native_data<-d[which(known)[seq_len(min(200L,sum(known)))]]
  manual<-u5_predict(b,native_data)
  native<-mgcv::predict.gam(b$fit,native_data,type='link',se.fit=TRUE,unconditional=!is.null(b$fit$Vc))
  check(paste(year,method,'native prediction parity'),max(abs(native$fit-(native_data$base_eta+manual$effect)))<1e-8)
  check(paste(year,method,'native posterior SE parity'),max(abs(native$se.fit-manual$latent_sd))<1e-8)
  rm(b);gc()
 }
 met<-fread(file.path(U5_REPORT,sprintf('metrics_%d.csv',year)))
 check(paste(year,'matched model denominators'),all(met[,uniqueN(n),by=.(phase,population)]$V1==1L))
 ix<-which(d$valid&d$take&d$supported)
 row<-met[phase=='all'&population=='supported_takes'&method==f$selected]
 check(paste(year,'selected loss reconstructs'),abs(row$loss-mean(cs_loss(d$y[ix],d[[f$selected]][ix])))<1e-12)
 v<-as.data.table(readRDS(file.path(U5_MODELS,sprintf('decision_comparison_%d.rds',year))))
 q<-as.data.table(readRDS(sprintf('data/models/catcher_framing_v2/decision_values_%d.rds',year)))
 check(paste(year,'decision exact keys and order'),identical(dv_key(v),dv_key(q)))
 check(paste(year,'swing value unchanged'),identical(v$q_swing,q$q_swing_basic))
 check(paste(year,'league reference unchanged'),identical(v$reference,q$p_swing_reference))
 check(paste(year,'decision support unchanged'),identical(v$primary_supported,q$primary_supported))
 ix<-which(v$primary_supported)
 check(paste(year,'gap arithmetic'),max(abs(v$gap[ix]-(v$q_swing[ix]-v$q_take[ix])))<1e-12)
 check(paste(year,'score arithmetic'),max(abs(v$score[ix]-(as.integer(v$observed_action[ix]=='swing')-v$reference[ix])*v$gap[ix]))<1e-12)
 st<-build_take_state_values(read_re_baseline());k<-match(q$re288_state,st$re288_state)
 qt<-(1-v$p_called_strike)*st$ball_value[k]+v$p_called_strike*st$called_strike_value[k]
 check(paste(year,'exact take branches'),max(abs(qt[ix]-v$q_take[ix]))<1e-12)
 check(paste(year,'no stale uncertainty intervals'),!any(c('gap_ci_low','gap_ci_high','gap_model_se_bound')%in%names(v)))
 rm(d,old,v,q);gc()
}
ah<-fread(file.path(U5_REPORT,'audit_hash.csv'));vd_check_hash(ah)
check('umpire-cluster sensitivity source hash matches',TRUE)
ac<-fread(file.path(U5_REPORT,'paired_umpire_clusters.csv'))
for(target in 2024:2025) {
 x<-ac[year==target&comparator=='baseline'&population=='supported_takes'&method==f$selected]
 p<-fread(file.path(U5_REPORT,sprintf('paired_%d.csv',target)))[phase=='all'&population=='supported_takes'&method==f$selected]
 check(paste(target,'cluster bootstrap retains same paired center'),abs(x$estimate-p$estimate)<1e-12)
 check(paste(target,'umpire cluster count matches data cohort'),x$clusters==a[season==target,uniqueN(umpire_id)])
}
u5_write(rbindlist(checks),'artifact_verification');cat(length(checks),'umpire artifact checks passed\n')
