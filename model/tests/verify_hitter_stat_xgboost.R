source('hitter_stat_xgboost.R')
checks <- list();check <- function(n,x) {stopifnot(isTRUE(x));checks[[n]]<<-data.table(check=n,passed=TRUE)}
manifest <- readRDS(file.path(H7_MODELS,'manifest.rds'));vd_check_hash(manifest$inputs)
check('All original model, source and data hashes preserved',TRUE)
m <- h7_load();ref <- manifest$reference
for(year in 2024:2025) {
  message('Verify hybrid ',year)
  d <- as.data.table(readRDS(file.path(H7_MODELS,sprintf('decision_values_%d.rds',year))))
  old <- as.data.table(readRDS(sprintf('data/models/hitter_stat_v6/decision_values_%d.rds',year)))
  raw <- t_read(year);k<-match(dv_key(d),dv_key(raw));stopifnot(!anyNA(k))
  tag <- as.character(year)
  check(paste(tag,'Exact pitch cohort and policy'),identical(dv_key(d),dv_key(old))&&identical(d$p_swing_reference,old$p_swing_reference))
  check(paste(tag,'Support only removes new-history extrapolation'),identical(d$primary_supported,old$primary_supported&!d$new_history_outside_range)&&!any(d$primary_supported&d$re_order_warning))
  check(paste(tag,'All 34 ordering warning states remain excluded'),sum(build_take_state_values()$state_value_order_warning)==34L&&!any(d$primary_supported&old$state_value_order_warning))
  check(paste(tag,'Tip and BIP-value GAMs unchanged'),max(abs(d$q_bip-old$q_bip))<1e-8&&
    max(abs(d$p_tip_given_nonbip-old$p_foul_tip_given_contact/(1-old$p_in_play_given_contact)))<1e-8)
  q <- (1-d$p_contact_given_swing)*old$q_whiff+d$p_contact_given_swing*(d$p_in_play_given_contact*d$q_bip+
    (1-d$p_in_play_given_contact)*((1-d$p_tip_given_nonbip)*old$q_ordinary_foul+d$p_tip_given_nonbip*old$q_foul_tip))
  check(paste(tag,'Independent swing value reconstruction'),max(abs(q-d$q_swing_basic))<1e-12)
  take <- hr_fread(sprintf('experiments/take_xgboost_v1/reports/predictions_%d.csv.gz',year))
  j <- match(dv_key(d),dv_key(take))
  check(paste(tag,'Complete take path matches verified experiment'),!anyNA(j)&&max(abs(d$q_take_basic-take$q_take_xgboost[j]))<1e-12)
  check(paste(tag,'Exact score arithmetic'),max(abs(d$score-(as.integer(d$observed_action=='swing')-d$p_swing_reference)*(q-d$q_take_basic)))<1e-12)
  check(paste(tag,'Factual targets preserved'),identical(d$factual_q,old$factual_q))
  check(paste(tag,'No stale GAM uncertainty'),!any(grepl('model_se|gap_ci|envelope',names(d))))
  ix <- unique(round(seq(1,nrow(d),length.out=50)));x<-raw[k[ix]]
  a <- predict_hitter_xgboost(x,m)
  check(paste(tag,'Saved predictions match raw inference'),max(abs(a$q_swing_basic-d$q_swing_basic[ix]))<1e-10&&max(abs(a$q_take_basic-d$q_take_basic[ix]))<1e-10)
  x[,`:=`(description='arbitrary',launch_speed=999,launch_angle=999,y=-999)]
  check(paste(tag,'Same-pitch outcome independence'),identical(a,predict_hitter_xgboost(x,m)))
  check(paste(tag,'Singleton and row-order invariance'),max(abs(predict_hitter_xgboost(x[1],m)$q_swing_basic-a$q_swing_basic[1]))<1e-10&&
    max(abs(predict_hitter_xgboost(x[.N:1],m)$q_swing_basic-rev(a$q_swing_basic)))<1e-10)
  h <- fread(file.path(H7_REPORT,sprintf('hitters_%d.csv',year)))
  cov <- d[,.(eligible=.N,supported=sum(primary_supported),coverage=mean(primary_supported)),by=batter]
  check(paste(tag,'Hitter coverage reconstructs'),identical(h$supported,cov$supported[match(h$batter,cov$batter)])&&
    max(abs(h$coverage-cov$coverage[match(h$batter,cov$batter)]))<1e-12)
  g <- d[primary_supported==TRUE,.(n=.N,total=sum(score),base=sum(baseline_score)),by=.(batter,game_pk)]
  setorder(g,batter,game_pk);set.seed(20261117L)
  rebuilt <- g[,{
    draws<-replicate(500,{i<-sample.int(.N,.N,TRUE);100*c(sum(total[i]),sum(total[i]-base[i]))/sum(n[i])})
    .(runs=100*sum(total)/sum(n),low=unname(quantile(draws[1,],.025)),high=unname(quantile(draws[1,],.975)),
      delta_low=unname(quantile(draws[2,],.025)),delta_high=unname(quantile(draws[2,],.975)),games=.N)
  },by=batter]
  j<-match(rebuilt$batter,h$batter)
  check(paste(tag,'Independent aggregation and fresh game intervals'),max(abs(rebuilt$runs-h$runs_per100[j]))<1e-10&&
    max(abs(rebuilt$low-h$sampling_low[j]))<1e-10&&max(abs(rebuilt$high-h$sampling_high[j]))<1e-10&&
    max(abs(rebuilt$delta_low-h$delta_low[j]))<1e-10&&max(abs(rebuilt$delta_high-h$delta_high[j]))<1e-10)
  check(paste(tag,'Qualification and frozen scaling'),identical(h$qualified,!is.na(h$games)&h$supported>=500&h$games>=60&h$coverage>=.7)&&
    max(abs(h$decision_plus-(100+10*(h$runs_per100-ref$center)/ref$scale)),na.rm=TRUE)<1e-10)
  met <- fread(file.path(H7_REPORT,'metrics.csv'))
  for(action in c('swing','take')) {
    keep<-d$primary_supported&d$observed_action==action&is.finite(d$factual_q)
    target<-d$factual_q[keep];p<-if(action=='swing')d$q_swing_basic[keep]else d$q_take_basic[keep]
    selected<-met$year==year&met$action==action&met$arm=='xgboost';saved<-met[selected]
    check(paste(tag,action,'Independent factual metrics'),abs(saved$rmse-sqrt(mean((p-target)^2)))<1e-12&&abs(saved$mae-mean(abs(p-target)))<1e-12)
  }
  rm(d,old,raw,take);gc(FALSE)
}
vd_check_hash(manifest$inputs)
fwrite(rbindlist(checks),file.path(H7_REPORT,'artifact_verification.csv'))
cat(length(checks),'hybrid artifact checks passed\n')
