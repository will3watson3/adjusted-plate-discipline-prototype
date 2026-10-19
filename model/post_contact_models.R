# Post-contact probabilities and ball-in-play expected run value.
# Run acquisition first, then: Rscript post_contact_models.R
source("contact_models.R")
source("run_expectancy_models.R")
source("take_value_models.R")
source("re288_order_flags.R")
PC_REPORT <- "reports/post_contact"
PC_MODELS <- "data/models/post_contact"
PC_CLASSES <- c("in_play","ordinary_foul","foul_tip")
PC_POWER_FILES <- c(batter="data/processed/batter_splits_2021.csv",pitcher="data/processed/pitcher_splits_2021.csv")

pc_state <- function(d) paste(paste0(as.integer(occupied(d$on_1b)),as.integer(occupied(d$on_2b)),
  as.integer(occupied(d$on_3b))),d$outs_when_up,paste(d$balls,d$strikes,sep="-"),sep="_")

# STEP 1. PSEUDOCODE: Read only the requested development/validation season.
# Retain physical contacts; ordinary foul and caught foul tip are separate labels.
# Build a 2022-only expectancy table for 2023 development target construction.
pc_read <- function(year) {
  if(length(year)!=1L||!year %in% 2022:2024) stop("Only 2022-2024 are permitted")
  raw<-as.data.table(readRDS(sprintf("data/processed/statcast_%d_clean.rds",year)))
  stopifnot(!anyNA(raw[,..CT_KEYS]),!anyDuplicated(raw,by=CT_KEYS),all(raw$game_year==year),
    !anyNA(raw$game_date),all(format(raw$game_date,"%Y")==as.character(year)))
  dev_re<-NULL
  if(year==2022L) {
    r<-copy(raw[,..needed]);r[,season:=year]
    states<-prepare_pitch_states(r)
    dev_re<-list(re288=fit_re288(states,250)$table,id="RE288_train2022_lambda250")
    fwrite(dev_re$re288,file.path(PC_REPORT,"RE288_development_train2022.csv"))
  }
  valid<-raw$analysis_eligible %in% TRUE & raw$description %chin% c("hit_into_play","foul","foul_tip") & ct_core_valid(raw)
  audit<-raw[,.(season=year,n=.N,retained=sum(valid[.I])),by=.(description,analysis_eligible)]
  fwrite(audit,file.path(PC_REPORT,sprintf("contact_population_%d.csv",year)))
  cols<-unique(c(CT_KEYS,CT_INPUTS,"game_date","game_year","description","analysis_eligible",
    "events","inning_topbot","post_bat_score","batter_name","pitcher_name"))
  d<-copy(raw[valid,..cols])
  d[,`:=`(season=year,contact_class=fcase(description=="hit_into_play","in_play",description=="foul","ordinary_foul",default="foul_tip"))]
  d[,re288_state:=pc_state(d)]
  setorderv(d,c("game_date",CT_KEYS))
  message("Loaded ",year,": ",nrow(d)," eligible contacts")
  list(data=d,development_re=dev_re)
}

# STEP 2. PSEUDOCODE: Join MLB's exact post-PA states by pitch key. Require
# agreement on batter, inning, event, runs, and runner conservation. Quarantine
# discrepancies, later runner actions, and incomplete final half-innings.
# The next pitch's runner state is NEVER used to label the batted-ball play.
pc_bip_targets <- function(contacts,api,re) {
  d<-copy(contacts[description=="hit_into_play"])
  stopifnot(!anyDuplicated(api,by=CT_KEYS),!anyDuplicated(d,by=CT_KEYS))
  key<-function(x) paste(x$game_pk,x$at_bat_number,x$pitch_number,sep="|")
  j<-match(key(d),key(api)); cols<-setdiff(names(api),c(CT_KEYS,"season"))
  d<-cbind(d,api[j,..cols]); d[,api_matched:=!is.na(j)]
  d[,runs_on_play:=post_bat_score-bat_score]
  d[,`:=`(outs_on_play=api_outs_after-outs_when_up,
    runners_before=as.integer(occupied(on_1b))+as.integer(occupied(on_2b))+as.integer(occupied(on_3b)),
    runners_after=as.integer(occupied(api_on_1b_after))+as.integer(occupied(api_on_2b_after))+as.integer(occupied(api_on_3b_after)))]
  d[,bases_after:=paste0(as.integer(occupied(api_on_1b_after)),as.integer(occupied(api_on_2b_after)),as.integer(occupied(api_on_3b_after)))]
  d[,state_after:=ifelse(api_outs_after==3L,"INNING_END",paste0(bases_after,"_",api_outs_after,"_0-0"))]
  d[,re_before:=re$re288$re288[match(re288_state,re$re288$re288_state)]]
  d[,re_after:=re$re288$re288[match(state_after,re$re288$re288_state)]]
  d[state_after=="INNING_END",re_after:=0]
  d[,target_status:=fcase(!api_matched,"missing_api_pitch",
    api_batter!=batter|api_inning!=inning|api_half!=ifelse(inning_topbot=="Top","top","bottom"),"identity_mismatch",
    api_event!=events|is.na(events),"event_mismatch",
    api_post_bat_score!=post_bat_score,"score_mismatch",
    api_later_runner_moves>0,"later_runner_movement",
    api_incomplete_final_half==1L,"incomplete_final_half",
    !api_outs_after %in% 0:3|!outs_on_play %in% 0:3,"invalid_outs",
    !is.finite(runs_on_play)|runs_on_play<0|runs_on_play!=floor(runs_on_play)|runs_on_play>runners_before+1,"invalid_runs",
    runs_on_play!=api_scoring_runners,"scoring_runner_mismatch",
    api_outs_after<3L & runners_before+1!=runners_after+runs_on_play+outs_on_play,"runner_conservation_mismatch",
    !is.finite(re_before)|!is.finite(re_after),"unmatched_re_state",default="verified")]
  d[,`:=`(q_bip_observed=ifelse(target_status=="verified",runs_on_play+re_after,NA_real_),
    re_target_id=re$id)]
  d[,bip_run_value_observed:=q_bip_observed-re_before]
  d
}

# STEP 3. PSEUDOCODE: Add ONLY historical 2021 contact-quality summaries.
# Shrink split exit velocity toward player overall, and overall toward league.
# Current-play exit velocity/launch angle are never prediction inputs.
pc_power_profiles <- function() {
  out<-lapply(names(PC_POWER_FILES),function(role) {
    x<-fread(PC_POWER_FILES[[role]]);hand<-if(role=="batter") "p_throws" else "stand"
    keys<-c(role,hand,"pitch_family");stopifnot(!anyDuplicated(x,by=keys))
    x[,weight:=ifelse(is.finite(mean_exit_velocity_mph)&is.finite(tracked_balls_in_play),tracked_balls_in_play,0)]
    x[,ev_sum:=ifelse(weight>0,weight*mean_exit_velocity_mph,0)]
    league<-sum(x$ev_sum)/sum(x$weight)
    total<-x[,.(n=sum(weight),ev_sum=sum(ev_sum)),by=role]
    total[,ev:=(ev_sum+50*league)/(n+50)]
    parent<-total$ev[match(x[[role]],total[[role]])]
    split<-data.table(split_key=paste(x[[role]],x[[hand]],x$pitch_family,sep="|"),
      n=x$weight,ev=(x$ev_sum+30*parent)/(x$weight+30))
    list(overall=total,split=split,league=league,hand=hand)
  });names(out)<-names(PC_POWER_FILES);out
}

pc_prep <- function(d) {
  p<-ct_preprocessor(d,ct_profiles());p$power<-pc_power_profiles()
  p$base_out_levels<-as.vector(outer(c("000","001","010","011","100","101","110","111"),0:2,paste,sep="_"))
  p
}

pc_transform <- function(d,prep) {
  t<-ct_transform(d,prep);f<-t$data
  state<-sub("_[0-3]-[0-2]$","",pc_state(d))
  invalid_state<-!state %in% prep$base_out_levels
  state[invalid_state]<-prep$base_out_levels[1]
  f[,base_out:=factor(state,levels=prep$base_out_levels)]
  t$invalid<-t$invalid|invalid_state
  for(role in c("batter","pitcher")) {
    prof<-prep$power[[role]];j<-match(d[[role]],prof$overall[[role]])
    ev<-prof$overall$ev[j];ev[is.na(ev)]<-prof$league
    k<-match(paste(d[[role]],d[[prof$hand]],d$pitch_family,sep="|"),prof$split$split_key)
    splitev<-prof$split$ev[k];splitev[is.na(splitev)]<-ev[is.na(splitev)]
    f[,(paste0(role,"_ev_2021")):=splitev]
  }
  t$data<-f;t
}

pc_rhs <- function(stage,kind) {
  basic<-"count + stand * throws + family + x + z + I(x^2) + I(z^2) + x:z + speed + movx + movz"
  smooth<-paste("count + stand * throws + family + te(x,z,k=c(6,6),bs=c('cr','cr'),by=stand)",
    "+ s(speed,k=6,bs='cr') + s(movx,k=5,bs='cr') + s(movz,k=5,bs='cr')",
    "+ s(spin,k=5,bs='cr') + s(extension,k=5,bs='cr')",
    "+ s(batter_history,k=5,bs='cr') + s(pitcher_history,k=5,bs='cr')")
  if(stage=="value") {
    if(kind=="baseline") return("base_out + count")
    return(paste("base_out +",smooth,"+ s(batter_ev_2021,k=5,bs='cr') + s(pitcher_ev_2021,k=5,bs='cr')"))
  }
  if(kind=="gam") smooth else basic
}

# STEP 4. PSEUDOCODE: Two conditional binomial models produce a coherent
# three-class distribution. The BIP value model fits nonnegative runs+RE_after
# using quasi-Poisson/log GAM; subtract pre-pitch RE only at scoring time.
pc_fit <- function(d,stage,kind,prep=pc_prep(d)) {
  stopifnot(all(d$season %in% 2022:2023))
  if(stage=="tip") d<-d[description %chin% c("foul","foul_tip")]
  if(stage=="value") d<-d[target_status=="verified"]
  t<-pc_transform(d,prep);f<-t$data
  f[,y:=switch(stage,bip=as.integer(d$description=="hit_into_play"),
    tip=as.integer(d$description=="foul_tip"),value=d$q_bip_observed)]
  stopifnot(!any(t$invalid),all(is.finite(f$y)),nrow(f)>0)
  rhs<-pc_rhs(stage,kind);form<-as.formula(paste("y~",rhs),env=globalenv())
  indicators<-intersect(all.vars(form),prep$missing)
  if(length(indicators)) form<-as.formula(paste("y~",rhs,"+",paste(paste0(indicators,"_missing"),collapse="+")),env=globalenv())
  allowed<-c("y","base_out","x","z",CT_NUMERIC,CT_FACTORS,"batter_history","pitcher_history",
    "batter_ev_2021","pitcher_ev_2021",paste0(prep$missing,"_missing"))
  stopifnot(!length(setdiff(all.vars(form),allowed)))
  family<-if(stage=="value") quasipoisson(link="log") else binomial()
  start<-proc.time()[[3]];warnings<-character()
  set.seed(20261004)
  fit<-withCallingHandlers(if(kind=="gam") bam(form,data=f,family=family,method="fREML",discrete=TRUE,
    select=TRUE,nthreads=1,na.action=na.fail,control=gam.control(maxit=100)) else
    glm(form,data=f,family=family,na.action=na.fail,control=glm.control(maxit=100)),
    warning=function(w) {warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning")})
  if(!isTRUE(fit$converged)||any(!is.finite(coef(fit)))||(kind=="gam"&&!isTRUE(fit$mgcv.conv))) stop("Nonconverged ",stage,"/",kind)
  message("Fitted ",stage,"/",kind,": ",nrow(d)," rows in ",round(proc.time()[[3]]-start,1),"s")
  list(model=fit,prep=prep,stage=stage,kind=kind,train_years=sort(unique(d$season)),n=nrow(d),
    formula=paste(deparse(form,width.cutoff=500),collapse=" "),warnings=unique(warnings),
    effective_df=if(kind=="gam") sum(fit$edf) else fit$rank,fit_seconds=proc.time()[[3]]-start)
}

pc_predict_one <- function(bundle,d) {
  t<-pc_transform(as.data.table(d),bundle$prep);p<-rep(NA_real_,nrow(d));valid<-which(!t$invalid)
  for(chunk in split(valid,ceiling(seq_along(valid)/25000))) p[chunk]<-as.numeric(predict(bundle$model,
    newdata=t$data[chunk],type="response",discrete=FALSE))
  stopifnot(all(is.finite(p[valid])),all(p[valid]>=0))
  if(bundle$stage!="value") stopifnot(all(p[valid]<=1))
  p
}

predict_post_contact <- function(bundle,pitches) {
  d<-as.data.table(pitches);pb<-pc_predict_one(bundle$bip,d);pt<-pc_predict_one(bundle$tip,d)
  data.table(p_in_play_given_contact=pb,p_ordinary_foul_given_contact=(1-pb)*(1-pt),p_foul_tip_given_contact=(1-pb)*pt)
}

predict_bip_value <- function(bundle,pitches,re=read_re_baseline()) {
  d<-as.data.table(pitches);q<-pc_predict_one(bundle,d);state<-pc_state(d)
  before<-re$re288$re288[match(state,re$re288$re288_state)]
  data.table(re288_state=state,re_before=before,q_bip=q,bip_run_value=q-before)
}

pc_matrix <- function(p) as.matrix(p[,.(p_in_play_given_contact,p_ordinary_foul_given_contact,p_foul_tip_given_contact)])
pc_losses <- function(d,p) {
  mat<-pc_matrix(p);i<-match(d$contact_class,PC_CLASSES)
  stopifnot(!anyNA(i),all(is.finite(mat)),all(mat>=0 & mat<=1),max(abs(rowSums(mat)-1))<1e-10)
  y<-matrix(0,nrow(d),3);y[cbind(seq_len(nrow(d)),i)]<-1
  data.table(log_loss=-log(pmax(1e-15,mat[cbind(seq_len(nrow(d)),i)])),
    brier=rowSums((mat-y)^2))
}
pc_metrics <- function(d,p) {
  loss<-pc_losses(d,p);data.table(n=nrow(d),log_loss=mean(loss$log_loss),multiclass_brier=mean(loss$brier))
}
pc_count_baseline <- function(train,test) {
  rate<-vapply(PC_CLASSES,function(label) mean(train$contact_class==label),numeric(1))
  tab<-train[,.(n=.N,bip=sum(contact_class=="in_play"),foul=sum(contact_class=="ordinary_foul"),tip=sum(contact_class=="foul_tip")),by=.(balls,strikes)]
  j<-match(paste(test$balls,test$strikes),paste(tab$balls,tab$strikes))
  mat<-sapply(1:3,function(k) { p<-(tab[[c("bip","foul","tip")[k]]][j]+30*rate[k])/(tab$n[j]+30);p[is.na(p)]<-rate[k];p })
  out<-as.data.table(mat);setnames(out,c("p_in_play_given_contact","p_ordinary_foul_given_contact","p_foul_tip_given_contact"));out
}
pc_value_metrics <- function(d,q) data.table(n=nrow(d),rmse=sqrt(mean((q-d$q_bip_observed)^2)),
  mae=mean(abs(q-d$q_bip_observed)),bias=mean(q-d$q_bip_observed),observed_q=mean(d$q_bip_observed),predicted_q=mean(q),
  observed_run_value=mean(d$bip_run_value_observed),predicted_run_value=mean(q-d$re_before))

pc_bootstrap <- function(game,differences,B=1000L) {
  g<-data.table(game=game,delta=differences)[,.(n=.N,total=sum(delta)),by=game]
  set.seed(20261004);draw<-replicate(B,{i<-sample.int(nrow(g),nrow(g),TRUE);sum(g$total[i])/sum(g$n[i])})
  data.table(estimate=mean(differences),ci_low=unname(quantile(draw,.025)),ci_high=unname(quantile(draw,.975)),games=nrow(g),replicates=B)
}

# STEP 5. PSEUDOCODE: Keep foul continuation with two strikes unchanged; a caught
# foul tip advances a strike and can strike out. These are ordinary-event values,
# excluding wild pitches, steals, foul-outs mislabeled as fouls, and extra action.
pc_branch_states <- function(re=read_re_baseline()) {
  s<-build_take_state_values(re)
  s[,ordinary_foul_state:=ifelse(strikes==2L,re288_state,strike_state)]
  s[,`:=`(q_ordinary_foul=re$re288$re288[match(ordinary_foul_state,re$re288$re288_state)],
    q_foul_tip=called_strike_value,q_whiff=called_strike_value)]
  out<-s[,.(re288_state,re_before,ordinary_foul_state,foul_tip_state=strike_state,q_ordinary_foul,q_foul_tip,q_whiff)]
  attach_re288_order_flags(out,s)
}

# STEP 6. PSEUDOCODE: Combine frozen contact probability with the conditional
# three-class distribution and BIP expected value. Subtract RE_before exactly once.
predict_swing_value <- function(contact_bundle,post_bundle,value_bundle,pitches,re=read_re_baseline()) {
  d<-as.data.table(pitches);contact<-predict_contact(contact_bundle,d);post<-predict_post_contact(post_bundle,d)
  value<-predict_bip_value(value_bundle,d,re);states<-pc_branch_states(re)
  j<-match(value$re288_state,states$re288_state);p<-contact$p_contact_given_swing
  out<-cbind(contact,post,value)
  out[,`:=`(p_bip_given_swing=p*post$p_in_play_given_contact,
    p_foul_given_swing=p*post$p_ordinary_foul_given_contact,p_tip_given_swing=p*post$p_foul_tip_given_contact,
    q_ordinary_foul=states$q_ordinary_foul[j],q_foul_tip=states$q_foul_tip[j],q_whiff=states$q_whiff[j])]
  out[,q_swing_basic:=p_bip_given_swing*q_bip+p_foul_given_swing*q_ordinary_foul+
    p_tip_given_swing*q_foul_tip+p_whiff_given_swing*q_whiff]
  out[,swing_run_value_basic:=q_swing_basic-re_before]
  out[!is.finite(q_swing_basic),status:="invalid_core_or_state_inputs"]
  attach_re288_order_flags(out,states=build_take_state_values(re))
}

run_post_contact <- function() {
  setDTthreads(2);dir.create(PC_REPORT,recursive=TRUE,showWarnings=FALSE);dir.create(PC_MODELS,recursive=TRUE,showWarnings=FALSE)
  api<-fread(file.path(PC_REPORT,"bip_post_states.csv"));manifest<-fread(file.path(PC_REPORT,"pbp_manifest.csv"))
  expected<-rbindlist(lapply(2022:2024,function(y) fread(sprintf("reports/game_coverage_%d.csv",y))[,.(game_pk,season=y)]))
  stopifnot(!anyDuplicated(api,by=CT_KEYS),!anyDuplicated(manifest$game_pk),setequal(manifest$game_pk,expected$game_pk),all(api$season %in% 2022:2024))
  re<-read_re_baseline()
  paths<-c("post_contact_models.R","re288_order_flags.R","contact_models.R","called_strike_models.R","take_value_models.R","run_expectancy_models.R",
    "acquire_bip_states.py",unname(CT_PROFILE_FILES),unname(PC_POWER_FILES),re$files$file,
    file.path(PC_REPORT,c("bip_post_states.csv","pbp_manifest.csv")),
    file.path(CT_MODELS,c("contact_gam_fit_2022_2023.rds","predictions_2024.rds")),
    "data/processed/statcast_2024_take_values.rds",sprintf("data/processed/statcast_%d_clean.rds",2022:2024))
  hashes<-data.table(path=paths,md5=unname(tools::md5sum(paths)))
  writeLines(c("2021 fixed histories; 2022 fit / 2023 development; 2022-2023 refit / 2024 validation; 2025 locked.",
    "Post-contact: two binomial stages, BIP vs other contact, then caught tip vs ordinary foul.",
    "Choose logistic or GAM by joint 2023 multiclass log loss. Count-only benchmark.",
    "BIP: verified MLB post-play states, runs + RE_after; choose state/count GLM or feature GAM by 2023 RMSE.",
    "Development RE uses 2022 only, lambda=250 fixed from existing baseline; final RE reuses frozen 2022-2023 tables.",
    "2024 is never used to select, tune, or calibrate either component. Bootstrap whole games.",
    "Post-contact ordinary-event run values ignore extra runner action on whiffs/fouls/tips; BIP targets quarantine unresolved events."),
    file.path(PC_REPORT,"PROTOCOL.txt"))
  a<-pc_read(2022L);devre<-a$development_re;train<-a$data;rm(a)
  dev<-pc_read(2023L)$data;prep<-pc_prep(train)
  tval<-pc_bip_targets(train,api[season==2022L],devre);dval<-pc_bip_targets(dev,api[season==2023L],devre)
  devmetrics<-list();devfits<-list();devpred<-list()
  for(kind in c("logistic","gam")) {
    b<-list(bip=pc_fit(train,"bip",kind,prep),tip=pc_fit(train,"tip",kind,prep))
    devfits[[kind]]<-b;devpred[[kind]]<-predict_post_contact(b,dev)
    devmetrics[[kind]]<-cbind(data.table(model=kind),pc_metrics(dev,devpred[[kind]]))
    cs_save(b,file.path(PC_MODELS,paste0("post_contact_",kind,"_train2022.rds")))
  }
  selection<-rbindlist(devmetrics);winner<-selection[order(log_loss,model)]$model[1]
  fwrite(selection,file.path(PC_REPORT,"post_contact_selection_2023.csv"))
  vm<-list();vf<-list();vprep<-pc_prep(tval[target_status=="verified"])
  for(kind in c("baseline","gam")) {
    b<-pc_fit(tval,"value",kind,vprep);vf[[kind]]<-b
    z<-dval[target_status=="verified"];q<-pc_predict_one(b,z)
    vm[[kind]]<-cbind(data.table(model=kind),pc_value_metrics(z,q))
    cs_save(b,file.path(PC_MODELS,paste0("bip_value_",kind,"_train2022.rds")))
  }
  vselection<-rbindlist(vm);vwinner<-vselection[order(rmse,model)]$model[1]
  fwrite(vselection,file.path(PC_REPORT,"bip_value_selection_2023.csv"))
  writeLines(c(paste0("post_contact=",winner),paste0("bip_value=",vwinner)),file.path(PC_REPORT,"SELECTED_MODELS.txt"))
  message("Frozen selections: post-contact ",winner,"; BIP value ",vwinner)
  both<-rbindlist(list(train,dev));prep<-pc_prep(both)
  postfit<-list(bip=pc_fit(both,"bip",winner,prep),tip=pc_fit(both,"tip",winner,prep))
  btrain<-pc_bip_targets(both,api[season %in% 2022:2023],re);vprep<-pc_prep(btrain[target_status=="verified"])
  valuefit<-pc_fit(btrain,"value",vwinner,vprep)
  valuebase<-if(vwinner=="baseline") valuefit else pc_fit(btrain,"value","baseline",vprep)
  cs_save(postfit,file.path(PC_MODELS,"post_contact_fit_2022_2023.rds"))
  cs_save(valuefit,file.path(PC_MODELS,"bip_value_fit_2022_2023.rds"))
  cs_save(valuebase,file.path(PC_MODELS,"bip_value_baseline_fit_2022_2023.rds"))
  # STEP 7. Open 2024 only after all selections and fits have been frozen.
  val<-pc_read(2024L)$data;vtarget<-pc_bip_targets(val,api[season==2024L],re)
  p<-predict_post_contact(postfit,val);pbase<-pc_count_baseline(both,val)
  metrics<-rbind(cbind(data.table(model="selected"),pc_metrics(val,p)),cbind(data.table(model="count_only"),pc_metrics(val,pbase)))
  fwrite(metrics,file.path(PC_REPORT,"post_contact_validation_2024.csv"))
  fwrite(pc_bootstrap(val$game_pk,pc_losses(val,p)$log_loss-pc_losses(val,pbase)$log_loss),file.path(PC_REPORT,"post_contact_bootstrap_2024.csv"))
  mat<-pc_matrix(p)
  calibration<-rbindlist(lapply(1:3,function(k) data.table(p=mat[,k],y=as.integer(val$contact_class==PC_CLASSES[k]))[,bin:=pmin(19L,floor(p*20))][,
    .(n=.N,predicted=mean(p),observed=mean(y)),by=bin][,class:=PC_CLASSES[k]]))
  fwrite(calibration,file.path(PC_REPORT,"post_contact_calibration_2024.csv"))
  fwrite(data.table(class=PC_CLASSES,observed=vapply(PC_CLASSES,function(k) mean(val$contact_class==k),numeric(1)),predicted=colMeans(mat)),
    file.path(PC_REPORT,"post_contact_class_rates_2024.csv"))
  z<-vtarget[target_status=="verified"];q<-pc_predict_one(valuefit,z);qb<-pc_predict_one(valuebase,z)
  fwrite(rbind(cbind(data.table(model="selected"),pc_value_metrics(z,q)),cbind(data.table(model="state_count_baseline"),pc_value_metrics(z,qb))),
    file.path(PC_REPORT,"bip_value_validation_2024.csv"))
  fwrite(pc_bootstrap(z$game_pk,(q-z$q_bip_observed)^2-(qb-z$q_bip_observed)^2),file.path(PC_REPORT,"bip_mse_bootstrap_2024.csv"))
  z[,`:=`(q_bip_prediction=q,bip_run_value_prediction=q-re_before,q_bip_baseline=qb)]
  cs_save(z,file.path(PC_MODELS,"bip_value_predictions_2024.rds"))
  strata<-list(pitch_family=z$pitch_family,base_out=sub("_[0-3]-[0-2]$","",z$re288_state),month=format(z$game_date,"%Y-%m"),
    inning_scope=ifelse(z$inning>9,"extra_innings","regulation"))
  slices<-rbindlist(lapply(names(strata),function(dim_name) data.table(slice=strata[[dim_name]],y=z$q_bip_observed,p=q)[,
    .(n=.N,rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),bias=mean(p-y)),by=slice][,dimension:=dim_name]))
  fwrite(slices,file.path(PC_REPORT,"bip_value_slices_2024.csv"))
  audit<-rbindlist(list(btrain,vtarget),fill=TRUE)
  fwrite(audit[,.(n=.N),by=.(season,target_status)],file.path(PC_REPORT,"bip_target_audit.csv"))
  fwrite(audit[target_status!="verified",c(CT_KEYS,"season","events","api_event","target_status"),with=FALSE],file.path(PC_REPORT,"bip_target_exclusions.csv"))
  fwrite(z[,head(.SD,2),by=events],file.path(PC_REPORT,"bip_value_examples_2024.csv"))
  postpred<-cbind(val,p);cs_save(postpred,file.path(PC_MODELS,"post_contact_predictions_2024.rds"))
  # Score all held-out swings, including whiffs, with the frozen contact model.
  # Reuse its saved feature-complete prediction rows rather than reopening data.
  swings<-as.data.table(readRDS(file.path(CT_MODELS,"predictions_2024.rds")))
  contactfit<-readRDS(file.path(CT_MODELS,"contact_gam_fit_2022_2023.rds"))
  combined<-predict_swing_value(contactfit,postfit,valuefit,swings,re)
  stopifnot(max(abs(combined$p_contact_given_swing-swings$p_contact_given_swing))<1e-10)
  identity_cols<-c(CT_KEYS,"game_date","season","batter","batter_name","pitcher","pitcher_name","description","balls","strikes",CT_INPUTS)
  full<-cbind(swings[,unique(identity_cols),with=FALSE],combined)
  # Already contains state/re/probability fields only once; CT_INPUTS is deduped.
  take<-as.data.table(readRDS("data/processed/statcast_2024_take_values.rds"))
  full<-combine_decision_values(full,take,build_take_state_values(re))
  full[,`:=`(time_role="forward_validation_2024",value_scope="ordinary_outcomes_no_extra_runner_action")]
  cs_save(full,file.path(PC_MODELS,"swing_values_2024.rds"))
  fwrite(full[,head(.SD,3),by=description],file.path(PC_REPORT,"combined_swing_examples_2024.csv"))
  fwrite(pc_branch_states(re),file.path(PC_REPORT,"foul_tip_whiff_state_values.csv"))
  fits<-c(list(final_bip=postfit$bip,final_tip=postfit$tip,final_value=valuefit,final_value_baseline=valuebase),
    setNames(vf,paste0("dev_value_",names(vf))),list(dev_bip_logistic=devfits$logistic$bip,dev_tip_logistic=devfits$logistic$tip,
      dev_bip_gam=devfits$gam$bip,dev_tip_gam=devfits$gam$tip))
  fwrite(rbindlist(lapply(names(fits),function(label) {b<-fits[[label]];data.table(fit=label,n=b$n,
    train_years=paste(b$train_years,collapse=","),converged=b$model$converged,edf=b$effective_df,
    seconds=b$fit_seconds,warnings=paste(b$warnings,collapse=" | "),formula=b$formula)})),file.path(PC_REPORT,"fit_audit.csv"))
  fwrite(rbindlist(list(train,dev,val))[,.(n=.N),by=.(season,contact_class)],file.path(PC_REPORT,"contact_outcome_counts.csv"))
  stopifnot(identical(hashes$md5,unname(tools::md5sum(hashes$path))))
  fwrite(hashes,file.path(PC_REPORT,"input_fingerprints.csv"))
  capture.output(sessionInfo(),file=file.path(PC_REPORT,"session_info.txt"))
  message("Completed post-contact, BIP expected value, and held-out swing-value integration.");print(metrics)
}

if(sys.nframe()==0L) run_post_contact()
