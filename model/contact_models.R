# P(contact | ordinary swing, pitch features). Run: Rscript contact_models.R
# Reuse the project's saving, scoring, and game-cluster uncertainty utilities.
source("called_strike_models.R")
CT_REPORT <- "reports/contact"
CT_MODELS <- "data/models/contact"
CT_KEYS <- c("game_pk", "at_bat_number", "pitch_number")
CT_SWINGS <- c("hit_into_play", "foul", "foul_tip", "swinging_strike", "swinging_strike_blocked")
CT_INPUTS <- c("plate_x", "plate_z", "sz_bot", "sz_top", "balls", "strikes", "stand",
  "p_throws", "pitch_family", "release_speed", "pfx_x", "pfx_z", "release_spin_rate",
  "release_extension", "outs_when_up", "on_1b", "on_2b", "on_3b", "inning",
  "bat_score", "fld_score", "batter", "pitcher")
CT_NUMERIC <- c("speed", "movx", "movz", "spin", "extension", "zone_height", "inning", "score_diff")
CT_FACTORS <- c("count", "stand", "throws", "family", "outs", "runners")
CT_PROFILE_FILES <- c(batter="reports/run_expectancy/batter_profiles_2021.csv",
  pitcher="reports/run_expectancy/pitcher_profiles_2021.csv")

# STEP 1. PSEUDOCODE: Map the observed swing to a binary contact label; keep the
# three requested outcome groups plus a caught-foul-tip flag for later valuation.
# Foul tips physically touch the bat, but do NOT get two-strike foul continuation.
contact_labels <- function(description) {
  outcome <- fcase(description == "hit_into_play", "in_play",
    description %chin% c("foul", "foul_tip"), "foul",
    description %chin% c("swinging_strike", "swinging_strike_blocked"), "whiff",
    default=NA_character_)
  data.table(outcome_class=outcome, y=ifelse(is.na(outcome), NA_integer_, as.integer(outcome != "whiff")),
    is_foul_tip=description %chin% "foul_tip")
}

ct_core_valid <- function(d) {
  is.finite(d$plate_x) & is.finite(d$plate_z) & is.finite(d$sz_bot) &
    is.finite(d$sz_top) & d$sz_top > d$sz_bot & d$balls %in% 0:3 &
    d$strikes %in% 0:2 & d$stand %in% c("L", "R") & d$p_throws %in% c("L", "R")
}

ct_read <- function(year, report_dir=CT_REPORT) {
  if (length(year)!=1L || !year %in% 2022:2024) stop("Contact development reads only 2022-2024")
  d <- as.data.table(readRDS(sprintf("data/processed/statcast_%d_clean.rds", year)))
  needed <- c(CT_KEYS, CT_INPUTS, "description", "analysis_eligible", "game_date", "game_year", "batter_name", "pitcher_name")
  if (length(setdiff(needed,names(d)))) stop("Missing contact input columns")
  if (anyNA(d[,..CT_KEYS]) || anyDuplicated(d,by=CT_KEYS)) stop("Invalid pitch keys")
  if (anyNA(d$game_year) || any(d$game_year!=year) || anyNA(d$game_date) ||
      any(format(d$game_date,"%Y")!=as.character(year))) stop("Season/date mismatch")
  keep <- d$analysis_eligible %in% TRUE & d$description %chin% CT_SWINGS & ct_core_valid(d)
  audit <- d[,.(season=year,n=.N,retained=sum(keep[.I])),by=.(description,analysis_eligible)]
  dir.create(report_dir,recursive=TRUE,showWarnings=FALSE)
  fwrite(audit,file.path(report_dir,sprintf("population_audit_%d.csv",year)))
  d <- copy(d[keep,..needed])
  d <- cbind(d,contact_labels(d$description)); d[,season:=year]
  setorderv(d,c("game_date",CT_KEYS))
  stopifnot(nrow(d)>0,uniqueN(d$y)==2L)
  message("Loaded ",year,": ",nrow(d)," ordinary swings; contact rate ",round(mean(d$y),5))
  d
}

# STEP 2. PSEUDOCODE: Freeze 2021 contact histories. Shrink player totals toward
# the league by 100 swings and hand/family splits toward that player by 50 swings.
# These fixed prior strengths are not selected on 2024. Unknown players use the
# 2021 league rate; missing splits use the player's shrunken overall history.
ct_profiles <- function() {
  profiles <- lapply(names(CT_PROFILE_FILES),function(role) {
    x <- fread(CT_PROFILE_FILES[[role]])
    hand <- if(role=="batter") "p_throws" else "stand"
    keys <- c(role,hand,"pitch_family")
    stopifnot(all(x$profile_season==2021L),!anyDuplicated(x,by=keys),
      !anyNA(x[,.(swings,contacts,whiffs)]),all(x$swings>=0),all(x$contacts>=0),
      all(x$whiffs>=0),all(x$contacts+x$whiffs==x$swings))
    league <- sum(x$contacts)/sum(x$swings)
    overall <- x[,.(n=sum(swings),contacts=sum(contacts)),by=role]
    overall[,rate:=(contacts+100*league)/(n+100)]
    parent <- overall$rate[match(x[[role]],overall[[role]])]
    split <- data.table(split_key=paste(x[[role]],x[[hand]],x$pitch_family,sep="|"),
      n=x$swings,rate=(x$contacts+50*parent)/(x$swings+50))
    list(overall=overall,split=split,league=league,hand=hand)
  })
  names(profiles)<-names(CT_PROFILE_FILES)
  stopifnot(abs(profiles$batter$league-profiles$pitcher$league)<1e-10)
  profiles
}

# STEP 3. PSEUDOCODE: Build an explicit allowlist of pre-outcome predictors.
# No description, launch measurements, bat tracking, actual contact, or run
# outcome is inspected here. Current-pitch tracking is retrospective information.
ct_raw_features <- function(d,profiles) {
  if(length(setdiff(CT_INPUTS,names(d)))) stop("Prediction input missing fields: ",paste(setdiff(CT_INPUTS,names(d)),collapse=", "))
  f <- data.table(x=d$plate_x,z=(d$plate_z-d$sz_bot)/(d$sz_top-d$sz_bot),
    count=paste(d$balls,d$strikes,sep="-"),stand=as.character(d$stand),throws=as.character(d$p_throws),
    family=as.character(d$pitch_family),speed=d$release_speed,movx=d$pfx_x,movz=d$pfx_z,
    spin=d$release_spin_rate,extension=d$release_extension,zone_height=d$sz_top-d$sz_bot,
    outs=as.character(d$outs_when_up),runners=as.character(as.integer(!is.na(d$on_1b))+
      as.integer(!is.na(d$on_2b))+as.integer(!is.na(d$on_3b))),inning=d$inning,score_diff=d$bat_score-d$fld_score)
  for(role in c("batter","pitcher")) {
    profile<-profiles[[role]]
    j<-match(d[[role]],profile$overall[[role]])
    rate<-profile$overall$rate[j]; rate[is.na(rate)]<-profile$league
    k<-match(paste(d[[role]],d[[profile$hand]],d$pitch_family,sep="|"),profile$split$split_key)
    split_rate<-profile$split$rate[k]; split_rate[is.na(split_rate)]<-rate[is.na(split_rate)]
    f[,(paste0(role,"_history")):=qlogis(split_rate)]
    f[,(paste0(role,"_history_n")):=ifelse(is.na(k),0L,profile$split$n[k])]
    f[,(paste0(role,"_unseen")):=is.na(j)]
  }
  f
}

ct_preprocessor <- function(d,profiles) {
  if(!"season" %in% names(d)||any(!d$season %in% 2022:2023)) stop("Preprocessing fits only on 2022-2023")
  f<-ct_raw_features(d,profiles)
  medians<-vapply(f[,..CT_NUMERIC],function(x) median(x[is.finite(x)]),numeric(1))
  levels<-lapply(f[,..CT_FACTORS],function(x) sort(unique(x[!is.na(x)])))
  references<-lapply(f[,..CT_FACTORS],function(x) names(sort(table(x),decreasing=TRUE))[1])
  if(any(!is.finite(medians))||any(lengths(levels)<2L)) stop("Insufficient training feature variation")
  missing<-CT_NUMERIC[vapply(f[,..CT_NUMERIC],function(x) any(!is.finite(x))&&any(is.finite(x)),logical(1))]
  support<-f[,.(n=.N),by=.(xb=floor(x/.2),zb=floor(z/.2),stand,family)]
  list(profiles=profiles,medians=medians,levels=levels,references=references,missing=missing,support=support,
    ranges=lapply(f[,c("x","z",CT_NUMERIC),with=FALSE],function(x) range(x[is.finite(x)])))
}

ct_transform <- function(d,prep) {
  f<-ct_raw_features(d,prep$profiles)
  imputed<-novel<-outside<-rep(FALSE,nrow(f))
  for(n in names(prep$ranges)) outside<-outside | (is.finite(f[[n]]) &
    (f[[n]]<prep$ranges[[n]][1] | f[[n]]>prep$ranges[[n]][2]))
  for(n in CT_NUMERIC) {
    bad<-!is.finite(f[[n]]); imputed<-imputed|bad
    if(n %in% prep$missing) f[,(paste0(n,"_missing")):=as.integer(bad)]
    set(f,which(bad),n,prep$medians[[n]])
  }
  for(n in CT_FACTORS) {
    bad<-is.na(f[[n]])|!f[[n]] %in% prep$levels[[n]]; novel<-novel|bad
    set(f,which(bad),n,prep$references[[n]])
    set(f,j=n,value=factor(f[[n]],levels=prep$levels[[n]]))
  }
  list(data=f,invalid=!ct_core_valid(d),imputed=imputed,novel=novel,outside=outside)
}

ct_specs <- function() {
  core<-paste("count + stand * throws + family + te(x,z,k=c(8,8),bs=c('cr','cr'),by=stand)",
    "+ s(speed,k=7,bs='cr') + te(movx,movz,k=c(5,5),bs=c('cr','cr'))",
    "+ s(spin,k=6,bs='cr') + s(extension,k=5,bs='cr') + s(zone_height,k=5,bs='cr')",
    "+ outs + runners + inning + score_diff")
  list(logistic="count + stand * throws + family + x + z + I(x^2) + I(z^2) + x:z + speed + movx + movz",
    gam_pooled=core,gam_history=paste(core,"+ s(batter_history,k=6,bs='cr') + s(pitcher_history,k=6,bs='cr')"))
}

# STEP 4. PSEUDOCODE: Fit binomial GAMs with penalized smooths using bam. Compare
# pooled vs past-history GAM on 2023 after training only on 2022. Select minimum
# log loss among converged GAMs, freeze that choice, then refit on 2022-2023.
ct_fit <- function(d,id,prep) {
  if(any(!d$season %in% 2022:2023)) stop("2024/2025 cannot enter model fitting")
  transformed<-ct_transform(d,prep); f<-transformed$data; f[,y:=d$y]
  if(any(transformed$invalid)||anyNA(f$y)||!all(f$y %in% 0:1)) stop("Invalid training inputs")
  rhs<-ct_specs()[[id]]; if(is.null(rhs)) stop("Unknown model")
  form<-as.formula(paste("y ~",rhs))
  allowed<-c("x","z",CT_NUMERIC,CT_FACTORS,"batter_history","pitcher_history")
  stopifnot(!length(setdiff(all.vars(form),c("y",allowed))))
  indicators<-intersect(all.vars(form),prep$missing)
  if(length(indicators)) indicators<-paste0(indicators,"_missing")
  if(length(indicators)) form<-as.formula(paste("y ~",rhs,"+",paste(indicators,collapse=" + ")))
  warnings<-character(); start<-proc.time()[[3]]
  set.seed(20261001)
  model<-withCallingHandlers(if(id=="logistic") glm(form,data=f,family=binomial(),na.action=na.fail)
    else bam(form,data=f,family=binomial(),method="fREML",discrete=TRUE,select=TRUE,
      nthreads=2,na.action=na.fail,control=gam.control(maxit=100)),
    warning=function(w) { warnings<<-c(warnings,conditionMessage(w));invokeRestart("muffleWarning") })
  if(!isTRUE(model$converged)||any(!is.finite(coef(model)))||
    (id!="logistic"&&!isTRUE(model$mgcv.conv))) stop("Fit did not converge: ",id)
  list(model=model,prep=prep,id=id,train_years=sort(unique(d$season)),train_n=nrow(d),
    train_end=as.character(max(d$game_date)),formula=paste(deparse(form,width.cutoff=500),collapse=" "),
    warnings=unique(warnings),fit_seconds=proc.time()[[3]]-start,
    effective_df=if(id=="logistic") model$rank else sum(model$edf),profile_season=2021L)
}

# This API accepts feature-only rows, preserves their order, and returns NA on
# invalid core inputs. Exact prediction avoids batch-dependent discretization.
predict_contact <- function(bundle,pitches) {
  d<-as.data.table(pitches); t<-ct_transform(d,bundle$prep); f<-t$data
  p<-rep(NA_real_,nrow(d)); valid<-which(!t$invalid)
  for(chunk in split(valid,ceiling(seq_along(valid)/25000L))) {
    p[chunk]<-as.numeric(predict(bundle$model,newdata=f[chunk],type="response",discrete=FALSE))
  }
  if(any(!is.finite(p[valid]))||any(p[valid]<0|p[valid]>1)) stop("Invalid contact probability")
  support<-bundle$prep$support
  n<-support$n[match(paste(floor(f$x/.2),floor(f$z/.2),f$stand,f$family),
    paste(support$xb,support$zb,support$stand,support$family))]; n[is.na(n)]<-0L
  data.table(p_contact_given_swing=p,p_whiff_given_swing=1-p,
    status=ifelse(t$invalid,"invalid_core_inputs",ifelse(t$novel,"unseen_category_fallback","scored")),
    secondary_imputed=t$imputed,outside_training_range=t$outside,nearby_training_swings=n,
    low_location_support=n<30L,unseen_batter_2021=f$batter_unseen,unseen_pitcher_2021=f$pitcher_unseen,
    batter_history_swings=f$batter_history_n,pitcher_history_swings=f$pitcher_history_n)
}

ct_count_predict <- function(train,test) {
  tab<-train[,.(p=(sum(y)+20*mean(train$y))/(.N+20)),by=.(balls,strikes)]
  p<-tab$p[match(paste(test$balls,test$strikes),paste(tab$balls,tab$strikes))]
  p[is.na(p)]<-mean(train$y); p
}

ct_diagnostics <- function(d,score) {
  p<-score$p_contact_given_swing
  dims<-list(count=paste(d$balls,d$strikes,sep="-"),pitch_family=d$pitch_family,stand=d$stand,
    pitcher_hand=d$p_throws,month=format(d$game_date,"%Y-%m"),
    zone=ifelse(abs(d$plate_x)<=17/24 & d$plate_z>=d$sz_bot & d$plate_z<=d$sz_top,"in_zone","out_of_zone"),
    batter_history=ifelse(score$unseen_batter_2021,"unseen_2021","known_2021"),
    pitcher_history=ifelse(score$unseen_pitcher_2021,"unseen_2021","known_2021"),
    location_support=ifelse(score$low_location_support,"sparse","supported"))
  rbindlist(lapply(names(dims),function(dimension_name) data.table(slice=dims[[dimension_name]],y=d$y,p=p)[,
    .(n=.N,observed_contact=mean(y),predicted_contact=mean(p),bias=mean(p-y),
      log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2)),by=slice][,dimension:=dimension_name]))
}

run_contact_models <- function() {
  setDTthreads(2)
  dir.create(CT_REPORT,recursive=TRUE,showWarnings=FALSE)
  dir.create(CT_MODELS,recursive=TRUE,showWarnings=FALSE)
  paths<-c("contact_models.R","called_strike_models.R",unname(CT_PROFILE_FILES),
    sprintf("data/processed/statcast_%d_clean.rds",2022:2024))
  fingerprints<-data.table(path=paths,md5=unname(tools::md5sum(paths)))
  writeLines(c("Target: physical contact conditional on an ordinary swing.",
    "Labels: hit_into_play/foul/foul_tip=1; swinging_strike/swinging_strike_blocked=0.",
    "2021 profiles only; 2022 training; 2023 GAM selection; 2022-2023 refit; 2024 validation; 2025 locked.",
    "Two GAM candidates (pooled/history), minimum 2023 log loss selects. Logistic/count benchmarks.",
    "No tuning or recalibration on 2024. Bootstrap whole games, 1000 replicates.",
    "Location support is a coarse diagnostic, not proof of counterfactual overlap.",
    "Foul tips are contact but are flagged separately for subsequent strike/run valuation."),
    file.path(CT_REPORT,"PROTOCOL.txt"))
  profiles<-ct_profiles(); train<-ct_read(2022L); dev<-ct_read(2023L)
  prep<-ct_preprocessor(train,profiles); metrics<-list(); fits<-list()
  for(id in names(ct_specs())) {
    message("Development fit: ",id)
    fit<-ct_fit(train,id,prep); fits[[id]]<-fit
    for(split in c("training_2022","selection_2023")) {
      d<-if(split=="training_2022") train else dev
      p<-predict_contact(fit,d)$p_contact_given_swing
      metrics[[paste(id,split)]]<-cbind(data.table(model=id,split=split),cs_metrics(d$y,p))
    }
    cs_save(fit,file.path(CT_MODELS,paste0(id,"_train2022.rds")))
    message(id,": fitted in ",round(fit$fit_seconds,1)," seconds")
  }
  selection<-rbindlist(metrics)
  best<-selection[split=="selection_2023" & grepl("^gam_",model)][order(log_loss,model)]$model[1]
  selection[,selected_gam:=model==best]
  fwrite(selection,file.path(CT_REPORT,"development_metrics.csv"))
  writeLines(best,file.path(CT_REPORT,"SELECTED_MODEL.txt"))
  message("Frozen selection: ",best,". Refit on 2022-2023 before opening 2024.")
  training<-rbindlist(list(train,dev)); prep<-ct_preprocessor(training,profiles)
  gam_fit<-ct_fit(training,best,prep); logistic_fit<-ct_fit(training,"logistic",prep)
  cs_save(gam_fit,file.path(CT_MODELS,"contact_gam_fit_2022_2023.rds"))
  cs_save(logistic_fit,file.path(CT_MODELS,"contact_logistic_fit_2022_2023.rds"))
  # STEP 5. PSEUDOCODE: Open held-out 2024 only after model selection/refit. Save
  # pitch-keyed predictions, calibration, slice diagnostics, and uncertainty.
  val<-ct_read(2024L); score<-predict_contact(gam_fit,val)
  pg<-score$p_contact_given_swing; pl<-predict_contact(logistic_fit,val)$p_contact_given_swing
  pc<-ct_count_predict(training,val)
  validation<-rbindlist(lapply(c("gam","logistic","count"),function(id) {
    p<-switch(id,gam=pg,logistic=pl,count=pc)
    cbind(data.table(model=id),cs_metrics(val$y,p))
  }))
  fwrite(validation,file.path(CT_REPORT,"validation_metrics_2024.csv"))
  boot<-cs_bootstrap(val,pg,pl,B=1000L)
  fwrite(boot,file.path(CT_REPORT,"game_bootstrap_2024.csv"))
  fwrite(ct_diagnostics(val,score),file.path(CT_REPORT,"validation_slices_2024.csv"))
  calibration<-data.table(y=val$y,p=pg)[,bin:=pmin(19L,floor(p*20))][,
    .(n=.N,predicted_contact=mean(p),observed_contact=mean(y)),by=bin][order(bin)]
  fwrite(calibration,file.path(CT_REPORT,"calibration_2024.csv"))
  predictions<-cbind(val,score)
  predictions[,`:=`(p_contact_logistic=pl,p_contact_count=pc,model_id=best,time_role="forward_validation_2024")]
  cs_save(predictions,file.path(CT_MODELS,"predictions_2024.rds"))
  fwrite(predictions[,head(.SD,4L),by=.(outcome_class,is_foul_tip)],file.path(CT_REPORT,"example_predictions_2024.csv"))
  counts<-rbindlist(list(train,dev,val))[,.(swings=.N,contacts=sum(y)),by=.(season,outcome_class,is_foul_tip)]
  fwrite(counts,file.path(CT_REPORT,"outcome_counts.csv"))
  fits<-c(fits,list(final_gam=gam_fit,final_logistic=logistic_fit))
  fit_audit<-rbindlist(lapply(names(fits),function(n) {
    b<-fits[[n]]; data.table(fit=n,train_years=paste(b$train_years,collapse=","),n=b$train_n,
      converged=b$model$converged,edf=b$effective_df,seconds=b$fit_seconds,warnings=paste(b$warnings,collapse=" | "),formula=b$formula)
  }))
  fwrite(fit_audit,file.path(CT_REPORT,"fit_audit.csv"))
  capture.output(summary(gam_fit$model),file=file.path(CT_REPORT,"gam_summary.txt"))
  capture.output(sessionInfo(),file=file.path(CT_REPORT,"session_info.txt"))
  stopifnot(identical(fingerprints$md5,unname(tools::md5sum(fingerprints$path))))
  fwrite(fingerprints,file.path(CT_REPORT,"input_fingerprints.csv"))
  # Predict with saved model and feature-only inputs; preserve row/batch invariance.
  idx<-unique(round(seq(1,nrow(val),length.out=40)))
  reload<-readRDS(file.path(CT_MODELS,"contact_gam_fit_2022_2023.rds"))
  check<-predict_contact(reload,val[idx,..CT_INPUTS])$p_contact_given_swing
  stopifnot(max(abs(check-pg[idx]))<1e-10,!anyDuplicated(predictions,by=CT_KEYS))
  fwrite(data.table(check=c("saved_model_feature_only_batch_invariance","unique_prediction_keys","probabilities_sum_to_one","input_fingerprints_unchanged"),
    passed=TRUE),file.path(CT_REPORT,"verification.csv"))
  png(file.path(CT_REPORT,"calibration_2024.png"),width=1200,height=750,res=140)
  par(mar=c(5,5,3,1))
  plot(calibration$predicted_contact,calibration$observed_contact,xlim=c(0,1),ylim=c(0,1),
    xlab="Predicted contact probability",ylab="Observed contact rate",pch=19,col="#176B87",
    cex=pmax(.6,sqrt(calibration$n/max(calibration$n))*2.8),main="Contact given swing: held-out 2024")
  abline(0,1,lty=2,col="gray50"); dev.off()
  message("Contact model complete. Selected ",best); print(validation)
  invisible(list(model=gam_fit,metrics=validation))
}

if(sys.nframe()==0L) run_contact_models()
