# Basic ordinary-take valuation. Run from the project root:
# Rscript take_value_models.R
source("attach_run_expectancy.R")
source("called_strike_models.R")
TV_REPORT <- "reports/take_value"
TV_KEYS <- c("game_pk","at_bat_number","pitch_number")

# PSEUDOCODE: Enumerate all 288 valid pre-pitch states. Advance only the count,
# forced walk runners, or one ordinary strikeout out. Next batter starts at 0-0.
build_take_state_values <- function(baseline=read_re_baseline()) {
  tab <- copy(baseline$re288[,.(re288_state,re_before=re288,state_training_n=n)])
  pieces <- tstrsplit(tab$re288_state,"_",fixed=TRUE)
  counts <- tstrsplit(pieces[[3]],"-",fixed=TRUE)
  tab[,`:=`(base_state=pieces[[1]],outs=as.integer(pieces[[2]]),balls=as.integer(counts[[1]]),strikes=as.integer(counts[[2]]))]
  r1<-substr(tab$base_state,1,1)=="1";r2<-substr(tab$base_state,2,2)=="1";r3<-substr(tab$base_state,3,3)=="1"
  # A walk advances only runners forced by the batter's award of first base.
  walk_bases<-paste0(1L,as.integer(r1|r2),as.integer(r3|(r1&r2)))
  is_walk<-tab$balls==3L;is_k<-tab$strikes==2L
  tab[,`:=`(ball_event=ifelse(is_walk,"walk","ball_count_advance"),
    strike_event=ifelse(is_k,"ordinary_strikeout","strike_count_advance"),
    ball_runs=as.integer(is_walk&r1&r2&r3),strike_runs=0L,
    ball_bases=ifelse(is_walk,walk_bases,base_state),ball_outs=outs,
    ball_balls=ifelse(is_walk,0L,balls+1L),ball_strikes=ifelse(is_walk,0L,strikes),
    strike_bases=base_state,strike_outs=outs+as.integer(is_k),
    strike_balls=ifelse(is_k,0L,balls),strike_strikes=ifelse(is_k,0L,strikes+1L))]
  tab[,`:=`(ball_state=paste(ball_bases,ball_outs,paste0(ball_balls,"-",ball_strikes),sep="_"),
    strike_state=ifelse(strike_outs==3L,"INNING_END",paste(strike_bases,strike_outs,paste0(strike_balls,"-",strike_strikes),sep="_")))]
  tab[,ball_re_after:=baseline$re288$re288[match(ball_state,baseline$re288$re288_state)]]
  tab[,strike_re_after:=baseline$re288$re288[match(strike_state,baseline$re288$re288_state)]]
  tab[strike_state=="INNING_END",strike_re_after:=0]
  tab[,`:=`(ball_value=ball_runs+ball_re_after,called_strike_value=strike_runs+strike_re_after)]
  tab[,`:=`(ball_run_value=ball_value-re_before,called_strike_run_value=called_strike_value-re_before,
    ball_minus_strike=ball_value-called_strike_value)]
  tab[,state_value_order_warning:=ball_minus_strike<0|ball_run_value<0|called_strike_run_value>0]
  stopifnot(nrow(tab)==288L,!anyDuplicated(tab$re288_state),
    all(is.finite(tab$ball_value)),all(is.finite(tab$called_strike_value)))
  setorder(tab,base_state,outs,balls,strikes)
  tab
}

# PSEUDOCODE: Take the probability-weighted average of the two next-state values;
# subtract pre-pitch RE once. This function never inspects the actual outcome.
combine_take_values <- function(state_keys,p_called_strike,states) {
  if(length(state_keys)!=length(p_called_strike)) stop("State/probability lengths differ")
  if(any(!is.na(p_called_strike)&(!is.finite(p_called_strike)|p_called_strike<0|p_called_strike>1))) stop("Invalid strike probability")
  i<-match(state_keys,states$re288_state)
  v<-states[i,.(re_before,ball_state,strike_state,ball_runs,strike_runs,ball_re_after,strike_re_after,
    ball_value,called_strike_value,ball_run_value,called_strike_run_value,ball_minus_strike,state_value_order_warning)]
  v[,p_called_strike_if_ordinary_take:=p_called_strike]
  v[,q_take_basic:=(1-p_called_strike_if_ordinary_take)*ball_value+p_called_strike_if_ordinary_take*called_strike_value]
  v[,take_run_value_basic:=q_take_basic-re_before]
  valid<-!is.na(i)&is.finite(p_called_strike)
  stopifnot(all(v$q_take_basic[valid]>=pmin(v$ball_value[valid],v$called_strike_value[valid])-1e-12),
    all(v$q_take_basic[valid]<=pmax(v$ball_value[valid],v$called_strike_value[valid])+1e-12))
  v
}

# PSEUDOCODE: Compare basic branch transitions to the next recorded pre-pitch
# state in the FULL sequence. Never delete exceptional rows before shifting.
# The next record can include intervening non-pitch action; mismatches are audit
# findings rather than assumed coding errors or extra predictors.
audit_take_transitions <- function(raw,joined,states,year) {
  fields<-c(TV_KEYS,"inning","inning_topbot","balls","strikes","outs_when_up","description","events","bat_score","post_bat_score")
  a<-copy(raw[,..fields]);a[,state:=joined$re288_state]
  setorderv(a,TV_KEYS)
  a[,`:=`(next_state=shift(state,type="lead"),next_inning=shift(inning,type="lead"),
    next_half=shift(inning_topbot,type="lead"),next_pa=shift(at_bat_number,type="lead"),
    next_pitch=shift(pitch_number,type="lead")),by=game_pk]
  a<-a[description %chin% c("ball","blocked_ball","called_strike")]
  i<-match(a$state,states$re288_state);strike<-a$description=="called_strike"
  a[,`:=`(expected_state=ifelse(strike,states$strike_state[i],states$ball_state[i]),
    expected_runs=ifelse(strike,states$strike_runs[i],states$ball_runs[i]),
    branch_event=ifelse(strike,states$strike_event[i],states$ball_event[i]),
    observed_runs=post_bat_score-bat_score,season=year)]
  a[,same_half:=!is.na(next_inning)&inning==next_inning&inning_topbot==next_half]
  a[,expected_pa_end:=branch_event %chin% c("walk","ordinary_strikeout")]
  a[,sequence_ok:=fifelse(expected_pa_end,next_pa>at_bat_number&next_pitch==1L,
    next_pa==at_bat_number&next_pitch==pitch_number+1L)]
  a[,state_matches:=fifelse(expected_state=="INNING_END",!is.na(next_inning)&!same_half,
    same_half&sequence_ok&!is.na(next_state)&next_state==expected_state)]
  a[,runs_match:=is.finite(observed_runs)&observed_runs==expected_runs]
  a[,audit_status:=fcase(is.na(expected_state),"invalid_pre_state",
    !is.finite(observed_runs),"missing_run_observation",
    is.na(next_inning),"no_next_record_unverified",
    state_matches %in% TRUE & runs_match %in% TRUE,"matches_basic_transition",
    !runs_match,"extra_or_different_runs",
    expected_state!="INNING_END" & same_half & !sequence_ok,"sequence_gap_or_pa_mismatch",
    default="different_state_or_runner_action")]
  a
}

run_take_values <- function(years=2022:2024) {
  if(!length(years)||any(!years %in% 2022:2024)||anyDuplicated(years)) stop("Only unique 2022-2024 seasons are authorized here")
  setDTthreads(2)
  dir.create(TV_REPORT,recursive=TRUE,showWarnings=FALSE)
  baseline<-read_re_baseline();states<-build_take_state_values(baseline)
  fwrite(states,file.path(TV_REPORT,"basic_take_state_values.csv"))
  fwrite(states[state_value_order_warning==TRUE],file.path(TV_REPORT,"state_value_order_warnings.csv"))
  model_path<-file.path(CS_MODELS,"gam_validation_fit_2022_2023.rds")
  bundle<-readRDS(model_path)
  stopifnot(identical(bundle$train_years,2022:2023),isTRUE(bundle$model$converged),bundle$spec$id=="gam_stronger_penalty")
  source_paths<-c("take_value_models.R","called_strike_models.R","attach_run_expectancy.R",model_path,baseline$files$file,
    sprintf("data/processed/statcast_%d_clean.rds",years))
  fingerprints<-data.table(path=source_paths,md5=unname(tools::md5sum(source_paths)))
  summaries<-list();audit_counts<-list();calibration<-list();examples<-list();validation<-list()
  for(year in years) {
    message("Take values: reading ",year)
    raw<-as.data.table(readRDS(sprintf("data/processed/statcast_%d_clean.rds",year)))
    joined<-attach_re_to_pitches(raw,year,baseline)
    original_keys<-copy(raw[,..TV_KEYS]);original_names<-names(raw)
    score_rows<-which(raw$analysis_eligible %in% TRUE & joined$re_match_status=="matched")
    message("Scoring ",length(score_rows)," eligible pitches in ",year)
    score<-predict_called_strike(bundle,raw[score_rows])
    probability<-rep(NA_real_,nrow(raw));probability[score_rows]<-score$p_called_strike
    values<-combine_take_values(joined$re288_state,probability,states)
    out<-cbind(raw[,c(TV_KEYS,"game_date","game_year","batter","pitcher","description","balls","strikes","outs_when_up","base_state","analysis_eligible"),with=FALSE],values)
    out[,`:=`(re288_state=joined$re288_state,re24_before=joined$re24_before,
      take_value_status=ifelse(joined$re_match_status!="matched","invalid_pre_state",
        ifelse(!analysis_eligible %in% TRUE,"outside_ordinary_pitch_scope",
          ifelse(!is.finite(q_take_basic),"invalid_probability_inputs",
            ifelse(state_value_order_warning %in% TRUE,"scored_state_value_warning","scored")))),
      take_value_time_role=if(year==2024L) "forward_validation_2024" else "fitted_season_not_out_of_sample",
      re_model_id=baseline$id,call_model_id="gam_stronger_penalty_train2022_2023",
      take_value_assumption="ordinary_call_no_extra_runner_action")]
    out[,`:=`(low_location_support=NA,unknown_catcher=NA,probability_input_status=NA_character_)]
    out[score_rows,`:=`(low_location_support=score$low_location_support,unknown_catcher=score$unknown_catcher,probability_input_status=score$status)]
    # This label-based column is a diagnostic branch value, NOT actual runs caused.
    out[,observed_call_branch_run_value_basic:=fcase(description=="called_strike",called_strike_run_value,
      description %chin% c("ball","blocked_ball"),ball_run_value,default=NA_real_)]
    stopifnot(identical(out[,..TV_KEYS],original_keys),!anyDuplicated(out,by=TV_KEYS),
      identical(names(raw),original_names),max(abs(values$re_before-joined$re288_before),na.rm=TRUE)<1e-12)
    if(year==2024L) {
      saved<-as.data.table(readRDS(file.path(CS_MODELS,"predictions_2024.rds")))
      reproduced<-out[saved,on=TV_KEYS,p_called_strike_if_ordinary_take]
      err<-max(abs(reproduced-saved$p_called_strike_gam))
      stopifnot(length(reproduced)==nrow(saved),is.finite(err),err<1e-10)
      validation[["probability_parity"]]<-data.table(check="reproduce_saved_2024_call_probabilities",n=length(reproduced),max_error=err,passed=TRUE)
    }
    cs_save(out,sprintf("data/processed/statcast_%d_take_values.rds",year))
    summaries[[as.character(year)]]<-out[,.(season=year,source_pitches=.N,eligible_pitches=sum(analysis_eligible %in% TRUE),
      scored_pitches=sum(is.finite(q_take_basic)),scored_observed_takes=sum(is.finite(q_take_basic)&description %chin% c("ball","blocked_ball","called_strike")),
      scored_other_ordinary_pitches=sum(is.finite(q_take_basic)&!description %chin% c("ball","blocked_ball","called_strike")),
      flagged_state_order=sum(is.finite(q_take_basic)&state_value_order_warning %in% TRUE),
      sparse_support=sum(is.finite(q_take_basic)&low_location_support %in% TRUE),
      time_role=unique(take_value_time_role))]
    audit<-audit_take_transitions(raw,joined,states,year)
    audit_counts[[as.character(year)]]<-audit[,.(pitches=.N),by=.(season,branch_event,audit_status)]
    fwrite(audit[audit_status!="matches_basic_transition"],file.path(TV_REPORT,sprintf("transition_exceptions_%d.csv",year)))
    fwrite(audit[,head(.SD,3),by=.(branch_event,audit_status)],file.path(TV_REPORT,sprintf("transition_audit_examples_%d.csv",year)))
    observed<-out[is.finite(q_take_basic)&description %chin% c("ball","blocked_ball","called_strike")]
    observed[,value_error:=take_run_value_basic-observed_call_branch_run_value_basic]
    calibration[[as.character(year)]]<-observed[,.(season=year,n=.N,predicted_mean_run_value=mean(take_run_value_basic),
      observed_call_branch_mean_run_value=mean(observed_call_branch_run_value_basic),mean_value_error=mean(value_error)),by=.(balls,strikes)]
    if(year==2024L) {
      mu<-mean(observed$value_error);se<-cs_cluster_se(observed$game_pk,observed$value_error)
      fwrite(data.table(n=nrow(observed),games=uniqueN(observed$game_pk),mean_value_error=mu,
        cluster_se=se,ci_low=mu-1.96*se,ci_high=mu+1.96*se,
        interpretation="Call-weighted state-value calibration; not realized-run or counterfactual accuracy"),file.path(TV_REPORT,"call_weighted_calibration_2024.csv"))
      # Representative high-leverage and ordinary examples, selected by state only.
      examples[[as.character(year)]]<-out[is.finite(q_take_basic),head(.SD,1),by=.(base_state,outs_when_up,balls,strikes)]
    }
    message("Finished ",year,": ",sum(is.finite(out$q_take_basic))," scored; ",audit[audit_status=="matches_basic_transition",.N]," observed transitions match the basic rule")
    rm(raw,joined,out,audit,observed,values,score);gc(verbose=FALSE)
  }
  stopifnot(identical(fingerprints$md5,unname(tools::md5sum(fingerprints$path))))
  fwrite(rbindlist(summaries),file.path(TV_REPORT,"coverage.csv"))
  fwrite(rbindlist(audit_counts),file.path(TV_REPORT,"transition_audit_summary.csv"))
  fwrite(rbindlist(calibration),file.path(TV_REPORT,"call_weighted_calibration_by_count.csv"))
  fwrite(rbindlist(examples),file.path(TV_REPORT,"example_pitch_values_2024.csv"))
  fwrite(rbindlist(validation),file.path(TV_REPORT,"verification.csv"))
  fwrite(fingerprints,file.path(TV_REPORT,"source_fingerprints.csv"))
  writeLines(capture.output(sessionInfo()),file.path(TV_REPORT,"session_info.txt"))
  print(rbindlist(summaries));message("Basic take-value connection complete. 2025 was not opened.")
}
if(sys.nframe()==0L) run_take_values()
