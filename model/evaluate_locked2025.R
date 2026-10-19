# Rscript evaluate_locked2025.R --evaluate
# This entry point checks a frozen, verified passing 2024 release BEFORE any
# 2025 feature/label/cache is opened. The check-only path never opens 2025.
source("decision_validation.R")

vd_assert_release <- function(path=file.path(VD_MODELS,"release_manifest.rds"),require_verification=TRUE) {
  if(!file.exists(path)) stop("2025 remains locked: no 2024 release manifest")
  r<-readRDS(path)
  if(!isTRUE(r$passed)||!all(r$gates$passed)) {
    stop("2025 remains locked: failed 2024 gates: ",paste(r$gates[passed==FALSE,gate],collapse=", "))
  }
  stopifnot(identical(r$train_years,2022:2023),identical(r$validation_year,2024L),
    is.finite(r$reference$scale),r$reference$scale>0)
  for(h in list(r$input_hashes,r$output_hashes,r$code_hashes)) vd_check_hash(h)
  if(require_verification) {
    vp<-file.path(VD_REPORT,"artifact_verification.csv")
    if(!file.exists(vp)) stop("2025 remains locked: artifact verification missing")
    v<-fread(vp)
    if(!all(v$passed)||!all(v$release_md5==unname(tools::md5sum(path))))
      stop("2025 remains locked: release has not passed matching artifact verification")
  }
  r
}

vd_score_test <- function(raw,m,re) {
  d<-vd_prepare(raw);d[,season:=2025L]
  stopifnot(all(raw$game_year==2025L))
  cp<-predict_called_strike(m$take,raw[match(dv_key(d),dv_key(raw))])
  tv<-cbind(d[,..CT_KEYS],combine_take_values(pc_state(d),cp$p_called_strike,build_take_state_values(re)))
  tv[,`:=`(re288_state=pc_state(d),take_value_status=ifelse(is.finite(q_take_basic),"scored","unavailable"),
    low_location_support=cp$low_location_support,unknown_catcher=cp$unknown_catcher)]
  full<-cbind(d,predict_swing_value(m$contact,m$post,m$value,d[,..CT_INPUTS],re))
  full<-combine_decision_values(full,tv,build_take_state_values(re))
  full[,`:=`(observed_action=ifelse(description %chin% CT_SWINGS,"swing","take"),
    swing_evaluation_scope=ifelse(description %chin% CT_SWINGS,"observed_swing_population","hypothetical_swing_on_observed_take"),
    time_role="locked_2025_evaluation",value_scope="ordinary_outcomes_no_extra_runner_action",
    p_called_strike_if_ordinary_take=cp$p_called_strike,take_run_value_basic=tv$take_run_value_basic)]
  k<-match(dv_key(d),dv_key(raw))
  for(nm in c("home_team","away_team","inning_topbot")) set(full,j=nm,value=raw[[nm]][k])
  full
}

vd_temporal_evaluation <- function(h) {
  prior<-fread(file.path(VD_REPORT,"hitter_exploratory_2024.csv"))
  pairs<-merge(prior,h,by="batter",suffixes=c("_2024","_2025"))
  pairs<-pairs[reference_eligible_2024==TRUE & reference_eligible_2025==TRUE]
  metrics<-rbindlist(lapply(c("runs_per_100","shrunken_runs_per_100"),function(field) {
    x<-pairs[[paste0(field,"_2024")]];y<-pairs[[paste0(field,"_2025")]]
    if(length(x)<20L||sd(x)==0||sd(y)==0) return(data.table(metric=field,hitters=length(x),correlation=NA_real_,ci_low=NA_real_,ci_high=NA_real_))
    set.seed(VD_SEED)
    draws<-replicate(1000L,{i<-sample.int(length(x),length(x),TRUE);cor(x[i],y[i])})
    data.table(metric=field,hitters=length(x),correlation=cor(x,y),
      ci_low=unname(quantile(draws,.025,na.rm=TRUE)),ci_high=unname(quantile(draws,.975,na.rm=TRUE)))
  }))
  metrics[,interpretation:="2024-to-2025 repeatability under shared frozen value models; not incremental offensive or causal validity"]
  fwrite(pairs,file.path(VD_REPORT,"hitter_year_pairs_2024_2025.csv"))
  fwrite(metrics,file.path(VD_REPORT,"hitter_temporal_evaluation_2025.csv"))
  metrics
}

vd_evaluate_2025 <- function(resume=FALSE) {
  setDTthreads(2)
  release<-vd_assert_release() # MUST be first; no 2025 reads above this point.
  marker<-file.path(VD_REPORT,"test_opened_2025.rds")
  release_md5<-unname(tools::md5sum(file.path(VD_MODELS,"release_manifest.rds")))
  if(file.exists(file.path(VD_MODELS,"evaluation_result_2025.rds"))) stop("2025 evaluation is complete; use the saved result without retuning")
  if(file.exists(marker)) {
    if(!resume) stop("2025 was already opened; use --resume only to finish the identical frozen evaluation")
    if(!identical(readRDS(marker)$release_md5,release_md5)) stop("Cannot resume with a changed release")
  } else {
    if(resume) stop("No interrupted evaluation to resume")
    cs_save(list(opened_utc=format(Sys.time(),tz="UTC",usetz=TRUE),release_md5=release_md5,
      status="evaluation_started_no_retuning"),marker)
  }
  # Acquire exact official post-play states using the already tested extractor.
  status<-system2("python3","acquire_evaluation_bip_states.py")
  if(status!=0L) stop("2025 acquisition failed; cached requests retained; evaluation marker records the attempted opening")
  m<-vd_load();re<-read_re_baseline();raw<-vd_read(2025L,allow_test=TRUE)
  policy<-readRDS(file.path(VD_MODELS,"league_policy_2022_2023.rds"))
  contract<-readRDS(file.path(VD_MODELS,"support_contract.rds"))
  d<-vd_score_test(raw,m,re)
  out<-vd_uncertainty(vd_annotate(d,raw,m,contract,policy),raw,m,policy,re)
  out[,primary_decision_status:=ifelse(!primary_supported,"unsupported",
    ifelse(preference=="no_clear_preference","supported_no_clear_preference",paste0("supported_interval_favors_",preference)))]
  api<-fread(file.path(VD_REPORT,"bip_post_states_2025.csv"))
  contacts<-copy(raw[analysis_eligible %in% TRUE & description=="hit_into_play"])
  contacts[,`:=`(season=2025L,re288_state=pc_state(contacts))]
  bip<-pc_bip_targets(contacts,api,re)
  val<-vd_validate(out,raw,m,policy,re,bip,2025L)
  agg<-vd_aggregate(val$data,2025L,release$reference)
  h<-agg$hitters
  if(all(val$gates$passed)) h[reference_eligible==TRUE,Decision_plus:=100+10*(shrunken_runs_per_100-release$reference$center)/release$reference$scale]
  fwrite(h,file.path(VD_REPORT,"hitter_evaluation_2025.csv"))
  temporal<-vd_temporal_evaluation(h)
  cs_save(val$data,file.path(VD_MODELS,"supported_decisions_2025.rds"))
  result<-list(opening=readRDS(marker),completed_utc=format(Sys.time(),tz="UTC",usetz=TRUE),gates=val$gates,
    frozen_reference=release$reference,temporal_repeatability=temporal,test_inputs=vd_hash(c("data/processed/statcast_2025_clean.rds",
      file.path(VD_REPORT,c("bip_post_states_2025.csv","pbp_manifest_2025.csv")))))
  cs_save(result,file.path(VD_MODELS,"evaluation_result_2025.rds"))
  vd_check_hash(release$input_hashes);print(val$gates)
  invisible(result)
}
if(sys.nframe()==0L) {
  a<-commandArgs(trailingOnly=TRUE)
  if(identical(a,"--check-lock")) {vd_assert_release();cat("2024 release verified; 2025 evaluation may proceed.\n")}
  else if(identical(a,"--evaluate")) vd_evaluate_2025()
  else if(identical(a,"--resume")) vd_evaluate_2025(resume=TRUE)
  else stop("Use --check-lock, --evaluate, or --resume")
}
