# Independent integrity checks for the one frozen test-year evaluation.
source("evaluate_locked2025.R")
setDTthreads(2)
release<-vd_assert_release()
result_path<-file.path(VD_MODELS,"evaluation_result_2025.rds")
stopifnot(file.exists(result_path))
result<-readRDS(result_path);vd_check_hash(result$test_inputs)
stopifnot(identical(result$frozen_reference,release$reference))
d<-as.data.table(readRDS(file.path(VD_MODELS,"supported_decisions_2025.rds")))
raw<-vd_read(2025L,allow_test=TRUE)
expected<-raw[analysis_eligible %in% TRUE]
check<-function(ok,name) {
  if(!isTRUE(ok)) stop("2025 artifact check failed: ",name)
  data.table(check=name,passed=TRUE)
}
checks<-list()
checks[[1]]<-check(!anyDuplicated(d,by=CT_KEYS)&&identical(d[,..CT_KEYS],expected[,..CT_KEYS])&&
  all(d$season==2025L),"complete_test_pitch_keys_order_and_season")
checks[[2]]<-check(all(vapply(unique(c(CT_INPUTS,"game_date","description","batter_name","pitcher_name","home_team","away_team","inning_topbot")),
  function(nm) identical(d[[nm]],expected[[nm]]),logical(1))),"source_feature_and_context_parity")
checks[[3]]<-check(identical(d$observed_action,ifelse(d$description %chin% CT_SWINGS,"swing","take"))&&
  all(d$swing_evaluation_scope[d$observed_action=="take"]=="hypothetical_swing_on_observed_take"),
  "observed_actions_and_hypothetical_scope")
manifest<-fread(file.path(VD_REPORT,"pbp_manifest_2025.csv"))
coverage<-fread("reports/game_coverage_2025.csv")
checks[[4]]<-check(!anyDuplicated(manifest$game_pk)&&nrow(manifest)==2430L&&all(manifest$season==2025L)&&
  setequal(manifest$game_pk,coverage$game_pk)&&setequal(manifest$game_pk,raw$game_pk),"complete_official_game_cohort")
api<-fread(file.path(VD_REPORT,"bip_post_states_2025.csv"))
checks[[5]]<-check(!anyDuplicated(api,by=CT_KEYS)&&all(api$season==2025L),"unique_official_bip_pitch_keys")
re<-read_re_baseline()
contacts<-copy(raw[analysis_eligible %in% TRUE & description=="hit_into_play"])
contacts[,`:=`(season=2025L,re288_state=pc_state(contacts))]
bip<-pc_bip_targets(contacts,api,re)
fwrite(bip[,.(n=.N),by=target_status],file.path(VD_REPORT,"bip_target_audit_2025.csv"))
fwrite(bip[target_status!="verified",c(CT_KEYS,"game_date","batter","pitcher","events","api_event","target_status"),with=FALSE],
  file.path(VD_REPORT,"bip_target_exclusions_2025.csv"))
targets<-vd_factual_targets(d,raw,bip,re)
checks[[6]]<-check(identical(d$factual_target_status,targets$factual_target_status)&&
  isTRUE(all.equal(d$factual_q,targets$factual_q,tolerance=1e-12)),"verified_factual_labels_reconstructed_from_official_states_and_full_sequence")
checks[[7]]<-check(all(d$primary_supported==(d$support_reason==""))&&
  all(is.na(d$primary_swing_minus_take[!d$primary_supported]))&&
  all(!d$primary_supported[d$extreme_bip_value|d$state_value_order_warning]),"unsupported_and_extreme_values_withheld")
s<-d[primary_supported==TRUE]
checks[[8]]<-check(all(is.finite(s$gap_model_se_bound))&&all(s$gap_model_se_bound>=0)&&
  all(s$preference==ifelse(s$gap_ci_low>0,"swing",ifelse(s$gap_ci_high<0,"take","no_clear_preference"))),
  "conditional_uncertainty_and_no_clear_preference")
q<-d$p_whiff_given_swing*d$q_whiff+d$p_bip_given_swing*d$q_bip+
  d$p_foul_given_swing*d$q_ordinary_foul+d$p_tip_given_swing*d$q_foul_tip
checks[[9]]<-check(max(abs(q-d$q_swing_basic)/pmax(1,abs(q)))<1e-12&&
  max(abs(d$swing_minus_take-(d$q_swing_basic-d$q_take_basic))/pmax(1,abs(d$swing_minus_take)))<1e-12,
  "four_branch_and_comparison_arithmetic")
m<-vd_load();policy<-readRDS(file.path(VD_MODELS,"league_policy_2022_2023.rds"))
contract<-readRDS(file.path(VD_MODELS,"support_contract.rds"))
probe<-d[,head(.SD,1),by=.(observed_action,primary_supported,pitch_family,preference)]
probe_raw<-raw[match(dv_key(probe),dv_key(raw))]
fresh<-vd_score_test(probe_raw,m,re)
checks[[10]]<-check(max(abs(fresh$q_swing_basic-probe$q_swing_basic)/pmax(1,abs(probe$q_swing_basic)))<1e-10&&
  max(abs(fresh$q_take_basic-probe$q_take_basic))<1e-10,"frozen_scoring_reproduced_on_stratified_test_rows")
annotated<-vd_annotate(fresh,probe_raw,m,contract,policy)
checks[[11]]<-check(identical(annotated$support_reason,probe$support_reason)&&
  max(abs(annotated$p_swing_reference-probe$p_swing_reference))<1e-12,"frozen_support_and_league_policy_reproduced")
unc<-vd_uncertainty(annotated,probe_raw,m,policy,re)
checks[[12]]<-check(max(abs(unc$gap_model_se_bound-probe$gap_model_se_bound),na.rm=TRUE)<1e-10&&
  max(abs(unc$above_reference_model_se_bound-probe$above_reference_model_se_bound),na.rm=TRUE)<1e-10,
  "conditional_uncertainty_reproduced_from_all_five_frozen_models")
h<-fread(file.path(VD_REPORT,"hitter_evaluation_2025.csv"))
checks[[13]]<-check(!anyDuplicated(h$batter)&&sum(h$eligible)==nrow(d)&&sum(h$supported)==nrow(s),"hitter_coverage_reconciles")
if(all(result$gates$passed)) {
  qh<-h[reference_eligible==TRUE]
  checks[[14]]<-check(max(abs(qh$Decision_plus-(100+10*(qh$shrunken_runs_per_100-release$reference$center)/release$reference$scale)))<1e-10&&
    all(is.na(h$Decision_plus[!h$reference_eligible])),"2025_index_uses_frozen_2024_center_scale_and_exposure_rules")
} else checks[[14]]<-check(all(is.na(h$Decision_plus)),"failed_test_gates_withhold_index")
pairs<-fread(file.path(VD_REPORT,"hitter_year_pairs_2024_2025.csv"))
temporal<-fread(file.path(VD_REPORT,"hitter_temporal_evaluation_2025.csv"))
if(nrow(pairs)<20L) {
  checks[[15]]<-check(all(temporal$hitters==nrow(pairs))&&all(is.na(temporal$correlation)),
    "insufficient_year_pairs_withhold_repeatability_instead_of_reporting_spurious_correlation")
} else checks[[15]]<-check(abs(cor(pairs$runs_per_100_2024,pairs$runs_per_100_2025)-temporal[metric=="runs_per_100",correlation])<1e-12,
  "year_to_year_repeatability_recomputed")
for(hashes in list(release$input_hashes,release$output_hashes,release$code_hashes,result$test_inputs)) vd_check_hash(hashes)
opening<-readRDS(file.path(VD_REPORT,"test_opened_2025.rds"))
manifest_digest<-unname(tools::md5sum(file.path(VD_MODELS,"release_manifest.rds")))
checks[[16]]<-check(identical(opening$release_md5,manifest_digest),"unchanged_release_and_test_input_fingerprints")
full_gates<-rbind(fread(file.path(VD_REPORT,"validation_gates_2025.csv")),
  fread(file.path(VD_REPORT,"hitter_release_gates_2025.csv")))
publish<-all(full_gates$passed)
withheld<-copy(h)
withheld[,release_status:=if(publish) "passed_declared_release_gates" else "withheld_failed_hitter_release_gates"]
if(!publish) withheld[,Decision_plus:=NA_real_]
checks[[17]]<-check(publish||all(is.na(withheld$Decision_plus)),"failed_hitter_release_gates_withhold_published_index")
fwrite(full_gates,file.path(VD_REPORT,"release_gates_2025.csv"))
fwrite(withheld,file.path(VD_REPORT,"hitter_release_2025.csv"))
out<-rbindlist(checks);out[,result_md5:=unname(tools::md5sum(result_path))]
fwrite(out,file.path(VD_REPORT,"artifact_verification_2025.csv"))
fwrite(probe[,head(.SD,1),by=.(observed_action,preference)],file.path(VD_REPORT,"decision_examples_2025.csv"))
print(out[,.(check,passed)])
