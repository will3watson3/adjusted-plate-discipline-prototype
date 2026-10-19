source("evaluate_locked2025.R")
setDTthreads(2)
path<-file.path(VD_MODELS,"supported_decisions_2024.rds")
d<-as.data.table(readRDS(path))
original<-as.data.table(readRDS("data/models/decision/decision_values_2024.rds"))
release<-readRDS(file.path(VD_MODELS,"release_manifest.rds"))
check<-function(ok,name) {
  if(!isTRUE(ok)) stop("Artifact check failed: ",name)
  data.table(check=name,passed=TRUE)
}
results<-list()
results[[1]]<-check(nrow(d)==704446L&&!anyDuplicated(d,by=CT_KEYS)&&
  identical(d[,..CT_KEYS],original[,..CT_KEYS]),"complete_unique_pitch_keys_and_order")
results[[2]]<-check(all(vapply(names(original),function(nm) identical(d[[nm]],original[[nm]]),logical(1))),
  "every_original_value_and_column_unchanged")
results[[3]]<-check(all(d$primary_supported==(d$support_reason==""))&&
  all(is.na(d$primary_swing_minus_take[!d$primary_supported]))&&
  identical(d$primary_swing_minus_take[d$primary_supported],d$swing_minus_take[d$primary_supported]),
  "unsupported_comparisons_are_missing_with_reasons")
results[[4]]<-check(all(!d$primary_supported[d$q_swing_basic>5])&&sum(d$q_swing_basic>5)==2L,
  "both_original_runaway_predictions_withheld")
results[[5]]<-check(all(!d$primary_supported[d$state_value_order_warning])&&
  all(d$preference[!d$primary_supported]=="unsupported"),"ordering_warnings_and_support_propagated")
s<-d[primary_supported==TRUE]
results[[6]]<-check(nrow(s)>0&&all(is.finite(s$gap_model_se_bound))&&all(s$gap_model_se_bound>=0)&&
  max(abs(s$gap_ci_low-(s$swing_minus_take-1.96*s$gap_model_se_bound)))<1e-12&&
  max(abs(s$gap_ci_high-(s$swing_minus_take+1.96*s$gap_model_se_bound)))<1e-12,
  "conditional_interval_arithmetic")
results[[7]]<-check(all(s$preference==ifelse(s$gap_ci_low>0,"swing",ifelse(s$gap_ci_high<0,"take","no_clear_preference"))),
  "no_clear_preference_uses_interval_not_point_sign")
results[[8]]<-check(all(s$contact_training_cell_n>=30L)&all(s$post_training_cell_n>=30L)&
  all(s$bip_training_cell_n>=30L)&all(s$take_training_cell_n>=30L)&
  all(s$p_swing_reference>=.05&s$p_swing_reference<=.95),"training_support_and_overlap_rules")
for(h in list(release$input_hashes,release$output_hashes,release$code_hashes)) vd_check_hash(h)
results[[9]]<-check(identical(release$train_years,2022:2023)&&identical(release$validation_year,2024L)&&
  identical(release$passed,all(release$gates$passed)),"chronology_fingerprints_and_gate_status")
hitters<-fread(file.path(VD_REPORT,"hitter_exploratory_2024.csv"))
results[[10]]<-check(!anyDuplicated(hitters$batter)&&sum(hitters$eligible)==nrow(d)&&
  sum(hitters$supported)==nrow(s)&&all(is.na(hitters$Decision_plus)),"hitter_coverage_and_unpublished_grades")
s[,above_reference:=(as.integer(observed_action=="swing")-p_swing_reference)*swing_minus_take]
means<-s[,.(rate=100*mean(above_reference),g=uniqueN(game_pk)),by=batter]
j<-match(means$batter,hitters$batter)
results[[11]]<-check(max(abs(means$rate-hitters$runs_per_100[j]))<1e-10&&
  identical(as.integer(means$g),as.integer(hitters$games[j])),"hitter_metric_and_game_clustering_recomputed")
results[[12]]<-check(!any(s$factual_target_status=="verified_bip" & s$description!="hit_into_play")&&
  all(is.finite(s$factual_q[s$factual_target_status %chin% c("verified_bip","verified_ordinary_transition")])),
  "factual_labels_are_action_specific_and_verified")
m<-vd_load();policy<-readRDS(file.path(VD_MODELS,"league_policy_2022_2023.rds"))
contract<-readRDS(file.path(VD_MODELS,"support_contract.rds"))
sample<-d[,head(.SD,1),by=.(observed_action,primary_supported,preference)]
sample<-rbindlist(list(sample,d[q_swing_basic>5]),use.names=TRUE)
sample<-unique(sample,by=CT_KEYS)
raw<-vd_read(2024L);raw<-raw[match(dv_key(sample),dv_key(raw))]
original_names<-names(original)
fresh<-vd_annotate(sample[,..original_names],raw,m,contract,policy)
poison<-copy(raw)[,`:=`(description="hit_into_play",events="home_run",launch_speed=120,launch_angle=40,post_bat_score=999)]
again<-vd_annotate(sample[,..original_names],poison,m,contract,policy)
reversed<-vd_annotate(sample[.N:1,..original_names],raw[.N:1],m,contract,policy)
results[[13]]<-check(identical(fresh$support_reason,again$support_reason)&&
  identical(fresh$p_swing_reference,again$p_swing_reference)&&
  identical(fresh$support_reason,rev(reversed$support_reason))&&
  max(abs(fresh$p_swing_reference-rev(reversed$p_swing_reference)))<1e-12,
  "support_and_reference_ignore_outcome_labels_and_batch_order")
results[[14]]<-check(all(vapply(contract$stages[1:4],function(stage) sum(stage$cells$n)==stage$n,logical(1)))&&
  contract$stages$bip$n==244790L&&identical(contract$train_years,2022:2023),
  "stage_support_uses_exact_training_population")
unc<-vd_uncertainty(fresh,raw,m,policy,read_re_baseline())
results[[15]]<-check(max(abs(unc$gap_model_se_bound-sample$gap_model_se_bound),na.rm=TRUE)<1e-10&&
  max(abs(unc$above_reference_model_se_bound-sample$above_reference_model_se_bound),na.rm=TRUE)<1e-10,
  "saved_conditional_uncertainty_reproduces_from_all_five_models")
# Exercise the guarded evaluator's scoring implementation on known 2024
# features with synthetic 2025 metadata. No actual test-season data is read.
synthetic<-copy(raw)
synthetic[,`:=`(game_year=2025L,game_date=as.Date(paste0("2025",substr(as.character(game_date),5,10))))]
test_score<-vd_score_test(synthetic,m,read_re_baseline())
j<-match(dv_key(sample),dv_key(test_score))
results[[16]]<-check(!anyNA(j)&&max(abs(sample$q_swing_basic-test_score$q_swing_basic[j]))<1e-8&&
  max(abs(sample$q_take_basic-test_score$q_take_basic[j]))<1e-10&&
  max(abs(sample$swing_minus_take-test_score$swing_minus_take[j]))<1e-8,
  "test_evaluator_scoring_parity_on_synthetic_metadata_without_opening_2025")
if(release$passed) {
  graded<-fread(file.path(VD_REPORT,"hitter_validated_2024.csv"))
  q<-graded[reference_eligible==TRUE];w<-q$supported/sum(q$supported)
  results[[17]]<-check(is.finite(release$reference$scale)&&release$reference$scale>0&&
    abs(sum(w*q$Decision_plus)-100)<1e-10&&abs(sum(w*(q$Decision_plus-100)^2)-100)<1e-9&&
    all(is.na(graded$Decision_plus[!graded$reference_eligible])),
    "frozen_index_center_100_scale_10_and_exposure_qualification")
}
if(!release$passed) {
  locked<-inherits(tryCatch(vd_assert_release(),error=identity),"error")
  results[[17]]<-check(locked&&!file.exists(file.path(VD_REPORT,"test_opened_2025.rds")),
    "failed_release_keeps_2025_unopened")
}
all<-rbindlist(results)
all[,release_md5:=unname(tools::md5sum(file.path(VD_MODELS,"release_manifest.rds")))]
fwrite(all,file.path(VD_REPORT,"artifact_verification.csv"))
print(all[,.(check,passed)])
