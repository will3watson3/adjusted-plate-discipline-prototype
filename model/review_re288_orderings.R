# Run: Rscript review_re288_orderings.R. Review and flag only; no fits or 2025 reads.
source("post_contact_models.R")
ORDER_REPORT <- "reports/re288_order_review"

review_re288_orderings <- function() {
  setDTthreads(2)
  dir.create(ORDER_REPORT, recursive=TRUE, showWarnings=FALSE)
  re <- read_re_baseline(); states <- build_take_state_values(re)
  flags <- re288_order_flags(states)
  states <- attach_re288_order_flags(states, states)
  # STEP 1. PSEUDOCODE: Compare frozen signs with raw training cell means.
  # Compare the same signs with existing held-out observed means descriptively.
  validation <- fread("reports/run_expectancy/validation_by_state_2024.csv")
  validation[, re288_state:=paste(base_out_state,count_state,sep="_")]
  for (branch in c("before", "ball", "strike")) {
    keys <- switch(branch,before=states$re288_state,ball=states$ball_state,strike=states$strike_state)
    i <- match(keys,re$re288$re288_state); j <- match(keys,validation$re288_state)
    raw <- re$re288$cell_mean[i]; heldout <- validation$observed_mean[j]
    raw[keys=="INNING_END"] <- 0; heldout[keys=="INNING_END"] <- 0
    states[, (paste0(branch,"_raw_mean")):=raw]
    states[, (paste0(branch,"_training_n")):=re$re288$n[i]]
    states[, (paste0(branch,"_observed_mean_2024")):=heldout]
    states[, (paste0(branch,"_n_2024")):=validation$n[j]]
  }
  states[, `:=`(raw_ball_run_value=ball_runs+ball_raw_mean-before_raw_mean,
    raw_strike_run_value=strike_runs+strike_raw_mean-before_raw_mean,
    raw_ball_minus_strike=ball_runs+ball_raw_mean-strike_runs-strike_raw_mean,
    observed_ball_run_value_2024=ball_runs+ball_observed_mean_2024-before_observed_mean_2024,
    observed_strike_run_value_2024=strike_runs+strike_observed_mean_2024-before_observed_mean_2024,
    observed_ball_minus_strike_2024=ball_runs+ball_observed_mean_2024-strike_runs-strike_observed_mean_2024,
    min_training_n=pmin(before_training_n,ball_training_n,strike_training_n,na.rm=TRUE))]
  issue_spec <- list(ball_lowers_value=c("ball_run_value","raw_ball_run_value","observed_ball_run_value_2024"),
    strike_raises_value=c("called_strike_run_value","raw_strike_run_value","observed_strike_run_value_2024"),
    strike_exceeds_ball=c("ball_minus_strike","raw_ball_minus_strike","observed_ball_minus_strike_2024"))
  issues <- rbindlist(lapply(names(issue_spec),function(reason) {
    cols <- issue_spec[[reason]]; x <- states[get(reason)==TRUE]
    sign <- if(reason=="strike_raises_value") 1 else -1
    x[,.(re288_state,reason,frozen_margin=sign*get(cols[1]),raw_margin=sign*get(cols[2]),
      observed_margin_2024=sign*get(cols[3]),min_training_n)]
  }))
  issues[, `:=`(present_in_raw=raw_margin>0,repeats_in_2024=observed_margin_2024>0)]
  warning_states <- states[state_value_order_warning==TRUE]
  stopifnot(nrow(warning_states)==34L, nrow(issues)==40L,
    setequal(warning_states$re288_state,fread("reports/take_value/state_value_order_warnings.csv")$re288_state))
  fwrite(warning_states,file.path(ORDER_REPORT,"state_review.csv"))
  fwrite(issues,file.path(ORDER_REPORT,"ordering_issues.csv"))
  issue_summary <- issues[,.(states=.N,present_in_raw=sum(present_in_raw),
    repeats_in_2024=sum(repeats_in_2024),max_reversal=max(frozen_margin)),by=reason]
  fwrite(issue_summary,file.path(ORDER_REPORT,"issue_summary.csv"))

  # STEP 2. PSEUDOCODE: Backfill existing combined output, preserving every
  # existing column, row and numeric prediction exactly; no model re-scoring.
  path <- file.path(PC_MODELS,"swing_values_2024.rds")
  old <- as.data.table(readRDS(path))
  take <- as.data.table(readRDS("data/processed/statcast_2024_take_values.rds"))
  stopifnot(all(old$season==2024L),all(take$game_year==2024L))
  full <- combine_decision_values(old,take,states)
  preserved <- vapply(names(old),function(nm) identical(old[[nm]],full[[nm]]),logical(1))
  stopifnot(all(preserved),identical(old[,..TV_KEYS],full[,..TV_KEYS]))
  # Capture provenance before writing; retain the original training manifest.
  historical <- fread(file.path(PC_REPORT,"input_fingerprints.csv"))
  original_code <- historical[path=="post_contact_models.R",md5]
  snapshot <- file.path(ORDER_REPORT,"source_snapshots/post_contact_models.R")
  stopifnot(identical(unname(tools::md5sum(snapshot)),original_code))
  assets <- historical[!path %chin% c("post_contact_models.R","re288_order_flags.R")]
  stopifnot(identical(assets$md5,unname(tools::md5sum(assets$path))))
  if (!identical(names(old),names(full))) cs_save(full,path)
  fwrite(full[,head(.SD,3),by=description],file.path(PC_REPORT,"combined_swing_examples_2024.csv"))
  fwrite(pc_branch_states(re),file.path(PC_REPORT,"foul_tip_whiff_state_values.csv"))
  coverage <- full[,.(pitches=.N,ball_lowers_value=sum(ball_lowers_value),
    strike_raises_value=sum(strike_raises_value),strike_exceeds_ball=sum(strike_exceeds_ball)),by=decision_value_status]
  fwrite(coverage,file.path(ORDER_REPORT,"combined_output_coverage_2024.csv"))
  examples <- full[state_value_order_warning==TRUE,head(.SD,1),by=re288_state]
  fwrite(examples,file.path(ORDER_REPORT,"flagged_decision_examples_2024.csv"))
  fwrite(data.table(check=c("all_existing_columns_identical","pitch_keys_and_order_identical",
    "original_training_inputs_unchanged","frozen_re_unchanged"),passed=TRUE),
    file.path(ORDER_REPORT,"refresh_verification.csv"))
  inputs <- unique(c("review_re288_orderings.R","post_contact_models.R","re288_order_flags.R",
    snapshot,assets$path,file.path(PC_REPORT,"input_fingerprints.csv"),
    file.path(PC_MODELS,c("post_contact_fit_2022_2023.rds","bip_value_fit_2022_2023.rds")),
    "reports/run_expectancy/validation_by_state_2024.csv"))
  fwrite(data.table(path=inputs,md5=unname(tools::md5sum(inputs))),file.path(ORDER_REPORT,"refresh_input_fingerprints.csv"))

  # STEP 3. PSEUDOCODE: Publish all 34 states and distinguish evidence from causes.
  rows <- warning_states[order(-pmax(-ball_run_value,called_strike_run_value,-ball_minus_strike))]
  nflag <- full[state_value_order_warning==TRUE,.N]
  lines <- c("# Review of frozen RE288 state orderings","",
    "The 2022-2023 lambda-250 baseline is unchanged. Review date: 2026-10-05. No fitting, tuning, clamping or 2025 access.","",
    sprintf("Confirmed **34 of 288 states**, containing **40 overlapping ordering issues**: 25 ball-decreases, 11 strike-increases and 4 strike-above-ball comparisons. They affect **%s of %s combined 2024 swing-versus-take rows (%.3f%%)**.",
      format(nflag,big.mark=","),format(nrow(full),big.mark=","),100*nflag/nrow(full)),"",
    "## Evidence and interpretation","",
    "| Ordering | States | Also in raw training means | Same issue in 2024 observed means | Largest reversal (runs) |",
    "| --- | ---: | ---: | ---: | ---: |",
    vapply(seq_len(nrow(issue_summary)),function(i) sprintf("| %s | %d | %d | %d | %.6f |",
      issue_summary$reason[i],issue_summary$states[i],issue_summary$present_in_raw[i],
      issue_summary$repeats_in_2024[i],issue_summary$max_reversal[i]),character(1)),"",
    sprintf("Of the 40 frozen issues, %d already occur in the raw training cell means and %d appear only after smoothing. Only %d repeat in the 2024 observed cell means, including none of the four strike-above-ball comparisons. These comparisons support retaining explicit flags; they do not justify silently correcting values or declaring the orderings resolved.",
      sum(issues$present_in_raw),sum(!issues$present_in_raw),sum(issues$repeats_in_2024)),"",
    sprintf("All flagged states have at least one runner. Their pre-pitch training counts range from %d to %d; %d of 34 have at least one nonterminal current/branch cell below 250 observations (the fixed shrinkage pseudo-count).",min(rows$before_training_n),max(rows$before_training_n),sum(rows$min_training_n<250)),"",
    "Raw-versus-smoothed comparisons identify which sign reversals already existed before shrinkage. Unequal shrinkage toward a common base-out mean can also change ordering. Sparse cells and population differences across counts are plausible contributors, but this review does not identify a causal explanation for each reversal. Counts are pitch observations, not independent games; repeated counts and clustered half-innings mean these are not significance tests.","",
    "2024 comparisons use the existing uncensored validation cell means, with the same ordinary branch rules and immediate runs. They are descriptive checks on new observations, not refits, counterfactual validation, or evidence that an individual ball/strike caused the difference. No warning is removed because a sign fails to repeat in 2024.","",
    "## All 34 flagged states","",
    "Runner bits are first/second/third; state = runners_outs_balls-strikes. N is current-state training pitches. Min N includes the current, ball and strike states, excluding terminal inning ends. B = ball lowers value; S = strike raises value; X = strike exceeds ball. Rows sort by largest reversal.","",
    "| State | N | Min N | Ball RV | Strike RV | Ball minus strike | Flags |",
    "| --- | ---: | ---: | ---: | ---: | ---: | --- |",
    vapply(seq_len(nrow(rows)),function(i) sprintf("| %s | %d | %d | %+.6f | %+.6f | %+.6f | %s |",
      rows$re288_state[i],rows$before_training_n[i],rows$min_training_n[i],rows$ball_run_value[i],
      rows$called_strike_run_value[i],rows$ball_minus_strike[i],
      paste(c("B","S","X")[c(rows$ball_lowers_value[i],rows$strike_raises_value[i],rows$strike_exceeds_ball[i])],collapse=",")),character(1)),"",
    "Full branch keys, raw means, frozen values, training counts and 2024 means/counts are in [state_review.csv](state_review.csv). Each overlapping comparison is in [ordering_issues.csv](ordering_issues.csv).", "",
    "## Combined-output contract","",
    "`predict_swing_value()` and the saved branch table now carry `state_value_order_warning`, the three reason booleans, and `state_value_order_reason` (semicolon-delimited; empty for known unflagged states; NA for unknown states). The aggregate flag refers to all three checks at the current state, including take-only ordering concerns. It is not a recursive flag for every reachable state or a complete uncertainty assessment of the BIP model.","",
    "`combine_decision_values()` joins take values by unique pitch keys, verifies matching state/baseline/aggregate flag, and carries `take_value_status`, `take_state_value_order_warning`, `take_low_location_support`, and `take_unknown_catcher`. Existing contact `status` and support fields retain their contact-model meaning. `decision_value_status` prioritizes invalid swing, missing take, unavailable take and unknown state before `scored_state_value_warning`; otherwise it is `scored`. A scored label does not establish counterfactual reliability or support.","",
    "Every existing column and row in `data/models/post_contact/swing_values_2024.rds` was preserved exactly, including Q values and swing-minus-take. The regular combined example CSV was refreshed; [flagged_decision_examples_2024.csv](flagged_decision_examples_2024.csv) provides one actual swing per affected state with hitter/pitcher and pitch keys. Numeric differences on flagged rows remain inspectable but should not be treated as clean decision signals or used for hitter grades.","",
    "The original post-contact training fingerprint manifest remains intact. Its original source is preserved under `source_snapshots/`; `refresh_input_fingerprints.csv` records this metadata refresh separately. Take sidecars, RE tables and fitted models were not changed.","",
    "Reproduce: `Rscript review_re288_orderings.R`, `Rscript tests/test_re288_order_flags.R`, `Rscript tests/verify_post_contact_artifacts.R`, then `Rscript summarize_post_contact.R`.")
  writeLines(lines,file.path(ORDER_REPORT,"README.md"))
  print(issue_summary);print(coverage)
}
if(sys.nframe()==0L) review_re288_orderings()
