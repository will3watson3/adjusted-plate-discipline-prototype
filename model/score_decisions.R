# Scoring only: Rscript score_decisions.R. Frozen 2022-2023 fits, 2024 only.
source("post_contact_models.R")
DV_REPORT <- "reports/decision"
DV_MODELS <- "data/models/decision"
DV_TAKES <- c("ball", "blocked_ball", "called_strike")
dv_key <- function(d) do.call(paste,c(d[,..CT_KEYS],sep="|"))

# STEP 1. PSEUDOCODE: Preserve source order; include only ordinary, eligible
# swings/takes. Reject unexpected eligible labels or mixed seasons explicitly.
decision_population <- function(raw) {
  d <- as.data.table(raw)
  if(anyNA(d[,..CT_KEYS])||anyDuplicated(d,by=CT_KEYS)) stop("Invalid source pitch keys")
  if(anyNA(d$game_year)||any(d$game_year!=2024L)||anyNA(d$game_date)||
     any(format(d$game_date,"%Y")!="2024")) stop("Only 2024 scoring is authorized")
  cols <- unique(c(CT_KEYS,CT_INPUTS,"game_date","game_year","description","batter_name","pitcher_name",
    intersect(c("home_team","away_team","inning_topbot"),names(d))))
  d <- copy(d[analysis_eligible %in% TRUE,..cols])
  if(any(!d$description %chin% c(CT_SWINGS,DV_TAKES))) stop("Unexpected eligible action")
  d[,season:=2024L]
  d
}

load_decision_models <- function() {
  m <- list(contact=readRDS(file.path(CT_MODELS,"contact_gam_fit_2022_2023.rds")),
    post=readRDS(file.path(PC_MODELS,"post_contact_fit_2022_2023.rds")),
    value=readRDS(file.path(PC_MODELS,"bip_value_fit_2022_2023.rds")))
  for(b in list(m$contact,m$post$bip,m$post$tip,m$value))
    stopifnot(identical(b$train_years,2022:2023),isTRUE(b$model$converged))
  m
}

# STEP 2. PSEUDOCODE: Reuse verified observed-swing rows exactly. Predict the
# hypothetical swing on takes from the feature allowlist, never the observed call.
# Join existing ordinary-take expectations by pitch key and preserve all flags.
score_decision_population <- function(d,saved_swings,take,models,re=read_re_baseline()) {
  d <- as.data.table(d); saved_swings <- as.data.table(saved_swings)
  if(anyDuplicated(d,by=CT_KEYS)||anyDuplicated(saved_swings,by=CT_KEYS)) stop("Duplicate scoring keys")
  swing_rows <- which(d$description %chin% CT_SWINGS)
  take_rows <- which(d$description %chin% DV_TAKES)
  if(length(swing_rows)+length(take_rows)!=nrow(d)) stop("Unexpected scoring action")
  if(!setequal(dv_key(d[swing_rows]),dv_key(saved_swings))) stop("Saved swing coverage differs")
  j <- match(dv_key(d[swing_rows]),dv_key(saved_swings))
  identity_cols <- unique(c(CT_KEYS,"game_date","season","batter","batter_name","pitcher","pitcher_name","description",CT_INPUTS))
  for(nm in identity_cols) if(!identical(d[[nm]][swing_rows],saved_swings[[nm]][j]))
    stop("Saved swing input mismatch: ",nm)
  swings <- copy(saved_swings[j])
  if(length(take_rows)) {
    taken <- d[take_rows]
    predictions <- predict_swing_value(models$contact,models$post,models$value,taken[,..CT_INPUTS],re)
    taken <- cbind(taken[,..identity_cols],predictions)
    taken <- combine_decision_values(taken,take,build_take_state_values(re))
    taken[,`:=`(time_role="forward_validation_2024",value_scope="ordinary_outcomes_no_extra_runner_action")]
  } else taken <- saved_swings[0]
  full <- rbindlist(list(swings,taken),use.names=TRUE)
  full <- full[match(dv_key(d),dv_key(full))]
  stopifnot(identical(full[,..CT_KEYS],d[,..CT_KEYS]))
  full[,`:=`(observed_action=ifelse(description %chin% DV_TAKES,"take","swing"),
    swing_evaluation_scope=ifelse(description %chin% DV_TAKES,
      "hypothetical_swing_on_observed_take","observed_swing_population"))]
  # Keep useful retrieval context and the net take value beside the net swing value.
  for(nm in intersect(c("home_team","away_team","inning_topbot"),names(d))) set(full,j=nm,value=d[[nm]])
  k <- match(dv_key(full),dv_key(take))
  full[,`:=`(p_called_strike_if_ordinary_take=take$p_called_strike_if_ordinary_take[k],
    take_run_value_basic=take$take_run_value_basic[k])]
  stopifnot(!anyNA(k))
  full
}

run_decision_scoring <- function() {
  setDTthreads(2)
  dir.create(DV_REPORT,recursive=TRUE,showWarnings=FALSE)
  dir.create(DV_MODELS,recursive=TRUE,showWarnings=FALSE)
  re <- read_re_baseline()
  paths <- c("score_decisions.R","post_contact_models.R","contact_models.R","called_strike_models.R",
    "take_value_models.R","re288_order_flags.R","attach_run_expectancy.R","run_expectancy_models.R",re$files$file,
    file.path(CT_MODELS,"contact_gam_fit_2022_2023.rds"),
    file.path(PC_MODELS,c("post_contact_fit_2022_2023.rds","bip_value_fit_2022_2023.rds","swing_values_2024.rds")),
    "data/processed/statcast_2024_clean.rds","data/processed/statcast_2024_take_values.rds")
  fingerprints <- data.table(path=paths,md5=unname(tools::md5sum(paths)))
  stopifnot(!anyNA(fingerprints$md5))
  raw <- as.data.table(readRDS("data/processed/statcast_2024_clean.rds"))
  d <- decision_population(raw)
  fwrite(raw[,.(pitches=.N),by=.(description,analysis_eligible,exclusion_reason)],file.path(DV_REPORT,"population_audit_2024.csv"))
  rm(raw);gc(verbose=FALSE)
  saved <- as.data.table(readRDS(file.path(PC_MODELS,"swing_values_2024.rds")))
  take <- as.data.table(readRDS("data/processed/statcast_2024_take_values.rds"))
  stopifnot(all(take$game_year==2024L),all(saved$season==2024L))
  models <- load_decision_models()
  message("Scoring hypothetical swings on ",sum(d$description %chin% DV_TAKES)," eligible takes; reusing ",nrow(saved)," observed swings.")
  full <- score_decision_population(d,saved,take,models,re)
  j <- match(dv_key(saved),dv_key(full))
  stopifnot(all(vapply(names(saved),function(nm) identical(saved[[nm]],full[[nm]][j]),logical(1))),
    !anyDuplicated(full,by=CT_KEYS),all(is.finite(full$q_swing_basic)),all(is.finite(full$q_take_basic)),
    max(abs(full$swing_minus_take-(full$swing_run_value_basic-full$take_run_value_basic)))<1e-12,
    identical(fingerprints$md5,unname(tools::md5sum(paths))))
  cs_save(full,file.path(DV_MODELS,"decision_values_2024.rds"))
  coverage <- full[,.(pitches=.N,state_order_warnings=sum(state_value_order_warning),
    low_swing_location_support=sum(low_location_support),outside_swing_training_range=sum(outside_training_range),
    low_take_location_support=sum(take_low_location_support)),by=observed_action]
  fwrite(coverage,file.path(DV_REPORT,"coverage_2024.csv"))
  fwrite(full[,.(min_q_swing=min(q_swing_basic),median_q_swing=median(q_swing_basic),
    max_q_swing=max(q_swing_basic),min_swing_minus_take=min(swing_minus_take),
    median_swing_minus_take=median(swing_minus_take),max_swing_minus_take=max(swing_minus_take)),
    by=observed_action],file.path(DV_REPORT,"value_ranges_2024.csv"))
  fwrite(full[,.(pitches=.N),by=.(observed_action,description,decision_value_status)],file.path(DV_REPORT,"status_2024.csv"))
  fwrite(full[,head(.SD,2),by=.(observed_action,description,state_value_order_warning,low_location_support)],
    file.path(DV_REPORT,"decision_examples_2024.csv"))
  fwrite(fingerprints,file.path(DV_REPORT,"input_fingerprints.csv"))
  writeLines(c("# Combined scoring for swings and takes","",
    sprintf("Saved %s eligible 2024 pitches: %s observed swings and %s observed takes.",
      format(nrow(full),big.mark=","),format(sum(full$observed_action=="swing"),big.mark=","),format(sum(full$observed_action=="take"),big.mark=",")),"",
    "Output: `data/models/decision/decision_values_2024.rds`. One row per eligible pitch, in original clean-source order. The original swing-only output remains unchanged. Excluded pitches are accounted for in `population_audit_2024.csv`; they are not assigned ordinary decision values.","",
    "Takes include ball, blocked ball and called strike. Existing scope excludes bunts, HBP, automatic/special calls and other ineligible pitches. `observed_action` distinguishes swings/takes; `swing_evaluation_scope` explicitly labels hypothetical swing predictions on takes. Observed calls are not used as swing-model predictors or substituted for expected take value.","",
    "Both actions have expected remaining-run Q values and net run values. `swing_minus_take = q_swing_basic - q_take_basic = swing_run_value_basic - take_run_value_basic`. Positive favors the modeled swing; negative favors the modeled ordinary take. This is a model comparison, not actual runs gained/lost or a finished hitter grade.","",
    "The frozen 2022-2023 contact, post-contact and BIP fits and RE288 table are reused without retraining. 2025 remains locked. All existing observed-swing columns are preserved exactly by key. Training support flags, RE ordering reasons, take status and combined decision status are retained. `status` remains the contact input status; `decision_value_status` describes the combined value calculation.","",
    "Taken pitches have no observed swing outcome. Scoring and arithmetic checks therefore do not validate counterfactual swing accuracy on takes. The location/range diagnostics describe contact-model support; they do not establish BIP-model support. No uncertainty threshold, action recommendation, or hitter aggregation is added.","",
    "Run `Rscript score_decisions.R`, then `Rscript tests/verify_decision_artifacts.R`. Tests: `Rscript tests/test_decision_scoring.R`. Coverage and representative pitch examples are saved beside this report."),file.path(DV_REPORT,"README.md"))
  print(coverage)
}
if(sys.nframe()==0L) run_decision_scoring()
