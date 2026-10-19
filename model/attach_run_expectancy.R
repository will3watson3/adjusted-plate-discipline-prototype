# Attach the saved RE24/RE288 baseline to every cleaned pitch, without filtering.
# Run from the project root: Rscript attach_run_expectancy.R
# Defaults to 2022-2024. 2021 is history/warm-up; 2025 remains the locked test.
if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table first")
library(data.table)

re_keys <- c("game_pk", "at_bat_number", "pitch_number")
re_columns <- c("re288_state", "re24_before", "re288_before", "re24_training_n",
  "re288_training_n", "re_match_status", "re_model_id", "re_training_seasons", "re_time_role")

read_re_baseline <- function(directory = "reports/run_expectancy") {
  paths <- file.path(directory, c("RE24_train_2022_2023.csv",
    "RE288_smoothed_train_2022_2023.csv", "model_manifest.csv"))
  if (!all(file.exists(paths))) stop("Missing saved run-expectancy tables or manifest")
  m <- list(re24 = fread(paths[1]), re288 = fread(paths[2]), manifest = fread(paths[3]))
  if (!all(c("base_out_state", "re24", "n") %in% names(m$re24)) ||
      !all(c("re288_state", "re288", "n") %in% names(m$re288))) stop("Invalid RE table schema")
  states24 <- unlist(lapply(c("000", "001", "010", "011", "100", "101", "110", "111"),
    function(b) paste(b, 0:2, sep = "_")))
  counts <- as.vector(outer(0:3, 0:2, paste, sep = "-"))
  states288 <- as.vector(outer(states24, counts, paste, sep = "_"))
  if (anyDuplicated(m$re24$base_out_state) || anyDuplicated(m$re288$re288_state) ||
      !setequal(m$re24$base_out_state, states24) ||
      !setequal(m$re288$re288_state, states288)) stop("RE tables must contain unique, complete states")
  for (pair in list(c("re24", "re24"), c("re288", "re288"))) {
    tab <- m[[pair[1]]]
    if (any(!is.finite(tab[[pair[2]]]) | tab[[pair[2]]] < 0) ||
        any(!is.finite(tab$n) | tab$n <= 0 | tab$n != floor(tab$n))) stop("Invalid RE values or sample sizes")
  }
  if (!all(c("model", "training_seasons", "re288_shrinkage_lambda") %in% names(m$manifest)) ||
      nrow(m$manifest) != 2L || !setequal(m$manifest$model, c("RE24", "RE288_smoothed")) ||
      anyNA(m$manifest$training_seasons) || any(m$manifest$training_seasons != "2022-2023") ||
      !isTRUE(all.equal(as.numeric(m$manifest[model == "RE288_smoothed", re288_shrinkage_lambda]), 250))) {
    stop("Saved baseline provenance changed; review the join before proceeding")
  }
  m$id <- "RE24_RE288_train2022_2023_lambda250"
  m$files <- data.table(file = paths, md5 = unname(tools::md5sum(paths)))
  m
}

attach_re_to_pitches <- function(pitches, year, baseline = read_re_baseline()) {
  if (length(year) != 1L || !is.finite(year) || year != floor(year)) stop("Provide one integer season")
  d <- copy(as.data.table(pitches))
  required <- c(re_keys, "balls", "strikes", "outs_when_up", "on_1b", "on_2b", "on_3b")
  missing <- setdiff(required, names(d))
  if (length(missing)) stop("Missing pitch fields: ", paste(missing, collapse = ", "))
  if (length(intersect(re_columns, names(d)))) stop("RE columns already exist; use the original clean data")
  if (!nrow(d) || anyNA(d[, ..re_keys]) || anyDuplicated(d, by = re_keys)) stop("Pitch keys must be nonmissing and unique")
  if ("game_year" %in% names(d) && (anyNA(d$game_year) || any(d$game_year != year))) stop("Season does not match pitch data")

  # STEP 1. PSEUDOCODE: Reconstruct the exact state encoding used by the RE fit.
  # The three base digits are first, second, third; NA/blank/zero means empty.
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  runners <- lapply(d[, .(on_1b, on_2b, on_3b)], function(x) {
    s <- trimws(as.character(x))
    empty <- is.na(x) | s == "" | s == "0"
    v <- num(s)
    list(occupied = !empty, valid = empty | (is.finite(v) & v > 0 & v == floor(v)))
  })
  outs <- num(d$outs_when_up)
  balls <- num(d$balls)
  strikes <- num(d$strikes)
  valid <- outs %in% 0:2 & balls %in% 0:3 & strikes %in% 0:2 &
    Reduce(`&`, lapply(runners, `[[`, "valid"))
  base <- do.call(paste0, lapply(runners, function(x) as.integer(x$occupied)))
  base_out <- paste(base, outs, sep = "_")
  state <- paste(base_out, paste(balls, strikes, sep = "-"), sep = "_")
  state[!valid] <- NA_character_
  base_out[!valid] <- NA_character_
  # Existing cleaned features must agree; never silently overwrite them.
  if ("base_state" %in% names(d) && any(valid & (is.na(d$base_state) | d$base_state != base))) stop("Existing base_state disagrees with runners")
  if ("base_out_state" %in% names(d) && any(valid & (is.na(d$base_out_state) | d$base_out_state != base_out))) stop("Existing base_out_state disagrees with state")

  # STEP 2. PSEUDOCODE: Exact many-to-one lookup. Preserve every original row,
  # column and order, including events excluded from swing/take analysis.
  i24 <- match(base_out, baseline$re24$base_out_state)
  i288 <- match(state, baseline$re288$re288_state)
  status <- fifelse(!valid, "invalid_pre_pitch_state",
    fifelse(is.na(i24) | is.na(i288), "unmatched_state", "matched"))
  d[, `:=`(re288_state = state,
    re24_before = baseline$re24$re24[i24], re288_before = baseline$re288$re288[i288],
    re24_training_n = baseline$re24$n[i24], re288_training_n = baseline$re288$n[i288],
    re_match_status = status, re_model_id = baseline$id, re_training_seasons = "2022-2023",
    re_time_role = if (year < 2022L) "before_fit_window_not_historical" else if (year <= 2023L)
      "training_season" else "after_fit_window")]
  d
}

run_re_join <- function(years = 2022:2024, project = ".") {
  processed <- file.path(project, "data", "processed")
  report_dir <- file.path(project, "reports", "pitch_run_expectancy")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  baseline <- read_re_baseline(file.path(project, "reports", "run_expectancy"))
  summaries <- state_counts <- issues <- examples <- vector("list", length(years))
  for (k in seq_along(years)) {
    y <- years[k]
    message("Attaching saved RE tables to ", y, "...")
    input <- file.path(processed, sprintf("statcast_%d_clean.rds", y))
    original <- as.data.table(readRDS(input))
    message("Read ", nrow(original), " pitches; matching pre-pitch states...")
    joined <- attach_re_to_pitches(original, y, baseline)
    # STEP 3. PSEUDOCODE: Verify exact input preservation before saving a new file.
    stopifnot(nrow(original) == nrow(joined), !anyDuplicated(joined, by = re_keys),
      all(vapply(names(original), function(n) identical(original[[n]], joined[[n]]), logical(1))))
    matched <- joined$re_match_status == "matched"
    stopifnot(all(is.finite(joined$re24_before[matched])), all(is.finite(joined$re288_before[matched])))
    output <- file.path(processed, sprintf("statcast_%d_with_re.rds", y))
    message("Verified original columns; saving ", output)
    # Level-one gzip keeps full pitch features while making this repeatable join fast.
    con <- gzfile(output, "wb", compression = 1)
    tryCatch(saveRDS(joined, con), finally = close(con))
    summaries[[k]] <- data.table(season = y, input_rows = nrow(original), output_rows = nrow(joined),
      matched_rows = sum(matched), invalid_rows = sum(joined$re_match_status == "invalid_pre_pitch_state"),
      unmatched_rows = sum(joined$re_match_status == "unmatched_state"),
      analysis_eligible_rows = sum(joined$analysis_eligible %in% TRUE),
      matched_analysis_eligible_rows = sum(matched & joined$analysis_eligible %in% TRUE),
      re_time_role = unique(joined$re_time_role), re_model_id = baseline$id,
      re_training_seasons = "2022-2023", input_file = input, output_file = output)
    state_counts[[k]] <- joined[, .(season = y, pitches = .N,
      re24_before = re24_before[1], re288_before = re288_before[1]), by = .(re288_state, re_match_status)]
    issues[[k]] <- joined[re_match_status != "matched", c(re_keys, "re288_state", "re_match_status"), with = FALSE][, season := y]
    sample_cols <- intersect(c(re_keys, "batter_name", "description", "balls", "strikes",
      "base_state", "outs_when_up", "re288_state", "re24_before", "re288_before", "re_time_role"), names(joined))
    examples[[k]] <- joined[!duplicated(re288_state), ..sample_cols][1:min(.N, 24L)][, season := y]
    rm(original, joined)
    gc(verbose = FALSE)
  }
  # STEP 4. PSEUDOCODE: Save join coverage, exceptions and model fingerprints.
  summary <- rbindlist(summaries)
  fwrite(summary, file.path(report_dir, "join_summary.csv"))
  fwrite(rbindlist(state_counts), file.path(report_dir, "coverage_by_state.csv"))
  fwrite(rbindlist(issues), file.path(report_dir, "unmatched_or_invalid_pitches.csv"))
  fwrite(rbindlist(examples), file.path(report_dir, "example_pitches.csv"))
  fwrite(baseline$files, file.path(report_dir, "model_fingerprints.csv"))
  print(summary[, .(season, input_rows, output_rows, matched_rows, invalid_rows, unmatched_rows, re_time_role)])
  invisible(summary)
}

if (sys.nframe() == 0L) run_re_join()
