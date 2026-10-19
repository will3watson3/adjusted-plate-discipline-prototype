# Pitch-level RE24 and smoothed RE288 models.
# Run: Rscript run_expectancy_models.R
# Train RE values on 2022-2023, tune the RE288 shrinkage on 2023 using 2022
# as the initial fit, and evaluate once on the held-out 2024 season.
# The 2021 batter/pitcher splits are exported as past-only profile inputs for
# the later pitch-outcome model; they are intentionally not predictors in RE.

if (!requireNamespace("data.table", quietly = TRUE)) stop("Install data.table first")
library(data.table)

project_dir <- normalizePath(getwd())
processed_dir <- file.path(project_dir, "data", "processed")
output_dir <- file.path(project_dir, "reports", "run_expectancy")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

needed <- c("game_pk", "inning", "inning_topbot", "outs_when_up", "balls", "strikes",
  "on_1b", "on_2b", "on_3b", "bat_score", "fld_score", "post_bat_score",
  "post_fld_score", "batter", "pitcher")

read_season <- function(year) {
  path <- file.path(processed_dir, sprintf("statcast_%d_clean.rds", year))
  if (!file.exists(path)) stop("Missing cleaned data: ", path)
  x <- readRDS(path)
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop(year, " is missing required columns: ", paste(missing, collapse = ", "))
  x <- as.data.table(x)[, ..needed]
  x[, season := as.integer(year)]
  x
}

occupied <- function(x) !is.na(x) & nzchar(trimws(as.character(x))) & as.character(x) != "0"

# Reconstruct pre-pitch state and completed-half-inning target using the clean
# full event table. Exclude terminal home walkoff half-innings because their
# remaining runs are censored when the game ends.
prepare_pitch_states <- function(x) {
  x <- copy(as.data.table(x))
  x[, model_row_id := .I]
  x[, `:=`(
    inning = suppressWarnings(as.integer(as.character(inning))),
    outs = suppressWarnings(as.integer(as.character(outs_when_up))),
    balls = suppressWarnings(as.integer(as.character(balls))),
    strikes = suppressWarnings(as.integer(as.character(strikes))),
    bat_score = suppressWarnings(as.numeric(as.character(bat_score))),
    fld_score = suppressWarnings(as.numeric(as.character(fld_score))),
    post_bat_score = suppressWarnings(as.numeric(as.character(post_bat_score))),
    post_fld_score = suppressWarnings(as.numeric(as.character(post_fld_score)))
  )]
  x[, `:=`(r1 = occupied(on_1b), r2 = occupied(on_2b), r3 = occupied(on_3b))]
  x[, base_state := paste0(as.integer(r1), as.integer(r2), as.integer(r3))]
  x[, base_out_state := paste0(base_state, "_", outs)]
  x[, count_state := paste0(balls, "-", strikes)]
  x[, re288_state := paste(base_out_state, count_state, sep = "_")]

  valid <- x[!is.na(game_pk) & !is.na(inning) & inning >= 1L &
    inning_topbot %chin% c("Top", "Bot") & outs %in% 0:2 &
    balls %in% 0:3 & strikes %in% 0:2 &
    is.finite(bat_score) & is.finite(post_bat_score) &
    is.finite(fld_score) & is.finite(post_fld_score)]
  if (!nrow(valid)) stop("No rows have valid state and score fields")

  halves <- valid[, .(
    end_bat = max(c(bat_score, post_bat_score), na.rm = TRUE),
    end_fld = max(c(fld_score, post_fld_score), na.rm = TRUE),
    n_rows = .N
  ), by = .(game_pk, inning, inning_topbot)]
  halves[, half_order := inning * 2L - as.integer(inning_topbot == "Top")]
  setorder(halves, game_pk, half_order)
  halves[, is_last_observed_half := seq_len(.N) == .N, by = game_pk]
  # A later half proves the current half ended. For a game's last half, retain
  # it only when the fielding team finished ahead (a completed losing half).
  halves[, completed_half := !is_last_observed_half | end_fld > end_bat]
  n_walkoff_halves <- halves[is_last_observed_half & end_bat > end_fld, .N]
  n_unresolved_final_halves <- halves[is_last_observed_half & end_bat == end_fld, .N]
  halves <- halves[completed_half == TRUE, .(game_pk, inning, inning_topbot, end_bat)]
  valid <- merge(valid, halves, by = c("game_pk", "inning", "inning_topbot"), all = FALSE)
  setorder(valid, model_row_id)
  valid[, runs_remaining := end_bat - bat_score]
  valid <- valid[is.finite(runs_remaining) & runs_remaining >= 0]
  valid[, season := as.integer(season)]
  attr(valid, "half_audit") <- list(
    total_half_innings = nrow(halves) + n_walkoff_halves + n_unresolved_final_halves,
    included_half_innings = uniqueN(halves, by = c("game_pk", "inning", "inning_topbot")),
    excluded_walkoff_halves = n_walkoff_halves,
    excluded_unresolved_final_halves = n_unresolved_final_halves
  )
  valid
}

fit_re24 <- function(train) {
  x <- as.data.table(train)
  tab <- x[, .(re24 = mean(runs_remaining), n = .N), by = base_out_state]
  global <- x[, mean(runs_remaining)]
  list(table = tab, global = global)
}

fit_re288 <- function(train, lambda) {
  if (length(lambda) != 1L || !is.finite(lambda) || lambda < 0) stop("lambda must be nonnegative")
  x <- as.data.table(train)
  base <- fit_re24(x)
  cells <- x[, .(cell_mean = mean(runs_remaining), n = .N), by = re288_state]
  cells[, base_out_state := sub("_[0-3]-[0-2]$", "", re288_state)]
  cells <- merge(cells, base$table, by = "base_out_state", all.x = TRUE, suffixes = c("", "_24"))
  cells[is.na(re24), re24 := base$global]
  cells[, re288 := if (lambda == 0) cell_mean else (n * cell_mean + lambda * re24) / (n + lambda)]
  list(table = cells[, .(re288_state, base_out_state, cell_mean, n, re24, re288)],
    lambda = lambda, fallback = base$global)
}

predict_re24 <- function(model, newdata) {
  d <- as.data.table(newdata)[, .(base_out_state)]
  d[, row_id := .I]
  p <- merge(d, model$table, by = "base_out_state", all.x = TRUE, sort = FALSE)
  setorder(p, row_id)
  p[is.na(re24), re24 := model$global]
  p$re24
}

predict_re288 <- function(model, newdata, re24_model) {
  d <- as.data.table(newdata)
  k <- d[, .(re288_state)]
  k[, row_id := .I]
  p <- merge(k, model$table[, .(re288_state, re288)],
    by = "re288_state", all.x = TRUE, sort = FALSE)
  setorder(p, row_id)
  fallback <- predict_re24(re24_model, d)
  p[is.na(re288), re288 := fallback[is.na(re288)]]
  p$re288
}

score_predictions <- function(model24, model288, data) {
  d <- copy(as.data.table(data))
  d[, `:=`(pred_re24 = predict_re24(model24, d),
    pred_re288 = predict_re288(model288, d, model24))]
  d
}

metric_row <- function(label, actual, predicted) {
  err <- predicted - actual
  data.table(model = label, n = length(actual), mae = mean(abs(err)),
    rmse = sqrt(mean(err^2)), mean_error = mean(err),
    observed_mean = mean(actual), predicted_mean = mean(predicted))
}

game_bootstrap_difference <- function(scored, B = 500L, seed = 20260930L) {
  g <- scored[, .(
    n = .N,
    abs24 = sum(abs(pred_re24 - runs_remaining)),
    abs288 = sum(abs(pred_re288 - runs_remaining)),
    sq24 = sum((pred_re24 - runs_remaining)^2),
    sq288 = sum((pred_re288 - runs_remaining)^2)
  ), by = game_pk]
  set.seed(seed)
  out <- replicate(B, {
    idx <- sample.int(nrow(g), nrow(g), replace = TRUE)
    n <- sum(g$n[idx])
    c(mae_diff = sum(g$abs288[idx] - g$abs24[idx]) / n,
      rmse_diff = sqrt(sum(g$sq288[idx]) / n) - sqrt(sum(g$sq24[idx]) / n))
  })
  data.table(comparison = c("RE288 minus RE24 MAE", "RE288 minus RE24 RMSE"),
    estimate = c(mean(abs(scored$pred_re288 - scored$runs_remaining)) -
      mean(abs(scored$pred_re24 - scored$runs_remaining)),
      sqrt(mean((scored$pred_re288 - scored$runs_remaining)^2)) -
      sqrt(mean((scored$pred_re24 - scored$runs_remaining)^2))),
    ci_low = apply(out, 1, quantile, probs = 0.025, names = FALSE),
    ci_high = apply(out, 1, quantile, probs = 0.975, names = FALSE),
    bootstrap_games = B)
}

tune_lambda <- function(train_2022, validation_2023,
  lambdas = c(0, 10, 25, 50, 75, 100, 150, 200, 225, 250, 275, 300, 350, 400, 500, 600)) {
  base <- fit_re24(train_2022)
  scores <- rbindlist(lapply(lambdas, function(lam) {
    smooth <- fit_re288(train_2022, lam)
    pred <- predict_re288(smooth, validation_2023, base)
    e <- pred - validation_2023$runs_remaining
    data.table(lambda = lam, mae = mean(abs(e)), rmse = sqrt(mean(e^2)),
      mean_error = mean(e), n = length(e))
  }))
  # Select by development MAE only; 2024 remains untouched for final validation.
  selected <- scores[which.min(mae), lambda]
  list(selected = selected, scores = scores)
}

export_2021_profiles <- function() {
  bpath <- file.path(processed_dir, "batter_splits_2021.csv")
  ppath <- file.path(processed_dir, "pitcher_splits_2021.csv")
  if (!file.exists(bpath) || !file.exists(ppath)) stop("Missing 2021 player-split files")
  b <- fread(bpath)
  p <- fread(ppath)
  bcols <- c("batter", "batter_name", "p_throws", "pitch_family", "ordinary_pitches",
    "swings", "chase_swings", "contacts", "whiffs", "swing_rate", "chase_rate",
    "zone_swing_rate", "contact_rate", "whiff_rate", "out_of_zone_contact_rate",
    "tracked_balls_in_play", "mean_exit_velocity_mph", "hard_hit_rate")
  pcols <- c("pitcher", "pitcher_name", "stand", "pitch_family", "ordinary_pitches",
    "swings", "chase_swings", "contacts", "whiffs", "swing_rate", "chase_rate",
    "zone_rate", "contact_rate", "whiff_rate", "called_strike_rate_on_takes",
    "mean_velocity_mph", "mean_spin_rpm", "mean_pfx_x_inches", "mean_pfx_z_inches",
    "mean_exit_velocity_mph", "hard_hit_rate")
  if (length(setdiff(bcols, names(b))) || length(setdiff(pcols, names(p)))) {
    stop("2021 split schema differs from the expected profile fields")
  }
  b <- b[, ..bcols]
  p <- p[, ..pcols]
  b[, profile_season := 2021L]
  p[, profile_season := 2021L]
  fwrite(b, file.path(output_dir, "batter_profiles_2021.csv"))
  fwrite(p, file.path(output_dir, "pitcher_profiles_2021.csv"))
  invisible(list(batter_rows = nrow(b), pitcher_rows = nrow(p)))
}

run_model <- function() {
  message("Loading 2022-2024 full cleaned pitch events...")
  seasons <- setNames(lapply(2022:2024, function(y) prepare_pitch_states(read_season(y))), 2022:2024)
  half_audit <- rbindlist(lapply(names(seasons), function(y) {
    a <- attr(seasons[[y]], "half_audit")
    data.table(season = as.integer(y), total_half_innings = a$total_half_innings,
      included_half_innings = a$included_half_innings,
      excluded_walkoff_halves = a$excluded_walkoff_halves,
      excluded_unresolved_final_halves = a$excluded_unresolved_final_halves)
  }), fill = TRUE)
  fwrite(half_audit, file.path(output_dir, "half_inning_audit.csv"))
  if (any(vapply(seasons, function(x) any(x$runs_remaining < 0), logical(1)))) stop("Negative run target")
  d22 <- seasons[["2022"]]
  d23 <- seasons[["2023"]]
  d24 <- seasons[["2024"]]

  message("Tuning RE288 shrinkage on 2023, with 2022 as the fit season...")
  tuning <- tune_lambda(d22, d23)
  fwrite(tuning$scores, file.path(output_dir, "re288_lambda_tuning_2023.csv"))
  lambda <- tuning$selected
  message("Selected shrinkage lambda: ", lambda)

  train <- rbindlist(list(d22, d23), use.names = TRUE)
  model24 <- fit_re24(train)
  model288 <- fit_re288(train, lambda)
  fwrite(model24$table, file.path(output_dir, "RE24_train_2022_2023.csv"))
  fwrite(model288$table, file.path(output_dir, "RE288_smoothed_train_2022_2023.csv"))
  model_info <- data.table(model = c("RE24", "RE288_smoothed"),
    training_seasons = "2022-2023", validation_season = "2024",
    re288_shrinkage_lambda = c(NA_real_, lambda),
    training_pitch_states = nrow(train), validation_pitch_states = nrow(d24),
    training_half_innings = uniqueN(train, by = c("game_pk", "inning", "inning_topbot")),
    validation_half_innings = uniqueN(d24, by = c("game_pk", "inning", "inning_topbot")))
  fwrite(model_info, file.path(output_dir, "model_manifest.csv"))

  message("Scoring held-out 2024...")
  scored <- score_predictions(model24, model288, d24)
  metrics <- rbind(metric_row("RE24", scored$runs_remaining, scored$pred_re24),
    metric_row("RE288_smoothed", scored$runs_remaining, scored$pred_re288))
  fwrite(metrics, file.path(output_dir, "validation_metrics_2024.csv"))
  by_state <- scored[, .(
    n = .N,
    observed_mean = mean(runs_remaining),
    re24_mean = mean(pred_re24),
    re24_bias = mean(pred_re24 - runs_remaining),
    re288_mean = mean(pred_re288),
    re288_bias = mean(pred_re288 - runs_remaining),
    re24_mae = mean(abs(pred_re24 - runs_remaining)),
    re288_mae = mean(abs(pred_re288 - runs_remaining))
  ), by = .(base_out_state, count_state)][order(base_out_state, count_state)]
  fwrite(by_state, file.path(output_dir, "validation_by_state_2024.csv"))
  intervals <- game_bootstrap_difference(scored)
  fwrite(intervals, file.path(output_dir, "validation_game_bootstrap_2024.csv"))

  profiles <- export_2021_profiles()
  audit_summary <- data.table(
    season = 2022:2024,
    valid_pitch_states = vapply(seasons, nrow, integer(1)),
    unique_games = vapply(seasons, function(x) uniqueN(x$game_pk), integer(1)),
    unique_batters = vapply(seasons, function(x) uniqueN(x$batter), integer(1)),
    unique_pitchers = vapply(seasons, function(x) uniqueN(x$pitcher), integer(1))
  )
  fwrite(audit_summary, file.path(output_dir, "season_state_counts.csv"))
  cat("RE24 / RE288 complete\n",
    "Selected lambda: ", lambda, "\n",
    "2024 held-out metrics:\n", paste(capture.output(print(metrics)), collapse = "\n"), "\n",
    "2024 game-cluster bootstrap:\n", paste(capture.output(print(intervals)), collapse = "\n"), "\n",
    "2021 profiles: ", profiles$batter_rows, " batter split rows; ", profiles$pitcher_rows,
    " pitcher split rows. Profiles are exports only and are not RE predictors.\n",
    file = file.path(output_dir, "run_expectancy_summary.txt"))
  print(metrics)
  print(intervals)
  message("Reports written to ", output_dir)
}

if (sys.nframe() == 0L) run_model()
