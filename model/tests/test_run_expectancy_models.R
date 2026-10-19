# Lightweight base-R/data.table tests for the RE model implementation.
# Run from the project root: Rscript tests/test_run_expectancy_models.R

source("run_expectancy_models.R")

toy <- data.table(
  game_pk = c(1, 1, 1, 1, 2),
  inning = c(1, 1, 1, 1, 9),
  inning_topbot = c("Top", "Top", "Bot", "Bot", "Bot"),
  outs_when_up = c(0, 1, 0, 2, 0),
  balls = c(0, 2, 1, 3, 0),
  strikes = c(0, 1, 0, 2, 0),
  on_1b = c(NA, 100L, NA, NA, NA),
  on_2b = c(NA, NA, NA, 200L, NA),
  on_3b = c(NA, NA, NA, NA, NA),
  bat_score = c(0, 0, 0, 1, 0),
  fld_score = c(0, 0, 0, 0, 0),
  post_bat_score = c(0, 1, 0, 2, 1),
  post_fld_score = c(0, 0, 0, 3, 0),
  batter = c(11L, 12L, 13L, 14L, 15L),
  pitcher = c(21L, 21L, 22L, 22L, 23L),
  season = 2022L
)

state_data <- prepare_pitch_states(toy)
stopifnot(nrow(state_data) == 4L) # terminal walkoff row is censored
stopifnot(all(state_data$runs_remaining >= 0))
stopifnot(identical(state_data$base_state, c("000", "100", "000", "010")))
stopifnot(identical(state_data$base_out_state, c("000_0", "100_1", "000_0", "010_2")))
stopifnot(identical(state_data$runs_remaining, c(1, 1, 2, 1)))

re24 <- fit_re24(state_data)
stopifnot(nrow(re24$table[base_out_state == "000_0"]) == 1L)
stopifnot(abs(re24$table[base_out_state == "000_0", re24] - 1.5) < 1e-12)
stopifnot(length(predict_re24(re24, state_data)) == nrow(state_data))

lambda <- 4
re288 <- fit_re288(state_data, lambda)
expected <- (1 * 1 + lambda * 1.5) / (1 + lambda)
observed <- re288$table[re288_state == "000_0_0-0", re288]
stopifnot(abs(observed - expected) < 1e-12)
stopifnot(all(is.finite(predict_re288(re288, state_data, re24))))
stopifnot(all(predict_re288(re288, state_data, re24) >= 0))

# The feature interface is deliberately state-only: identity/profile/score
# fields cannot change an RE prediction once the base-out-count state is fixed.
with_noise <- copy(state_data)
with_noise[, `:=`(batter = rev(batter), pitcher = rev(pitcher), score_diff_batting = 99)]
stopifnot(identical(predict_re24(re24, state_data), predict_re24(re24, with_noise)))
stopifnot(identical(predict_re288(re288, state_data, re24),
  predict_re288(re288, with_noise, re24)))

cat("All run-expectancy unit checks passed.\n")
