# Run: Rscript tests/test_attach_run_expectancy.R
source("attach_run_expectancy.R")
baseline <- read_re_baseline()
toy <- data.table(game_pk = c(2L, 1L, 1L, 1L), at_bat_number = c(1L, 2L, 1L, 3L),
  pitch_number = 1L, balls = c(0, 3, 0, 2), strikes = c(0, 2, 0, 2),
  outs_when_up = c(0, 2, 1, 1), on_1b = c(NA, 11, NA, NA),
  on_2b = c(NA, NA, 12, NA), on_3b = c(NA, 13, NA, 14),
  description = c("automatic_ball", "foul", "ball", "hit_into_play"),
  analysis_eligible = c(FALSE, TRUE, TRUE, TRUE), keep_me = letters[1:4])
joined <- attach_re_to_pitches(toy, 2024L, baseline)
stopifnot(identical(joined$re288_state, c("000_0_0-0", "101_2_3-2", "010_1_0-0", "001_1_2-2")),
  all(joined$re_match_status == "matched"), nrow(joined) == nrow(toy),
  all(vapply(names(toy), function(n) identical(toy[[n]], joined[[n]]), logical(1))),
  all(joined$re_time_role == "after_fit_window"), all(is.finite(joined$re288_before)))
stopifnot(joined$re288_before[1] == baseline$re288[re288_state == "000_0_0-0", re288],
  joined$re24_before[1] == baseline$re24[base_out_state == "000_0", re24])

# RE is pre-pitch: changing the observed action/outcome cannot change its value.
other_outcome <- copy(toy)
other_outcome[, description := rev(description)]
stopifnot(identical(joined$re288_before, attach_re_to_pitches(other_outcome, 2024L, baseline)$re288_before))
# Both fitted seasons are labeled even when a particular inning was excluded from fitting.
for (year in 2022:2023) stopifnot(all(attach_re_to_pitches(toy, year, baseline)$re_time_role == "training_season"))
stopifnot(all(attach_re_to_pitches(toy, 2021, baseline)$re_time_role == "before_fit_window_not_historical"))

# Invalid counts/outs/runner IDs remain in the output with NA values, never fallbacks.
bad <- copy(toy)
bad[, balls := c(4, 1.5, NA, 0)]
bad[4, on_1b := -1]
out <- attach_re_to_pitches(bad, 2024L, baseline)
stopifnot(nrow(out) == 4L, all(out$re_match_status == "invalid_pre_pitch_state"),
  all(is.na(out$re288_before)), all(is.na(out$re24_before)))

fails <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = identity), "error")
stopifnot(fails(attach_re_to_pitches(rbind(toy, toy[1]), 2024L, baseline)),
  fails(attach_re_to_pitches(joined, 2024L, baseline)))
wrong_bases <- copy(toy)[, base_state := "000"]
stopifnot(fails(attach_re_to_pitches(wrong_bases, 2024L, baseline)))
wrong_year <- copy(toy)[, game_year := 2023L]
stopifnot(fails(attach_re_to_pitches(wrong_year, 2024L, baseline)))

# Reject duplicate or missing model keys instead of silently multiplying rows.
tmp <- tempfile("re-join-test-")
dir.create(tmp)
invisible(file.copy(baseline$files$file, tmp))
fwrite(rbind(baseline$re288, baseline$re288[1]), file.path(tmp, "RE288_smoothed_train_2022_2023.csv"))
stopifnot(fails(read_re_baseline(tmp)))
fwrite(baseline$re288[-1], file.path(tmp, "RE288_smoothed_train_2022_2023.csv"))
stopifnot(fails(read_re_baseline(tmp)))
fwrite(baseline$re288, file.path(tmp, "RE288_smoothed_train_2022_2023.csv"))
bad_manifest <- copy(baseline$manifest)[model == "RE288_smoothed", re288_shrinkage_lambda := NA_real_]
fwrite(bad_manifest, file.path(tmp, "model_manifest.csv"))
stopifnot(fails(read_re_baseline(tmp)))
unlink(tmp, recursive = TRUE)
cat("All pitch RE join checks passed.\n")
