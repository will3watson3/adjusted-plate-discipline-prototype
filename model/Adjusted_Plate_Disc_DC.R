# MLB plate-discipline data: acquisition, cleaning, and descriptive summaries.
# Run with Rscript or source this file from any working directory.
# Pass --season=2021 to select a season; default remains 2025.
# PSEUDOCODE comments explain each step. Raw downloads are cached, never overwritten.

# STEP 1. PSEUDOCODE: Find the project beside this script, load its packages,
# and create output folders. This also supports RStudio's Source button.
script_path <- {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else {
    source_files <- vapply(sys.frames(), function(frame) {
      value <- frame$ofile
      if (is.null(value)) "" else as.character(value[[1]])
    }, character(1))
    source_files <- source_files[nzchar(source_files)]
    if (length(source_files)) tail(source_files, 1) else NULL
  }
}
project_dir <- if (is.null(script_path)) getwd() else dirname(normalizePath(script_path))
setwd(project_dir)
project_library <- file.path(project_dir, ".R-library")
if (dir.exists(project_library)) .libPaths(c(normalizePath(project_library), .libPaths()))
for (p in c("baseballr", "data.table")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Install package: ", p)
}
baseballr_version <- if ("baseballr" %in% loadedNamespaces()) {
  getNamespaceVersion("baseballr")
} else {
  packageVersion("baseballr")
}
if (baseballr_version < "2.0.0") {
  stop("This script needs baseballr >= 2.0.0. Restart R and source this file from the project folder; ",
       "its local .R-library contains baseballr 2.0.0.")
}
library(data.table)
options(timeout = 240)
season_arg <- grep("^--season=", commandArgs(trailingOnly = TRUE), value = TRUE)
season <- if (length(season_arg)) as.integer(sub("^--season=", "", season_arg[[1]])) else {
  as.integer(Sys.getenv("STATCAST_SEASON", "2025"))
}
if (is.na(season) || season < 2008L || season > as.integer(format(Sys.Date(), "%Y")) - 1L) {
  stop("Choose a completed Statcast season from 2008 through last year.")
}
season_tag <- as.character(season)
for (p in c("data/raw", "data/processed", "reports")) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# STEP 2. PSEUDOCODE: Fetch the schedule; keep completed regular-season games;
# collapse repeated schedule listings for postponed/resumed games by game ID.
schedule_file <- file.path("data/raw", paste0("schedule_", season_tag, ".rds"))
if (!file.exists(schedule_file)) saveRDS(baseballr::mlb_schedule(season), schedule_file)
schedule <- as.data.table(readRDS(schedule_file))
games <- unique(schedule[game_type == "R" & status_detailed_state %in% c("Final", "Completed Early"),
  .(game_pk, official_date, venue_id, venue_name, day_night,
    scheduled_innings, game_number, double_header, teams_home_team_id, teams_away_team_id)], by = "game_pk")
stopifnot(nrow(games) > 0L, !anyDuplicated(games$game_pk))
fwrite(games, file.path("reports", paste0("games_", season_tag, ".csv")))

# STEP 3. PSEUDOCODE: Query all players in three-day windows; retry failures;
# save each successful response so reruns do not download the season again.
# Two requests at a time; fewer than 25,000 rows per request is required.
dates <- seq(as.Date(min(games$official_date)), as.Date(max(games$official_date)), by = "3 days")
windows <- data.table(start = dates, end = pmin(dates + 2, as.Date(max(games$official_date))))
windows <- windows[vapply(seq_len(.N), function(i) any(as.Date(games$official_date) >= start[i] &
  as.Date(games$official_date) <= end[i]), logical(1))]
fetch_window <- function(i) {
  a <- windows$start[i]; b <- windows$end[i]
  path <- sprintf("data/raw/statcast_%s_%s.rds", a, b)
  if (!file.exists(path)) {
    for (attempt in 1:3) {
      result <- tryCatch(baseballr::statcast_search(a, b, player_type = "pitcher"), error = identity)
      if (!inherits(result, "error") && is.data.frame(result) && nrow(result) > 0 &&
          all(c("game_pk", "at_bat_number", "pitch_number", "game_type") %in% names(result))) break
      if (attempt == 3) stop("Download failed for ", a, " to ", b)
      Sys.sleep(5 * attempt)
    }
    if (nrow(result) >= 25000) stop("Possible truncated response: ", a, "; use smaller windows")
    saveRDS(result, path)
  }
  message("Cached ", i, "/", nrow(windows), ": ", a, " to ", b)
  path
}
cluster <- parallel::makePSOCKcluster(2, outfile = "")
parallel::clusterExport(cluster, c("windows", "fetch_window"))
invisible(parallel::clusterExport(cluster, "project_dir"))
invisible(parallel::clusterEvalQ(cluster, {
  setwd(project_dir)
  project_library <- file.path(project_dir, ".R-library")
  if (dir.exists(project_library)) .libPaths(c(normalizePath(project_library), .libPaths()))
  options(timeout = 240)
}))
paths <- tryCatch(parallel::parLapplyLB(cluster, seq_len(nrow(windows)), fetch_window),
  finally = parallel::stopCluster(cluster))
stopifnot(length(paths) == nrow(windows), all(file.exists(unlist(paths))))
paths <- unlist(paths)
if ("--download-only" %in% commandArgs(trailingOnly = TRUE)) quit(save = "no")

# STEP 4. PSEUDOCODE: Combine chunks, restrict to the selected regular season, normalize
# blanks, remove exact duplicates, and STOP if two different rows share a pitch key.
raw <- rbindlist(lapply(paths, readRDS), use.names = TRUE, fill = TRUE)
source_columns <- names(raw)
stopifnot(!anyDuplicated(source_columns))
raw_rows <- nrow(raw)
d <- copy(raw[game_type == "R" & game_year == season])
regular_rows <- nrow(d)
rm(raw); invisible(gc(verbose = FALSE))
for (v in names(d)[vapply(d, is.character, logical(1))]) {
  z <- trimws(d[[v]]); z[z %in% c("", "null", "NULL", "NA")] <- NA_character_
  set(d, j = v, value = z)
}
d <- unique(d)
exact_duplicates_removed <- regular_rows - nrow(d)
keys <- c("game_pk", "at_bat_number", "pitch_number")
required <- c(keys, "batter", "pitcher", "description", "balls", "strikes", "outs_when_up",
  "inning", "inning_topbot", "plate_x", "plate_z", "sz_top", "sz_bot", "zone",
  "pitch_type", "release_speed", "pfx_x", "pfx_z", "bat_score", "fld_score",
  "on_1b", "on_2b", "on_3b", "stand", "p_throws", "events")
stopifnot(all(required %in% names(d)))
for (v in c(keys, "batter", "pitcher", "balls", "strikes", "outs_when_up", "inning",
            "plate_x", "plate_z", "sz_top", "sz_bot", "zone", "release_speed", "pfx_x", "pfx_z")) {
  z <- suppressWarnings(as.numeric(d[[v]]))
  if (any(!is.na(d[[v]]) & is.na(z))) stop("Unparseable numeric value in ", v)
  set(d, j = v, value = z)
}
conflicts <- d[duplicated(d, by = keys) | duplicated(d, by = keys, fromLast = TRUE)]
fwrite(conflicts, file.path("reports", paste0("conflicting_pitch_keys_", season_tag, ".csv")))
stopifnot(nrow(conflicts) == 0L, !anyNA(d[, ..keys]), !anyNA(d$batter), !anyNA(d$pitcher))
d[, game_date := as.IDate(game_date)]
stopifnot(all(d$balls %in% 0:3), all(d$strikes %in% 0:2), all(d$outs_when_up %in% 0:2),
  all(d$inning >= 1), all(d$inning_topbot %in% c("Top", "Bot")),
  all(d$pitch_number >= 1), all(d$at_bat_number >= 1),
  all(d$game_date >= as.IDate(min(games$official_date)) & d$game_date <= as.IDate(max(games$official_date))))
stopifnot(setequal(d$game_pk, games$game_pk))
setorderv(d, c("game_date", keys))
d[, pitch_id := paste(game_pk, at_bat_number, pitch_number, sep = "_")]
d[, pa_id := paste(game_pk, at_bat_number, sep = "_")]

# STEP 5. PSEUDOCODE: Join park/game metadata and player names by numeric IDs.
# Never treat the ambiguous Savant player_name field as a batter-name lookup.
d <- merge(d, games[, !"official_date"], by = "game_pk", all.x = TRUE, sort = FALSE)
ids <- sort(unique(c(d$batter, d$pitcher, d$fielder_2)))
ids <- ids[!is.na(ids)]
people_file <- file.path("data/raw", paste0("players_", season_tag, ".rds"))
if (!file.exists(people_file)) {
  people <- rbindlist(lapply(split(ids, ceiling(seq_along(ids) / 100)), function(x) {
    baseballr::mlb_people(person_ids = x)
  }), fill = TRUE)
  saveRDS(people, people_file)
}
people <- as.data.table(readRDS(people_file))
stopifnot(!anyDuplicated(people$id), all(ids %in% people$id))
players <- people[, .(player_id = id, full_name, birth_date, bat_side_code, pitch_hand_code)]
fwrite(players, file.path("data/processed", paste0("players_", season_tag, ".csv")))
d[, batter_name := players$full_name[match(batter, players$player_id)]]
d[, pitcher_name := players$full_name[match(pitcher, players$player_id)]]
d[, catcher_name := players$full_name[match(fielder_2, players$player_id)]]
stopifnot(!anyNA(d$batter_name), !anyNA(d$pitcher_name), nrow(d) == regular_rows - exact_duplicates_removed)

# STEP 6. PSEUDOCODE: Explicitly map every pitch description to swing/take and
# special-event flags. Retain all rows; no guessing for unfamiliar descriptions.
ordinary_swings <- c("swinging_strike", "swinging_strike_blocked", "foul", "foul_tip", "hit_into_play")
ordinary_takes <- c("ball", "blocked_ball", "called_strike")
bunts <- c("foul_bunt", "missed_bunt", "bunt_foul_tip")
special <- c("hit_by_pitch", "pitchout", "intent_ball", "intentional_ball", "automatic_ball",
  "automatic_strike", "swinging_pitchout", "foul_pitchout", "pitchout_hit_into_play")
unknown <- setdiff(unique(d$description), c(ordinary_swings, ordinary_takes, bunts, special))
if (length(unknown)) stop("Review new descriptions before proceeding: ", paste(unknown, collapse = ", "))
d[, is_bunt := description %in% bunts | events %in% c("sac_bunt", "sac_bunt_double_play") |
    (!is.na(des) & grepl("\\bbunt", des, ignore.case = TRUE))]
d[, is_automatic := description %in% c("automatic_ball", "automatic_strike")]
d[, is_pitchout_or_intentional := description %in% c("pitchout", "intent_ball", "intentional_ball",
    "swinging_pitchout", "foul_pitchout", "pitchout_hit_into_play")]
d[, is_hbp := description == "hit_by_pitch"]
d[, is_interference := events %in% c("catcher_interf", "batter_interference")]
d[, swing := fifelse(description %in% c(ordinary_swings, bunts, "swinging_pitchout", "foul_pitchout", "pitchout_hit_into_play"),
    1L, fifelse(description %in% c(ordinary_takes, "hit_by_pitch", "pitchout", "intent_ball", "intentional_ball"), 0L, NA_integer_))]
d[, whiff := fifelse(swing == 1L, as.integer(description %in% c("swinging_strike", "swinging_strike_blocked", "missed_bunt", "swinging_pitchout")), NA_integer_)]
d[, contact := fifelse(swing == 1L, 1L - whiff, NA_integer_)]
d[, ordinary_pitch := description %in% c(ordinary_swings, ordinary_takes) & !is_bunt & !is_interference]
d[, in_play := description %in% c("hit_into_play", "pitchout_hit_into_play")]
d[, called_strike := description == "called_strike"]

# STEP 7. PSEUDOCODE: Derive pre-pitch count, runners, score, inning, handedness,
# pitch family and location. Empty runner IDs mean empty bases; missing tracking
# remains missing. Do not impute takes' bat speed or launch measurements.
d[, `:=`(count = paste(balls, strikes, sep = "-"), two_strikes = strikes == 2,
  three_balls = balls == 3, full_count = balls == 3 & strikes == 2,
  runner_on_1b = !is.na(on_1b), runner_on_2b = !is.na(on_2b), runner_on_3b = !is.na(on_3b))]
d[, runners_on := as.integer(runner_on_1b) + as.integer(runner_on_2b) + as.integer(runner_on_3b)]
d[, base_state := paste0(as.integer(runner_on_1b), as.integer(runner_on_2b), as.integer(runner_on_3b))]
d[, `:=`(risp = runner_on_2b | runner_on_3b, bases_loaded = runners_on == 3,
  base_out_state = paste(base_state, outs_when_up, sep = "_"),
  score_diff_batting = bat_score - fld_score,
  batting_team = fifelse(inning_topbot == "Top", away_team, home_team),
  pitching_team = fifelse(inning_topbot == "Top", home_team, away_team),
  batter_is_home = inning_topbot == "Bot", same_hand = stand == p_throws)]
d[, score_state := fcase(score_diff_batting < 0, "trailing", score_diff_batting == 0, "tied", score_diff_batting > 0, "leading")]
d[, inning_group := fcase(inning <= 3, "1-3", inning <= 6, "4-6", inning <= 9, "7-9", default = "extras")]
d[, `:=`(late_close = inning >= 7 & abs(score_diff_batting) <= 2, extra_innings = inning > 9,
  month = format(game_date, "%Y-%m"))]
d[, pitch_family := fcase(pitch_type %in% c("FF", "SI", "FC", "FA"), "fastball",
  pitch_type %in% c("SL", "ST", "SV", "CU", "KC", "CS"), "breaking",
  pitch_type %in% c("CH", "FS", "FO", "SC"), "offspeed", default = "other_or_unknown")]
d[, valid_location := is.finite(plate_x) & is.finite(plate_z) & is.finite(sz_top) & is.finite(sz_bot) & sz_top > sz_bot]
d[, zone_height_ft := fifelse(valid_location, sz_top - sz_bot, NA_real_)]
d[, zone_z_normalized := fifelse(valid_location, (plate_z - sz_bot) / zone_height_ft, NA_real_)]
d[, in_zone_rectangle := fifelse(valid_location, abs(plate_x) <= 17/24 & plate_z >= sz_bot & plate_z <= sz_top, NA)]
d[, in_zone_savant := fifelse(zone %in% 1:9, TRUE, fifelse(zone %in% 11:14, FALSE, NA))]
d[, `:=`(pfx_x_inches = pfx_x * 12, pfx_z_inches = pfx_z * 12)]
# A 17-inch-wide rectangle is a location proxy, not the umpire's strike call.
# No movement variable is relabeled as total break or induced break.

# STEP 8. PSEUDOCODE: Sort the COMPLETE data before calculating previous-pitch
# fields. Reset at each PA and require consecutive pitch numbers. A gap is not
# a first pitch. Automatic calls remain in the sequence and are explicitly labeled.
setorderv(d, c("game_date", keys))
previous_fields <- c("pitch_number", "pitch_type", "release_speed", "plate_x", "plate_z", "pfx_x", "pfx_z", "description", "pitcher")
d[, paste0("prev_", previous_fields) := shift(.SD), by = .(game_pk, at_bat_number), .SDcols = previous_fields]
d[, `:=`(is_first_pitch = pitch_number == 1,
  previous_pitch_missing = pitch_number > 1 & (is.na(prev_pitch_number) | prev_pitch_number != pitch_number - 1))]
for (v in paste0("prev_", previous_fields)) set(d, i = which(d$previous_pitch_missing), j = v, value = NA)
d[, delta_velocity_mph := release_speed - prev_release_speed]
d[, prior_recorded_pitch_events_in_game := seq_len(.N) - 1L, by = .(game_pk, pitcher)]

# STEP 9. PSEUDOCODE: Flag a core analysis subset without deleting unusual rows.
# Optional spin/bat-tracking missingness does not make an otherwise usable pitch
# ineligible. Flags indicate data suitability, not a fitted Decision+ score.
d[, core_tracking_complete := valid_location & is.finite(release_speed) & release_speed > 0 &
  !is.na(pitch_type) & is.finite(pfx_x) & is.finite(pfx_z)]
d[, context_complete := stand %in% c("L", "R") & p_throws %in% c("L", "R") &
  !is.na(bat_score) & !is.na(fld_score)]
d[, exclusion_reason := fcase(is_automatic, "automatic_count_award",
  is_pitchout_or_intentional, "pitchout_or_intentional", is_hbp, "hit_by_pitch",
  is_interference, "interference", is_bunt, "bunt", !ordinary_pitch, "other_action",
  !core_tracking_complete, "missing_or_invalid_core_tracking", !context_complete, "missing_context",
  default = "eligible")]
d[, analysis_eligible := exclusion_reason == "eligible"]
fwrite(d[analysis_eligible == FALSE, c(keys, "batter", "pitcher", "description", "exclusion_reason"), with = FALSE],
  file.path("reports", paste0("exclusions_", season_tag, ".csv")))

# STEP 10. PSEUDOCODE: Identify recognized completed PA outcomes once per PA.
# Keep non-PA events (e.g. caught stealing) outside the BB/K denominator.
pa_events <- c("single", "double", "triple", "home_run", "field_out", "strikeout", "walk", "intent_walk",
  "hit_by_pitch", "force_out", "grounded_into_double_play", "field_error", "fielders_choice",
  "fielders_choice_out", "sac_fly", "sac_bunt", "double_play", "triple_play", "strikeout_double_play",
  "sac_fly_double_play", "sac_bunt_double_play", "catcher_interf")
d[, completed_pa := events %in% pa_events]
stopifnot(!anyDuplicated(d[completed_pa == TRUE, pa_id]))
pas <- d[completed_pa == TRUE, .(game_pk, game_date, pa_id, at_bat_number, batter, batter_name,
  pitcher, pitcher_name, batting_team, pitching_team, events, balls, strikes,
  inning, inning_topbot, outs_when_up, base_state, score_diff_batting,
  launch_speed, launch_angle, estimated_woba_using_speedangle)]
fwrite(pas, file.path("data/processed", paste0("plate_appearances_", season_tag, ".csv")))

# STEP 11. PSEUDOCODE: Summarize players and game situations using explicit
# denominators. Return NA for zero opportunities, never zero or infinity.
# All discipline rates below exclude identified bunts and other special events.
# PSEUDOCODE: Also fetch official season totals for AVG/OBP/SLG/OPS and ERA/WHIP.
# Keep them labeled official and do not join season totals onto individual pitches.
official <- list()
for (g in c("hitting", "pitching")) {
  file <- paste0("data/raw/official_", g, "_", season_tag, ".rds")
  if (!file.exists(file)) saveRDS(baseballr::mlb_stats(stat_type = "season", player_pool = "all",
    game_type = "R", stat_group = g, season = season, sport_ids = 1, limit = 5000), file)
  z <- as.data.table(readRDS(file))
  stopifnot(nrow(z) > 0, !anyDuplicated(z$player_id), nrow(z) == unique(z$total_splits))
  columns <- intersect(c("player_id", "player_full_name", "num_teams", "age", "games_played", "games_started",
    "plate_appearances", "at_bats", "batters_faced", "hits", "doubles", "triples", "home_runs", "runs", "rbi",
    "base_on_balls", "intentional_walks", "strike_outs", "hit_by_pitch", "sac_bunts", "sac_flies",
    "stolen_bases", "caught_stealing", "avg", "obp", "slg", "ops", "babip", "number_of_pitches",
    "era", "whip", "innings_pitched", "outs", "earned_runs", "wins", "losses", "saves", "holds",
    "strikeouts_per9inn", "walks_per9inn", "home_runs_per9", "inherited_runners", "inherited_runners_scored"), names(z))
  z <- z[, ..columns]
  for (v in intersect(c("avg", "obp", "slg", "ops", "babip", "era", "whip", "strikeouts_per9inn", "walks_per9inn", "home_runs_per9"), names(z))) {
    set(z, j = v, value = suppressWarnings(as.numeric(z[[v]])))
  }
  if (g == "pitching") z[, innings_decimal := outs / 3] # 10.2 in baseball notation means 10 + 2/3.
  denominator <- if (g == "hitting") z$plate_appearances else z$batters_faced
  z[, `:=`(walk_rate = fifelse(denominator > 0, base_on_balls / denominator, NA_real_),
    strikeout_rate = fifelse(denominator > 0, strike_outs / denominator, NA_real_))]
  official[[g]] <- z
  fwrite(z, paste0("data/processed/official_", g, "_", season_tag, ".csv"))
}
ratio <- function(n, den) if (den > 0) n / den else NA_real_
avg <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
summarize_pitches <- function(x, groups) {
  x[, {
    ok <- ordinary_pitch
    zone_known <- ok & !is.na(in_zone_savant)
    iz <- zone_known & in_zone_savant %in% TRUE
    oz <- zone_known & in_zone_savant %in% FALSE
    sw <- ok & swing %in% 1L
    takes <- ok & swing %in% 0L
    n <- sum(ok); ns <- sum(sw); nz <- sum(iz); no <- sum(oz)
    wh <- sum(whiff[sw], na.rm = TRUE); co <- sum(contact[sw], na.rm = TRUE)
    list(recorded_events = .N, ordinary_pitches = n, analysis_eligible_pitches = sum(analysis_eligible),
      swings = ns, whiffs = wh, contacts = co, takes = sum(takes),
      zone_pitches = nz, out_of_zone_pitches = no, unknown_zone_pitches = sum(ok & is.na(in_zone_savant)),
      zone_swings = sum(sw & iz), chase_swings = sum(sw & oz),
      swing_rate = ratio(ns, n), chase_rate = ratio(sum(sw & oz), no),
      zone_swing_rate = ratio(sum(sw & iz), nz), zone_rate = ratio(nz, nz + no),
      contact_rate = ratio(co, ns), whiff_rate = ratio(wh, ns), swinging_strike_rate = ratio(wh, n),
      zone_contact_rate = ratio(sum(contact[sw & iz], na.rm = TRUE), sum(sw & iz)),
      out_of_zone_contact_rate = ratio(sum(contact[sw & oz], na.rm = TRUE), sum(sw & oz)),
      called_strike_rate_on_takes = ratio(sum(called_strike & takes), sum(takes)),
      csw_rate = ratio(sum(called_strike & ok) + wh, n),
      mean_velocity_mph = avg(release_speed[ok]), mean_spin_rpm = avg(release_spin_rate[ok]),
      mean_pfx_x_inches = avg(pfx_x_inches[ok]), mean_pfx_z_inches = avg(pfx_z_inches[ok]),
      tracked_balls_in_play = sum(in_play & is.finite(launch_speed)),
      mean_exit_velocity_mph = avg(launch_speed[in_play]),
      mean_launch_angle_deg = avg(launch_angle[in_play]),
      hard_hit_rate = ratio(sum(in_play & launch_speed >= 95, na.rm = TRUE), sum(in_play & is.finite(launch_speed))))
  }, by = groups]
}
for (role in c("batter", "pitcher")) {
  group <- c(role, paste0(role, "_name"))
  summary <- summarize_pitches(d, group)
  pa_summary <- pas[, .(completed_pa = .N, walks = sum(events %in% c("walk", "intent_walk")),
    intentional_walks = sum(events == "intent_walk"), strikeouts = sum(events %in% c("strikeout", "strikeout_double_play")),
    hits = sum(events %in% c("single", "double", "triple", "home_run")), home_runs = sum(events == "home_run"),
    hit_by_pitch = sum(events == "hit_by_pitch")), by = group]
  summary <- merge(summary, pa_summary, by = group, all.x = TRUE)
  summary[, `:=`(walk_rate = walks / completed_pa, strikeout_rate = strikeouts / completed_pa)]
  z <- copy(official[[if (role == "batter") "hitting" else "pitching"]])
  setnames(z, names(z), ifelse(names(z) == "player_id", role, paste0("official_", names(z))))
  summary <- merge(summary, z, by = role, all.x = TRUE)
  stopifnot(nrow(summary) == uniqueN(d[[role]]), sum(summary$recorded_events) == nrow(d),
    all(summary$swings == summary$contacts + summary$whiffs))
  rates <- grep("_rate$", names(summary), value = TRUE)
  stopifnot(all(vapply(summary[, ..rates], function(x) all(is.na(x) | (is.finite(x) & x >= 0 & x <= 1)), logical(1))))
  fwrite(summary, paste0("data/processed/", role, "_summary_", season_tag, ".csv"))
}
fwrite(summarize_pitches(d, c("balls", "strikes", "outs_when_up", "base_state", "inning_group", "score_state")),
  paste0("data/processed/game_state_summary_", season_tag, ".csv"))
fwrite(summarize_pitches(d, c("pitch_type", "pitch_family", "stand", "p_throws")),
  paste0("data/processed/pitch_type_summary_", season_tag, ".csv"))
fwrite(summarize_pitches(d, c("batter", "batter_name", "p_throws", "pitch_family")),
  paste0("data/processed/batter_splits_", season_tag, ".csv"))
fwrite(summarize_pitches(d, c("pitcher", "pitcher_name", "stand", "pitch_family")),
  paste0("data/processed/pitcher_splits_", season_tag, ".csv"))

# STEP 12. PSEUDOCODE: Audit coverage, missingness, events, and key integrity;
# export the complete cleaned table plus its smaller analysis-ready subset.
# PSEUDOCODE: Compare league PA/outcome totals with independently queried team totals.
official_team_file <- file.path("data/raw", paste0("official_team_hitting_", season_tag, ".rds"))
if (!file.exists(official_team_file)) saveRDS(baseballr::mlb_teams_stats(stat_type = "season", game_type = "R",
  stat_group = "hitting", season = season, sport_ids = 1), official_team_file)
team_totals <- as.data.table(readRDS(official_team_file))
stopifnot(nrow(team_totals) == 30L, uniqueN(team_totals$team_id) == 30L)
reconciliation <- data.table(metric = c("nonautomatic_pitch_events", "completed_pa", "walks_including_intentional", "intentional_walks", "strikeouts", "hits", "home_runs", "hit_by_pitch"),
  statcast = c(sum(!d$is_automatic), nrow(pas), sum(pas$events %in% c("walk", "intent_walk")), sum(pas$events == "intent_walk"),
    sum(pas$events %in% c("strikeout", "strikeout_double_play")), sum(pas$events %in% c("single", "double", "triple", "home_run")),
    sum(pas$events == "home_run"), sum(pas$events == "hit_by_pitch")),
  official = c(sum(team_totals$number_of_pitches), sum(team_totals$plate_appearances), sum(team_totals$base_on_balls), sum(team_totals$intentional_walks),
    sum(team_totals$strike_outs), sum(team_totals$hits), sum(team_totals$home_runs), sum(team_totals$hit_by_pitch)))
reconciliation[, difference := statcast - official]
fwrite(reconciliation, file.path("reports", paste0("official_totals_reconciliation_", season_tag, ".csv")))
outcome_totals_match <- all(reconciliation$difference == 0L)
fwrite(d[, .N, by = .(description, swing, ordinary_pitch, exclusion_reason)],
  file.path("reports", paste0("action_mapping_counts_", season_tag, ".csv")))
fwrite(d[, .N, by = .(events, completed_pa)], file.path("reports", paste0("pa_event_counts_", season_tag, ".csv")))
fwrite(d[, .N, by = exclusion_reason], file.path("reports", paste0("exclusion_counts_", season_tag, ".csv")))
fwrite(d[, .(recorded_events = .N, completed_pa = sum(completed_pa),
  eligible_pitches = sum(analysis_eligible), missing_location = sum(!valid_location),
  missing_core_tracking = sum(!core_tracking_complete), sequence_gaps = sum(previous_pitch_missing)),
  by = .(game_pk, game_date, home_team, away_team)],
  file.path("reports", paste0("game_coverage_", season_tag, ".csv")))
for (group in c("month", "pitch_type", "venue_name", "batter", "pitcher")) {
  fwrite(d[, .(recorded_events = .N, missing_location = sum(!valid_location),
    missing_core_tracking = sum(!core_tracking_complete), eligible_pitches = sum(analysis_eligible)), by = group],
    paste0("reports/coverage_by_", group, "_", season_tag, ".csv"))
}
missingness <- data.table(variable = names(d), data_type = vapply(d, function(x) class(x)[1], character(1)),
  missing_n = vapply(d, function(x) sum(is.na(x)), integer(1)))
missingness[, missing_rate := missing_n / nrow(d)]
fwrite(missingness, file.path("reports", paste0("missingness_", season_tag, ".csv")))
source_paths <- c(paths, schedule_file, people_file, official_team_file,
  file.path("data/raw", paste0("official_hitting_", season_tag, ".rds")),
  file.path("data/raw", paste0("official_pitching_", season_tag, ".rds")))
manifest <- rbindlist(lapply(source_paths, function(p) {
  x <- readRDS(p)
  data.table(file = p, rows = nrow(x), columns = ncol(x), downloaded_at = as.character(file.info(p)$mtime),
    md5 = unname(tools::md5sum(p)))
}))
fwrite(manifest, file.path("reports", paste0("download_manifest_", season_tag, ".csv")))
stopifnot(all(manifest$rows < 25000), !anyDuplicated(d$pitch_id), all(d$analysis_eligible == (d$exclusion_reason == "eligible")))
fwrite(d, file.path("data/processed", paste0("statcast_", season_tag, "_clean.csv.gz")))
saveRDS(d, file.path("data/processed", paste0("statcast_", season_tag, "_clean.rds")), compress = TRUE)
fwrite(d[analysis_eligible == TRUE], file.path("data/processed", paste0("statcast_", season_tag, "_analysis.csv.gz")))

# STEP 13. PSEUDOCODE: Write an explicit predictor allowlist so current outcomes,
# bat tracking, future schedule fields, and full-season summaries cannot leak in.
features <- c("balls", "strikes", "outs_when_up", "inning", "inning_topbot", "base_state", "risp",
  "score_diff_batting", "batter_is_home", "stand", "p_throws", "same_hand", "pitch_type", "pitch_family",
  "release_speed", "plate_x", "plate_z", "sz_top", "sz_bot", "zone_z_normalized", "pfx_x", "pfx_z",
  "release_spin_rate", "spin_axis", "release_extension", "release_pos_x", "release_pos_y", "release_pos_z",
  "arm_angle", "venue_id", "prev_pitch_type", "prev_release_speed", "prev_plate_x", "prev_plate_z",
  "prev_pfx_x", "prev_pfx_z", "prev_description", "prev_pitcher", "is_first_pitch", "previous_pitch_missing",
  "delta_velocity_mph", "prior_recorded_pitch_events_in_game")
stopifnot(all(features %in% names(d)))
fwrite(data.table(variable = features), file.path("reports", paste0("predictor_allowlist_", season_tag, ".csv")))
dictionary <- copy(missingness)
dictionary[, origin := fifelse(variable %in% source_columns, "Savant via baseballr", "derived or MLB lookup")]
dictionary[, role := fifelse(variable %in% features, "candidate_predictor", "not_allowlisted")]
fwrite(dictionary, file.path("reports", paste0("column_inventory_", season_tag, ".csv")))
capture.output(sessionInfo(), file = file.path("reports", paste0("R_session_info_", season_tag, ".txt")))
reconciliation_note <- if (outcome_totals_match) {
  "Passed: league pitch, PA, BB, IBB, K, H, HR, and HBP totals exactly match official MLB team totals."
} else {
  c("Review: one or more Statcast totals differ from official MLB team-season totals.",
    paste(reconciliation[ difference != 0L, paste0(metric, " difference: ", difference)]))
}
writeLines(c(paste0("# ", season_tag, " data audit"), "", paste("Generated:", Sys.time()),
  paste("baseballr version:", packageVersion("baseballr")),
  paste("Downloaded rows (includes spring games in overlapping windows):", raw_rows),
  paste("Regular-season rows before deduplication:", regular_rows),
  paste("Exact duplicate rows removed:", exact_duplicates_removed),
  paste("Cleaned rows:", nrow(d)), paste("Columns:", ncol(d)),
  paste("Unique regular-season games:", uniqueN(d$game_pk), "of", nrow(games)),
  paste("Batters:", uniqueN(d$batter)), paste("Pitchers:", uniqueN(d$pitcher)),
  paste("Recognized completed PAs:", nrow(pas)), paste("Analysis-eligible pitches:", sum(d$analysis_eligible)),
  paste("Nonautomatic pitch events:", sum(!d$is_automatic)),
  paste("Missing/invalid core tracking:", sum(!d$core_tracking_complete)),
  paste("Missing preceding pitch records:", sum(d$previous_pitch_missing)),
  "", "Passed: all scheduled completed regular-season games present; unique pitch keys; valid counts/outs;",
  "all descriptions mapped; no join row multiplication; no request at the 25,000-row guard; player names resolved.",
  reconciliation_note,
  "", "Limit: schedule reconciliation verifies game coverage, not independent pitch-by-pitch completeness.",
  paste0("See reports/official_totals_reconciliation_", season_tag, ".csv for measured differences against MLB team-season PA/outcome totals."),
  "Use the separately labeled official_* player statistics for official season performance totals and rates.",
  "PA totals and BB/K rates are based on represented Savant PA outcomes; no-pitch intentional walks may be absent.",
  "Player attribution is the batter/pitcher on the terminal event; mid-PA substitutions can differ from official scoring.",
  "Bunts in play use PA description text plus sacrifice events; attempted bunts taken are not reliably observable.",
  "Savant run/win-expectancy fields are preserved source labels, not independently reconstructed or validated targets."),
  file.path("reports", paste0("DATA_AUDIT_", season_tag, ".md")))
message("Finished ", season_tag, ": ", nrow(d), " cleaned rows; ", sum(d$analysis_eligible), " analysis-eligible pitches.")
