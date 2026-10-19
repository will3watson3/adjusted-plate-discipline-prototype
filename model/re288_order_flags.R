# Diagnostics only: preserve the frozen RE values and their strict sign checks.
RE_ORDER_FLAGS <- c("ball_lowers_value", "strike_raises_value", "strike_exceeds_ball",
  "state_value_order_warning", "state_value_order_reason")

re288_order_flags <- function(states) {
  x <- copy(as.data.table(states))
  x[, `:=`(ball_lowers_value=ball_run_value<0,
    strike_raises_value=called_strike_run_value>0, strike_exceeds_ball=ball_minus_strike<0)]
  x[, state_value_order_warning:=ball_lowers_value|strike_raises_value|strike_exceeds_ball]
  x[, state_value_order_reason:=vapply(seq_len(.N), function(i) {
    tags <- c("ball_lowers_value", "strike_raises_value", "strike_exceeds_ball")
    paste(tags[c(ball_lowers_value[i], strike_raises_value[i], strike_exceeds_ball[i])], collapse=";")
  }, character(1))]
  x[, c("re288_state", RE_ORDER_FLAGS), with=FALSE]
}

# PSEUDOCODE: Match the CURRENT pre-pitch state; unknown states stay NA.
# These are local ordering warnings, not a complete uncertainty/support test.
attach_re288_order_flags <- function(values, states=build_take_state_values()) {
  out <- copy(as.data.table(values)); flags <- re288_order_flags(states)
  j <- match(out$re288_state, flags$re288_state)
  for (nm in RE_ORDER_FLAGS) set(out, j=nm, value=flags[[nm]][j])
  out
}

# PSEUDOCODE: Join by unique pitch keys, require matching state/baseline/flags,
# carry take support separately, and label unusable comparisons before warnings.
# Keep raw values for inspection. Never convert unknown flags into FALSE.
combine_decision_values <- function(swings, take, states=build_take_state_values()) {
  out <- attach_re288_order_flags(swings, states); take <- as.data.table(take)
  for (d in list(out, take)) {
    if (anyNA(d[, ..TV_KEYS]) || anyDuplicated(d, by=TV_KEYS)) stop("Decision pitch keys must be unique and nonmissing")
  }
  key <- function(d) do.call(paste, c(d[, ..TV_KEYS], sep="|"))
  j <- match(key(out), key(take)); matched <- which(!is.na(j))
  same <- function(a,b) (is.na(a)&is.na(b)) | (!is.na(a)&!is.na(b)&a==b)
  for (nm in c("re288_state", "re_before", "state_value_order_warning")) {
    if (!all(same(out[[nm]][matched], take[[nm]][j[matched]]))) stop("Swing/take mismatch: ", nm)
  }
  out[, `:=`(q_take_basic=take$q_take_basic[j], take_value_status=take$take_value_status[j],
    take_state_value_order_warning=take$state_value_order_warning[j],
    take_low_location_support=take$low_location_support[j], take_unknown_catcher=take$unknown_catcher[j])]
  out[, swing_minus_take:=q_swing_basic-q_take_basic]
  out[, decision_value_status:=fcase(!is.finite(q_swing_basic), "invalid_swing_value",
    is.na(j), "missing_take_value", !is.finite(q_take_basic), "take_value_unavailable",
    is.na(state_value_order_warning), "state_order_unverified",
    state_value_order_warning %in% TRUE, "scored_state_value_warning", default="scored")]
  out
}
