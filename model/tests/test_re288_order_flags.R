source("post_contact_models.R")
setDTthreads(2)
fails <- function(expr) inherits(tryCatch(expr,error=identity),"error")
states <- build_take_state_values(); f <- re288_order_flags(states)
stopifnot(sum(f$state_value_order_warning)==34L,sum(f$ball_lowers_value)==25L,
  sum(f$strike_raises_value)==11L,sum(f$strike_exceeds_ball)==4L,
  all(f$state_value_order_warning==(f$ball_lowers_value|f$strike_raises_value|f$strike_exceeds_ball)),
  all(nzchar(f$state_value_order_reason)==f$state_value_order_warning))
fixture <- attach_re288_order_flags(data.table(re288_state=c("001_0_2-0","111_0_3-1","000_0_0-0","bad",NA)))
stopifnot(identical(fixture$state_value_order_warning,c(TRUE,TRUE,FALSE,NA,NA)),
  fixture$state_value_order_reason[1]=="ball_lowers_value;strike_raises_value;strike_exceeds_ball",
  fixture$state_value_order_reason[2]=="strike_raises_value",is.na(fixture$state_value_order_reason[4]))

# All 288 states, reversed take order, no duplicated rows, preserved raw values.
sw <- data.table(game_pk=seq_len(288L),at_bat_number=1L,pitch_number=1L,
  re288_state=states$re288_state,re_before=states$re_before,q_swing_basic=states$re_before+.03,
  status="scored",low_location_support=FALSE)
tv <- combine_take_values(states$re288_state,rep(.5,288),states)
tv <- cbind(sw[,..TV_KEYS],data.table(re288_state=states$re288_state),tv)
tv[, `:=`(take_value_status=ifelse(state_value_order_warning,"scored_state_value_warning","scored"),
  low_location_support=TRUE,unknown_catcher=FALSE)]
out <- combine_decision_values(sw,tv[288:1])
stopifnot(identical(out[,..TV_KEYS],sw[,..TV_KEYS]),identical(out$q_swing_basic,sw$q_swing_basic),
  identical(out$state_value_order_warning,f$state_value_order_warning),
  all(out$take_low_location_support),!any(out$low_location_support),all(out$status=="scored"),
  sum(out$decision_value_status=="scored_state_value_warning")==34L,
  max(abs(out$swing_minus_take-(sw$q_swing_basic-tv$q_take_basic)))<1e-12)
stopifnot(identical(out,combine_decision_values(out,tv)))
# Missing/excluded/invalid states stay unusable, never falsely clean.
stopifnot(combine_decision_values(sw[1],tv[0])$decision_value_status=="missing_take_value")
excluded <- copy(tv[1]);excluded[,`:=`(q_take_basic=NA_real_,take_value_status="outside_ordinary_pitch_scope")]
stopifnot(combine_decision_values(sw[1],excluded)$decision_value_status=="take_value_unavailable")
bad <- copy(sw[1]);bad[,q_swing_basic:=NA_real_]
stopifnot(combine_decision_values(bad,tv)$decision_value_status=="invalid_swing_value")
bad <- copy(sw[1]);bad[,`:=`(re288_state="bad",re_before=NA_real_)]
bt <- copy(tv[1]);bt[,`:=`(re288_state="bad",re_before=NA_real_,state_value_order_warning=NA)]
stopifnot(combine_decision_values(bad,bt)$decision_value_status=="state_order_unverified")
for(nm in c("re288_state","re_before","state_value_order_warning")) {
  bt <- copy(tv[1]);set(bt,j=nm,value=switch(nm,re288_state="bad",re_before=-1,state_value_order_warning=TRUE))
  stopifnot(fails(combine_decision_values(sw[1],bt)))
}
stopifnot(fails(combine_decision_values(sw,rbind(tv,tv[1]))),
  fails(combine_decision_values(rbind(sw,sw[1]),tv)))
cat("All RE288 ordering checks passed: 288 states, keyed joins, unknowns, invalid values, support separation and mismatches.\n")
