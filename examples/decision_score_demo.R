# Synthetic illustration, not a fitted MLB prediction or validation sample.
if (!file.exists('model/take_value_models.R')) stop('Run from repository root')
setwd('model')
if (dir.exists('.R-library')) .libPaths(c(normalizePath('.R-library'), .libPaths()))
source('take_value_models.R')
states <- data.table(re288_state=c('000_0_0-0','000_0_0-2'),
  re_before=c(.50,.40),ball_state=c('000_0_1-0','000_0_1-2'),
  strike_state=c('000_0_0-1','000_1_0-0'),ball_runs=0,strike_runs=0,
  ball_re_after=c(.55,.53),strike_re_after=c(.46,.30),
  ball_value=c(.55,.53),called_strike_value=c(.46,.30),
  ball_run_value=c(.05,.13),called_strike_run_value=c(-.04,-.10),
  ball_minus_strike=c(.09,.23),state_value_order_warning=FALSE)
d <- data.table(state=rep(states$re288_state,each=2),action=rep(c('swing','take'),2),
  p_strike=.5,p_league_swing=.7,q_swing=rep(c(.54,.50),each=2))
v <- combine_take_values(d$state,d$p_strike,states)
d[,q_take:=v$q_take_basic]
d[,gap:=q_swing-q_take]
d[,decision_score:=(as.integer(action=='swing')-p_league_swing)*gap]
stopifnot(max(abs(d$decision_score-c(.0105,-.0245,.0255,-.0595)))<1e-12)
print(d)
cat('All numbers are illustrative. Actual home run/out outcomes are not scoring inputs.\n')
