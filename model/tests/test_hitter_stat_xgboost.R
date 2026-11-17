source('hitter_stat_xgboost.R')
states <- data.table(q_whiff=c(.1,.2),q_ordinary_foul=c(.3,.4),q_foul_tip=c(.1,.2))
p <- h7_branches(c(.8,.6),c(.4,.5),c(.02,.03),c(.9,1.1),states)
stopifnot(max(abs(p$p_whiff_given_swing+p$p_bip_given_swing+p$p_foul_given_swing+p$p_tip_given_swing-1))<1e-12)
expected <- c(.2*.1+.8*(.4*.9+.6*(.98*.3+.02*.1)),.4*.2+.6*(.5*1.1+.5*(.97*.4+.03*.2)))
stopifnot(max(abs(p$q_swing_basic-expected))<1e-12)
stopifnot(inherits(try(h7_branches(1.2,.5,.1,1,states[1]),silent=TRUE),'try-error'))
d <- data.table(batter=rep(1:2,each=600),batter_name=rep(c('A','B'),each=600),
  game_pk=rep(rep(1:60,each=10),2),primary_supported=TRUE,score=.01,baseline_score=.005)
r <- h7_summary(d,list(center=0,scale=1))$hitters
stopifnot(all(r$qualified),all(abs(r$decision_plus-110)<1e-10),all(abs(r$delta_plus-5)<1e-10),
  all(abs(r$sampling_low-1)<1e-10),all(abs(r$sampling_high-1)<1e-10))
d[batter==1,primary_supported:=FALSE]
r <- h7_summary(d,list(center=0,scale=1))$hitters
stopifnot(!r[batter==1,qualified],r[batter==2,qualified])
cat('6 hybrid unit checks passed\n')
