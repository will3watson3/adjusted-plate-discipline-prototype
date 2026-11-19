# Check the public leaderboard and native pitch API against the promoted outputs.
source('current_hitter_stat.R')
m <- h8_load()
for(year in 2024:2025) {
  h <- current_hitter_stat(year)
  stopifnot(all(h$qualified),all(h$statistic_version==H8_VERSION),
    !is.unsorted(-h$decision_plus))
  d <- readRDS(file.path(H8_MODELS,sprintf('decision_values_%d.rds',year)))
  raw <- t_read(year)
  k <- unique(round(seq(1,nrow(d),length.out=32)))
  x <- raw[match(dv_key(d[k]),dv_key(raw))]
  predicted <- predict_hitter_xgboost(x,m)
  for(column in c('q_bip','q_swing_basic','q_take_basic'))
    stopifnot(max(abs(predicted[[column]]-d[[column]][k]))<1e-9)
  # Supplying old BIP values along with frozen tip probabilities must still
  # use the richer value model, including for a single pitch.
  stub <- copy(x[1,..CT_KEYS])
  stub[,p_in_play_given_contact:=0.5]
  stub[,p_foul_tip_given_contact:=0.5*predicted$p_tip_given_nonbip[1]]
  stub[,q_bip:=-999]
  x[,`:=`(launch_speed=999,launch_angle=999,launch_speed_angle=6L,bb_type='ground_ball')]
  one <- predict_hitter_xgboost(x[1],m,stub)
  stopifnot(abs(one$q_bip-predicted$q_bip[1])<1e-10,abs(one$q_swing_basic-predicted$q_swing_basic[1])<1e-10)
  message('Verified v8 reader, raw predictor and cached-tip interface for ',year)
}
