source("take_value_models.R")
states<-build_take_state_values()
fails<-function(x)inherits(tryCatch({force(x);NULL},error=identity),"error")
stopifnot(nrow(states)==288L,uniqueN(states$re288_state)==288L,
  all(states$strike_re_after[states$strike_state=="INNING_END"]==0),
  sum(states$strike_state=="INNING_END")==32L)
# Independent explicit walk mapping, all 8 base patterns and all outs/strike counts.
walk_map<-c("000"="100","001"="101","010"="110","011"="111","100"="110","101"="111","110"="111","111"="111")
w<-states[balls==3L]
stopifnot(identical(w$ball_bases,unname(walk_map[w$base_state])),
  all(w$ball_runs==as.integer(w$base_state=="111")),all(w$ball_balls==0),all(w$ball_strikes==0),all(w$ball_outs==w$outs))
# Exhaustively check every nonterminal branch and all ordinary Ks.
b<-states[balls<3L];k<-states[strikes==2L];s<-states[strikes<2L]
stopifnot(all(b$ball_bases==b$base_state),all(b$ball_outs==b$outs),all(b$ball_balls==b$balls+1),all(b$ball_strikes==b$strikes),all(b$ball_runs==0),
  all(s$strike_bases==s$base_state),all(s$strike_outs==s$outs),all(s$strike_balls==s$balls),all(s$strike_strikes==s$strikes+1),
  all(k$strike_outs==k$outs+1),all(k$strike_balls==0),all(k$strike_strikes==0),all(k$strike_bases==k$base_state))
# Probabilities 0 and 1 reproduce exact branches, .5 averages, subtract once.
for(p in c(0,.5,1)) {
  z<-combine_take_values(states$re288_state,rep(p,288),states)
  stopifnot(max(abs(z$q_take_basic-((1-p)*states$ball_value+p*states$called_strike_value)))<1e-12,
    max(abs(z$take_run_value_basic-((1-p)*states$ball_run_value+p*states$called_strike_run_value)))<1e-12)
}
z<-combine_take_values(c("111_2_3-2","000_2_0-2","bad"),c(.25,1,.4),states)
stopifnot(z$ball_runs[1]==1,z$called_strike_value[1]==0,z$take_run_value_basic[2]==-z$re_before[2],is.na(z$q_take_basic[3]))
stopifnot(is.na(combine_take_values("000_0_0-0",NA_real_,states)$q_take_basic),
  fails(combine_take_values("000_0_0-0",1.1,states)),fails(combine_take_values("000_0_0-0",Inf,states)),
  fails(combine_take_values("000_0_0-0",c(.1,.2),states)),fails(run_take_values(2025L)))
# Order invariance and unchanged caller input.
set.seed(100);i<-sample.int(288);pp<-runif(288)
a<-combine_take_values(states$re288_state,pp,states);bb<-combine_take_values(states$re288_state[i],pp[i],states)
stopifnot(identical(a$q_take_basic[i],bb$q_take_basic))
# Synthetic full sequence: walk, strikeout, ordinary count change, extra advancement,
# and inning-ending K. Keep exceptional rows in place when finding next states.
raw<-data.table(game_pk=rep(1:5,each=2),at_bat_number=c(1,2,1,2,1,1,1,1,1,2),
  pitch_number=c(4,1,3,1,1,2,1,2,3,1),inning=c(rep(1,9),2),inning_topbot="Top",
  balls=c(3,0,0,0,0,1,0,1,0,0),strikes=c(0,0,2,0,0,0,0,0,2,0),
  outs_when_up=c(0,0,0,1,0,0,0,0,2,0),description=rep(c("ball","hit_into_play"),5),
  events=NA_character_,bat_score=0,post_bat_score=0)
raw[c(3,9),description:="called_strike"]
joined<-data.table(re288_state=c("111_0_3-0","111_0_0-0","100_0_0-2","100_1_0-0", "000_0_0-0","000_0_1-0", "100_0_0-0","010_0_1-0", "000_2_0-2","000_0_0-0"))
raw[1,post_bat_score:=1]
audit<-audit_take_transitions(raw,joined,states,2024L)
stopifnot(all(audit[game_pk %in% c(1,2,3,5),audit_status]=="matches_basic_transition"),
  audit[game_pk==4,audit_status]=="different_state_or_runner_action")
# No later record means transition cannot be independently checked.
audit2<-audit_take_transitions(raw[1],joined[1],states,2024L)
stopifnot(audit2$audit_status=="no_next_record_unverified")
cat("All basic take-value tests passed.\n")
print(states[state_value_order_warning==TRUE,.(re288_state,ball_run_value,called_strike_run_value,ball_minus_strike)])
