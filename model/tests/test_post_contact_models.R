# The contact tests supply a synthetic feature dataset and verify the shared
# transformer. These checks add branching, run targets, and value integration.
source("tests/test_contact_models.R")
source("post_contact_models.R")
setDTthreads(2)
re<-read_re_baseline();s<-pc_branch_states(re)
stopifnot(nrow(s)==288L,all(s$q_foul_tip==s$q_whiff))
two<-grepl("-2$",s$re288_state)
stopifnot(all(s$ordinary_foul_state[two]==s$re288_state[two]),
  all(s$q_ordinary_foul[two]==s$re_before[two]),
  all(s$q_foul_tip[grepl("_2_[0-3]-2$",s$re288_state)]==0))
stopifnot(all(s$q_ordinary_foul[!two]==s$q_foul_tip[!two]))

# Independently specified HR, inning-ending DP, single, later action, walkoff,
# impossible conservation, wrong batter, and wrong run-score examples.
raw<-data.table(game_pk=1:8,at_bat_number=1,pitch_number=1,description="hit_into_play",
  batter=99L,inning=1,inning_topbot="Bot",season=2022L,balls=0L,strikes=0L,
  outs_when_up=c(1,1,0,0,0,0,0,0),on_1b=c(1,1,NA,1,NA,1,NA,1),
  on_2b=c(2,rep(NA,7)),on_3b=c(3,rep(NA,7)),bat_score=0,
  post_bat_score=c(4,0,0,0,0,0,0,1),events=c("home_run","grounded_into_double_play",rep("single",6)))
raw[,re288_state:=pc_state(raw)]
api<-data.table(game_pk=1:8,at_bat_number=1,pitch_number=1,season=2022L,
  api_batter=c(rep(99L,6),100L,99L),api_inning=1,api_half="bottom",api_event=raw$events,
  api_outs_after=c(1,3,0,0,0,0,0,0),api_on_1b_after=c(NA,NA,99,99,99,NA,99,99),
  api_on_2b_after=c(rep(NA,3),1,rep(NA,4)),api_on_3b_after=NA_integer_,
  api_post_bat_score=raw$post_bat_score,api_scoring_runners=c(4,rep(0,7)),
  api_later_runner_moves=c(0,0,0,1,0,0,0,0),api_incomplete_final_half=c(0,0,0,0,1,0,0,0))
z<-pc_bip_targets(raw,api,re)
stopifnot(all(z$target_status[1:3]=="verified"),z$state_after[1]=="000_1_0-0",
  abs(z$q_bip_observed[1]-(4+re$re288[re288_state=="000_1_0-0",re288]))<1e-12,
  z$state_after[2]=="INNING_END",z$q_bip_observed[2]==0,z$bip_run_value_observed[2]==-z$re_before[2],
  identical(z$target_status[4:8],c("later_runner_movement","incomplete_final_half",
    "runner_conservation_mismatch","identity_mismatch","scoring_runner_mismatch")),all(is.na(z$q_bip_observed[4:8])))
stopifnot(pc_bip_targets(raw[1],api[0],re)$target_status=="missing_api_pitch")

set.seed(101)
d[,description:=sample(c("hit_into_play","foul","foul_tip"),.N,TRUE,c(.47,.48,.05))]
d[,`:=`(contact_class=fcase(description=="hit_into_play","in_play",description=="foul","ordinary_foul",default="foul_tip"),
  target_status="verified",q_bip_observed=rgamma(.N,shape=1,rate=2))]
prep<-pc_prep(d)
post<-list(bip=pc_fit(d,"bip","gam",prep),tip=pc_fit(d,"tip","gam",prep))
value<-pc_fit(d,"value","gam",prep)
sc<-predict_post_contact(post,d[1:40]);stopifnot(max(abs(rowSums(pc_matrix(sc))-1))<1e-12)
poison<-copy(d[1:40])[,`:=`(description="foul_tip",events="home_run",launch_speed=120,
  launch_angle=30,post_bat_score=50,q_bip_observed=100)]
stopifnot(identical(sc,predict_post_contact(post,poison)),
  identical(predict_bip_value(value,d[1:40]),predict_bip_value(value,poison)))
stopifnot(max(abs(pc_matrix(sc)[40:1,]-pc_matrix(predict_post_contact(post,d[40:1]))))<1e-10)
# `fit` from the shared tests is a valid synthetic contact model.
combined<-predict_swing_value(fit,post,value,d[1:40])
stopifnot(max(abs(combined$p_bip_given_swing+combined$p_foul_given_swing+
  combined$p_tip_given_swing+combined$p_whiff_given_swing-1))<1e-12,
  max(abs(combined$swing_run_value_basic-(combined$q_swing_basic-combined$re_before)))<1e-12,
  all(combined$q_bip>=0),all(is.finite(combined$q_swing_basic)))
invalid<-copy(d[1:2]);invalid[1,plate_x:=NA_real_];invalid[2,outs_when_up:=4L]
stopifnot(all(is.na(predict_swing_value(fit,post,value,invalid)$q_swing_basic)))
stopifnot(fails(pc_read(2025)),fails(pc_fit(copy(d)[,season:=2024L],"bip","gam",prep)))
temp<-tempfile();saveRDS(post,temp);stopifnot(identical(sc,predict_post_contact(readRDS(temp),d[1:40])));unlink(temp)
cat("All post-contact checks passed: separate tips/fouls, post-play states, excluded ambiguities, leakage, coherent probabilities, and expected-value integration.\n")
