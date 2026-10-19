source("contact_models.R")
setDTthreads(2)
fails<-function(expr) inherits(tryCatch(force(expr),error=identity),"error")
labels<-contact_labels(c(CT_SWINGS,"ball","foul_bunt","missed_bunt","automatic_strike",NA_character_))
stopifnot(identical(labels$y,c(1L,1L,1L,0L,0L,rep(NA_integer_,5))),
  identical(labels$is_foul_tip,c(FALSE,FALSE,TRUE,rep(FALSE,7))))
stopifnot(fails(ct_read(2025)),fails(ct_read(2021)))
profiles<-ct_profiles()
# Synthetic training exercises real smoothing and exact prediction without
# requiring a production fit. Outcomes depend only on location and speed.
set.seed(19); n<-3000L
d<-data.table(plate_x=runif(n,-2,2),plate_z=runif(n,.5,4.5),sz_bot=runif(n,1.3,1.7),sz_top=runif(n,3.2,3.8),
  balls=sample(0:3,n,TRUE),strikes=sample(0:2,n,TRUE),stand=sample(c("L","R"),n,TRUE),
  p_throws=sample(c("L","R"),n,TRUE),pitch_family=sample(c("fastball","breaking","offspeed"),n,TRUE),
  release_speed=rnorm(n,90,5),pfx_x=rnorm(n),pfx_z=rnorm(n),release_spin_rate=rnorm(n,2200,300),
  release_extension=rnorm(n,6,.3),outs_when_up=sample(0:2,n,TRUE),
  on_1b=sample(c(NA_integer_,1L),n,TRUE),on_2b=sample(c(NA_integer_,2L),n,TRUE),
  on_3b=sample(c(NA_integer_,3L),n,TRUE),inning=sample(1:9,n,TRUE),
  bat_score=sample(0:8,n,TRUE),fld_score=sample(0:8,n,TRUE),
  batter=sample(profiles$batter$overall$batter,n,TRUE),pitcher=sample(profiles$pitcher$overall$pitcher,n,TRUE),
  game_date=as.Date("2022-06-01"),season=2022L)
d[,y:=rbinom(.N,1,plogis(2-.7*plate_x^2-.07*(release_speed-90)))]
d[1:20,release_spin_rate:=NA_real_]
prep<-ct_preprocessor(d,profiles)
stopifnot(fails(ct_preprocessor(copy(d)[,season:=2024L],profiles)),
  fails(ct_fit(copy(d)[,season:=2024L],"gam_pooled",prep)))
fit<-ct_fit(d,"gam_history",prep)
x<-copy(d[1:40,..CT_INPUTS]); p<-predict_contact(fit,x)
stopifnot(all(is.finite(p$p_contact_given_swing)),all(p$p_contact_given_swing>=0 & p$p_contact_given_swing<=1),
  max(abs(p$p_contact_given_swing+p$p_whiff_given_swing-1))<1e-14)
# Same-pitch labels and contact quality cannot affect prediction.
polluted<-copy(x)[,`:=`(description="swinging_strike",contact=0L,y=0L,launch_speed=120,delta_run_exp=99)]
stopifnot(identical(p,predict_contact(fit,polluted)))
revp<-predict_contact(fit,x[40:1])$p_contact_given_swing
single<-vapply(1:40,function(i) predict_contact(fit,x[i])$p_contact_given_swing,numeric(1))
stopifnot(max(abs(rev(revp)-p$p_contact_given_swing))<1e-10,max(abs(single-p$p_contact_given_swing))<1e-10)
bad<-copy(x)
bad[1,plate_x:=NA_real_];bad[2,sz_top:=sz_bot];bad[3,balls:=4L]
bad[4,`:=`(batter=-1L,pitcher=-1L,pitch_family="novel")]
bad[5,release_speed:=NA_real_]
q<-predict_contact(fit,bad)
stopifnot(all(is.na(q$p_contact_given_swing[1:3])),all(q$status[1:3]=="invalid_core_inputs"),
  q$unseen_batter_2021[4],q$unseen_pitcher_2021[4],q$status[4]=="unseen_category_fallback",
  is.finite(q$p_contact_given_swing[4]),q$secondary_imputed[5],is.finite(q$p_contact_given_swing[5]))
unknown<-ct_raw_features(bad[4],profiles)
stopifnot(abs(unknown$batter_history-qlogis(profiles$batter$league))<1e-12)
stopifnot(nrow(predict_contact(fit,x[0]))==0L)
temp<-tempfile(fileext=".rds");saveRDS(fit,temp)
stopifnot(identical(p,predict_contact(readRDS(temp),x)));unlink(temp)
logistic<-ct_fit(d,"logistic",prep)
stopifnot(all(is.finite(predict_contact(logistic,x)$p_contact_given_swing)))
diagnostics<-ct_diagnostics(d[1:40],p)
stopifnot(setequal(unique(diagnostics$dimension),c("count","pitch_family","stand","pitcher_hand",
  "month","zone","batter_history","pitcher_history","location_support")),
  all(diagnostics[,.(total=sum(n)),by=dimension]$total==40L))
cat("All contact model checks passed: outcome mapping, temporal guards, leakage, missing inputs, unseen players, exact batch/order invariance, serialization.\n")
