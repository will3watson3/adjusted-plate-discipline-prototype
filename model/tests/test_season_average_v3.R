source("season_average_v3.R")
setDTthreads(2);set.seed(62)
n<-12000L
d<-data.table(primary_supported=TRUE,balls=sample(0:3,n,TRUE),strikes=sample(0:2,n,TRUE),
  pitch_family=sample(c("fastball","breaking","offspeed"),n,TRUE),stand=sample(c("L","R"),n,TRUE),
  p_throws=sample(c("L","R"),n,TRUE),outs_when_up=sample(0:2,n,TRUE),
  re288_state=paste0(sample(c("000","001","010","011","100","101","110","111"),n,TRUE),"_0_0-0"),
  game_pk=sample(1:100,n,TRUE),game_date=as.Date("2024-04-01"),
  p_contact_given_swing=runif(n,.15,.9),p_in_play_given_contact=runif(n,.1,.8),
  p_swing_reference=runif(n,.1,.9),p_called_strike_if_ordinary_take=runif(n,.1,.9),q_bip=runif(n,.1,1))
d[,p_foul_tip_given_contact:=(1-p_in_play_given_contact)*.08]
d[,observed_action:=ifelse(runif(n)<p_swing_reference,"swing","take")]
d[,description:=ifelse(observed_action=="take",ifelse(runif(n)<plogis(.2+qlogis(p_called_strike_if_ordinary_take)),"called_strike","ball"),
  sample(c("hit_into_play","foul","foul_tip","swinging_strike"),n,TRUE))]
d[,factual_q:=rgamma(n,shape=2,scale=q_bip/2)]
checks<-character()
check<-function(name,condition) {stopifnot(isTRUE(condition));checks<<-c(checks,name)}
train<-d;probe<-copy(d[1:100]);probe[,game_date:=as.Date("2024-05-01")]
for(kind in SV_COMPONENTS) {
  b<-sv_fit(train,kind,"identity");base<-sv_component(probe,kind)$p
  p<-sv_predict(b,probe,base)
  check(paste(kind,"identity unchanged"),identical(p$p,base)&&all(p$calibration_link_se==0))
}
for(method in c("intercept","slope","context")) {
  b<-sv_fit(train,"take",method);p<-sv_predict(b,probe,probe$p_called_strike_if_ordinary_take)
  check(paste(method,"valid probability and cluster covariance"),all(p$p>=0&p$p<=1)&&all(is.finite(p$calibration_link_se)))
  changed<-copy(probe);changed[,description:="changed"]
  check(paste(method,"current label invariant"),identical(p,sv_predict(b,changed,probe$p_called_strike_if_ordinary_take)))
  check(paste(method,"training chronology rejected"),inherits(try(sv_predict(b,train[1:2],train$p_called_strike_if_ordinary_take[1:2]),silent=TRUE),"try-error"))
}
b<-sv_fit(train,"bip_value","intercept")
c<-sv_component(train,"bip_value");ix<-which(c$keep)
check("Multiplicative BIP ratio matches likelihood identity",abs(exp(coef(b$model)[1])-sum(c$y[ix])/sum(c$p[ix]))<1e-7)
b<-sv_fit(train,"take","slope")
pp<-sv_predict(b,probe,probe$p_called_strike_if_ordinary_take)
parts<-rbindlist(lapply(split(seq_len(nrow(probe)),ceiling(seq_len(nrow(probe))/11)),function(i)
  sv_predict(b,probe[i],probe$p_called_strike_if_ordinary_take[i])))
check("Prediction batch invariance",max(abs(pp$p-parts$p))<1e-12&&max(abs(pp$calibration_link_se-parts$calibration_link_se))<1e-12)
check("Power-calibrated BIP means stay positive",all(sv_predict(sv_fit(train,"bip_value","slope"),probe,probe$q_bip)$p>0))
print(data.table(check=checks,passed=TRUE));cat(length(checks),"calibration checks passed\n")
