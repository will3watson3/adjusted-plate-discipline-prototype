source("catcher_framing_v2.R")
setDTthreads(2)
checks<-character()
check<-function(name,condition) {stopifnot(isTRUE(condition));checks<<-c(checks,name)}
q<-fr_quadrature()
check("Normal quadrature moments",abs(sum(q$weights)-1)<1e-12 &&
  abs(sum(q$weights*q$nodes))<1e-12 && abs(sum(q$weights*q$nodes^2)-1)<1e-12)
z<-fr_integrate(c(-2,0,2),c(0,0,0))
check("Zero variance integration",max(abs(z$p-plogis(c(-2,0,2))))<1e-12)
z<-fr_integrate(c(-2,0,2),c(.4,.4,.4))
check("Population integration symmetry",abs(z$p[2]-.5)<1e-12 && abs(sum(z$p[c(1,3)])-1)<1e-12)
h<-1e-5
finite<-(fr_integrate(c(-2,0,2)+h,rep(.4,3))$p-fr_integrate(c(-2,0,2)-h,rep(.4,3))$p)/(2*h)
check("Integrated probability derivative",max(abs(z$derivative-finite))<1e-9)

# Known generating model: two catchers have the same raw rate but very different
# sample sizes. Partial pooling should shrink the sparse catcher more strongly.
set.seed(91)
ids<-rep(as.character(1:30),each=500)
u<-rnorm(30,0,.35);eta<-rnorm(length(ids),0,1.2)
f<-data.table(y=rbinom(length(ids),1,plogis(eta+u[as.integer(ids)])),
  eta=eta,catcher=factor(ids))
fit<-bam(y~offset(eta)+s(catcher,bs="re"),data=f,family=binomial(),
  method="fREML",discrete=TRUE,nthreads=2)
vc<-gam.vcomp(fit)
sd<-if(is.matrix(vc)) unname(vc["s(catcher)","std.dev"]) else unname(vc["s(catcher)"])
check("Recover shared population variation",is.finite(sd)&&sd>.15&&sd<.65)
pool<-rbindlist(list(f,data.table(y=c(rep(1,7),rep(0,3)),eta=0,catcher="sparse"),
  data.table(y=c(rep(1,700),rep(0,300)),eta=0,catcher="rich")))
pool[,catcher:=factor(as.character(catcher))]
pooled<-bam(y~offset(eta)+s(catcher,bs="re"),data=pool,family=binomial(),
  method="fREML",discrete=TRUE,nthreads=2)
pr<-predict(pooled,data.table(eta=0,catcher=factor(c("sparse","rich"),levels=levels(pool$catcher))),
  type="link",se.fit=TRUE)
check("Sparse evidence shrinks more",abs(pr$fit[1]-coef(pooled)[1])<abs(pr$fit[2]-coef(pooled)[1]))
check("Sparse evidence retains more uncertainty",pr$se.fit[1]>pr$se.fit[2])

# Exercise real feature transforms, chronology and posterior inference with a
# small real-data fixture; only 2022 evidence is used for 2023 prediction.
base<-readRDS("data/models/called_strike/candidate_gam_stronger_penalty.rds")
raw<-fr_read(2022L)
history<-raw[cs_eligible(raw)&!is.na(fielder_2)]
counts<-history[,.N,by=fielder_2][order(-N)]
history<-history[fielder_2 %in% counts$fielder_2[1:12]][seq_len(min(.N,16000L))]
skill<-fr_fit(history,base,2023L)
check("Skill only uses previous season",skill$prior_season==2022L&&skill$prediction_year==2023L)
check("Finite posterior skill",all(is.finite(skill$catchers$latent_sd))&&all(skill$catchers$latent_sd>0))
probe<-copy(history[1:20]);probe[,`:=`(game_year=2023L,game_date=as.Date("2023-04-01"))]
probe[1, fielder_2:=99999999]
p<-fr_predict(skill,base,probe)
check("Unseen catcher gets full population variance",p$catcher_evidence[1]=="population_prior_no_history"&&
  abs(p$framing_latent_sd[1]^2-(skill$tau^2+skill$mu_variance))<1e-12)
changed<-copy(probe);changed[,description:="anything"]
check("Current label invariance",identical(p,fr_predict(skill,base,changed)))
batched<-rbindlist(lapply(split(seq_len(nrow(probe)),rep(1:2,length.out=nrow(probe))),
  function(i) fr_predict(skill,base,probe[i])))
batched<-batched[match(dv_key(probe),dv_key(batched))]
check("Prediction batch invariance",max(abs(p$p_called_strike-batched$p_called_strike))<1e-12)
check("Probabilities and uncertainty finite",all(is.finite(p$p_called_strike))&&
  all(p$p_called_strike>=0&p$p_called_strike<=1)&&all(p$probability_model_se_bound>=0))
bad<-try(fr_fit(history,base,2024L),silent=TRUE)
check("Wrong prior season rejected",inherits(bad,"try-error"))
bad<-copy(probe);bad[,game_year:=2022L]
check("Wrong scoring year rejected",inherits(try(fr_predict(skill,base,bad),silent=TRUE),"try-error"))

# Ensure removing s(catcher) makes the base invariant to catcher identity.
changed<-copy(probe);changed[,fielder_2:=counts$fielder_2[2]]
check("No original catcher effect remains",max(abs(fr_base(base,probe)$eta-fr_base(base,changed)$eta))<1e-12)
print(data.table(check=checks,passed=TRUE))
cat(length(checks),"checks passed\n")
