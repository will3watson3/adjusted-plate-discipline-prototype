source('experiments/hitter_region_v1/run.R')
setDTthreads(2)
set.seed(410)
# Independent lme4 Laplace likelihood comparison, no fixed intercept.
G<-60L;n<-5000L;group<-sample(sprintf('g%03d',1:G),n,TRUE);u<-rnorm(G,0,.5)
eta<-rnorm(n,-.7,.4);y<-rbinom(n,1,plogis(eta+u[match(group,sort(unique(group)))]))
b<-hr_pool(y,eta,group,'binomial')
d<-data.frame(y=y,eta=eta,group=factor(group))
l<-lme4::glmer(y~0+offset(eta)+(1|group),data=d,family=binomial(),nAGQ=1,
  control=lme4::glmerControl(optimizer='bobyqa'))
stopifnot(b$converged,abs(b$tau-as.numeric(lme4::VarCorr(l)$group)^.5)<1e-4,
  max(abs(b$effects$effect-lme4::ranef(l)$group[,1]))<1e-4)
cat('PASS binomial sparse solver vs independent lme4 Laplace fit\n')
# Independent dense mgcv quasi-Poisson RE fit, fixed phi and offset, ML.
y<-rgamma(n,shape=2,scale=exp(eta+u[match(group,sort(unique(group)))])/2)
b<-hr_pool(y,eta,group,'quasipoisson',phi=.8)
d<-data.frame(y=y,eta=eta,group=factor(group))
m<-gam(y~0+offset(eta)+s(group,bs='re'),data=d,family=quasipoisson(),method='ML',scale=.8)
tau<-sqrt(.8/as.numeric(m$sp))
cat('quasi tau sparse=',b$tau,' dense mgcv=',tau,'\n')
stopifnot(b$converged,abs(b$tau-tau)<.01,max(abs(b$effects$effect-coef(m)))<.01)
cat('PASS quasi-Poisson sparse solver vs independent dense mgcv fit\n')
# Identical evidence yields stronger shrinkage for the smaller group.
group<-rep(c('a','b','c'),c(2,200,200));y<-c(1,1,rep(1,180),rep(0,20),rep(1,20),rep(0,180));eta<-rep(0,402)
b<-hr_pool(y,eta,group,'binomial')
stopifnot(abs(b$effects[group=='a',effect])<abs(b$effects[group=='b',effect]))
stopifnot(identical(hr_region(c(1,9,11,14,10,15,NA)),c('1','9','11','14','missing_invalid','missing_invalid','missing_invalid')))
z<-data.table(batter=c(1,1,NA),zone=c(1,10,1));a<-hr_adjust(b,z,rep(0,3));stopifnot(all(a$effect==0),all(a$n==0))
cat('PASS sparse shrinkage, explicit zone validation and zero-effect fallback\n')
null<-hr_pool(rep(c(0,1),100),rep(0,200),rep(letters[1:10],each=20),'binomial')
stopifnot(null$tau==0,all(null$effects$effect==0))
cat('PASS zero-variance boundary when groups have no residual signal\n')
# Actual-history feature equivalence, including 2023 rows receiving 2022 profiles.
profiles<-setNames(lapply(2021:2023,cp_profiles),2021:2023);power<-setNames(lapply(2021:2023,hr_power),2021:2023)
original<-readRDS('data/models/post_contact/post_contact_fit_2022_2023.rds')$bip
original$stage<-'bip';d<-as.data.table(readRDS('data/processed/statcast_2023_clean.rds'))
d<-d[analysis_eligible==TRUE & ct_core_valid(d)][1:50];d[,season:=2023L]
f<-hr_features(d,original,profiles,power);expected<-cp_features(d,original$prep,profiles)
stopifnot(identical(f$batter_history,expected$batter_history),identical(f$pitcher_history,expected$pitcher_history))
pp<-original$prep;pp$profiles<-profiles[['2022']];pp$power<-power[['2022']]
stopifnot(identical(f$batter_ev_2021,pc_transform(d,pp)$data$batter_ev_2021))
power2021<-pc_power_profiles();stopifnot(isTRUE(all.equal(power2021,lapply(power[['2021']],function(x){x$history_year<-NULL;x}))))
cat('PASS chronological contact and EV history equivalence; original 2021 power definition unchanged\n')
cat('ALL TESTS PASSED\n')
