source('called_strike_v4.R')
checks<-list();check<-function(name,x){stopifnot(isTRUE(x));checks[[name]]<<-TRUE}
set.seed(42);n<-6000L
x<-data.table(game_year=2024L,game_date=as.Date('2024-03-01')+rep(0:59,each=100),
 p=runif(n,.1,.9),take=TRUE,valid=TRUE)
x[,y:=rbinom(n,1,plogis(qlogis(p)-.5))]
a<-v4_intercept(x$p,x$y)
check('intercept estimates known shift',abs(a+.5)<.1)
check('intercept score equation',abs(mean(plogis(qlogis(x$p)+a)-x$y))<1e-8)
a<-v4_online(x);b<-v4_online(x,28)
check('strict earlier dates',all(a$ledger$train_end<a$ledger$start,na.rm=TRUE))
check('finite probabilities',all(is.finite(a$p)&a$p>0&a$p<1))
check('cold start identity',max(abs(a$p[1:1000]-x$p[1:1000]))<1e-14)
z<-copy(x);z[game_date>=as.Date('2024-04-01'),y:=1-y]
c<-v4_online(z)
check('future labels do not affect earlier predictions',identical(a$p[x$game_date<as.Date('2024-04-01')],c$p[x$game_date<as.Date('2024-04-01')]))
z<-x[sample(.N)];zz<-v4_online(z)
j<-match(paste(x$game_date,x$p),paste(z$game_date,z$p))
check('prediction permutation invariance',max(abs(a$p-zz$p[j]))<1e-12)
check('recent window counts bounded',max(b$ledger$n)<=2800L)
skill<-list(mu=.1,tau=.2,mu_variance=.01,catchers=data.table(catcher='1',latent_mean=.3,latent_sd=.05))
f<-v4_framing(skill,data.table(fielder_2=c(1,2,NA)))
check('known posterior preserved',f$mu[1]==.3&&f$sd[1]==.05)
check('unknown full prior retained',all(f$mu[2:3]==.1)&&all(abs(f$sd[2:3]-sqrt(.05))<1e-12))
cat(length(checks),'unit checks passed\n')
