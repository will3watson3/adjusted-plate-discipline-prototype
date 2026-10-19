source('umpire_v5.R')
checks<-list();check<-function(name,yes){stopifnot(isTRUE(yes));checks[[name]]<<-TRUE}
V<-matrix(c(.04,.002,.003,.001,.002,.03,.004,.002,.003,.004,.02,.003,.001,.002,.003,.01),4,4)
b<-list(fit=list(coefficients=c(.1,.2,-.3,.4)),V=V,train_end='2023-09-30',prediction_year=2024L,
 groups=list(catcher=list(table=data.table(id='c1',index=2L),tau=.2),
 umpire=list(table=data.table(id='u1',index=3L),tau=.3),ump_region=list(table=data.table(id='u1:side',index=4L),tau=.4)))
d<-data.table(base_eta=c(-.2,0),game_year=2024L,game_date=as.Date('2024-06-01'),catcher=c('c1','newc'),umpire=c('u1','newu'),ump_region=c('u1:side','newu:above'))
p<-u5_predict(b,d)
check('joint latent mean includes each effect once',abs(p$effect[1]-.4)<1e-12)
check('joint posterior covariance retained',abs(p$latent_sd[1]^2-sum(V))<1e-12)
check('unseen groups use full population variance',abs(p$latent_sd[2]^2-(V[1,1]+.2^2+.3^2+.4^2))<1e-12)
check('unseen effects centered at population mean',p$effect[2]==.1)
check('known normal integration correct',max(abs(p$p-fr_integrate(d$base_eta+p$effect,p$latent_sd)$p))<1e-12)
check('unknown IDs explicitly flagged',all(unlist(p$unknown[2,-'row'])))
z<-copy(d);z[,game_date:=as.Date('2023-09-01')]
check('prediction chronology guarded',inherits(try(u5_predict(b,z),silent=TRUE),'try-error'))
z<-copy(d);z[,description:=c('called_strike','ball')];q<-u5_predict(b,z);z[,description:=rev(description)]
check('current outcome does not enter probability',identical(q$p,u5_predict(b,z)$p))
a<-data.table(game_pk=c(1L,2L),umpire_id=c(4L,5L),umpire_name=c('A','B'),status=c('unique_game_assignment','multiple_home_plate'))
x<-data.table(game_pk=c(2L,1L,2L),region=c('above','side','below'))
j<-u5_join(x,a)
check('game join preserves pitch order',identical(j$game_pk,x$game_pk))
check('ambiguous umpire not assigned',all(is.na(j$umpire[c(1,3)])))
check('known umpire-region join',j$ump_region[2]=='4:side')
check('duplicate game identities rejected',inherits(try(u5_join(x,rbind(a,a[1])),silent=TRUE),'try-error'))
set.seed(99);n<-6000L
x<-data.table(game_year=2024L,game_date=as.Date('2024-03-01')+rep(0:59,each=100),valid=TRUE,take=TRUE,y=rbinom(n,1,.4))
p0<-runif(n,.1,.8);r<-u5_online(x,p0);z<-copy(x);z[game_date>=as.Date('2024-04-01'),y:=1-y]
r2<-u5_online(z,p0);ix<-x$game_date<as.Date('2024-04-01')
check('future outcomes do not affect earlier weekly correction',identical(r$p[ix],r2$p[ix]))
check('weekly history ends strictly before scoring',all(r$ledger$train_end<r$ledger$start,na.rm=TRUE))
br<-list(fit=list(coefficients=c(.1,.2,.3)),V=matrix(c(.04,.002,.003,.002,.03,.004,.003,.004,.02),3,3),
 fixed_indices=1:2,region_levels=c('A','B'),train_end='2023-09-30',prediction_year=2024L,
 groups=list(catcher=list(table=data.table(id='c1',index=3L),tau=.2)))
xr<-data.table(base_eta=0,game_year=2024L,game_date=as.Date('2024-06-01'),catcher='c1',region=c('A','B'))
pr<-u5_predict(br,xr)
check('shared regional fixed effect counted once',max(abs(pr$effect-c(.4,.6)))<1e-12)
check('fixed random covariance retained',abs(pr$latent_sd[2]^2-sum(br$V))<1e-12)
xr[,region:='novel']
check('unseen fixed region cannot silently drop rows',inherits(try(u5_predict(br,xr),silent=TRUE),'try-error'))
check('prediction names cannot overwrite actor identities',!any(U5_METHODS%chin%c('umpire','catcher','ump_region','region')))
vf<-list(coefficients=0,fitted.values=c(.5,.5))
check('valid nested-objective fit accepted',is.list(u5_validate_fit(vf,c(0,1),c(0,0))))
vf$coefficients<-1e15
check('false convergence cannot hide divergent coefficients',inherits(try(u5_validate_fit(vf,c(0,1),c(0,0)),silent=TRUE),'try-error'))
vf$coefficients<-0;vf$fitted.values<-c(.99,.01)
check('worse training objective rejected',inherits(try(u5_validate_fit(vf,c(0,1),c(0,0)),silent=TRUE),'try-error'))
cat(length(checks),'umpire unit checks passed\n')
