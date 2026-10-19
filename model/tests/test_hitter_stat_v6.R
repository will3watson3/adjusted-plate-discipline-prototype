source('hitter_stat_v6.R')
setDTthreads(2);set.seed(19)
checks<-list();check<-function(n,x){stopifnot(isTRUE(x));checks[[length(checks)+1L]]<<-data.table(check=n,passed=TRUE)}
d<-data.table(game_year=2023L,game_date=as.IDate('2023-06-01'),valid=TRUE,take=TRUE,
  base_eta=rnorm(800),catcher=rep(letters[1:4],200),umpire=rep(LETTERS[1:5],160),
  region=rep(c('above','below','side','inside_rectangle'),each=200),y=rbinom(800,1,.5))
d[,ump_region:=paste(umpire,region,sep=':')]
b<-u5_fit(d,'umpire_region_model',2024L)
x<-copy(d[1:20]);x[,`:=`(game_year=2024L,game_date=as.IDate('2024-06-01'),batter=1L)]
w<-seq(-.5,.5,length.out=20);p<-u5_predict(b,x)
X<-predict(b$fit,transform(as.data.frame(x),base_eta=0),type='lpmatrix')
G<-colSums(X*w);v<-100*sqrt(as.numeric(t(G)%*%b$V%*%G))/20
check('joint full covariance matches native design matrix',abs(h6_joint_precision(b,x,w,1,20)-v)<1e-10)
x<-x[rep(1,20)];x[,`:=`(catcher='unknown_c',umpire='unknown_u',ump_region='unknown_u:above')]
check('opposite signed repeated-group gradients cancel',h6_joint_precision(b,x,rep(c(1,-1),10),1,20)<1e-10)
v<-h6_joint_precision(b,x,rep(1,20),1,20)
x[,`:=`(catcher=paste0('newc',1:20),umpire=paste0('newu',1:20),ump_region=paste0('newr',1:20))]
check('repeat unseen identity does not average away',h6_joint_precision(b,x,rep(1,20),1,20)<v)
x[,batter:=rep(1:2,each=10)]
check('separate hitters preserve per-hitter denominator',all(is.finite(h6_joint_precision(b,x,rep(1,20),1:2,c(10,10)))))
eta<-c(-2,0,2);s<-c(.1,.2,.3);alpha<--.3;eps<-1e-5
f<-function(e) plogis(qlogis(fr_integrate(e,s)$p)+alpha)
q<-fr_integrate(eta,s);pf<-f(eta)
deriv<-pf*(1-pf)*q$derivative/(q$p*(1-q$p))
check('integrated calibrated derivative finite difference',max(abs(deriv-(f(eta+eps)-f(eta-eps))/(2*eps)))<1e-9)
f<-data.table(game_year=2024L,game_date=as.IDate(c('2024-01-01','2024-01-08')))
l<-data.table(start=as.IDate(c('2024-01-01','2024-01-08')))
check('weekly index is date based',identical(h6_week_index(f,l),1:2))
check('constant game rates have zero block sampling error',h6_block(rep(10L,20),rep(1,20),7,B=30)[1]<1e-12)
hist<-rbindlist(rep(list(d),4));hist[,`:=`(game_year=2024L,
  game_date=as.IDate('2024-04-01')+rep(0:31,each=100),game_pk=rep(1:32,each=100))]
l<-u5_online(hist,u5_predict(b,hist)$p)$ledger
se<-h6_weekly(hist,b,l)
check('weekly sandwich uncertainty positive after sufficient history',all(se$se[se$n>=2000]>0))
hist[game_date==max(game_date),y:=1L-y]
check('current and future outcomes cannot alter weekly uncertainty',identical(se,h6_weekly(hist,b,l)))
fwrite(rbindlist(checks),file.path(H6_REPORT,'unit_verification.csv'))
cat(length(checks),'hitter integration unit checks passed\n')
