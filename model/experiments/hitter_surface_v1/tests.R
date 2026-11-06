source('experiments/hitter_surface_v1/surface.R')
set.seed(301)
n<-2400;h<-rep(1:40,each=60);eta<-rnorm(n,-.3,.5);X<-matrix(1,n,1,dimnames=list(NULL,'b0'))
y<-rbinom(n,1,plogis(eta+rnorm(40,0,.5)[h]));a<-hs_pool(y,eta,X,h)
b<-lme4::glmer(y~0+offset(eta)+(1|h),data=data.frame(y,eta,h=factor(h)),family=binomial(),nAGQ=1,
 control=lme4::glmerControl(optimizer='bobyqa',tolPwrss=1e-9))
stopifnot(abs(a$tau-sqrt(as.numeric(lme4::VarCorr(b)$h)))<1e-4,max(abs(a$effects[,1]-lme4::ranef(b)$h[,1]))<1e-4)
cat('PASS scalar constrained modular fit vs ordinary glmer\n')
# Independent dense six-dimensional group Laplace objective (synthetic only).
n<-1800;h<-rep(1:30,each=60);eta<-rnorm(n,-.2,.3);basis<-hs_basis(rnorm(n),runif(n));X<-basis$X
u<-matrix(rnorm(30*6,sd=.25),30,6);y<-rbinom(n,1,plogis(eta+rowSums(X*u[h,])))
a<-hs_pool(y,eta,X,h)
dense<-function(tau,details=FALSE){
 U<-matrix(0,30,6);v<-0
 for(j in 1:30){i<-which(h==j);xx<-X[i,];ee<-eta[i];yy<-y[i];u<-rep(0,6)
  for(it in 1:100){p<-plogis(ee+xx%*%u);H<-crossprod(xx,xx*as.vector(p*(1-p)))+diag(1/tau^2,6)
   step<-solve(H,crossprod(xx,yy-p)-u/tau^2);u<-u+step;if(max(abs(step))<1e-10)break}
  e<-as.vector(ee+xx%*%u);p<-plogis(e);H<-crossprod(xx,xx*(p*(1-p)))
  v<-v-sum(yy*e-pmax(e,0)-log1p(exp(-abs(e))))+sum(u*u)/(2*tau^2)+as.numeric(determinant(diag(6)+tau^2*H,logarithm=TRUE)$modulus)/2
  U[j,]<-u
 };if(details)list(value=2*v,effects=U) else 2*v
}
o<-optimize(function(t)dense(exp(t)),log(c(.0001,4)),tol=1e-6);v<-dense(a$tau,TRUE)
cat('Independent Laplace absolute deviance difference:',abs(v$value-a$deviance),' mode difference:',max(abs(v$effects[match(rownames(a$effects),as.character(1:30)),]-a$effects)),'\n')
stopifnot(abs(exp(o$minimum)-a$tau)<1e-4,abs(v$value-a$deviance)<1e-4,
 max(abs(v$effects[match(rownames(a$effects),as.character(1:30)),]-a$effects))<1e-5,
 max(abs(crossprod(X)/n-diag(6)))<1e-10,max(abs(colMeans(X[,-1])))<1e-10)
cat('PASS six-coefficient spherical Laplace objective/modes vs independent dense calculation; centered orthonormal basis\n')
# Identical location/outcome ratios, sparse versus dense hitter; all directions penalized.
h<-rep(1:3,c(2,200,200));X<-matrix(1,402,1,dimnames=list(NULL,'b0'))
y<-c(1,1,rep(1,180),rep(0,20),rep(1,20),rep(0,180));a<-hs_pool(y,rep(0,402),X,h)
stopifnot(abs(a$effects['1',1])<abs(a$effects['2',1]),a$diagnostics[hitter=='1',edf]<a$diagnostics[hitter=='2',edf])
cat('PASS stronger shrinkage and lower effective complexity for sparse evidence\n')
bundle<-list(surface=list(effects=matrix(.1,1,6,dimnames=list('1',paste0('b',0:5))),diagnostics=data.table(hitter='1',n=100)),basis=basis$prep)
d<-data.table(batter=c(1,999,NA,1),sz_top=3,sz_bot=1);f<-data.table(x=c(0,0,0,NA),z=.5)
r<-hs_adjust(bundle,d,f);stopifnot(all(r$effect[2:4]==0),r$fallback[4]=='invalid_location')
# No jumps at approximate old grid edges: perturb coordinates; zone labels are irrelevant.
d<-data.table(batter=rep(1,12));f<-CJ(x=c(-.83,-.28,.28,.83),z=c(0,1/3,2/3))
a<-hs_adjust(bundle,d,f)$effect;ff<-copy(f);ff[,x:=x+1e-8];b<-hs_adjust(bundle,d,ff)$effect
stopifnot(max(abs(a-b))<1e-6)
cat('PASS unseen/missing hitter, invalid location fallback and smooth boundary behavior\n')
# Same evidence ratio at the same location in the full six-term surface.
h<-rep(1:3,c(2,200,200));y<-c(1,1,rep(1,180),rep(0,20),rep(1,20),rep(0,180))
X<-matrix(rep(basis$X[1,],each=402),402,6,dimnames=list(NULL,paste0('b',0:5)))
a<-hs_pool(y,rep(0,402),X,h)
effect<-as.vector(a$effects%*%basis$X[1,])
stopifnot(abs(effect[match('1',rownames(a$effects))])<abs(effect[match('2',rownames(a$effects))]))
null<-hs_pool(rep(c(0,1),100),rep(0,200),matrix(1,200,1,dimnames=list(NULL,'b0')),rep(1:10,each=20))
stopifnot(null$tau==0,all(null$effects==0))
cat('PASS full-surface sparse shrinkage and exact zero-variance boundary\n')
cat('ALL SYNTHETIC TESTS PASSED\n')
