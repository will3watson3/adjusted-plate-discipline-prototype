# Reuse read-only definitions; redirect all inherited output paths immediately.
source('experiments/hitter_region_v1/run.R')
HS_ROOT <- 'experiments/hitter_surface_v1'
HR_ROOT <- HS_ROOT
HR_REPORT <- file.path(HS_ROOT,'reports')
HR_MODELS <- file.path(HS_ROOT,'models')
HS_OLD <- 'experiments/hitter_region_v1/models'
HS_ARMS <- c('control','regional','continuous')
setDTthreads(2)
hs_raw_basis <- function(x,z) cbind(x,z,x*x,x*z,z*z)
hs_basis <- function(x,z,prep=NULL) {
  if(is.null(prep)) {
    prep<-list(center=c(mean(x),mean(z)),scale=c(sd(x),sd(z)))
    a<-hs_raw_basis((x-prep$center[1])/prep$scale[1],(z-prep$center[2])/prep$scale[2])
    prep$mean<-colMeans(a);a<-sweep(a,2,prep$mean)
    prep$rotation<-solve(chol(crossprod(a)/nrow(a)))
  }
  a<-hs_raw_basis((x-prep$center[1])/prep$scale[1],(z-prep$center[2])/prep$scale[2])
  X<-cbind(1,sweep(a,2,prep$mean)%*%prep$rotation)
  colnames(X)<-paste0('b',0:5)
  list(X=X,prep=prep)
}
# Established lme4 Laplace objective, constrained to one spherical covariance.
hs_pool <- function(y,eta,X,hitter) {
  stopifnot(all(is.finite(X)),all(is.finite(eta)),!anyNA(hitter))
  dat<-data.frame(y=y,eta=eta,hitter=factor(hitter),X,check.names=FALSE)
  form<-as.formula(paste('y ~ 0 + offset(eta) + (0 +',paste(colnames(X),collapse='+'),'| hitter)'))
  ctrl<-lme4::glmerControl(tolPwrss=1e-9,calc.derivs=FALSE)
  mod<-lme4::glFormula(form,data=dat,family=binomial(),control=ctrl)
  stopifnot(ncol(mod$X)==0)
  args<-mod;args$formula<-NULL
  dev<-do.call(lme4::mkGlmerDevfun,c(args,list(nAGQ=1L,control=ctrl)))
  diagonal<-which(mod$reTrms$lower==0)
  theta<-function(tau){a<-numeric(length(mod$reTrms$theta));a[diagonal]<-tau;a}
  objective<-function(tau)dev(theta(tau))
  opt<-optimize(function(t)objective(exp(t)),log(c(1e-4,4)),tol=1e-6)
  zero<-objective(0);tau<-if(zero<=opt$objective)0 else exp(opt$minimum)
  stopifnot(tau<3.99)
  value<-objective(tau)
  fit<-lme4::mkMerMod(environment(dev),list(par=theta(tau),fval=value,conv=0L),mod$reTrms,fr=mod$fr)
  effects<-as.matrix(lme4::ranef(fit)$hitter)
  # Independently inspect penalized mode equations and conditional effective df.
  k<-match(as.character(hitter),rownames(effects));p<-plogis(eta+rowSums(X*effects[k,,drop=FALSE]))
  groups<-split(seq_along(y),k);diag<-rbindlist(lapply(seq_len(nrow(effects)),function(j){
    i<-groups[[as.character(j)]];xx<-X[i,,drop=FALSE];H<-crossprod(xx,xx*(p[i]*(1-p[i])))
    grad<-if(tau==0)0 else max(abs(crossprod(xx,y[i]-p[i])-effects[j,]/tau^2))
    edf<-if(tau==0)0 else sum(diag(solve(H+diag(1/tau^2,ncol(X)),H)))
    data.table(hitter=rownames(effects)[j],n=length(i),edf=edf,max_gradient=grad)
  }))
  stopifnot(max(diag$max_gradient)<.01)
  list(effects=effects,tau=tau,variance=tau^2,deviance=value,zero_deviance=zero,
    diagnostics=diag,edf=sum(diag$edf),max_gradient=max(diag$max_gradient),converged=TRUE,
    sparse_design_bytes=as.numeric(object.size(mod$reTrms$Zt)),coefficient_count=length(effects),
    method='lme4_sparse_Laplace_spherical_random_coefficients',tolPwrss=1e-9)
}
hs_adjust <- function(bundle,d,f) {
  k<-match(as.character(d$batter),rownames(bundle$surface$effects))
  valid<-is.finite(f$x)&is.finite(f$z)
  if(all(c('sz_top','sz_bot')%in%names(d)))valid<-valid&is.finite(d$sz_top-d$sz_bot)&d$sz_top>d$sz_bot
  effect<-numeric(nrow(d));i<-which(valid&!is.na(k))
  if(length(i))effect[i]<-rowSums(hs_basis(f$x[i],f$z[i],bundle$basis)$X*bundle$surface$effects[k[i],,drop=FALSE])
  n<-bundle$surface$diagnostics$n[match(as.character(d$batter),bundle$surface$diagnostics$hitter)];n[is.na(n)]<-0L
  list(effect=effect,n=n,fallback=ifelse(!valid,'invalid_location',ifelse(is.na(d$batter),'missing_hitter',ifelse(is.na(k),'unseen_hitter','seen'))))
}
hs_predict <- function(bundle,d,histories) {
  f<-hr_features(d,bundle,histories$contact,histories$power);eta<-hr_eta(bundle$model,f)
  cat<-hr_adjust(bundle$pool,d,eta);smooth<-hs_adjust(bundle,d,f)
  list(control=plogis(eta),regional=plogis(cat$eta),continuous=plogis(eta+smooth$effect),
    features=f,categorical=cat,smooth=smooth)
}
hs_band <- function(n)as.character(cut(n,c(-1,0,9,49,199,Inf),labels=c('0','1-9','10-49','50-199','200+')))
