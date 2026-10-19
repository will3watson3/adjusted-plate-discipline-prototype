source("decision_validation.R")
setDTthreads(2)
fails <- function(x) inherits(tryCatch(force(x),error=identity),"error")
# Actual log-link runaway regression: flagged density/range/value -> missing primary.
f<-data.table(x=c(0,4.631295626),z=c(.5,-5.3606696),stand="L",family="fastball")
tab<-list(ranges=list(x=c(-2.114691,2.167270),z=c(-1.468669,2.072788)))
stopifnot(identical(vd_range_flags(f,tab),c(FALSE,TRUE)),
  identical(vd_reasons(data.table(sparse=c(FALSE,TRUE),outside=c(FALSE,TRUE))),c("","sparse;outside")),
  fails(vd_read(2025L)))
# Check all five chain-rule derivatives against numerical central differences.
eta<-c(qlogis(.76),qlogis(.47),qlogis(.06),log(.8),qlogis(.4))
calc<-function(v) {
  c<-plogis(v[1]);b<-plogis(v[2]);t<-plogis(v[3]);q<-exp(v[4]);p<-plogis(v[5])
  (1-c)*.3+c*(b*q+(1-b)*((1-t)*.5+t*.3))-((1-p)*.7+p*.3)
}
numeric<-vapply(1:5,function(j) {hi<-lo<-eta;hi[j]<-hi[j]+1e-6;lo[j]<-lo[j]-1e-6;(calc(hi)-calc(lo))/2e-6},numeric(1))
analytic<-as.numeric(vd_gap_derivatives(.76,.47,.06,.8,.5,.3,.3,.4,.7,.3))
stopifnot(max(abs(numeric-analytic))<1e-9)
# Dependence bound must be no smaller than an independence SE, including negatives.
contrib<-analytic*c(.1,.15,.2,.08,.12)
stopifnot(sum(abs(contrib))>=sqrt(sum(contrib^2)))
# Regression for predict.bam discarding Vc: an explicit corrected covariance
# four times Vp must double the returned link SE, even for a bam-class object.
set.seed(17)
toy<-data.table(x=seq(-1,1,length.out=200));toy[,y:=sin(x)+rnorm(.N,sd=.1)]
fit<-gam(y~s(x,k=5,bs="cr"),data=toy,method="REML")
fit$Vc<-4*fit$Vp;class(fit)<-unique(c("bam",class(fit)))
conditional<-mgcv::predict.gam(fit,toy[1:10],se.fit=TRUE,type="link",unconditional=FALSE)$se.fit
stopifnot(max(abs(vd_link_se(fit,toy[1:10])$se-2*conditional))<1e-10)
# Game-cluster SE uses game totals/residuals rather than treating pitches as iid.
stopifnot(vd_cluster_se(c(2,2),c(1,-1))==.5,is.na(vd_cluster_se(2,1)))
b<-vd_boot_mean(c(1,1,2,2),c(1,1,-1,-1),100L)
stopifnot(b$estimate==0,b$ci_low<=0,b$ci_high>=0)
# Regression: reference center must be created even when center_raw exists.
# Write synthetic aggregation outputs only in an isolated temporary directory.
old_report<-VD_REPORT;VD_REPORT<-file.path(tempdir(),"decision_aggregation_test")
dir.create(VD_REPORT,showWarnings=FALSE)
toy<-CJ(batter=1:24,game_pk=1:80,pitch=1:10)
toy[,`:=`(batter_name=paste0("Synthetic ",batter),primary_supported=TRUE,
  observed_action=ifelse(pitch<=2+batter%%7,"swing","take"),p_swing_reference=.5,
  swing_minus_take=.1,above_reference_model_se_bound=.001,preference="no_clear_preference",
  game_date=as.Date(ifelse(game_pk<=40,"2024-05-01","2024-08-01")))]
agg<-vd_aggregate(toy)
VD_REPORT<-old_report
stopifnot(is.finite(agg$reference[["center"]]),is.finite(agg$reference$scale),agg$reference$scale>0,
  nrow(agg$hitters)==24L,sum(agg$hitters$supported)==nrow(toy))
cat("Decision validation unit tests passed: extrapolation, lock, reasons, derivatives, dependence bound, corrected GAM covariance and game clustering.\n")
