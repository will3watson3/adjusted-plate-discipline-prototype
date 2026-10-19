source("called_strike_models.R")
set.seed(4123)
n <- 3000L
d <- data.table(game_pk=rep(1:100,each=30),at_bat_number=seq_len(n),pitch_number=1L,
  game_date=as.IDate("2022-05-01")+rep(0:99,each=30),game_year=2022L,season=2022L,
  batter=1L,pitcher=2L,plate_x=rnorm(n,0,.8),sz_bot=1.5,sz_top=3.5,
  balls=sample(0:3,n,TRUE),strikes=sample(0:2,n,TRUE),
  stand=sample(c("L","R"),n,TRUE),p_throws=sample(c("L","R"),n,TRUE),
  pitch_family=sample(c("fastball","breaking","offspeed"),n,TRUE),
  fielder_2=sample(100:109,n,TRUE),release_speed=rnorm(n,91,4),pfx_x=rnorm(n),pfx_z=rnorm(n),
  outs_when_up=sample(0:2,n,TRUE),on_1b=sample(c(NA,20L),n,TRUE),
  on_2b=sample(c(NA,21L),n,TRUE),on_3b=sample(c(NA,22L),n,TRUE),
  inning=sample(1:9,n,TRUE),bat_score=sample(0:5,n,TRUE),fld_score=sample(0:5,n,TRUE),
  prev_plate_x=rnorm(n),prev_plate_z=rnorm(n,2.5,.5),
  prev_description=sample(c(NA,"ball","called_strike","foul"),n,TRUE),
  previous_pitch_missing=FALSE,is_first_pitch=FALSE,analysis_eligible=TRUE)
d[,sz_top:=rnorm(.N,3.5,.12)]
d[,plate_z:=1.5+2*runif(.N,-.3,1.3)]
d[,y:=rbinom(.N,1,plogis(2.5-5*plate_x^2-7*((plate_z-1.5)/2-.5)^2))]
d[,description:=ifelse(y==1,"called_strike","ball")]
d[is.na(prev_description),`:=`(prev_plate_x=NA_real_,prev_plate_z=NA_real_,is_first_pitch=TRUE)]
stopifnot(all(cs_eligible(d)))
labels<-copy(d[1:6])
labels[,description:=c("called_strike","ball","blocked_ball","swinging_strike","hit_by_pitch","automatic_strike")]
stopifnot(identical(cs_eligible(labels),c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)))

# Preprocessing learns medians and levels only from its training argument.
prep<-cs_preprocessor(d)
future<-copy(d[1:20]); future[,release_speed:=NA_real_]
stopifnot(all(cs_transform(future,prep)$data$speed==median(d$release_speed)))
stopifnot(!any(c("description","y","re288_before","events") %in% names(cs_transform(d,prep)$data)))
specs<-cs_candidates()
fails<-function(expr) inherits(tryCatch({force(expr);NULL},error=identity),"error")
bad<-copy(specs[1]);bad[,rhs:="description + re288_before"]
stopifnot(fails(cs_formula(bad,prep)),fails(cs_read(2025L)))

for(candidate_id in c("logit_quadratic","logit_quartic_pitch","gam_pitch_height_catcher")) {
  message("Testing fit and prediction: ",candidate_id)
  spec<-specs[id==candidate_id]
  b<-cs_fit(d,spec,prep)
  p<-predict_called_strike(b,d)
  stopifnot(all(is.finite(p$p_called_strike)),all(p$p_called_strike>=0 & p$p_called_strike<=1))
  # GAM training uses discretization; serving must evaluate its exact basis.
  reference_p <- if(inherits(b$model,"bam")) plogis(as.numeric(
    mgcv::predict.gam(b$model,cs_transform(d,prep)$data,type="lpmatrix") %*% coef(b$model))) else as.numeric(fitted(b$model))
  stopifnot(max(abs(p$p_called_strike-reference_p))<1e-6)
  if(inherits(b$model,"cs_ridge")) {
    f<-cs_transform(d,prep)$data
    x<-model.matrix(delete.response(b$model$terms),data=f)[,-1,drop=FALSE]
    reference<-glmnet::glmnet(x,d$y,family="binomial",alpha=0,lambda=c(.01,.001,.0001),thresh=1e-12,maxit=100000)
    expected<-as.numeric(predict(reference,newx=x,s=.0001,type="response"))
    stopifnot(reference$jerr==0,max(abs(expected-p$p_called_strike))<1e-4,
      b$model$fit$max_abs_gradient<1e-8,all(diff(b$model$fit$objective_history)<=1e-12))
  }
  changed<-copy(d); changed[,`:=`(description="hit_into_play",y=1L-y,events="home_run",re288_before=99)]
  stopifnot(identical(p,predict_called_strike(b,changed)))
  idx<-sample.int(n)
  stopifnot(max(abs(p$p_called_strike[idx]-predict_called_strike(b,d[idx])$p_called_strike))<1e-10)
  subset_idx<-unique(round(seq(1,n,length.out=37)))
  stopifnot(max(abs(p$p_called_strike[subset_idx]-predict_called_strike(b,d[subset_idx])$p_called_strike))<1e-10)
  bad_inputs<-copy(d[1:3]);bad_inputs[,plate_x:=NA_real_]
  invalid<-predict_called_strike(b,bad_inputs)
  stopifnot(all(is.na(invalid$p_called_strike)),all(invalid$status=="invalid_core_inputs"))
  unseen<-copy(d[1:10]);unseen[,fielder_2:=999999L]
  unknown<-predict_called_strike(b,unseen)
  stopifnot(all(unknown$unknown_catcher),all(is.finite(unknown$p_called_strike)))
  if(spec$family=="gam") {
    f<-cs_transform(unseen,prep)$data
    expected<-plogis(as.numeric(predict(b$model,f,type="link",exclude="s(catcher)",discrete=FALSE)))
    stopifnot(max(abs(unknown$p_called_strike-expected))<1e-12)
  }
  stopifnot(nrow(predict_called_strike(b,d[0]))==0)
  path<-tempfile(fileext=".rds");cs_save(b,path)
  stopifnot(identical(p,predict_called_strike(readRDS(path),d)))
  unlink(path)
}

stopifnot(abs(mean(cs_loss(c(0,1),c(.25,.75)))+log(.75))<1e-12,
  abs(cs_cluster_se(1:4,c(1,2,3,4))-sd(1:4)/2)<1e-12,
  fails(cs_loss(c(0,1),c(-.1,.9))))
# This sample size reproduces the original integer-overflow risk in AUC.
large_y<-rep(c(0L,0L,1L,1L),50000)
large_p<-rep(c(.1,.6,.4,.9),50000)
stopifnot(abs(cs_metrics(large_y,large_p)$auc-.75)<1e-12)
# Selection must prefer a simpler tied candidate and reject failed guardrails.
mock<-CJ(id=c("small","large"),split=c("training_2022","validation_2023"))
mock[,`:=`(family="gam",log_loss=.3,brier=.1,bias=0,calibration_slope=1,converged=TRUE,
  effective_df=ifelse(id=="small",10,100))]
pp<-rep(.4,n)
pick<-cs_select(mock,list(small=pp,large=pp),d,"gam")
stopifnot(pick$id=="small")
mock[,calibration_slope:=.5]
stopifnot(fails(cs_select(mock,list(small=pp,large=pp),d,"gam")))
# Game bootstrap uses paired predictions, so identical models have zero differences.
boot<-cs_bootstrap(d,pp,pp,B=20)
stopifnot(all(boot[grepl("minus",metric),estimate]==0),all(boot[grepl("minus",metric),ci_high]==0))
# Report labels must not resolve to the column named n inside data.table.
slices<-cs_slices(d,pp,"gam",predict_called_strike(b,d))
stopifnot(setequal(unique(slices$dimension),c("count","stand","pitch_family","month","zone_region","catcher_known","location_support")),
  identical(unique(slices$model),"gam"))
metrics<-data.table(model=c("gam","logistic"),brier=c(.1,.2),bias=0,calibration_slope=1,loss_gap=.001)
boot[metric=="gam_minus_logit_loss",ci_high:= -.01]
gates<-cs_confirmation_gates(metrics,boot,data.table(material_regression=FALSE))
stopifnot(nrow(gates)==6L,all(gates$passed))
metrics[model=="gam",calibration_slope:=.5]
stopifnot(!cs_confirmation_gates(metrics,boot,data.table(material_regression=FALSE))[check=="gam_calibration_slope_0.8_to_1.2",passed])
metrics[,model:=366871L]
stopifnot(fails(cs_confirmation_gates(metrics,boot,data.table(material_regression=FALSE))))
cat("All called-strike model tests passed.\n")
