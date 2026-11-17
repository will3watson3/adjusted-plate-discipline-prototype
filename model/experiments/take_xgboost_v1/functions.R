source('umpire_v5.R')
suppressPackageStartupMessages(library(xgboost))
TROOT <- 'experiments/take_xgboost_v1'
TM <- file.path(TROOT, 'models'); TR <- file.path(TROOT, 'reports')
TSEED <- 20261117L
setDTthreads(2)
t_write <- function(d, name) fwrite(d, file.path(TR, paste0(name, '.csv')))
t_hash <- function(paths) data.table(path=paths, md5=unname(tools::md5sum(paths)))
t_base_path <- function(development=FALSE) file.path('data/models/called_strike',
  if(development) 'candidate_gam_stronger_penalty.rds' else 'gam_validation_fit_2022_2023.rds')
t_read <- function(year, training=FALSE) {
  d <- as.data.table(readRDS(sprintf('data/processed/statcast_%d_clean.rds', year)))
  stopifnot(!anyDuplicated(d, by=CS_KEYS), !anyNA(d[, ..CS_KEYS]), all(d$game_year==year))
  if(training) d <- d[cs_eligible(d)]
  d[, `:=`(y=as.integer(description=='called_strike'), season=year)]
  setorderv(d, c('game_date', CS_KEYS)); d
}
t_schema <- function(base) {
  features <- setdiff(all.vars(formula(base$model)), c('y','catcher'))
  factors <- intersect(features, CS_FACTORS)
  list(features=features, numeric=setdiff(features,factors), levels=base$prep$levels[factors])
}
t_matrix <- function(d, prep, schema) {
  f <- cs_transform(d, prep)$data
  out <- as.matrix(f[, schema$numeric, with=FALSE]); storage.mode(out) <- 'double'
  for(nm in names(schema$levels)) {
    lev <- schema$levels[[nm]]
    a <- matrix(vapply(lev, function(v) as.numeric(as.character(f[[nm]])==v), numeric(nrow(f))), nrow=nrow(f))
    colnames(a) <- paste(nm,lev,sep='='); out <- cbind(out,a)
  }
  stopifnot(all(is.finite(out)), !anyDuplicated(colnames(out))); out
}
t_params <- function(depth) list(objective='binary:logistic', eval_metric='logloss', tree_method='hist',
  device='cpu', eta=.05, max_depth=as.integer(depth), min_child_weight=50, lambda=10, alpha=1,
  subsample=.8, colsample_bytree=.9, max_bin=256, nthread=2, seed=TSEED, validate_parameters=TRUE)
t_eta <- function(model, d, bundle) as.numeric(predict(model,
  xgb.DMatrix(t_matrix(d,bundle$prep,bundle$schema), nthread=2), outputmargin=TRUE))
t_actor_data <- function(d, eta, prep) {
  f <- cs_transform(d, prep)
  out <- d[, c(CS_KEYS,'game_date','game_year','fielder_2','description','plate_x','plate_z','sz_bot','sz_top'), with=FALSE]
  out[, `:=`(base_eta=eta, valid=!f$invalid&!f$novel_category, catcher=as.character(fielder_2),
    y=as.integer(description=='called_strike'), take=description%chin%c('ball','blocked_ball','called_strike'),
    z=(plate_z-sz_bot)/(sz_top-sz_bot))]
  out[,region:=fifelse(z<0,'below',fifelse(z>1,'above',fifelse(abs(plate_x)>.83,'side','inside_rectangle')))]
  u5_join(out, fread('reports/umpire_v5/umpire_assignments.csv'))
}
t_fit_actor <- function(d, eta, prep, arm, year, phase, backbone_path) {
  a <- t_actor_data(d,eta,prep)
  b <- u5_fit(a,'umpire_region_model',year)
  b$provenance <- list(phase=phase, arm=arm, backbone=t_hash(backbone_path),
    offset_keys=d[, ..CS_KEYS], offsets=eta, prep=prep)
  cs_save(b,file.path(TM,sprintf('%s_%s_actors_%d.rds',phase,arm,year)))
  b
}
t_probability_metrics <- function(y,p) {
  lp <- qlogis(pmin(1-1e-10,pmax(1e-10,p)))
  joint <- suppressWarnings(glm(y~lp,family=binomial()))
  intercept <- suppressWarnings(glm(y~1+offset(lp),family=binomial()))
  data.table(n=length(y),log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),bias=mean(p-y),
    calibration_intercept=unname(coef(intercept)[1]),calibration_slope=unname(coef(joint)[2]),
    joint_intercept=unname(coef(joint)[1]))
}
t_value_metrics <- function(y,p) data.table(n=length(y),rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),bias=mean(p-y))
t_boot <- function(game,y,a,b,value=FALSE) {
  losses <- if(value) list(rmse=list((a-y)^2,(b-y)^2),mae=list(abs(a-y),abs(b-y)),bias=list(a-y,b-y)) else
    list(log_loss=list(cs_loss(y,a),cs_loss(y,b)),brier=list((a-y)^2,(b-y)^2),bias=list(a-y,b-y))
  rbindlist(lapply(names(losses),function(metric) {
    ll <- losses[[metric]]
    g <- data.table(game,a=ll[[1]],b=ll[[2]])[,.(n=.N,a=sum(a),b=sum(b)),by=game]
    calc <- function(i) {
      aa <- sum(g$a[i])/sum(g$n[i]); bb <- sum(g$b[i])/sum(g$n[i])
      if(metric=='rmse') sqrt(aa)-sqrt(bb) else aa-bb
    }
    set.seed(TSEED); draws <- replicate(500,calc(sample.int(nrow(g),nrow(g),TRUE)))
    data.table(metric,difference=calc(seq_len(nrow(g))),ci_low=unname(quantile(draws,.025)),
      ci_high=unname(quantile(draws,.975)),games=nrow(g),replicates=500L)
  }))
}
