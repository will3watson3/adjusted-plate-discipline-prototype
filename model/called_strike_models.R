# Reproducible ordinary called-strike models. Run from the project root:
# Rscript -e 'source("called_strike_models.R"); run_called_strike_experiment()'
suppressPackageStartupMessages({ library(data.table); library(mgcv) })
CS_SEED <- 20260930L
CS_KEYS <- c("game_pk", "at_bat_number", "pitch_number")
CS_REPORT <- "reports/called_strike"
CS_MODELS <- "data/models/called_strike"
CS_INPUTS <- c(CS_KEYS, "game_date", "game_year", "batter", "pitcher", "description",
  "analysis_eligible", "plate_x", "plate_z", "sz_bot", "sz_top", "balls", "strikes",
  "stand", "p_throws", "pitch_family", "fielder_2", "release_speed", "pfx_x", "pfx_z",
  "outs_when_up", "on_1b", "on_2b", "on_3b", "inning", "bat_score", "fld_score",
  "prev_plate_x", "prev_plate_z", "prev_description", "previous_pitch_missing", "is_first_pitch")
CS_FEATURES <- c("x", "z", "count", "stand", "throws", "family", "catcher", "speed",
  "movx", "movz", "zone_height", "outs", "runners", "inning", "score_diff", "prevx",
  "prevz", "prev_call", "prev_gap", "first_pitch")
CS_NUMERIC <- c("speed", "movx", "movz", "zone_height", "inning", "score_diff", "prevx", "prevz")
CS_FACTORS <- c("count", "stand", "throws", "family", "catcher", "outs", "runners", "prev_call")

# Fixed, light ridge regularization makes high-order logistic terms estimable
# when unpenalized IRLS is unstable on extreme locations. No penalty tuning on
# confirmation data. Lambda=1e-4 is fixed. Newton refits are independently checked
# against the original glmnet candidate solver on synthetic and 2022 data.
cs_ridge_fit <- function(form, f) {
  if (!requireNamespace("glmnet",quietly=TRUE)) stop("glmnet is required for quartic logistic fits")
  terms <- terms(form,data=f)
  x <- model.matrix(delete.response(terms),data=f)[,-1,drop=FALSE]
  lambda <- 1e-4
  # Conditional effective df including the unpenalized intercept. glmnet uses
  # population standard deviations. Weighted centering removes the intercept.
  center <- colMeans(x)
  sd <- sqrt(colMeans(x^2)-center^2)
  if(any(!is.finite(sd)|sd<=0)) stop("Constant ridge design column")
  z <- sweep(sweep(x,2,center),2,sd,"/")
  # Solve the identical standardized binomial ridge objective with damped Newton
  # steps. At ~30 coefficients a dense Hessian is cheap; coordinate descent can
  # stall on strongly correlated polynomial terms in larger season combinations.
  design <- cbind(1,z)
  penalty <- c(0,rep(lambda,ncol(z)))
  beta <- c(qlogis(mean(f$y)),rep(0,ncol(z)))
  objective <- function(beta) {
    eta <- as.numeric(design %*% beta)
    mean(pmax(eta,0)+log1p(exp(-abs(eta)))-f$y*eta)+sum(penalty*beta^2)/2
  }
  history <- numeric(); converged <- FALSE
  for(iter in 1:100) {
    eta <- as.numeric(design %*% beta); p <- plogis(eta)
    gradient <- as.numeric(crossprod(design,p-f$y))/nrow(x)+penalty*beta
    history <- c(history,objective(beta))
    if(max(abs(gradient))<1e-8) { converged<-TRUE; break }
    hessian <- crossprod(design*sqrt(p*(1-p)))/nrow(x)+diag(penalty)
    step <- as.numeric(solve(hessian,gradient))
    scale <- 1
    while(objective(beta-scale*step)>tail(history,1)-1e-4*scale*sum(gradient*step)) {
      scale <- scale/2
      if(scale<2^-30) stop("Ridge Newton line search stalled")
    }
    beta <- beta-scale*step
  }
  if(!converged) stop("Ridge Newton gradient did not converge")
  fit <- list(lambda=lambda,jerr=0,npasses=iter,max_abs_gradient=max(abs(gradient)),objective_history=history)
  w <- p*(1-p)
  weighted_mean <- colSums(z*w)/sum(w)
  z <- sweep(z,2,weighted_mean)
  a <- crossprod(z*sqrt(w))
  edf <- 1+sum(diag(solve(a+diag(nrow(x)*lambda,ncol(x)),a)))
  coefficients <- c(beta[1]-sum(beta[-1]*center/sd),beta[-1]/sd)
  names(coefficients) <- c("(Intercept)",colnames(x))
  structure(list(fit=fit,lambda=lambda,terms=terms,formula=form,columns=colnames(x),
    coefficients=coefficients,fitted.values=p,converged=TRUE,rank=edf,solver="damped_newton"),class="cs_ridge")
}

formula.cs_ridge <- function(x,...) x$formula
predict.cs_ridge <- function(object,newdata,type="link",...) {
  x <- model.matrix(delete.response(object$terms),data=newdata)[,-1,drop=FALSE]
  if(!identical(colnames(x),object$columns)) stop("Ridge prediction design changed")
  if(identical(object$solver,"damped_newton")) {
    eta<-as.numeric(object$coefficients[1]+x %*% object$coefficients[-1])
    if(type=="link") eta else if(type=="response") plogis(eta) else stop("Unsupported ridge prediction type")
  } else {
    if(!requireNamespace("glmnet",quietly=TRUE)) stop("glmnet is required to read legacy candidate fits")
    as.numeric(predict(object$fit,newx=x,s=object$lambda,type=type))
  }
}

cs_save <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  con <- gzfile(tmp, "wb", compression = 1)
  tryCatch(saveRDS(object, con), finally = close(con))
  if (!file.rename(tmp, path)) stop("Could not save ", path)
}

cs_eligible <- function(d) {
  d$analysis_eligible %in% TRUE & d$description %in% c("called_strike", "ball", "blocked_ball") &
    is.finite(d$plate_x) & is.finite(d$plate_z) & is.finite(d$sz_bot) &
    is.finite(d$sz_top) & d$sz_top > d$sz_bot & d$balls %in% 0:3 & d$strikes %in% 0:2 &
    d$stand %in% c("L", "R") & d$p_throws %in% c("L", "R")
}

cs_read <- function(year) {
  if (!year %in% 2022:2024) stop("This experiment can read only 2022-2024")
  path <- sprintf("data/processed/statcast_%d_clean.rds", year)
  raw <- as.data.table(readRDS(path))
  if (length(setdiff(CS_INPUTS, names(raw)))) stop("Missing input fields")
  if (anyNA(raw[, ..CS_KEYS]) || anyDuplicated(raw, by = CS_KEYS)) stop("Invalid pitch keys")
  if (anyNA(raw$game_year) || any(raw$game_year != year) || anyNA(raw$game_date) ||
      any(format(raw$game_date, "%Y") != as.character(year))) stop("Season/date mismatch")
  keep <- cs_eligible(raw)
  audit <- raw[, .(season = year, n = .N, retained = sum(keep[.I])), by = .(description, analysis_eligible)]
  d <- copy(raw[keep, ..CS_INPUTS])
  d[, y := as.integer(description == "called_strike")]
  d[, season := year]
  setorderv(d, c("game_date", CS_KEYS))
  stopifnot(nrow(d) > 0, uniqueN(d$y) == 2L)
  fwrite(audit, file.path(CS_REPORT, sprintf("population_audit_%d.csv", year)))
  message("Loaded ", year, ": ", nrow(d), " ordinary takes across ", uniqueN(d$game_pk), " games")
  d
}

# PSEUDOCODE: Transform only allowlisted pre-outcome fields. Outcomes are not
# inspected here, so exactly the same function can score taken or swung-at pitches.
cs_raw_features <- function(d) {
  required <- setdiff(CS_INPUTS, c(CS_KEYS, "game_date", "game_year", "batter", "pitcher", "description", "analysis_eligible"))
  if (length(setdiff(required, names(d)))) stop("Prediction input missing fields: ", paste(setdiff(required, names(d)), collapse = ", "))
  previous <- fifelse(is.na(d$prev_description), "none",
    fifelse(d$prev_description %in% c("ball", "blocked_ball"), "ball",
    fifelse(d$prev_description == "called_strike", "called_strike", "other")))
  data.table(x = d$plate_x, z = (d$plate_z - d$sz_bot) / (d$sz_top - d$sz_bot),
    count = paste(d$balls, d$strikes, sep = "-"), stand = as.character(d$stand),
    throws = as.character(d$p_throws), family = as.character(d$pitch_family),
    catcher = as.character(d$fielder_2), speed = d$release_speed, movx = d$pfx_x, movz = d$pfx_z,
    zone_height = d$sz_top - d$sz_bot, outs = as.character(d$outs_when_up),
    runners = as.character(as.integer(!is.na(d$on_1b)) + as.integer(!is.na(d$on_2b)) + as.integer(!is.na(d$on_3b))),
    inning = d$inning, score_diff = d$bat_score - d$fld_score, prevx = d$prev_plate_x,
    prevz = d$prev_plate_z, prev_call = previous, prev_gap = as.integer(d$previous_pitch_missing %in% TRUE),
    first_pitch = as.integer(d$is_first_pitch %in% TRUE))
}

cs_preprocessor <- function(d) {
  f <- cs_raw_features(d)
  levels <- lapply(f[, ..CS_FACTORS], function(x) sort(unique(x[!is.na(x)])))
  references <- lapply(f[, ..CS_FACTORS], function(x) names(sort(table(x), decreasing = TRUE))[1])
  medians <- vapply(f[, ..CS_NUMERIC], function(x) median(x[is.finite(x)]), numeric(1))
  if (any(!is.finite(medians)) || any(lengths(levels) < 2L)) stop("Insufficient training feature variation")
  missing_indicators <- CS_NUMERIC[vapply(f[, ..CS_NUMERIC], function(x) any(!is.finite(x)) && any(is.finite(x)), logical(1))]
  support <- f[, .(n_takes = .N), by = .(xb = floor(x / .1), zb = floor(z / .1), stand)]
  list(levels = levels, references = references, medians = medians,
    missing_indicators = missing_indicators, support = support,
    ranges = list(x = range(f$x), z = range(f$z)))
}

cs_transform <- function(d, prep) {
  f <- cs_raw_features(d)
  invalid <- !is.finite(f$x) | !is.finite(f$z) | !is.finite(f$zone_height) | f$zone_height <= 0 |
    !d$balls %in% 0:3 | !d$strikes %in% 0:2 | !d$stand %in% c("L", "R") | !d$p_throws %in% c("L", "R")
  novel <- rep(FALSE, nrow(d))
  unknown_catcher <- is.na(f$catcher) | !f$catcher %in% prep$levels$catcher
  missing_secondary <- rep(FALSE, nrow(d))
  for (n in CS_NUMERIC) {
    bad <- !is.finite(f[[n]])
    missing_secondary <- missing_secondary | bad
    if (n %in% prep$missing_indicators) f[, (paste0(n, "_missing")) := as.integer(bad)]
    set(f, which(bad), n, prep$medians[[n]])
  }
  for (n in CS_FACTORS) {
    bad <- is.na(f[[n]]) | !f[[n]] %in% prep$levels[[n]]
    if (n != "catcher") novel <- novel | bad
    set(f, which(bad), n, prep$references[[n]])
    set(f, j = n, value = factor(f[[n]], levels = prep$levels[[n]]))
  }
  # Fixed polynomial terms avoid prediction-time re-estimation of a basis.
  # Center z at mid-zone for numerical conditioning; x is already plate-centered.
  for (degree in 2:4) for (a in 0:degree) {
    b <- degree - a
    f[, (paste0("loc_", a, "_", b)) := x^a * (z - .5)^b]
  }
  list(data = f, invalid = invalid, novel_category = novel,
    unknown_catcher = unknown_catcher, missing_secondary = missing_secondary)
}

cs_candidates <- function() {
  hands <- "count + stand * throws"
  poly <- function(deg) paste(c("x", "z", unlist(lapply(2:deg, function(j) paste0("loc_", 0:j, "_", j:0)))), collapse = " + ")
  spatial <- function(k, by_stand = FALSE) sprintf("te(x, z, k=c(%d,%d), bs=c('cr','cr')%s)", k, k, if(by_stand) ", by=stand" else "")
  pitch <- "family + s(speed,k=6,bs='cr') + s(movx,k=6,bs='cr') + s(movz,k=6,bs='cr')"
  height <- "s(zone_height,k=6,bs='cr')"
  catcher <- "s(catcher,bs='re')"
  core <- paste(hands, spatial(10, TRUE), pitch, sep = " + ")
  full <- paste(core, height, catcher, sep = " + ")
  specs <- data.table(id = c("logit_quadratic", "logit_quartic", "logit_quartic_pitch",
    "gam_location_k8", "gam_location_k12", "gam_stance", "gam_pitch", "gam_pitch_height",
    "gam_pitch_catcher", "gam_pitch_height_catcher", "gam_context", "gam_sequence", "gam_capacity_k14", "gam_stronger_penalty"),
    family = c(rep("logistic", 3), rep("gam", 11)), gamma = c(rep(1, 13), 1.4),
    rhs = c(paste(poly(2), hands, sep=" + "), paste(poly(4), hands, sep=" + "),
      paste(poly(4), hands, "family + speed + movx + movz", sep=" + "),
      paste(hands, spatial(8), sep=" + "), paste(hands, spatial(12), sep=" + "),
      paste(hands, spatial(10, TRUE), sep=" + "), core, paste(core,height,sep=" + "),
      paste(core,catcher,sep=" + "), full,
      paste(full,"outs + runners + inning + score_diff",sep=" + "),
      paste(full,"prevx + prevz + prev_call + prev_gap + first_pitch",sep=" + "),
      paste(hands, spatial(14,TRUE), pitch,height,catcher,sep=" + "), full))
  specs
}

cs_formula <- function(spec, prep) {
  form <- as.formula(paste("y ~", spec$rhs), env = globalenv())
  used <- all.vars(form)[-1L]
  allowed <- c(CS_FEATURES, unlist(lapply(2:4, function(j) paste0("loc_",0:j,"_",j:0))))
  if (length(setdiff(used, allowed))) stop("Non-allowlisted predictors")
  missing <- intersect(used, prep$missing_indicators)
  if (length(missing)) form <- as.formula(paste(deparse(form, width.cutoff=500L), "+", paste(paste0(missing,"_missing"), collapse=" + ")), env=globalenv())
  form
}

cs_fit <- function(d, spec, prep = cs_preprocessor(d)) {
  set.seed(CS_SEED)
  transformed <- cs_transform(d, prep)
  if (any(transformed$invalid)) stop("Invalid training locations/counts/handedness")
  f <- transformed$data
  f[, y := d$y]
  form <- cs_formula(spec, prep)
  warnings <- character()
  start <- proc.time()[[3]]
  model <- withCallingHandlers({
    if (spec$family == "gam") bam(form, data = f, family = binomial(), method = "fREML",
      discrete = TRUE, select = TRUE, gamma = spec$gamma, nthreads = 2,
      na.action = na.fail, control = gam.control(maxit=100)) else if(grepl("quartic",spec$id))
      cs_ridge_fit(form,f) else glm(form, data = f, family = binomial(), na.action = na.fail, control = glm.control(maxit=100))
  }, warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (!isTRUE(model$converged) || any(!is.finite(coef(model))) ||
      (spec$family=="gam" && !isTRUE(model$mgcv.conv))) stop("Model did not converge: ", spec$id)
  # Tiny fitted probabilities are normal far from the zone; persist all warnings.
  list(model = model, prep = prep, spec = spec, formula = paste(deparse(form, width.cutoff=500L), collapse=" "),
    fit_seconds = proc.time()[[3]] - start, warnings = unique(warnings),
    train_years = sort(unique(d$season)), train_n = nrow(d),
    train_end = as.character(max(d$game_date)), seed = CS_SEED,
    effective_df = if (spec$family == "gam") sum(model$edf) else model$rank,
    code_hash = unname(tools::md5sum("called_strike_models.R")),
    versions = c(R=as.character(getRversion()), mgcv=as.character(packageVersion("mgcv")),
      glmnet=as.character(packageVersion("glmnet")),data.table=as.character(packageVersion("data.table"))))
}

predict_called_strike <- function(bundle, pitches) {
  d <- as.data.table(pitches)
  if (!nrow(d)) return(data.table(p_called_strike=numeric(), status=character(), unknown_catcher=logical(),
    novel_category=logical(), secondary_imputed=logical(), nearby_training_takes=integer(), low_location_support=logical()))
  transformed <- cs_transform(d, bundle$prep)
  f <- transformed$data
  valid <- which(!transformed$invalid)
  p <- rep(NA_real_, nrow(d))
  has_catcher <- "catcher" %in% all.vars(formula(bundle$model))
  for (chunk in split(valid, ceiling(seq_along(valid) / 50000L))) {
    # Exact basis evaluation avoids batch-dependent re-discretization by predict.bam.
    eta <- as.numeric(predict(bundle$model, newdata = f[chunk], type="link", discrete=FALSE))
    unknown <- transformed$unknown_catcher[chunk]
    if (has_catcher && any(unknown)) eta[unknown] <- as.numeric(predict(bundle$model,
      newdata=f[chunk[unknown]], type="link", exclude="s(catcher)", discrete=FALSE))
    p[chunk] <- plogis(eta)
  }
  if (any(!is.finite(p[valid])) || any(p[valid] < 0 | p[valid] > 1)) stop("Invalid model probabilities")
  key <- paste(floor(f$x/.1), floor(f$z/.1), as.character(f$stand))
  support <- bundle$prep$support
  n <- support$n_takes[match(key, paste(support$xb,support$zb,support$stand))]
  n[is.na(n)] <- 0L
  data.table(p_called_strike=p,
    status=fifelse(transformed$invalid,"invalid_core_inputs",fifelse(transformed$novel_category,"unseen_fixed_category_fallback","scored")),
    unknown_catcher=transformed$unknown_catcher, novel_category=transformed$novel_category,
    secondary_imputed=transformed$missing_secondary, nearby_training_takes=n,
    low_location_support=n < 30L)
}

cs_loss <- function(y,p) {
  if (length(y) != length(p) || anyNA(y) || any(!y %in% 0:1) || any(!is.finite(p)) || any(p<0|p>1)) stop("Invalid metric inputs")
  p <- pmin(1-1e-15,pmax(1e-15,p))
  -(y*log(p)+(1-y)*log1p(-p))
}

cs_metrics <- function(y,p) {
  loss <- cs_loss(y,p)
  lp <- qlogis(pmin(1-1e-10,pmax(1e-10,p)))
  cal <- suppressWarnings(glm(y~lp, family=binomial()))
  intercept_only <- suppressWarnings(glm(y~1+offset(lp),family=binomial()))
  # Doubles are essential: n1*n0 exceeds R's integer range on full seasons.
  n1 <- as.numeric(sum(y)); n0 <- as.numeric(length(y))-n1
  auc <- if(n1 && n0) (sum(rank(p,ties.method="average")[y==1])-n1*(n1+1)/2)/(n1*n0) else NA_real_
  data.table(n=length(y), observed_rate=mean(y), mean_prediction=mean(p),
    log_loss=mean(loss), brier=mean((y-p)^2), auc=auc, accuracy=mean((p>=.5)==y),
    bias=mean(p-y), calibration_intercept=unname(coef(intercept_only)[1]),
    calibration_slope=unname(coef(cal)[2]), joint_intercept=unname(coef(cal)[1]),
    true_positive=sum(p>=.5 & y==1), false_positive=sum(p>=.5 & y==0),
    false_negative=sum(p<.5 & y==1), true_negative=sum(p<.5 & y==0))
}

cs_cluster_se <- function(game, difference) {
  g <- data.table(game=game,d=difference)[, .(n=.N,total=sum(d)),by=game]
  G <- nrow(g); N <- sum(g$n); mu <- sum(g$total)/N
  if(G < 2L) return(NA_real_)
  sqrt(G/(G-1)*sum((g$total-g$n*mu)^2))/N
}

cs_select <- function(results, predictions, validation, family_name) {
  tab <- copy(results[family == family_name & split == "validation_2023"])
  train <- results[family == family_name & split == "training_2022", .(id, train_loss=log_loss)]
  tab <- merge(tab,train,by="id")
  tab[, loss_gap := log_loss-train_loss]
  tab[, guardrails_pass := abs(bias)<=.015 & between(calibration_slope,.8,1.2) & loss_gap<=.02 & converged]
  eligible <- tab[guardrails_pass==TRUE][order(log_loss)]
  if (!nrow(eligible)) stop("No ", family_name, " passes frozen selection gates")
  best <- eligible$id[1]
  best_loss <- cs_loss(validation$y,predictions[[best]])
  early <- as.integer(format(validation$game_date,"%m")) <= 6L
  tab[, `:=`(paired_se=NA_real_, early_difference=NA_real_, late_difference=NA_real_, within_tolerance=FALSE)]
  for(candidate_id in eligible$id) {
    diff <- cs_loss(validation$y,predictions[[candidate_id]])-best_loss
    se <- cs_cluster_se(validation$game_pk,diff)
    tab[id==candidate_id, `:=`(paired_se=se, early_difference=mean(diff[early]), late_difference=mean(diff[!early]))]
  }
  tab[, within_tolerance := guardrails_pass & log_loss-eligible$log_loss[1] <= paired_se+.0005 &
    brier<=eligible$brier[1]+.001 & early_difference<=.002 & late_difference<=.002]
  choice <- tab[within_tolerance==TRUE][order(effective_df,log_loss)]$id[1]
  if(is.na(choice)) stop("No candidate inside selection tolerance")
  tab[, selected := id==choice]
  list(id=choice, table=tab, minimum_loss_id=best)
}

cs_bootstrap <- function(d, p_gam, p_logistic, B=1000L) {
  z <- data.table(game=d$game_pk, gam_loss=cs_loss(d$y,p_gam), logit_loss=cs_loss(d$y,p_logistic),
    gam_brier=(d$y-p_gam)^2, logit_brier=(d$y-p_logistic)^2)
  g <- z[, lapply(.SD,sum),by=game]
  g[, n := z[, .N,by=game]$N]
  cols <- c("gam_loss","logit_loss","gam_brier","logit_brier")
  set.seed(CS_SEED)
  draws <- replicate(B, {
    i <- sample.int(nrow(g),nrow(g),replace=TRUE)
    val <- colSums(as.matrix(g[i,..cols]))/sum(g$n[i])
    c(val,loss_difference=val[1]-val[2],brier_difference=val[3]-val[4])
  })
  estimate <- c(colSums(as.matrix(g[,..cols]))/sum(g$n),
    mean(z$gam_loss-z$logit_loss),mean(z$gam_brier-z$logit_brier))
  data.table(metric=c(cols,"gam_minus_logit_loss","gam_minus_logit_brier"), estimate=unname(estimate),
    ci_low=apply(draws,1,quantile,.025,names=FALSE),ci_high=apply(draws,1,quantile,.975,names=FALSE),
    bootstrap_replicates=B, games=nrow(g))
}

cs_slices <- function(d,p,model_name,score_info) {
  width <- 17/24
  inside <- abs(d$plate_x)<=width & d$plate_z>=d$sz_bot & d$plate_z<=d$sz_top
  dx <- pmax(abs(d$plate_x)-width,0)
  dz <- pmax(d$sz_bot-d$plate_z,d$plate_z-d$sz_top,0)
  distance <- ifelse(inside,pmin(width-abs(d$plate_x),d$plate_z-d$sz_bot,d$sz_top-d$plate_z),sqrt(dx^2+dz^2))
  strata <- list(count=paste(d$balls,d$strikes,sep="-"),stand=d$stand,pitch_family=d$pitch_family,
    month=format(d$game_date,"%Y-%m"),zone_region=ifelse(distance<=.25,"within_3_inches_of_edge",ifelse(inside,"interior","far_outside")),
    catcher_known=ifelse(score_info$unknown_catcher,"unseen_catcher","known_catcher"),
    location_support=ifelse(score_info$low_location_support,"sparse","supported"))
  out <- rbindlist(lapply(names(strata),function(dimension_name) {
    x <- data.table(slice=strata[[dimension_name]],y=d$y,p=p)
    x[, .(n=.N,observed_rate=mean(y),mean_prediction=mean(p),bias=mean(p-y),
      log_loss=mean(cs_loss(y,p)),brier=mean((y-p)^2)),by=slice][, dimension:=dimension_name]
  }))
  out[, model:=model_name]
  out
}

cs_confirmation_gates <- function(metrics,boot,comparison) {
  stopifnot(nrow(metrics)==2L,is.character(metrics$model),setequal(metrics$model,c("gam","logistic")),
    !anyDuplicated(metrics$model),nrow(boot[metric=="gam_minus_logit_loss"])==1L,
    nrow(comparison)>0,!anyNA(comparison$material_regression))
  gammetric<-metrics[model=="gam"]; logisticmetric<-metrics[model=="logistic"]
  pass<-c(boot[metric=="gam_minus_logit_loss",ci_high]<0,
    gammetric$brier<=logisticmetric$brier,abs(gammetric$bias)<=.015,
    between(gammetric$calibration_slope,.8,1.2),gammetric$loss_gap<=.02,!any(comparison$material_regression))
  stopifnot(length(pass)==6L,!anyNA(pass))
  data.table(check=c("gam_log_loss_improves_with_95pct_interval","gam_brier_no_worse",
    "gam_bias_within_0.015","gam_calibration_slope_0.8_to_1.2","gam_train_validation_gap_at_most_0.02","no_material_slice_regressions"),passed=pass)
}

cs_reliability <- function(y,p,model_name) {
  data.table(y=y,p=p,bin=pmin(19L,floor(p*20)))[,
    .(n=.N,mean_prediction=mean(p),observed_rate=mean(y)),by=bin][,model:=model_name]
}

cs_plots <- function(results, calibration, validation, predictions, bundles) {
  png(file.path(CS_REPORT,"model_comparison.png"),width=1500,height=1000,res=160)
  par(mar=c(5,12,3,1))
  z <- results[split=="validation_2023"][order(-log_loss)]
  barplot(z$log_loss,names.arg=z$id,horiz=TRUE,las=1,col=ifelse(z$family=="gam","#277b8e","#b26b37"),
    xlab="2023 validation log loss (lower is better)",main="Called-strike candidate comparison",cex.names=.7)
  dev.off()
  png(file.path(CS_REPORT,"validation_2024.png"),width=1600,height=800,res=160)
  par(mfrow=c(1,2),mar=c(4,4,3,1))
  colors <- c(gam="#277b8e",logistic="#b26b37")
  plot(0:1,0:1,type="n",xlab="Predicted strike probability",ylab="Observed strike frequency",main="2024 calibration")
  abline(0,1,lty=2,col="gray50")
  for(model_name in names(colors)) {
    z<-calibration[model==model_name][order(mean_prediction)]
    stopifnot(nrow(z)>0)
    lines(z$mean_prediction,z$observed_rate,type="b",pch=16,col=colors[[model_name]])
  }
  legend("topleft",names(colors),col=colors,lty=1,pch=16,bty="n")
  month <- format(validation$game_date,"%Y-%m")
  monthly <- data.table(month=month,gam=cs_loss(validation$y,predictions$gam),logistic=cs_loss(validation$y,predictions$logistic))[,lapply(.SD,mean),by=month][order(month)]
  matplot(seq_len(nrow(monthly)),as.matrix(monthly[,.(gam,logistic)]),type="b",pch=16,lty=1,col=colors,
    xaxt="n",xlab="2024 month",ylab="Log loss",main="Later-season performance")
  axis(1,at=seq_len(nrow(monthly)),labels=sub("2024-","",monthly$month))
  legend("topleft",names(colors),col=colors,lty=1,pch=16,bty="n")
  dev.off()
  # Predict a supported reference context on a dense spatial grid, not a fit to
  # outcomes on the grid. This checks zone shape and left/right asymmetry.
  png(file.path(CS_REPORT,"strike_probability_surfaces.png"),width=1600,height=800,res=160)
  par(mfrow=c(1,2),mar=c(4,4,3,1))
  x <- seq(-1.5,1.5,length.out=101); z <- seq(-.5,1.5,length.out=101)
  for(hand in c("L","R")) {
    grid <- validation[rep(which(validation$stand==hand)[1],length(x)*length(z))]
    coords <- CJ(z=z,x=x)
    grid[, `:=`(plate_x=coords$x,plate_z=sz_bot+coords$z*(sz_top-sz_bot),balls=0,strikes=0)]
    pp <- predict_called_strike(bundles$gam,grid)$p_called_strike
    image(x,z,matrix(pp,nrow=length(x)),zlim=c(0,1),col=hcl.colors(30,"YlOrRd",rev=TRUE),
      xlab="Horizontal location (feet)",ylab="Normalized zone height",main=paste(hand,"batter: reference 0-0 context"))
    contour(x,z,matrix(pp,nrow=length(x)),levels=c(.1,.5,.9),add=TRUE,drawlabels=TRUE)
    rect(-17/24,0,17/24,1,lty=2,lwd=2)
  }
  dev.off()
}

run_called_strike_experiment <- function(resume=TRUE, confirmation_only=FALSE, reuse_validation_fits=FALSE) {
  dir.create(CS_REPORT,recursive=TRUE,showWarnings=FALSE)
  dir.create(CS_MODELS,recursive=TRUE,showWarnings=FALSE)
  set.seed(CS_SEED)
  setDTthreads(2)
  hash <- unname(tools::md5sum("called_strike_models.R"))
  specs <- cs_candidates()
  if(!confirmation_only) fwrite(specs,file.path(CS_REPORT,"candidate_specifications.csv"))
  train <- cs_read(2022L); valid <- cs_read(2023L)
  stopifnot(max(train$game_date)<min(valid$game_date),!length(intersect(train$game_pk,valid$game_pk)))
  prep <- cs_preprocessor(train)
  fingerprint_paths <- c("called_strike_models.R",file.path(CS_REPORT,"EXPERIMENT_PROTOCOL.md"),
    sprintf("data/processed/statcast_%d_clean.rds",2022:2023))
  fingerprints <- data.table(path=fingerprint_paths,md5=unname(tools::md5sum(fingerprint_paths)))
  if(confirmation_only) {
    # Reuse frozen selection after a numerical iteration-budget repair only.
    # Verify training inputs and the source snapshot that actually produced it.
    selection<-readRDS(file.path(CS_MODELS,"frozen_selection_2023.rds"))
    results<-fread(file.path(CS_REPORT,"candidate_metrics.csv"))
    keyed<-readRDS(file.path(CS_MODELS,"candidate_predictions_2023.rds"))
    stopifnot(identical(keyed[,..CS_KEYS],valid[,..CS_KEYS]),identical(keyed$y,valid$y))
    pred<-as.list(keyed[,specs$id,with=FALSE])
    selection_source_hash<-unname(tools::md5sum(file.path(CS_REPORT,"selection_implementation_snapshot.R")))
    for(n in names(selection)) {
      original<-readRDS(file.path(CS_MODELS,paste0("candidate_",selection[[n]]$id,".rds")))
      stopifnot(identical(original$code_hash,selection_source_hash),
        identical(original$fingerprints[grepl("clean.rds$",path),md5],fingerprints[grepl("clean.rds$",path),md5]),
        identical(cs_select(results,pred,valid,n)$id,selection[[n]]$id))
    }
    snapshot_paths<-file.path(CS_REPORT,c("selection_implementation_snapshot.R","SELECTION_PROTOCOL_SNAPSHOT.md"))
    fingerprints<-rbind(fingerprints,data.table(path=snapshot_paths,md5=unname(tools::md5sum(snapshot_paths))))
    message("Resuming frozen specifications; no feature selection is repeated using 2024.")
  } else {
  fwrite(fingerprints,file.path(CS_REPORT,"source_fingerprints.csv"))
  results <- list(); pred <- list(); diagnostics <- list(); failures <- list()
  for(i in seq_len(nrow(specs))) {
    spec <- specs[i]
    message("Candidate ",i,"/",nrow(specs),": ",spec$id)
    path <- file.path(CS_MODELS,paste0("candidate_",spec$id,".rds"))
    bundle <- NULL
    if(resume && file.exists(path)) {
      saved <- readRDS(path)
      if(identical(saved$code_hash,hash) && identical(saved$fingerprints,fingerprints)) bundle<-saved
    }
    if(is.null(bundle)) {
      bundle <- tryCatch(cs_fit(train,spec,prep),error=function(e) e)
      if(inherits(bundle,"error")) {
        failures[[length(failures)+1L]]<-data.table(id=spec$id,error=conditionMessage(bundle))
        fwrite(rbindlist(failures),file.path(CS_REPORT,"failed_candidates.csv"))
        message("Failed: ",conditionMessage(bundle)); next
      }
      bundle$fingerprints <- fingerprints
      cs_save(bundle,path)
    }
    ptrain <- predict_called_strike(bundle,train)$p_called_strike
    pvalid <- predict_called_strike(bundle,valid)$p_called_strike
    pred[[spec$id]] <- pvalid
    for(split_name in c("training_2022","validation_2023","validation_2023_early","validation_2023_late")) {
      istrain <- split_name=="training_2022"
      d <- if(istrain) train else valid; p <- if(istrain) ptrain else pvalid
      use <- rep(TRUE,nrow(d))
      month <- as.integer(format(d$game_date,"%m"))
      if(split_name=="validation_2023_early") use<-month<=6
      if(split_name=="validation_2023_late") use<-month>6
      metric <- cs_metrics(d$y[use],p[use])
      metric[, `:=`(id=spec$id,family=spec$family,split=split_name,
        effective_df=bundle$effective_df,converged=isTRUE(bundle$model$converged),fit_seconds=bundle$fit_seconds)]
      results[[length(results)+1L]]<-metric
    }
    diagnostic <- data.table(id=spec$id,formula=bundle$formula,warnings=paste(bundle$warnings,collapse=" | "),
      effective_df=bundle$effective_df,coefficients=length(coef(bundle$model)),fit_seconds=bundle$fit_seconds)
    diagnostics[[length(diagnostics)+1L]]<-diagnostic
    fwrite(rbindlist(results),file.path(CS_REPORT,"candidate_metrics.csv"))
    fwrite(rbindlist(diagnostics),file.path(CS_REPORT,"fit_diagnostics.csv"))
    message("  2023 log loss: ",signif(mean(cs_loss(valid$y,pvalid)),6),"; effective df: ",round(bundle$effective_df,1))
    rm(bundle); gc(verbose=FALSE)
  }
  results<-rbindlist(results)
  if(!length(failures)) fwrite(data.table(id=character(),error=character()),file.path(CS_REPORT,"failed_candidates.csv"))
  countmeans<-train[,.(p=mean(y)),by=.(balls,strikes)]
  pc<-countmeans$p[match(paste(valid$balls,valid$strikes),paste(countmeans$balls,countmeans$strikes))]
  fwrite(cs_metrics(valid$y,pc),file.path(CS_REPORT,"count_only_baseline_2023.csv"))
  selection <- lapply(c("logistic","gam"),function(n) cs_select(results,pred,valid,n))
  names(selection)<-c("logistic","gam")
  fwrite(rbindlist(lapply(selection,`[[`,"table")),file.path(CS_REPORT,"selection_2023.csv"))
  cs_save(selection,file.path(CS_MODELS,"frozen_selection_2023.rds"))
  keyed<-valid[,c(CS_KEYS,"game_date","y"),with=FALSE]
  for(n in names(pred)) keyed[,(n):=pred[[n]]]
  cs_save(keyed,file.path(CS_MODELS,"candidate_predictions_2023.rds"))
  }
  message("Frozen selection: GAM=",selection$gam$id,"; logistic=",selection$logistic$id)
  # Only now read confirmation-season labels. Do not revise candidates using them.
  confirm<-cs_read(2024L)
  stopifnot(max(valid$game_date)<min(confirm$game_date),
    !length(intersect(confirm$game_pk,c(train$game_pk,valid$game_pk))))
  fingerprints<-rbind(fingerprints,data.table(path="data/processed/statcast_2024_clean.rds",
    md5=unname(tools::md5sum("data/processed/statcast_2024_clean.rds"))))
  if(reuse_validation_fits) {
    validation_source<-file.path(CS_REPORT,"validation_implementation_snapshot.R")
    fingerprints<-rbind(fingerprints,data.table(path=validation_source,md5=unname(tools::md5sum(validation_source))))
  }
  fwrite(fingerprints,file.path(CS_REPORT,"source_fingerprints.csv"))
  fitdata<-rbind(train,valid); finalprep<-cs_preprocessor(fitdata)
  bundles<-list(); predictions<-list(); scores<-list(); confirm_metrics<-list(); slices<-list(); calibration<-list(); basis<-list()
  for(model_name in c("logistic","gam")) {
    chosen<-selection[[model_name]]$id
    path<-file.path(CS_MODELS,paste0(model_name,"_validation_fit_2022_2023.rds"))
    if(reuse_validation_fits && file.exists(path)) {
      message("Reusing converged validation fit: ",model_name)
      bundle<-readRDS(path)
      stopifnot(identical(bundle$train_years,2022:2023),bundle$train_n==nrow(fitdata),
        isTRUE(all.equal(bundle$spec,specs[id==chosen],check.attributes=FALSE)),
        isTRUE(all.equal(bundle$prep,finalprep,check.attributes=FALSE)),
        identical(bundle$code_hash,unname(tools::md5sum(file.path(CS_REPORT,"validation_implementation_snapshot.R")))) )
    } else {
      message("Refitting selected ",model_name," on 2022-2023: ",chosen)
      bundle<-cs_fit(fitdata,specs[id==chosen],finalprep)
      bundle$fingerprints<-fingerprints
      bundle$role<-"validation_fit_2022_2023"
      cs_save(bundle,path)
    }
    bundle<-readRDS(path)
    score<-predict_called_strike(bundle,confirm)
    p<-score$p_called_strike
    stopifnot(length(p)==nrow(confirm),all(is.finite(p)))
    metric<-cs_metrics(confirm$y,p)
    metric[,`:=`(model=model_name,candidate=chosen,split="confirmation_2024",
      training_log_loss=mean(cs_loss(fitdata$y,predict_called_strike(bundle,fitdata)$p_called_strike)))]
    metric[,loss_gap:=log_loss-training_log_loss]
    confirm_metrics[[model_name]]<-metric
    predictions[[model_name]]<-p; scores[[model_name]]<-score; bundles[[model_name]]<-bundle
    slices[[model_name]]<-cs_slices(confirm,p,model_name,score)
    calibration[[model_name]]<-cs_reliability(confirm$y,p,model_name)
    if(model_name=="gam") {
      set.seed(CS_SEED)
      kc<-k.check(bundle$model,subsample=5000,n.rep=200)
      basis[[model_name]]<-data.table(term=rownames(kc),as.data.frame(kc),check.names=FALSE)
      fwrite(basis[[model_name]],file.path(CS_REPORT,"selected_gam_basis_checks.csv"))
      sm<-as.data.table(summary(bundle$model)$s.table,keep.rownames="term")
      fwrite(sm,file.path(CS_REPORT,"selected_gam_smooths.csv"))
    }
  }
  confirm_metrics<-rbindlist(confirm_metrics); slices<-rbindlist(slices); calibration<-rbindlist(calibration)
  boot<-cs_bootstrap(confirm,predictions$gam,predictions$logistic)
  comparison<-dcast(slices,dimension+slice+n~model,value.var="log_loss")
  comparison[,gam_minus_logistic:=gam-logistic]
  comparison[,material_regression:=n>=1000 & gam_minus_logistic>.005]
  gates<-cs_confirmation_gates(confirm_metrics,boot,comparison)
  fwrite(confirm_metrics,file.path(CS_REPORT,"confirmation_metrics_2024.csv"))
  fwrite(boot,file.path(CS_REPORT,"game_bootstrap_2024.csv"))
  fwrite(slices,file.path(CS_REPORT,"slices_2024.csv"))
  fwrite(comparison,file.path(CS_REPORT,"slice_comparison_2024.csv"))
  fwrite(calibration,file.path(CS_REPORT,"calibration_2024.csv"))
  fwrite(gates,file.path(CS_REPORT,"confirmation_gates.csv"))
  scored<-confirm[,c(CS_KEYS,"game_date","batter","pitcher","description","y"),with=FALSE]
  scored[,`:=`(p_called_strike_gam=predictions$gam,p_called_strike_logistic=predictions$logistic,
    unknown_catcher=scores$gam$unknown_catcher,nearby_training_takes=scores$gam$nearby_training_takes,
    low_location_support=scores$gam$low_location_support)]
  cs_save(scored,file.path(CS_MODELS,"predictions_2024.rds"))
  errors<-copy(scored)
  errors[,gam_log_loss:=cs_loss(y,p_called_strike_gam)]
  fwrite(errors[order(-gam_log_loss)][1:100],file.path(CS_REPORT,"largest_errors_2024.csv"))
  fwrite(data.table(season=2022:2024,n=c(nrow(train),nrow(valid),nrow(confirm)),
    games=c(uniqueN(train$game_pk),uniqueN(valid$game_pk),uniqueN(confirm$game_pk)),
    strike_rate=c(mean(train$y),mean(valid$y),mean(confirm$y))),file.path(CS_REPORT,"sample_summary.csv"))
  cs_plots(results,calibration,confirm,predictions,bundles)
  if(all(gates$passed)) {
    deployment<-rbind(fitdata,confirm); deploymentprep<-cs_preprocessor(deployment)
    for(n in c("logistic","gam")) {
      message("Final fit on 2022-2024: ",n)
      chosen<-selection[[n]]$id
      b<-cs_fit(deployment,specs[id==chosen],deploymentprep)
      b$role<-"final_fit_2022_2024_not_tested_on_2025"
      b$confirmation_gates<-gates; b$fingerprints<-fingerprints
      cs_save(b,file.path(CS_MODELS,paste0(n,"_final_fit_2022_2024.rds")))
      # Persistence parity: same keyed rows, same probabilities after reload.
      probe<-confirm[unique(round(seq(1,.N,length.out=1000)))]
      restored<-readRDS(file.path(CS_MODELS,paste0(n,"_final_fit_2022_2024.rds")))
      stopifnot(identical(predict_called_strike(b,probe),predict_called_strike(restored,probe)))
      rm(b,restored);gc(verbose=FALSE)
    }
  } else message("Confirmation gate failure: final-fit promotion withheld. See confirmation_gates.csv")
  writeLines(capture.output(sessionInfo()),file.path(CS_REPORT,"session_info.txt"))
  stopifnot(identical(hash,unname(tools::md5sum("called_strike_models.R")) ))
  print(confirm_metrics); print(gates)
  message("Experiment complete. 2025 was not opened.")
  invisible(list(selection=selection,metrics=confirm_metrics,gates=gates))
}
