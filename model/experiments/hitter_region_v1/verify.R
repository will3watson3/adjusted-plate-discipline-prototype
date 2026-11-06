source('experiments/hitter_region_v1/run.R')
setDTthreads(2)
checks<-list();check<-function(name,ok){stopifnot(isTRUE(ok));checks[[name]]<<-data.table(check=name,passed=TRUE)}
h<-fread(file.path(HR_REPORT,'input_hashes.csv'));vd_check_hash(h);check('original_input_hashes_unchanged',TRUE)
manifest<-readRDS(file.path(HR_MODELS,'manifest.rds'));check('candidate_not_activated',identical(manifest$activated,FALSE))
check('source_matches_manifest',identical(unname(manifest$code_md5),unname(tools::md5sum(file.path(HR_ROOT,'run.R')))))
metrics<-fread(file.path(HR_REPORT,'metrics_2024.csv'))
for(arm in c('control','regional')) {
  b<-hr_fread(file.path(HR_REPORT,paste0('branch_predictions_',arm,'_2024.csv.gz')))
  check(paste(arm,'unique_branch_keys'),!anyDuplicated(b,by=CT_KEYS))
  mat<-as.matrix(b[,.(whiff,bip,foul,tip)])
  check(paste(arm,'branch_distribution'),all(is.finite(mat))&&all(mat>=0&mat<=1)&&max(abs(rowSums(mat)-1))<1e-12)
  for(stage in HR_STAGES) {
    p<-hr_fread(file.path(HR_REPORT,paste0('predictions_',stage,'_2024.csv.gz')))
    i<-match(dv_key(p),dv_key(b));check(paste(arm,stage,'unique_aligned_keys'),!anyNA(i)&&!anyDuplicated(p,by=CT_KEYS))
    reconstructed<-switch(stage,contact=1-b$whiff[i],bip=b$bip[i]/(1-b$whiff[i]),
      tip=b$tip[i]/(b$tip[i]+b$foul[i]),value=b$q_bip[i])
    check(paste(arm,stage,'factual_matches_all_pitch_predictions'),max(abs(reconstructed-p[[arm]]))<1e-9)
    expected<-hr_metrics(p$y,p[[arm]],stage)
    selected_rows<-which(metrics$stage==stage & metrics$arm==arm);actual<-metrics[selected_rows]
    for(nm in names(expected))check(paste(arm,stage,nm,'recomputed'),abs(expected[[nm]]-actual[[nm]])<1e-9)
    slices<-fread(file.path(HR_REPORT,'slices_2024.csv'))
    selected_rows<-which(slices$stage==stage & slices$arm==arm);selected<-slices[selected_rows]
    check(paste(arm,stage,'slice_counts'),all(selected[,.(total=sum(n)),by=dimension]$total==nrow(p)))
    for(dim in unique(selected$dimension)) {
      z<-data.table(slice=p[[dim]],y=p$y,p=p[[arm]])
      if(stage=='value') recomputed<-z[,.(n=.N,rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),mean_error=mean(p-y)),by=slice] else
        recomputed<-z[,.(n=.N,log_loss=mean(cs_loss(y,p)),brier=mean((p-y)^2),mean_error=mean(p-y)),by=slice]
      ix<-which(selected$dimension==dim);saved<-selected[ix]
      saved<-saved[match(recomputed$slice,saved$slice)]
      for(nm in setdiff(names(recomputed),'slice'))check(paste(arm,stage,dim,nm,'slice_recomputed'),
        max(abs(recomputed[[nm]]-saved[[nm]]))<1e-9)
    }
  }
}
s<-hr_fread(file.path(HR_REPORT,'decision_predictions_2024.csv.gz'))
old<-as.data.table(readRDS('data/models/hitter_stat_v6/decision_values_2024.rds'));i<-match(dv_key(s),dv_key(old))
check('take_path_frozen',max(abs(s$q_take-old$q_take_basic[i]))<1e-12)
check('policy_frozen',max(abs(s$p_swing_reference-old$p_swing_reference[i]))<1e-12)
check('no_unsupported_promotions',all(!s$primary_supported|old$primary_supported[i]))
check('no_old_model_intervals',!any(grepl('gap_ci|model_only|preference|conditional_envelope',names(s))))
for(arm in c('control','regional'))check(paste(arm,'unchanged_score_formula'),
  max(abs(s[[paste0('score_',arm)]]-(as.integer(s$description%chin%CT_SWINGS)-s$p_swing_reference)*(s[[paste0('q_swing_',arm)]]-s$q_take)))<1e-12)
for(arm in c('control','regional')) {
  b<-hr_fread(file.path(HR_REPORT,paste0('branch_predictions_',arm,'_2024.csv.gz')))
  j<-match(dv_key(b),dv_key(old))
  reconstructed<-b$whiff*old$q_whiff[j]+b$foul*old$q_ordinary_foul[j]+b$tip*old$q_foul_tip[j]+b$bip*b$q_bip
  check(paste(arm,'unchanged_deterministic_states'),max(abs(reconstructed-b$q_swing))<1e-10)
}
hit<-fread(file.path(HR_REPORT,'hitter_changes_2024.csv'));ref<-readRDS('data/models/hitter_stat_v6/season_reference_2024.rds')
check('qualification_unchanged',identical(hit$qualified,hit$supported>=500&hit$games>=60&hit$coverage>=.7))
for(arm in c('control','regional')) {
  calc<-s[primary_supported==TRUE,.(rate=100*mean(get(paste0('score_',arm)))),by=batter]
  j<-match(calc$batter,hit$batter)
  check(paste(arm,'hitter_means'),max(abs(calc$rate-hit[[paste0(arm,'_runs100')]][j]))<1e-10)
  check(paste(arm,'frozen_reference'),max(abs((100+10*(calc$rate-ref$center)/ref$scale)-hit[[paste0(arm,'_decision_plus')]][j]))<1e-9)
}
for(stage in HR_STAGES) {
  b<-readRDS(file.path(HR_MODELS,paste0(stage,'.rds')))
  check(paste(stage,'convergence'),isTRUE(b$model$converged)&&isTRUE(b$model$mgcv.conv)&&isTRUE(b$pool$converged))
  check(paste(stage,'variance_shared'),length(b$pool$variance)==1&&is.finite(b$pool$variance)&&b$pool$variance>=0)
  check(paste(stage,'chronology'),identical(b$history_years,c('2022'=2021L,'2023'=2022L,'2024'=2023L)))
  original<-switch(stage,contact=readRDS('data/models/contact/contact_gam_fit_2022_2023.rds'),
    bip=readRDS('data/models/post_contact/post_contact_fit_2022_2023.rds')$bip,
    tip=readRDS('data/models/post_contact/post_contact_fit_2022_2023.rds')$tip,
    value=readRDS('data/models/post_contact/bip_value_fit_2022_2023.rds'))
  check(paste(stage,'formula_unchanged'),identical(deparse(formula(b$model)),deparse(formula(original$model))))
  check(paste(stage,'family_link_unchanged'),identical(b$model$family$family,original$model$family$family)&&identical(b$model$family$link,original$model$family$link))
  check(paste(stage,'training_population_unchanged'),b$n==if(stage=='contact')original$train_n else original$n)
}
fwrite(rbindlist(checks),file.path(HR_REPORT,'independent_verification.csv'))
cat(length(checks),'independent artifact checks passed\n')
