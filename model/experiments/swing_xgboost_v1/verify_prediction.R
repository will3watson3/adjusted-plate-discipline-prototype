source('experiments/swing_xgboost_v1/predict.R')
d<-hr_read(2025)[1:10];stored<-fread(cmd=paste('gzip -dc',shQuote(file.path(SR,'predictions_all_2025.csv.gz'))));j<-match(s_key(d),s_key(stored))
checks<-list();check<-function(name,ok){stopifnot(isTRUE(ok));checks[[name]]<<-data.table(check=name,passed=TRUE)}
for(arm_name in SARMS){
 p<-predict_swing_xgboost(d,arm_name);one<-predict_swing_xgboost(d[1],arm_name)
 check(paste(arm_name,'independent_RE_transitions_and_batch_prediction'),max(abs(p$q_swing-stored[[paste0('q_swing_',arm_name)]][j]))<1e-9)
 check(paste(arm_name,'single_pitch_prediction'),abs(one$q_swing-p$q_swing[1])<1e-9)
}
fwrite(rbindlist(checks),file.path(SR,'prediction_verification.csv'));cat(length(checks),'public prediction checks passed\n')
