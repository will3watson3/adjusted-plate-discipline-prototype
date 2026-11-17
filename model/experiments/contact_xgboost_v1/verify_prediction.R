source('experiments/contact_xgboost_v1/predict.R')
b<-readRDS(file.path(XM,'bundle.rds'));model<-xgb.load(file.path(XM,'contact.ubj'))
d<-ct_read(2025,XR)[1:20];expected<-x_predict(model,d,b)
checks<-list();check<-function(name,ok){stopifnot(isTRUE(ok));checks[[name]]<<-data.table(check=name,passed=TRUE)}
single<-predict_contact_xgboost(d[1]);batch<-predict_contact_xgboost(d)
check('single_pitch_equals_native_batch_prediction',nrow(single)==1&&abs(single$p_contact_given_swing-expected[1])<1e-7)
check('batch_prediction_matches_frozen_model',max(abs(batch$p_contact_given_swing-expected))<1e-7)
check('pitch_keys_preserved',all(vapply(CT_KEYS,function(nm)identical(d[[nm]],batch[[nm]]),logical(1))))
bad<-copy(d[1]);bad[,plate_x:=NA_real_]
check('single_invalid_pitch_rejected',inherits(try(predict_contact_xgboost(bad),silent=TRUE),'try-error'))
fwrite(rbindlist(checks),file.path(XR,'prediction_verification.csv'));cat(length(checks),'public prediction checks passed\n')
