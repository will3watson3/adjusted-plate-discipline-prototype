source('experiments/contact_xgboost_v1/functions.R')
# Batch or single-pitch prediction from the frozen model and its feature bundle.
predict_contact_xgboost<-function(d){
 d<-as.data.table(d);stopifnot(nrow(d)>0L)
 n<-nrow(d)
 # Preserve a two-dimensional factor-encoding matrix for singleton input.
 # Tree predictions are row-independent; the duplicate is discarded.
 if(n==1L)d<-rbindlist(list(d,d))
 bundle<-readRDS(file.path(XM,'bundle.rds'))
 model<-xgb.load(file.path(XM,'contact.ubj'))
 data.table(d[seq_len(n),..CT_KEYS],p_contact_given_swing=x_predict(model,d,bundle)[seq_len(n)])
}
