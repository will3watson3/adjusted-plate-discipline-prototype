source('experiments/swing_xgboost_v1/functions.R')
# Raw model estimates; callers must apply the existing Decision+ support policy.
predict_swing_xgboost<-function(pitches,arm='xgboost'){
 stopifnot(arm%in%SARMS);d<-as.data.table(pitches);stopifnot(nrow(d)>0)
 h<-readRDS(file.path(SM,'histories.rds'));re<-read_re_baseline();p<-list()
 for(stage in HR_STAGES){
  if(arm=='gam'||(arm=='contact_only'&&stage!='contact')){
   b<-s_base(stage);p[[stage]]<-b$model$family$linkinv(hr_eta(b$model,hr_features(d,b,h$contact,h$power)))
  }else if(stage=='contact'){
   b<-readRDS('experiments/contact_xgboost_v1/models/bundle.rds');m<-xgb.load('experiments/contact_xgboost_v1/models/contact.ubj')
   z<-if(nrow(d)==1)rbindlist(list(d,d))else d;p[[stage]]<-x_predict(m,z,b)[seq_len(nrow(d))]
  }else{
   b<-readRDS(file.path(SM,paste0(stage,'.rds')));p[[stage]]<-s_predict(xgb.load(file.path(SM,paste0(stage,'.ubj'))),d,b,h)
  }
 }
 states<-pc_branch_states(re);j<-match(pc_state(d),states$re288_state);stopifnot(!anyNA(j))
 out<-s_branches(p$contact,p$bip,p$tip,p$value,states[j])
 out[,swing_run_value:=q_swing-states$re_before[j]]
 cbind(d[,..CT_KEYS],out)
}
