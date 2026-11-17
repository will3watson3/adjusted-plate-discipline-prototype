source('experiments/contact_xgboost_v1/functions.R')
read<-function(name)fread(file.path(XR,name))
m<-read('metrics.csv');boot<-read('paired_game_bootstrap.csv');sel<-read('selection.csv');slices<-read('slices.csv');cal<-read('calibration.csv')
checks<-read('independent_verification.csv');unit<-read('unit_verification.csv');predchecks<-read('prediction_verification.csv');stopifnot(all(checks$passed),all(unit$passed),all(predchecks$passed))
bundle<-readRDS(file.path(XM,'bundle.rds'))
table<-function(d,cols){
 fmt<-function(x)if(is.numeric(x))format(x,digits=7,trim=TRUE,scientific=FALSE)else as.character(x)
 z<-as.data.frame(lapply(d[,..cols],fmt));c(paste0('| ',paste(cols,collapse=' | '),' |'),paste0('| ',paste(rep('---',length(cols)),collapse=' | '),' |'),apply(z,1,function(x)paste0('| ',paste(x,collapse=' | '),' |')))
}
primary<-boot[year==2025&reference=='gam_prior_season'&metric=='log_loss']
verdict<-if(primary$ci_high<0)'XGBoost improves 2025 contact log loss versus the matched-feature updated-history GAM; the paired game-bootstrap interval is below zero.' else if(primary$ci_low>0)'XGBoost has worse 2025 contact log loss than the matched-feature updated-history GAM; the paired game-bootstrap interval is above zero.' else 'The 2025 paired game-bootstrap interval does not clearly distinguish XGBoost from the matched-feature updated-history GAM on contact log loss.'
gx<-m[year==2025&model=='xgboost_prior_season'];gg<-m[year==2025&model=='gam_prior_season']
newx<-slices[year==2025&dimension=='batter_prior_profile'&slice=='unseen'&model=='xgboost_prior_season']
newg<-slices[year==2025&dimension=='batter_prior_profile'&slice=='unseen'&model=='gam_prior_season']
lines<-c('# XGBoost versus contact GAMs: exploratory 2025 comparison','',verdict,'',
 sprintf('The 2025 paired log-loss difference is %.6f [%.6f, %.6f], a %.2f%% relative reduction. Brier improves from %.6f to %.6f. Gains also appear in 2024.',primary$difference,primary$ci_low,primary$ci_high,100*(gg$log_loss-gx$log_loss)/gg$log_loss,gg$brier,gx$brier),'',
 sprintf('Calibration is a tradeoff: 2025 slope is %.4f versus %.4f, and mean overprediction is %.3f versus %.3f percentage points. Among %d swings by hitters without a previous-season profile, mean overprediction rises from %.3f to %.3f percentage points despite lower log loss. No 2025 calibration correction was fitted.',gx$calibration_slope,gg$calibration_slope,100*gx$bias,100*gg$bias,newx$n,100*newg$bias,100*newx$bias),'',
'This is a factual contact-probability experiment on ordinary swings. The selected XGBoost model was frozen before computing evaluation predictions. 2025 was previously inspected elsewhere, so this is exploratory later-season evidence. No candidate was activated.','',
'## Same-feature comparison','',
'Primary comparison: xgboost_prior_season versus gam_prior_season. Both fit on the same 675,353 swings from 2022–23, use identical numeric transformations, factor information, missingness indicators and completed prior-season contact histories. XGBoost one-hot encodes factors and learns tree interactions instead of GAM smooth terms. No batter/pitcher identity, source zone, same-pitch outcome or extra history enters the candidate.','',
'gam_frozen_2021 is the active contact GAM baseline with its original 2021 histories. It is a useful secondary operational comparison, but its input history values differ; it does not isolate model family as cleanly as the primary comparison. The hitter-specific regional/continuous effects are not part of this matched-feature comparison because they use additional hitter-identity information.','',
paste('Exact predictors:',paste(bundle$schema$features,collapse=', ')), '',
'## Overfitting controls and chronological selection','',
'Train development models on 2022, validate on 2023, and fit development preprocessing only on 2022. Three depths (3/4/5) were specified before training; all other settings were fixed. Early stopping uses validation log loss, with 60-round patience and a 1,200-round cap. Training/validation curves and every candidate are retained. The selected depth and round count are refit on all 2022–23 data with the GAM’s final training preprocessing. No 2024/2025 tuning, test-set early stopping or recalibration was performed.','',
'Fixed settings: learning rate .05; minimum child Hessian weight 50; L2 penalty 10; L1 penalty 1; row subsampling .8; column subsampling .9; CPU histogram trees; two threads; seed 20261116; unweighted binary log loss. These controls constrain complexity; they cannot guarantee absence of overfitting.','',
table(sel,names(sel)),'',sprintf('Selected depth %d with %d boosting rounds. Final training log loss %.6f; final fit took %.1f seconds. Installed R xgboost version: %s.',bundle$selected$depth,bundle$nrounds,bundle$training_log_loss,bundle$seconds,bundle$xgboost_version),'',
'cap_reached flags runs that exhausted the prespecified compute budget. A capped run may be undertrained; it was not extended after inspecting later-season outcomes.','',
'## Evaluation','',table(m,c('year','model','n','log_loss','brier','bias','auc','calibration_intercept','calibration_slope')),'',
'Calibration intercept is intercept-only recalibration with logit prediction as offset. Slope comes from a jointly fitted intercept/slope logistic diagnostic. Predictions themselves are unchanged.','',
table(boot,c('year','candidate','reference','metric','difference','ci_low','ci_high')),'',
'Negative log-loss/Brier differences favor XGBoost. Mean-error differences describe signed prediction bias and have no universally favorable direction. Intervals use 500 paired whole-game resamples, conditional on all fits, preprocessing, histories and model selection. They omit full training refits and selection uncertainty. 2024 is a secondary descriptive replication and did not select the model.','',
'## Sparse and new hitters','',table(slices[year==2025&dimension%in%c('training_exposure','batter_prior_profile','pitcher_prior_profile')],c('dimension','slice','model','n','log_loss','brier','bias')),'',
'Training exposure refers to observed eligible 2022–23 swings. Prior-profile availability refers to the immediately preceding season. New identities use the established pooled history fallback, while the tree can still respond to their other pitch features. Full count, pitch-family and handedness slices are in slices.csv; they are descriptive and unadjusted for multiple comparisons.','',
'## Verification and limits','',sprintf('%d independent artifact checks, %d small-fixture tests and %d public prediction checks passed.',nrow(checks),nrow(unit),nrow(predchecks)),
'Checks cover source/model hashes, exact GAM training-feature parity, training-only development preprocessing, identical evaluation cohorts, chronology, feature allowlist, early-stopping selection, native serialization, deterministic fixture refitting, corruption of outcomes/labels, invalid core rejection, unseen histories, row/batch invariance, reproduced predictions, metrics, slices, calibration bins and paired bootstrap intervals.','',
'The primary comparison isolates predictive model family under a shared feature contract. It does not test a complete XGBoost Decision+ pipeline, hypothetical swing values on takes, or causal decision talent. BIP/tip/value/take/policy models are outside this experiment.','',
'## Reproduction','', 'From the project root, with existing project data and GAM artifacts available. train.R refuses to overwrite an existing frozen experiment; use a new version directory for a new training experiment.','',
'```sh','Rscript experiments/contact_xgboost_v1/tests.R','Rscript experiments/contact_xgboost_v1/train.R','Rscript experiments/contact_xgboost_v1/evaluate.R','Rscript experiments/contact_xgboost_v1/verify.R','Rscript experiments/contact_xgboost_v1/verify_prediction.R','Rscript experiments/contact_xgboost_v1/report.R','```','',
'Package versions are in sessionInfo.txt. The frozen model is models/contact.ubj; bundle.rds contains the exact preprocessing, schema, histories, parameters and chronology. input_hashes.csv and final_code_hashes.csv record provenance.','',
'[XGBoost parameter documentation](https://xgboost.readthedocs.io/en/stable/parameter.html) · [R training API](https://xgboost.readthedocs.io/en/stable/r_docs/R-package/docs/reference/xgb.train.html)','',
'![Calibration and development curves](comparison.png)')
writeLines(lines,file.path(XR,'REPORT.md'))
png(file.path(XR,'comparison.png'),width=1800,height=1400,res=180)
par(mfrow=c(2,2),mar=c(4.5,4.5,3,1),las=1)
colors<-c(gam_frozen_2021='#777777',gam_prior_season='#2466aa',xgboost_prior_season='#c24e22')
for(yr in 2024:2025){
 plot(NA,xlim=c(0,1),ylim=c(0,1),xlab='Mean predicted probability',ylab='Observed contact frequency',main=paste(yr,'contact calibration'));abline(0,1,col='#bbbbbb',lty=2)
 for(arm in names(colors)){z<-cal[year==yr&model==arm&n>=100];setorder(z,predicted);lines(z$predicted,z$observed,col=colors[arm],type='b',pch=16,cex=.5)}
 legend('topleft',names(colors),col=colors,lty=1,bty='n',cex=.65)
}
curves<-rbindlist(lapply(3:5,function(depth)read(paste0('development_depth_',depth,'.csv'))[,depth:=depth]))
plot(NA,xlim=range(curves$iter),ylim=range(curves$validation_logloss),xlab='Boosting round',ylab='2023 validation log loss',main='Prespecified development candidates')
for(depth_value in 3:5){z<-curves[depth==depth_value];lines(z$iter,z$validation_logloss,col=c('#777777','#2466aa','#c24e22')[depth_value-2])}
legend('topright',paste('Depth',3:5),col=c('#777777','#2466aa','#c24e22'),lty=1,bty='n',cex=.8)
z<-curves[depth==bundle$selected$depth]
matplot(z$iter,z[,.(train_logloss,validation_logloss)],type='l',lty=1,col=c('#2466aa','#c24e22'),xlab='Boosting round',ylab='Contact log loss',main='Selected candidate: train versus validation')
abline(v=bundle$nrounds,lty=2,col='#777777');legend('topright',c('2022 training','2023 validation'),col=c('#2466aa','#c24e22'),lty=1,bty='n',cex=.8)
dev.off()
fwrite(x_hash(list.files(XROOT,pattern='\\.(R|md)$',full.names=TRUE)),file.path(XR,'final_code_hashes.csv'))
cat(verdict,'\n')
