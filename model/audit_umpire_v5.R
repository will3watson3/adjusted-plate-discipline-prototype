# Sensitivity to resampling an entire umpire's evaluation games together.
source('umpire_v5.R')
result<-list()
for(year in 2024:2025) {
 d<-as.data.table(readRDS(file.path(U5_MODELS,sprintf('predictions_%d.rds',year))))
 for(pop in c('all_takes','supported_takes')) {
  z<-d[valid&take & if(pop=='supported_takes') supported else TRUE]
  id<-z$umpire;id[is.na(id)]<-paste0('unassigned_game_',z$game_pk[is.na(id)])
  for(comparator in c('baseline','region_only')) for(method in U5_METHODS) {
   a<-sv_boot(id,cs_loss(z$y,z[[method]])-cs_loss(z$y,z[[comparator]]),B=1000L)
   setnames(a,'games','clusters')
   result[[length(result)+1L]]<-cbind(data.table(year,population=pop,comparator,method),a)
  }
 }
}
u5_write(rbindlist(result),'paired_umpire_clusters')
u5_write(vd_hash('audit_umpire_v5.R'),'audit_hash')
