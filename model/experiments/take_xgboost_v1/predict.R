source('experiments/take_xgboost_v1/functions.R')
# Saved pre-date weekly ledgers make this API independent of batch labels/order.
# These are historical replay predictions, restricted to audited 2024/2025 dates.
predict_take_comparison <- function(pitches, arm=c('xgboost','gam')) {
  arm <- match.arg(arm); d <- copy(as.data.table(pitches))
  year <- unique(d$game_year); stopifnot(length(year)==1L,year%in%2024:2025)
  if(!'description'%in%names(d)) d[,description:=NA_character_]
  bundle <- readRDS(file.path(TM,'bundle.rds'))
  eta <- if(arm=='gam') fr_base(readRDS(t_base_path()),d,se=FALSE)$eta else
    t_eta(xgb.load(file.path(TM,'take.ubj')),d,bundle)
  actor <- readRDS(file.path(TM,sprintf('final_%s_actors_%d.rds',arm,year)))
  p <- u5_predict(actor,t_actor_data(d,eta,bundle$prep))
  ledger <- fread(file.path(TR,sprintf('weekly_%s_%d.csv',arm,year)))
  anchor <- as.Date(sprintf('%d-01-01',year))
  block <- anchor+7L*(as.integer(as.Date(d$game_date)-anchor)%/%7L)
  k <- match(as.character(block),as.character(ledger$start)); stopifnot(!anyNA(k))
  data.table(base=plogis(eta),actor=p$p,
    complete=plogis(qlogis(pmin(1-1e-8,pmax(1e-8,p$p)))+ledger$intercept[k]))
}
