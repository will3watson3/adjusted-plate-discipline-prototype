source('experiments/take_xgboost_v1/functions.R')
dir.create(TR,recursive=TRUE,showWarnings=FALSE)
# Existing focused tests exercise covariance, unknown actors and future labels.
existing <- capture.output(sys.source('tests/test_umpire_v5.R',envir=new.env(parent=globalenv())))
writeLines(existing,file.path(TR,'inherited_actor_tests.txt'))
checks <- list()
check <- function(name,ok) {stopifnot(isTRUE(ok));checks[[name]]<<-data.table(check=name,passed=TRUE)}
d <- t_read(2022,TRUE)[1:300]
b <- readRDS(t_base_path(TRUE)); schema <- t_schema(b); f <- cs_transform(d,b$prep)$data
x <- t_matrix(d,b$prep,schema)
check('Raw location retained',identical(x[,'x'],d$plate_x))
check('Vertical location normalized exactly',identical(x[,'z'],(d$plate_z-d$sz_bot)/(d$sz_top-d$sz_bot)))
for(nm in names(schema$levels)) check(paste('All factor information retained',nm),
  all(rowSums(x[,paste(nm,schema$levels[[nm]],sep='='),drop=FALSE])==1))
z <- copy(d); z[,`:=`(description='hit_into_play',y=1-y,launch_speed=999,launch_angle=999,fielder_2=-999L,batter=-999L,pitcher=-999L)]
check('Actor identities and same-pitch outcomes excluded from matrix',identical(x,t_matrix(z,b$prep,schema)))
z <- copy(d); z[1,`:=`(release_speed=NA_real_,pfx_x=Inf,pitch_family='new_family')]
m <- t_matrix(z,b$prep,schema)
check('Training medians used for missing numeric values',m[1,'speed']==b$prep$medians['speed']&&m[1,'movx']==b$prep$medians['movx'])
check('Novel fixed factor uses identical GAM reference',m[1,paste0('family=',b$prep$references$family)]==1&&cs_transform(z,b$prep)$novel_category[1])
check('Singleton matrix identical to batch',identical(unname(x[1,,drop=FALSE]),unname(t_matrix(d[1],b$prep,schema))))
check('Matrix row-order invariance',identical(x,t_matrix(d[.N:1],b$prep,schema)[nrow(d):1,]))
check('No history features in adopted backbone',!any(grepl('prev|history|first_pitch',schema$features)))
# Calibration must ignore every label in the current block and later dates.
set.seed(TSEED)
z <- data.table(game_year=2024L,game_date=as.Date('2024-03-01')+rep(0:59,each=100),valid=TRUE,take=TRUE,y=rbinom(6000,1,.4))
p <- runif(6000,.1,.8); before <- u5_online(z,p)
cutoff <- as.Date('2024-04-01'); z[game_date>=cutoff,y:=1-y]
after <- u5_online(z,p)
check('Future-label mutation cannot change earlier predictions',identical(before$p[z$game_date<cutoff],after$p[z$game_date<cutoff]))
check('Weekly training endpoints strictly earlier',all(before$ledger$train_end<before$ledger$start,na.rm=TRUE))
check('One-week block labels do not enter its calibration',all(before$ledger$intercept[before$ledger$start<=cutoff]==after$ledger$intercept[after$ledger$start<=cutoff]))
t_write(rbindlist(checks),'unit_tests'); message(length(checks),' focused checks passed; ',tail(existing,1))
