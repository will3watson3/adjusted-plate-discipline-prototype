source('experiments/bip_value_profiles_v1/functions.R')
dir.create(BR,recursive=TRUE,showWarnings=FALSE)
checks <- list()
check <- function(n,x){stopifnot(isTRUE(x));checks[[n]]<<-data.table(check=n,passed=TRUE)}
# One missing label for each metric; the missing observations differ.
d <- data.table(batter=c(1,1,1,2,2,5),pitcher=c(3,3,3,4,4,6),p_throws='R',stand='L',
  pitch_family=c('Fastball','Fastball','Breaking','Fastball','Breaking','Fastball'),in_play=TRUE,season=2021,
  launch_speed_angle=c(6,1,NA,NA,NA,6),bb_type=c('ground_ball',NA,'fly_ball',NA,NA,'ground_ball'))
for(role in c('batter','pitcher'))for(metric in c('barrel','gb')) {
  p <- bp_rates(d,role,metric);id <- if(role=='batter')1 else 3
  j <- match(id,p$overall[[role]])
  league <- 2/3;parent <- (1+50*league)/52
  check(paste(role,metric,'missing observations excluded'),p$overall$n[j]==2&&p$overall$total[j]==1&&p$league==league)
  known <- copy(d[1]);look <- bp_lookup(known,p,role,'rate')
  den <- if(metric=='barrel')2 else 1
  check(paste(role,metric,'two-stage shrinkage'),abs(look$value-(1+30*parent)/(den+30))<1e-14)
  missing <- copy(known);missing[,pitch_family:='unknown']
  check(paste(role,metric,'missing split parent fallback'),bp_lookup(missing,p,role,'rate')$value==parent)
  unseen <- copy(known);unseen[,(role):=-999L]
  check(paste(role,metric,'unseen league fallback'),bp_lookup(unseen,p,role,'rate')$value==league)
  zero <- copy(d[4]);check(paste(role,metric,'zero-denominator player fallback'),abs(bp_lookup(zero,p,role,'rate')$value-league)<1e-14)
}
bp_write(rbindlist(checks),'unit_tests');cat(length(checks),'focused tests passed\n')
