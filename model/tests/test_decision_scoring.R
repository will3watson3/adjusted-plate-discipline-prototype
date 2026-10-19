source("score_decisions.R")
setDTthreads(2)
fails <- function(expr) inherits(tryCatch(force(expr),error=identity),"error")
models <- load_decision_models()
saved <- as.data.table(readRDS(file.path(PC_MODELS,"swing_values_2024.rds")))
take <- as.data.table(readRDS("data/processed/statcast_2024_take_values.rds"))
# Same physical features, different recorded action: only metadata may change.
ix <- unique(c(1:6,which(saved$state_value_order_warning)[1:3]))
original <- saved[ix]; d <- copy(original)
d[,`:=`(game_year=2024L,analysis_eligible=TRUE)]
d[1:3,description:=DV_TAKES]
sw <- original[4:9]
pop <- decision_population(d)
out <- score_decision_population(pop,sw,take,models)
stopifnot(identical(out[,..CT_KEYS],d[,..CT_KEYS]),sum(out$observed_action=="take")==3L,
  max(abs(out$q_swing_basic-original$q_swing_basic))<1e-10,
  all(out$swing_evaluation_scope[1:3]=="hypothetical_swing_on_observed_take"),
  identical(out$state_value_order_warning,original$state_value_order_warning))
numeric_cols <- c("p_contact_given_swing","q_swing_basic","q_take_basic","swing_minus_take")
poison <- copy(pop)[,`:=`(events="home_run",launch_speed=120,launch_angle=30,post_bat_score=999)]
poison[1:3,description:=rev(DV_TAKES)]
again <- score_decision_population(poison,sw,take,models)
stopifnot(identical(out[,..numeric_cols],again[,..numeric_cols]))
reversed <- score_decision_population(pop[9:1],sw[6:1],take,models)
stopifnot(max(abs(reversed$q_swing_basic-rev(out$q_swing_basic)))<1e-10)
excluded <- copy(d[1]);excluded[,`:=`(description="hit_by_pitch",analysis_eligible=FALSE)]
stopifnot(nrow(decision_population(excluded))==0L)
stopifnot(fails(decision_population(copy(excluded)[,analysis_eligible:=TRUE])),
  fails(decision_population(copy(d)[,game_year:=2025L])),
  fails(decision_population(rbind(d,d[1]))),
  fails(score_decision_population(pop,sw[-1],take,models)))
changed <- copy(pop);changed[4,release_speed:=release_speed+1]
stopifnot(fails(score_decision_population(changed,sw,take,models)))
invalid <- copy(pop);invalid[1,plate_x:=NA_real_]
bad <- score_decision_population(invalid,sw,take,models)
stopifnot(is.na(bad$q_swing_basic[1]),bad$decision_value_status[1]=="invalid_swing_value")
only_take <- score_decision_population(pop[1:3],sw[0],take,models)
only_swing <- score_decision_population(pop[4:9],sw,take,models)
stopifnot(nrow(only_take)==3L,nrow(only_swing)==6L,
  identical(only_swing$q_swing_basic,sw$q_swing_basic))
cat("Decision scoring tests passed: both actions, unchanged swing values, label independence, input guards and invalid inputs.\n")
