# Run from repository root. No data downloads or fits happen here.
if (!file.exists('model/current_hitter_stat.R')) stop('Run from repository root')
lib <- file.path(getwd(), 'model', '.R-library')
dir.create(lib, recursive=TRUE, showWarnings=FALSE)
.libPaths(c(lib, .libPaths()))
minimum <- c(data.table='1.16.0', mgcv='1.9.1', glmnet='4.1.8', baseballr='2.0.0')
if (getRversion() < '4.4.0') stop('Use R >= 4.4')
for (p in names(minimum)) {
  if (!p %in% rownames(installed.packages()) || packageVersion(p) < minimum[[p]]) {
    install.packages(p, lib=lib, repos='https://cloud.r-project.org')
  }
}
for (p in names(minimum)) {
  if (!requireNamespace(p, quietly=TRUE) || packageVersion(p) < minimum[[p]]) stop('Dependency version too old: ',p)
  cat(p, as.character(packageVersion(p)), '\n')
}
dir.create('model/reports/reproduction', recursive=TRUE, showWarnings=FALSE)
writeLines(capture.output(sessionInfo()), 'model/reports/reproduction/resolved_session.txt')
cat('Dependencies ready. Next: python3 scripts/check.py\n')
