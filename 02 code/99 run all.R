# 99 run all.R
#
# Runs the whole pipeline: every numbered script in this directory, in
# ascending order, each in a fresh R session, stopping at the first
# failure. The number is the run order.
#
# Before running, set dir_root at the top of "00 config.R" - the raw
# survey data is not included in this repository (see 00 config.R and the
# project README).
#
# Run this from inside "02 code/":  Rscript "99 run all.R"

### 1. SCRIPTS -----------------------------------------------------------
# The run order is simply this directory sorted ascending.

dir_code <- getwd()

scripts <- c(
  "01 build respondents.R",
  "02 build dyads.R",
  "03 build gn edgelist.R",
  "10 descriptives - origins and labels.R",
  "11 descriptives - setting figures.R",
  "12 descriptives - batch composition.R",
  "13 descriptives - sample and firms.R",
  "14 descriptives - recall.R",
  "15 descriptives - livelihoods.R",
  "16 descriptives - ties and dyads.R",
  "17 pipeline numbers.R",
  "20 analysis - balance.R",
  "21 analysis - channels.R",
  "22 analysis - layer design.R",
  "23 analysis - multiplexity.R",
  "24 analysis - test audit.R")

stopifnot(identical(scripts, sort(scripts)))
if (!file.exists(file.path(dir_code, "00 config.R"))) {
  stop("Run this from inside \"02 code/\" so the scripts can find ",
       "\"00 config.R\".", call. = FALSE)
}

### 2. RUN -----------------------------------------------------------
# Each script in a fresh R session; stop at the first failure.

rscript <- file.path(R.home("bin"), "Rscript")
t0 <- Sys.time()
for (s in scripts) {
  cat("\n=====", s, "=====\n")
  status <- system2(rscript, args = shQuote(file.path(dir_code, s)))
  if (status != 0) {
    stop("FAILED: ", s, " (exit status ", status, ")", call. = FALSE)
  }
}
cat("\nPipeline complete in",
    round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    "minutes.\n")
