# Acceptance: the finished white run. One file per family, arm and split, every
# failure reported, white keys, the red protocol, and the one-split timing.
# Spec: "White uses the red protocol unchanged", "One white split is timed before the full run",
# "Every white fit is on file and every failure is reported", "White paths and cache keys carry the dataset".
white <- "results/reevaluation/white"
if (!dir.exists(white)) {
  stop("Missing ", white, ": the white run has not happened")
}
source("reevaluation/gam_and_stack.R")

families <- c(
  "lr", "svm_poly", "svm_radial", "lda", "fda", "rf", "xgb",
  "lgb", "xgb_full", "score_rf", "score_lgb", "score_lin"
)
arms <- list(
  lr = c("rose", "weights", "none"), svm_poly = c("rose", "weights", "none"),
  svm_radial = c("rose", "weights", "none"), lda = c("rose", "weights", "none"),
  fda = c("rose", "weights", "none"), rf = c("rose", "weights", "none"),
  xgb = c("rose", "weights", "none"), lgb = c("weights", "none"),
  xgb_full = c("weights", "none"), score_rf = "score", score_lgb = "score",
  score_lin = "score", gam = c("weights", "none")
)
seeds <- 2021:2025
grid <- do.call(rbind, lapply(names(arms), function(model) {
  expand.grid(model = model, arm = arms[[model]], seed = seeds, stringsAsFactors = FALSE)
}))
grid$file <- sprintf("%s__%s__%d.rds", grid$model, grid$arm, grid$seed)
stopifnot(nrow(grid) == 150L)

# The one-split timing.
one <- readRDS(file.path(white, "run_one_split.rds"))
stopifnot(identical(one$seeds, 2021L), identical(one$v, 10L), identical(one$repeats, 10L))
stopifnot(identical(one$combinations, 28L), is.finite(one$wall_seconds), one$wall_seconds > 0)

# The full run record.
run <- readRDS(file.path(white, "run.rds"))
stopifnot(identical(as.integer(run$seeds), seeds), identical(run$v, 10L), identical(run$repeats, 10L))
stopifnot(identical(run$combinations, 140L), identical(sort(run$model_tags), sort(families)))

# Exactly one per-fit file per family, arm and seed.
on_disk <- list.files(white, pattern = "^[a-z_]+__[a-z]+__[0-9]{4}\\.rds$")
stopifnot(identical(sort(on_disk), sort(grid$file)))
for (file in c(sprintf("stack__%d.rds", seeds), sprintf("average__%d.rds", seeds),
               "stack_decision.rds", "metrics.rds", "baseline.rds", "failures.tsv")) {
  stopifnot(file.exists(file.path(white, file)))
}

status <- character(nrow(grid))
for (i in seq_len(nrow(grid))) {
  r <- readRDS(file.path(white, grid$file[[i]]))
  m <- r$metrics
  stopifnot(nrow(m) == 1L, identical(m$model, grid$model[[i]]), identical(m$arm, grid$arm[[i]]))
  stopifnot(identical(as.integer(m$seed), grid$seed[[i]]))
  stopifnot(m$status %in% c("ok", "na", "unsupported"))
  status[[i]] <- m$status
  if (identical(grid$model[[i]], "gam")) {
    stopifnot(identical(r$dataset, "white"))
    next
  }
  k <- r$key
  stopifnot(identical(k$dataset, "white"), identical(k$v, 10L), identical(k$repeats, 10L))
  stopifnot(identical(k$metric, "bal_accuracy"), identical(k$event, "high"))
  stopifnot("data/winequality-white.csv" %in% names(k$sources))
  stopifnot(!"data/winequality-red.csv" %in% names(k$sources))
}

# Every failure is reported in failures.tsv, and nothing else is.
failures <- utils::read.delim(file.path(white, "failures.tsv"), colClasses = "character")
stopifnot(identical(names(failures), c("model", "arm", "seed", "status", "detail")))
bad <- grid[status != "ok", ]
stopifnot(identical(
  sort(paste(failures$model, failures$arm, failures$seed)),
  sort(paste(bad$model, bad$arm, bad$seed))
))
stopifnot(all(failures$status %in% c("na", "unsupported")))

metrics <- readRDS(file.path(white, "metrics.rds"))
stopifnot(nrow(metrics) == 140L)

# The stack follows the red protocol on white out-of-fold predictions.
for (seed in seeds) {
  s <- readRDS(file.path(white, sprintf("stack__%d.rds", seed)))
  stopifnot(identical(s$dataset, "white"))
  if (identical(s$status, "not reproducible")) next
  stopifnot(identical(s$n_test_scored, 1L))
  stopifnot(identical(sort(s$inputs), sort(c("svm_radial__none", "rf__none", "score_rf__score"))))
  stopifnot(all(s$oof_rows %in% s$train_index), length(intersect(s$oof_rows, s$test_index)) == 0)
  a <- readRDS(file.path(white, sprintf("average__%d.rds", seed)))
  stopifnot(identical(a$dataset, "white"))
}
decision <- readRDS(file.path(white, "stack_decision.rds"))
stopifnot(identical(decision$dataset, "white"))
stopifnot(length(decision$stack) == 5, length(decision$svm) == 5, length(decision$average) == 5)
if (!anyNA(decision$stack)) {
  stopifnot(identical(decision$keep, stack_drop_rule(decision$stack, decision$svm, decision$average)$keep))
}

cat("white results: ok\n")
