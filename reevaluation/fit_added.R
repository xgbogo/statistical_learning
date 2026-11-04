source("libraries.R")
library(ranger, warn.conflicts = FALSE)
library(xgboost, warn.conflicts = FALSE)
library(hardhat, warn.conflicts = FALSE)
source("reevaluation/step_winsorise.R")
source("reevaluation/metrics.R")
source("reevaluation/recipes.R")
source("reevaluation/models.R")
source("reevaluation/fit_reevaluation.R")

added_tags <- c("lgb", "xgb_full", "score_rf", "score_lgb", "score_lin")
fits_dir <- reevaluation_dir()

guard_paths <- function() {
  list.files(fits_dir, pattern = "^(lr|svm_poly|svm_radial|lda|fda|rf|xgb)__.*\\.rds$", full.names = TRUE)
}

md5_of <- function(paths) {
  sums <- tools::md5sum(paths)
  stats::setNames(unname(sums), basename(paths))
}

before <- md5_of(guard_paths())
if (length(before) != 105) {
  stop("Expected 105 existing fit files, found ", length(before))
}
graded_before <- unname(tools::md5sum("main.pdf"))

smoke <- fit_reevaluation(
  fits_dir = fits_dir,
  seeds = 2021L,
  model_tags = added_tags,
  metrics_file = "metrics_smoke.rds",
  run_file = "run_smoke.rds",
  write_baseline = FALSE
)
if (smoke$combinations != 7L) {
  stop("Smoke was not the seven new model arms")
}
smoke_metrics <- readRDS(file.path(fits_dir, "metrics_smoke.rds"))
if (any(smoke_metrics$status == "na")) {
  print(smoke_metrics[smoke_metrics$status != "ok", c("model", "arm", "status", "detail")])
  stop("Smoke fit did not score every new arm")
}
for (row in seq_len(nrow(smoke_metrics))) {
  if (!identical(smoke_metrics$status[[row]], "ok")) {
    next
  }
  file <- file.path(
    fits_dir,
    sprintf("%s__%s__2021.rds", smoke_metrics$model[[row]], smoke_metrics$arm[[row]])
  )
  saved <- readRDS(file)
  if (is.null(saved$oof) || is.null(saved$test) || nrow(saved$oof) == 0 || nrow(saved$test) == 0) {
    stop("Smoke file is missing predictions: ", file)
  }
  if (length(intersect(saved$train_index, saved$test_index)) != 0) {
    stop("Smoke split overlaps: ", file)
  }
}

full <- fit_reevaluation(
  fits_dir = fits_dir,
  seeds = 2021:2025,
  model_tags = added_tags,
  metrics_file = "metrics_added.rds",
  run_file = "run_added.rds",
  write_baseline = FALSE
)
if (full$combinations != 35L) {
  stop("The added run was not 35 outer jobs")
}

run <- readRDS(file.path(fits_dir, "run_added.rds"))
run$smoke_seconds <- smoke$wall_seconds
run$total_seconds <- smoke$wall_seconds + full$wall_seconds
saveRDS(run, file.path(fits_dir, "run_added.rds"))

after <- md5_of(guard_paths())
if (!identical(before[names(after)], after) || !identical(names(before), names(after))) {
  stop("An existing fit file changed")
}
graded_after <- unname(tools::md5sum("main.pdf"))
if (!identical(graded_before, graded_after)) {
  stop("main.pdf changed")
}

cat(sprintf(
  "smoke_seconds=%.1f full_seconds=%.1f total_seconds=%.1f cores=%d combinations=%d\n",
  smoke$wall_seconds,
  full$wall_seconds,
  run$total_seconds,
  full$cores,
  full$combinations
))
