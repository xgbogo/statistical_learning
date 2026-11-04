# Acceptance: penalised GAM, no grid, effective degrees of freedom per term.
source("reevaluation/gam_and_stack.R")
source("libraries.R")
source("reevaluation/step_winsorise.R")
source("reevaluation/metrics.R")
source("reevaluation/recipes.R")
source("reevaluation/models.R")

measurements <- c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
  "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH",
  "sulphates", "alcohol")

gam <- reevaluation_models()$gam
stopifnot(identical(gam$model$engine, "mgcv"))
stopifnot(is.null(gam$grid), is.null(gam$grid_fn))
stopifnot(length(tune::tune_args(gam$model)$id) == 0)
stopifnot(identical(gam$arms, c("weights", "none")))

for (arm in c("weights", "none")) {
  for (seed in 2021:2025) {
    r <- readRDS(sprintf("results/reevaluation/gam__%s__%d.rds", arm, seed))
    stopifnot(identical(sort(names(r$metrics$edf[[1]])), sort(make.names(measurements))))
    stopifnot(all(r$metrics$edf[[1]] >= 0))
    stopifnot(!is.null(r$oof), !is.null(r$test))
    stopifnot(length(intersect(r$train_index, r$test_index)) == 0)
    # Cut-off comes from OOF probabilities, not the test set.
    t <- choose_threshold(r$oof$quality, r$oof$.pred_high)
    stopifnot(isTRUE(all.equal(t, r$metrics$threshold)))
  }
}
cat("gam: ok\n")
