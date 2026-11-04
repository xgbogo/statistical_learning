source("reevaluation/metrics.R")

summary <- readRDS("results/reevaluation/red/metrics_pr_original.rds")
run <- readRDS("results/reevaluation/red/run_pr_original.rds")
stored <- readRDS("results/reevaluation/red/metrics.rds")

stopifnot(nrow(summary) == 105)
stopifnot(all(summary$pr_status == "ok"))
stopifnot(all(is.finite(summary$pr_auc)))
stopifnot(all(summary$max_abs == 0))
stopifnot(sum(summary$exact_first) == 43)
stopifnot(sum(summary$pass == "lecuyer") == 62)
stopifnot(length(run$not_reproducible) == 0)
stopifnot(!"pr_auc" %in% names(stored))

sample <- readRDS("results/reevaluation/red/pr_original/svm_radial__none__2021.rds")
row <- stored[stored$model == "svm_radial" & stored$arm == "none" & stored$seed == 2021, ]
rescored <- score_at_threshold(sample$test$quality, sample$test$.pred_high, row$threshold)
stopifnot(isTRUE(all.equal(rescored$pr_auc, sample$pr_auc)))
stopifnot(abs(rescored$roc_auc - row$roc_auc) == 0)
stopifnot(rescored$tp == row$tp && rescored$fn == row$fn)

original_fit <- readRDS("results/reevaluation/red/svm_radial__none__2021.rds")
stopifnot(is.null(original_fit$test))
stopifnot(identical(names(original_fit), c("key", "metrics")))

guard_now <- sub(
  "^results/reevaluation/",
  "results/reevaluation/red/",
  names(run$guard_md5)
)
current <- tools::md5sum(guard_now)
stopifnot(identical(unname(current), unname(run$guard_md5)))
stopifnot(unname(tools::md5sum("main.pdf")) == "2848ef7280e31c430dfd9f3e1fc66ebb")
