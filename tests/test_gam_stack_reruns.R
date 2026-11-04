# Acceptance: original SVM and RF gain OOF predictions only behind the 1e-9 gate.
source("reevaluation/gam_and_stack.R")

stored <- readRDS("results/reevaluation/metrics.rds")
row <- stored[stored$model == "svm_radial" & stored$arm == "none" & stored$seed == 2021, ]
same <- row[, c("roc_auc", "bal_accuracy", "accuracy", "tp", "fp", "tn", "fn")]
stopifnot(isTRUE(reproduces_stored(row, same, tol = 1e-9)))
off <- same; off$roc_auc <- off$roc_auc + 1e-8
stopifnot(isFALSE(reproduces_stored(row, off, tol = 1e-9)))
off <- same; off$tp <- off$tp + 1L
stopifnot(isFALSE(reproduces_stored(row, off, tol = 1e-9)))

for (model in c("svm_radial", "rf")) {
  for (seed in 2021:2025) {
    r <- readRDS(sprintf("results/reevaluation/oof_rerun/%s__none__%d.rds", model, seed))
    if (isTRUE(r$pass)) {
      stopifnot(r$max_abs <= 1e-9)
      stopifnot(!is.null(r$oof), nrow(r$oof) > 0)
      stopifnot(identical(r$params, stored$params[stored$model == model & stored$arm == "none" & stored$seed == seed]))
    } else {
      stopifnot(identical(r$status, "not reproducible"), is.null(r$oof))
      s <- readRDS(sprintf("results/reevaluation/stack__%d.rds", seed))
      stopifnot(identical(s$status, "not reproducible"))
    }
  }
}

# Existing per-fit files are not rewritten.
before <- system("git diff --quiet b86768e -- results/reevaluation/svm_radial__none__2021.rds results/reevaluation/rf__none__2021.rds", ignore.stdout = TRUE)
stopifnot(before == 0)
cat("reruns: ok\n")
