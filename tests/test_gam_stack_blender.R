# Acceptance: the blender learns only from OOF data and scores the test set once.
source("reevaluation/gam_and_stack.R")
source("reevaluation/metrics.R")

# Repeats are averaged to one prediction per training row.
oof <- data.frame(.row = rep(1:4, times = 10), id = rep(sprintf("Repeat%02d", 1:10), each = 4),
  .pred_high = rep(c(0.1, 0.2, 0.7, 0.9), times = 10) + rep(seq(-0.045, 0.045, length.out = 10), each = 4))
avg <- average_oof_repeats(oof)
stopifnot(nrow(avg) == 4, identical(sort(avg$.row), 1:4))
stopifnot(isTRUE(all.equal(avg$.pred_high[order(avg$.row)], c(0.1, 0.2, 0.7, 0.9))))

set.seed(1)
n <- 300
y <- factor(ifelse(runif(n) < 0.14, "high", "low"), levels = c("high", "low"))
x <- data.frame(svm = ifelse(y == "high", 0.6, 0.2) + rnorm(n, 0, 0.15),
  rf = ifelse(y == "high", 0.55, 0.25) + rnorm(n, 0, 0.15),
  score_rf = ifelse(y == "high", 6.8, 5.6) + rnorm(n, 0, 0.4))
test_a <- x[1:50, ] + 0
test_b <- test_a; test_b[] <- lapply(test_b, function(v) rev(v) * 3)

a <- fit_blender(x, y, test_a, seed = 2021)
b <- fit_blender(x, y, test_b, seed = 2021)
# Penalty and cut-off do not depend on the test inputs.
stopifnot(identical(a$penalty, b$penalty), identical(a$threshold, b$threshold))
stopifnot(identical(a$n_test_scored, 1L))
stopifnot(length(a$test_pred) == nrow(test_a))
# They do depend on the OOF data.
x2 <- x; x2$svm <- sample(x2$svm)
c2 <- fit_blender(x2, y, test_a, seed = 2021)
stopifnot(!identical(c(a$penalty, a$threshold), c(c2$penalty, c2$threshold)))

for (seed in 2021:2025) {
  s <- readRDS(sprintf("results/reevaluation/stack__%d.rds", seed))
  if (identical(s$status, "not reproducible")) next
  stopifnot(identical(s$n_test_scored, 1L))
  stopifnot(identical(sort(s$inputs), sort(c("svm_radial__none", "rf__none", "score_rf__score"))))
  stopifnot(all(s$oof_rows %in% s$train_index), length(intersect(s$oof_rows, s$test_index)) == 0)
  for (name in c("stack", "average")) {
    m <- readRDS(sprintf("results/reevaluation/%s__%d.rds", name, seed))$metrics
    stopifnot(all(c("bal_accuracy", "roc_auc", "pr_auc", "precision", "recall", "threshold") %in% names(m)))
  }
}
cat("blender: ok\n")
