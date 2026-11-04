# Acceptance: the pre-registered drop rule (spec: "The drop rule is fixed before running").
source("reevaluation/gam_and_stack.R")

svm <- c(0.84, 0.85, 0.83, 0.86, 0.85)
avg <- c(0.83, 0.84, 0.83, 0.85, 0.84)

# Clear, consistent gain over both references: keep.
keep <- stack_drop_rule(stack = svm + c(0.02, 0.03, 0.02, 0.025, 0.03), svm = svm, average = avg)
stopifnot(isTRUE(keep$keep))

# Only 3 of 5 splits positive against the SVM: drop, even with a positive mean.
three <- stack_drop_rule(stack = svm + c(0.05, 0.04, 0.03, -0.01, -0.01), svm = svm, average = avg - 0.05)
stopifnot(identical(three$svm$positive, 3L))
stopifnot(isFALSE(three$keep))

# 4 of 5 positive but mean gain not above the sd of the differences: drop.
noisy <- stack_drop_rule(stack = svm + c(0.001, 0.001, 0.001, 0.001, -0.02), svm = svm, average = avg - 0.05)
stopifnot(noisy$svm$positive >= 4L)
stopifnot(noisy$svm$mean <= noisy$svm$sd)
stopifnot(isFALSE(noisy$keep))

# Beats the SVM but not the simple average: drop.
vs_avg <- stack_drop_rule(stack = svm + 0.02 + c(0, 0.001, 0.002, 0.003, 0.004), svm = svm, average = svm + 0.03)
stopifnot(isTRUE(vs_avg$svm$mean > vs_avg$svm$sd))
stopifnot(isFALSE(vs_avg$keep))

# sd is the sample sd of the paired differences.
d <- c(0.02, 0.03, 0.02, 0.025, 0.03)
stopifnot(isTRUE(all.equal(keep$svm$sd, sd(d))))

# The recorded verdict matches the rule applied to the saved test balanced accuracy.
decision <- readRDS("results/reevaluation/stack_decision.rds")
stopifnot(length(decision$stack) == 5, length(decision$svm) == 5, length(decision$average) == 5)
stopifnot(identical(decision$keep, stack_drop_rule(decision$stack, decision$svm, decision$average)$keep))
cat("drop rule: ok\n")
