# Acceptance for fix-failed-fit-recording: the stored guess-low PR AUC is the
# test prevalence 44/321, the value the report prints, not 0.569.
run <- readRDS("results/reevaluation/red/run_pr_original.rds")
stopifnot(length(run$guess_low_pr_auc) == 5)
stopifnot(isTRUE(all.equal(run$guess_low_pr_auc, rep(44 / 321, 5))))
message("guess-low stored PR AUC: ok")
