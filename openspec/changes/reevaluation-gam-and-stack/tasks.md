# Tasks

- [x] Add the GAM spec (mgcv, `s(x, k = 5)` on all 11, `select = TRUE`, REML) with weights and none arms. Run 10 outer jobs and save out-of-fold and test probabilities.
- [x] Rerun the radial SVM and random forest (none) at their stored hyperparameters through the same 10 by 10 to save out-of-fold probabilities. Apply the 1e-9 test-reproduction gate.
- [x] Build the repeat-averaged out-of-fold matrix. Fit the glmnet blender with its own CV (log-loss), take its out-of-fold cut-off, and score the test set once.
- [x] Build the simple average, with the score mapped to a probability on out-of-fold scores, take its out-of-fold cut-off, and score the test set once.
- [x] Apply the drop rule against the radial SVM and the average, and report the paired differences and the verdict.
- [x] Add the GAM partial-effect plots, the effective degrees of freedom per term and the one-line likelihood note to `reevaluation/report.Rnw`.
- [x] Confirm the `main.pdf` hash is unchanged and `fit.R` and `utils/recipes/step_recipe.R` have no edits.

## Acceptance tests (written first, by the non-builder)

The tests in `tests/test_gam_stack_*.R` define the interface the build must provide. `reevaluation/gam_and_stack.R` must be self-contained, sourcing its own dependencies, and must export:

- `reproduces_stored(stored_row, refit, tol = 1e-9)`
- `average_oof_repeats(oof)`
- `fit_blender(oof_x, oof_y, test_x, seed)`, which returns `penalty`, `threshold`, `test_pred` and `n_test_scored`
- `stack_drop_rule(stack, svm, average)`, which returns `keep` plus `positive`, `mean` and `sd` for each reference

It also adds `reevaluation_models()$gam` and writes these files under `results/reevaluation/`:

- `gam__{arm}__{seed}.rds`, with `metrics$edf` per term
- `oof_rerun/{model}__none__{seed}.rds`
- `stack__{seed}.rds`
- `average__{seed}.rds`
- `stack_decision.rds`

Run each test with `Rscript tests/<file>` from the repo root. All of them must pass before the change is archived.
