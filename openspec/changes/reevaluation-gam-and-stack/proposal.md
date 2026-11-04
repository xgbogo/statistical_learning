# Proposal

## Why

The re-evaluation has no interpretable non-linear model. It also never tests whether combining the top models beats the radial SVM. The radial SVM leads, the random forest is tied with it on paired splits, and the forest on the score is close behind. A GAM shows the shape of each measurement's effect. A stack answers whether the tie hides useful disagreement. Both use the existing five-split protocol.

## What Changes

Size: S. Ten GAM outer jobs, one stack and one simple average per seed. New re-evaluation numbers are added. Existing re-evaluation numbers and graded files do not change.

- **GAM.** `parsnip::gen_additive_mod()` on the mgcv engine, binomial. Every one of the 11 measurements gets a penalised smooth `s(x, k = 5)`, with `select = TRUE` and `method = "REML"`. There is no tuning grid. REML chooses the smoothness on likelihood, and `select = TRUE` can shrink a term to linear or to zero. Arms are class weights and none. There is no ROSE, the same as the other new jobs. The 10 by 10 on the training split supplies out-of-fold probabilities for the cut-off and the reported CV balanced accuracy. The test set is scored once. The report shows partial-effect plots and the effective degrees of freedom per term across the five seeds.
- **Out-of-fold predictions for the two original bases.** The radial SVM and random forest with no resampling saved only test probabilities. Each is rerun through the same 10 by 10 resamples at its stored hyperparameters, with no grid, to save its out-of-fold probabilities. Selection is not repeated, and the 10 by 10 and the grids are not shrunk. A rerun is kept only if its test predictions reproduce the stored test metrics within 1e-9, using the same gate as the PR AUC refit.
- **Stack.** The inputs are the radial SVM (none), the random forest (none), and the random forest on the score, using out-of-fold predictions averaged over the 10 repeats per training row. The blender is penalised logistic regression (glmnet). Its penalty is chosen by its own repeated 10-fold CV on that out-of-fold matrix, on log-loss. Its cut-off comes from the blender's own out-of-fold probabilities, on balanced accuracy. The bases' test predictions are their full-training-split fits, already saved. The blender scores the test set once.
- **Simple average.** The mean of the SVM probability, the forest probability, and the score forest's predicted score mapped to a probability. That mapping is a logistic fit on the out-of-fold scores only. The cut-off comes from out-of-fold predictions.

### Drop rule (fixed before any run)

The stack is kept only if, on test balanced accuracy, it clears both comparisons below:

- against the radial SVM (none)
- against the simple average

For each comparison, at least 4 of the 5 paired split differences must be positive, and the mean difference must be greater than their sample sd. Otherwise the report says the stack is not worth keeping, and its numbers stay only as the evidence for that. ROC AUC and PR AUC are reported but do not decide.

### Invariants

The protocol is unchanged:

- the same five seeds, 2021 to 2025
- the same stratified 80/20 splits
- 10 by 10 CV on the training split only
- out-of-fold cut-offs
- one test score

The existing grids and per-fit files are not edited. `main.pdf`, the graded conclusion, `fit.R`, and `utils/recipes/step_recipe.R` are not edited.

### Out of scope

Spline logistic regression (`step_ns`), stacking more than three bases, TabPFN.

### Runtime

The run should take roughly 9 minutes on 4 cores. That is an estimate from GAM fits timed on the bot's machine (1.3 to 1.8 seconds each, with 100 CV fits and one final fit per outer job, across 10 jobs) and from the recorded SVM and forest timings in `reevaluation-more-models-and-metrics` for the out-of-fold reruns (about 290 CPU seconds). The blender and the average take seconds. The report records the measured time afterwards.

## Capabilities

### New Capabilities

- `gam-and-stack`: penalised GAM, out-of-fold predictions for the two original bases, the stack, the simple average and the pre-registered drop rule, on the existing five-split protocol.

### Modified Capabilities

- None.

## Impact

New code goes under `reevaluation/`. `renv.lock` gains mgcv and glmnet only if they are missing, and locked versions are not bumped. Results go to `results/reevaluation/`, in new files. The write-up stays in `reevaluation/report.Rnw` and gains one line on why the GAM's smoothness is chosen on likelihood rather than balanced accuracy. Graded files are untouched.
