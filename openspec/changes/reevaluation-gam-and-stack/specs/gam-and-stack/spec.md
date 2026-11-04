# Spec Delta

## Purpose

Add an interpretable penalised GAM and a pre-registered stacked ensemble to the red-wine re-evaluation, on the existing five-split protocol, without changing graded files or existing results.

## ADDED Requirements

### Requirement: A penalised GAM is fit on every measurement
The GAM SHALL be `gen_additive_mod()` on the mgcv engine, binomial, with `s(x, k = 5)` on each of the 11 measurements, `select = TRUE` and `method = "REML"`. It SHALL be fit with training-split class weights and without them. Smoothness SHALL be chosen by REML inside each fit, with no tuning grid. The 10 by 10 on the training split SHALL supply out-of-fold probabilities for the cut-off, and the test set SHALL be scored once.

#### Scenario: Smoothness is not tuned on balanced accuracy
- **WHEN** a GAM outer job runs
- **THEN** no candidate grid is searched, and balanced accuracy is used only to choose the cut-off from out-of-fold probabilities

#### Scenario: Shape is reported
- **WHEN** the report is built
- **THEN** it shows partial-effect plots and the effective degrees of freedom per term for each seed, and one line explains why GAM smoothness is chosen on likelihood

### Requirement: Original bases gain out-of-fold predictions without reselection
The radial SVM and random forest with no resampling SHALL be rerun through the same 10 by 10 resamples at their stored hyperparameters, to save out-of-fold probabilities. No grid SHALL be searched. A rerun SHALL be kept only if its test predictions reproduce the stored test metrics within 1e-9. Existing per-fit files SHALL NOT be changed.

#### Scenario: A rerun that does not reproduce is not stacked
- **WHEN** a rerun misses the stored test metrics on both the first attempt and the original L'Ecuyer stream
- **THEN** that seed's stack and average are reported as not reproducible and not scored

### Requirement: The stack learns only from out-of-fold predictions
The stack inputs SHALL be the radial SVM (none), random forest (none) and random forest on the score, using out-of-fold predictions averaged over the 10 repeats per training row. The blender SHALL be penalised logistic regression with its penalty chosen by its own repeated 10-fold CV on that matrix, on log-loss.

#### Scenario: The blender never sees test rows
- **WHEN** the blender penalty is chosen
- **THEN** only training-split out-of-fold predictions are read

### Requirement: The stack's cut-off and test score follow the protocol
The stack's cut-off SHALL come from the blender's own out-of-fold probabilities, on balanced accuracy. Test inputs SHALL be the bases' full-training-split test predictions, and the stack SHALL score the test set once.

#### Scenario: The test set is scored once
- **WHEN** the stack is evaluated
- **THEN** the cut-off is fixed before the test predictions are read, and the test set is scored once

### Requirement: A simple average is the stack's baseline
The average SHALL be the mean of the SVM probability, the forest probability, and the score forest's predicted score mapped to a probability. That mapping SHALL be a logistic fit on out-of-fold scores only. Its cut-off SHALL come from out-of-fold predictions.

#### Scenario: The average is scored like every other model
- **WHEN** the average is evaluated
- **THEN** it reports balanced accuracy, ROC AUC, PR AUC, precision and recall at its out-of-fold cut-off

### Requirement: The drop rule is fixed before running
The stack SHALL be kept only if, on test balanced accuracy, it clears both comparisons: one against the radial SVM (none) and one against the simple average. For each comparison, at least 4 of the 5 paired split differences SHALL be positive, and the mean difference SHALL exceed their sample sd. Otherwise the report SHALL say the stack is not worth keeping.

#### Scenario: A small gain that straddles zero is dropped
- **WHEN** the stack's mean gain over either reference does not exceed the sd of the paired differences, or fewer than 4 splits are positive
- **THEN** the report says the stack is not worth keeping

### Requirement: The existing protocol and graded files stay
The five seeds 2021 to 2025, the stratified 80/20 splits, the training-only 10 by 10, out-of-fold cut-offs and one test score SHALL stay. Existing grids and per-fit files SHALL NOT be edited. This change SHALL NOT edit `main.pdf`, the graded conclusion, `fit.R`, or `utils/recipes/step_recipe.R`.

#### Scenario: Graded artifacts are untouched
- **WHEN** this change has been implemented
- **THEN** the committed `main.pdf` hash is unchanged and `fit.R` and `utils/recipes/step_recipe.R` have no edits
