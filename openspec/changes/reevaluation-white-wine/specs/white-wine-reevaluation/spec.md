# Spec Delta

## Purpose

Run the existing re-evaluation on white wines only, behind a dataset argument that defaults to red. Red labels, red results, red analysis files and graded files do not change.

## ADDED Requirements

### Requirement: White labels come from their own recode
`utils/white_wine.R` SHALL define `recode_white_to_high_low(data, factor_name)`. It SHALL return a factor with levels `c("low", "high")`, where quality 7 and above is `high` and everything else is `low`. It SHALL define `load_white_recoded_data(location = "data/winequality-white.csv", factor_name = "quality")`, which reads the white CSV with `load_raw_data()`, sets `type` to `"white"` on every row and recodes `quality` with `recode_white_to_high_low()`. `utils/index.R` SHALL source `utils/white_wine.R`, so `source("libraries.R")` defines both.

#### Scenario: White counts (`tests/test_white_wine_recode.R`)
- **WHEN** `load_white_recoded_data()` is called
- **THEN** it returns 4898 rows with no missing value, every label is `high` or `low`, 1060 are `high` and 3838 are `low`, and the five grade-9 wines are `high`

### Requirement: Red labels and the red recode stay as they are
`utils/recode_to_high_low.R` SHALL NOT be edited. Its git blob hash SHALL stay `1a43af38e03ac974a7df242793c6faf4f8c2a5ec`. `load_recoded_data("data/winequality-red.csv", "quality")` SHALL keep levels `c("low", "high")` with 217 `high` and 1382 `low`.

#### Scenario: Red recode (`tests/test_white_wine_recode.R`)
- **WHEN** the red CSV is loaded and recoded
- **THEN** the recode file's blob hash and the red label counts are unchanged

### Requirement: The dataset argument defaults to red
These functions SHALL take `dataset = "red"`: `load_recode_and_score(path = dataset_csv(dataset), dataset = "red")`, `job_key(job, source_md5, v, repeats, dataset = "red")` and `fit_reevaluation(..., dataset = "red")` in `reevaluation/fit_reevaluation.R`; `run_gam_and_stack(fits_dir = reevaluation_dir(dataset), dataset = "red")` in `reevaluation/gam_and_stack.R`; and `combination_key(model_tag, recipe_tag, shared_md5, dataset = "red")` and `fit_combinations(dataset = "red")` in `utils/estimation/fit_combinations.R`. `dataset` SHALL accept only `"red"` and `"white"`. Called without `dataset`, each SHALL behave as it does today. The red `job_key()` and `combination_key()` SHALL return the same list as today for the same inputs, with no `dataset` field. `source_files` and `shared_source_files` SHALL keep their current values, so the red keys hash the same file list. Editing a file in that list changes its md5, as any code edit does.

#### Scenario: Red keys and paths (`tests/test_white_wine_dataset_arg.R`)
- **WHEN** a red key is built with and without `dataset = "red"`
- **THEN** both equal today's key, `reevaluation_dir()` is `results/reevaluation` and `combination_fits_dir()` is `results/fits`

#### Scenario: Red entry point (`tests/test_white_wine_entry_points.R`)
- **WHEN** `fit_reevaluation(seeds = 2021L, model_tags = character(0))` runs in a copy of the repo
- **THEN** it writes `metrics.rds`, `run.rds` and `baseline.rds` to `results/reevaluation/` and creates no `results/reevaluation/white/`

### Requirement: White paths and cache keys carry the dataset
`dataset_csv("white")` SHALL be `data/winequality-white.csv`. `reevaluation_dir("white")` SHALL be `results/reevaluation/white`. `combination_fits_dir("white")` SHALL be `results/reevaluation/white/fits`. `reevaluation_source_files("white")` SHALL be `source_files` with `utils/white_wine.R` and `data/winequality-white.csv` in place of `data/winequality-red.csv`. `combination_shared_files("white")` SHALL be `shared_source_files` with `utils/white_wine.R` and `data/winequality-white.csv` in place of `utils/load_recoded_data.R`, `utils/recode_to_high_low.R` and `data/winequality-red.csv`. A white `job_key()` or `combination_key()` SHALL be the red key with `dataset = "white"` appended as its last field. `protected_fit_path()` SHALL be `TRUE` only for red per-fit files, so white files for the original models can be created. Every white per-fit file SHALL store its key. The GAM, stack, average and decision files SHALL store `dataset = "white"`.

#### Scenario: White keys and paths (`tests/test_white_wine_dataset_arg.R`)
- **WHEN** white keys, directories and source lists are built
- **THEN** they match the values above, and `protected_fit_path("results/reevaluation/white/lr__none__2021.rds")` is `FALSE`

### Requirement: The white run reads only white inputs
A white run SHALL read only `data/winequality-white.csv` and files under `results/reevaluation/white/`. It SHALL NOT open the red CSV or any red intermediate file, and SHALL NOT write outside the white paths. There is no red comparison and no pooling.

#### Scenario: No red CSV present (`tests/test_white_wine_entry_points.R`)
- **WHEN** `fit_reevaluation(dataset = "white", seeds = 2021L, model_tags = character(0))` runs in a copy of the repo with no `data/winequality-red.csv` and no red results
- **THEN** it succeeds, every file it writes is under `results/reevaluation/white/`, and the baseline test split has more than 900 rows

### Requirement: White uses the red protocol unchanged
The white run SHALL use seeds 2021 to 2025, stratified 80/20 outer splits, 10 by 10 stratified CV on the training split, the existing grids including the polynomial and radial SVM, the 12 families `lr`, `svm_poly`, `svm_radial`, `lda`, `fda`, `rf`, `xgb`, `lgb`, `xgb_full`, `score_rf`, `score_lgb`, `score_lin` with their red arms, the GAM and the stack, selection on balanced accuracy with `high` as the event, cut-offs tuned on out-of-fold scores, paired differences against the radial SVM (none), and `stack_drop_rule()`. The stack inputs SHALL be white out-of-fold predictions of `svm_radial__none`, `rf__none` and `score_rf__score`. The CV and grids SHALL NOT be reduced.

#### Scenario: Run record (`tests/test_white_wine_results.R`)
- **WHEN** `results/reevaluation/white/run.rds` is read
- **THEN** it records seeds 2021 to 2025, `v = 10`, `repeats = 10`, 140 combinations and the 12 families, and `stack_decision.rds` matches `stack_drop_rule()` on its own values

### Requirement: One white split is timed before the full run
`Rscript reevaluation/white/fit_white.R one_split` SHALL run seed 2021 for the 12 families and write `results/reevaluation/white/run_one_split.rds` with `seeds = 2021L`, `v = 10L`, `repeats = 10L`, `combinations = 28L` and a positive `wall_seconds`. `Rscript reevaluation/white/fit_white.R full` SHALL then run all five seeds and the GAM and stack.

#### Scenario: Timing recorded (`tests/test_white_wine_results.R`)
- **WHEN** the white run is finished
- **THEN** `run_one_split.rds` exists with the values above

### Requirement: Every white fit is on file and every failure is reported
`results/reevaluation/white/` SHALL hold exactly one `{model}__{arm}__{seed}.rds` file for each of the 150 red family, arm and seed combinations (140 fits and 10 GAM fits), plus `stack__{seed}.rds` and `average__{seed}.rds` per seed, `stack_decision.rds`, `metrics.rds` and `baseline.rds`. `results/reevaluation/white/failures.tsv` SHALL list, with columns `model`, `arm`, `seed`, `status` and `detail`, exactly the per-fit files whose status is not `ok`. It SHALL have a header even when empty.

#### Scenario: Files and failures (`tests/test_white_wine_results.R`)
- **WHEN** the white directory is listed
- **THEN** the per-fit files match the 150 names, each key has `dataset = "white"` and hashes the white CSV, and `failures.tsv` matches the non-`ok` files

### Requirement: Red files are checksummed before and after the white run
`reevaluation/white/red_checksums.R` SHALL define `red_checksum_paths()` and `write_red_checksums(file)`, using base R only. `red_checksum_paths()` SHALL return, relative to the working directory and sorted in C-locale order, every file under `results/reevaluation/` except `results/reevaluation/white/`, every file under `results/fits/` and `build/` except `build/white/`, plus `reevaluation/report.Rnw`, `reevaluation/gam_stack_decisions.tsv`, `data/winequality-red.csv` and `main.pdf`. Missing `results/fits/` or `build/` SHALL be skipped. `write_red_checksums(file)` SHALL write one line `<md5>  <path>` per path. `Rscript reevaluation/white/red_checksums.R before` SHALL write `results/reevaluation/white/red_checksums_before.txt`. `... after` SHALL write `red_checksums_after.txt` and exit non-zero if it differs from the before file.

#### Scenario: Manifest tool (`tests/test_white_wine_red_checksums.R`)
- **WHEN** the tool runs in a scratch tree and a red file is then changed
- **THEN** the paths and lines match the rule above, `after` exits 0 on an unchanged tree and non-zero after the change

#### Scenario: Red untouched by the run (`tests/test_white_wine_red_untouched.R`)
- **WHEN** the full white run is finished
- **THEN** both manifests exist, are identical, and cover every red per-fit file

### Requirement: The white analysis lives in its own files
The white write-up SHALL be `reevaluation/white/report.Rnw`, rendered by `render_white_reevaluation()` in `reevaluation/white/render.R` to `build/white/reevaluation_white.pdf`, with figures under `build/white/figures/`. It SHALL read results only from `results/reevaluation/white/`, SHALL NOT name the red CSV or red figures, and SHALL report the one-split time from `run_one_split.rds` and the failures from `failures.tsv`. No red analysis file SHALL be edited or overwritten.

#### Scenario: White-only write-up (`tests/test_white_wine_report.R`)
- **WHEN** `reevaluation/white/report.Rnw` is read
- **THEN** every `results/reevaluation` path in it is under `results/reevaluation/white/`, and it names `run_one_split.rds` and `failures.tsv`

### Requirement: The README gains a white-wine section
`README.md` SHALL have one `## White-wine re-evaluation` section that names `results/reevaluation/white/` and `Rscript reevaluation/white/fit_white.R`.

#### Scenario: README (`tests/test_white_wine_report.R`)
- **WHEN** `README.md` is read
- **THEN** it has exactly one such heading and the section names both

### Requirement: Red results reproduce and the existing tests stay green
Red results SHALL reproduce within 1e-9 and every existing test in `tests/` SHALL still pass.

#### Scenario: Existing suite
- **WHEN** each existing `tests/*.R` is run with `Rscript` from the repo root
- **THEN** it exits 0

### Requirement: Graded files are byte-identical
`main.pdf` SHALL keep md5 `2848ef7280e31c430dfd9f3e1fc66ebb`. `conclusion/conclusion.Rnw`, `fit.R` and `utils/recipes/step_recipe.R` SHALL keep git blob hashes `9a2c4f4078efa5dbe557fa81452c9a6b3140159a`, `9b6d84a119dcf78d7ece361380ae5fecb5bd7095` and `4e7c2c1b5d40268db23211460afc162c8680c2dc`.

#### Scenario: Graded files (`tests/test_white_wine_red_untouched.R`)
- **WHEN** the full white run is finished
- **THEN** the hashes match
