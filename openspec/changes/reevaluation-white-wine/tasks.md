# Tasks

## 1. Acceptance tests

- [x] 1.1 Add `tests/test_white_wine_*.R` and show that each fails on the current code because the white-wine feature is missing.

## 2. Code

- [ ] 2.1 Add `utils/white_wine.R` with `recode_white_to_high_low()` and `load_white_recoded_data()`, and source it from `utils/index.R`. Leave `utils/recode_to_high_low.R`, `utils/load_raw_data.R` and `utils/load_recoded_data.R` unedited.
- [ ] 2.2 Add `dataset_csv()`, `reevaluation_dir()` and `reevaluation_source_files()` to `reevaluation/fit_reevaluation.R`, and a `dataset = "red"` argument to `load_recode_and_score()`, `job_key()` and `fit_reevaluation()`. Limit `protected_fit_path()` to red per-fit files.
- [ ] 2.3 Add `dataset = "red"` to `run_gam_and_stack()` in `reevaluation/gam_and_stack.R`, so a white run reads and writes only under `results/reevaluation/white/` and builds its stack from white out-of-fold predictions.
- [ ] 2.4 Add `combination_fits_dir()`, `combination_shared_files()` and a `dataset = "red"` argument to `combination_key()` and `fit_combinations()` in `utils/estimation/fit_combinations.R`.
- [ ] 2.5 Add `reevaluation/white/red_checksums.R` and `reevaluation/white/fit_white.R`.

## 3. Run

- [ ] 3.1 Record red checksums: `Rscript reevaluation/white/red_checksums.R before`.
- [ ] 3.2 Time one split: `Rscript reevaluation/white/fit_white.R one_split`. Report the wall time before starting the full run.
- [ ] 3.3 Full run: `Rscript reevaluation/white/fit_white.R full`, on all five seeds with the 10 by 10 and the full grids. If it is slow, give it more time or cores. Do not reduce the CV or the grids.
- [ ] 3.4 Verify red checksums: `Rscript reevaluation/white/red_checksums.R after`. It must exit 0.

## 4. Write-up

- [ ] 4.1 Write `reevaluation/white/report.Rnw` from white results only, with its tables and figures, the one-split time and `failures.tsv`. Render it with `render_white_reevaluation()` to `build/white/reevaluation_white.pdf`.
- [ ] 4.2 Add the `## White-wine re-evaluation` section to `README.md`.
- [ ] 4.3 Run every test in `tests/` with `Rscript tests/<file>` from the repo root. All must pass, and `main.pdf` must still hash to `2848ef7280e31c430dfd9f3e1fc66ebb`.
