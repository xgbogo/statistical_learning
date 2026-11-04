# Proposal

## Why

The 2020 conclusion recommends repeating the analysis on other wine types, and `data/winequality-white.csv` is already in the repository but no script reads it. This change runs a white-wine analysis on white wines only. It makes no comparison with red and doesn't change red.

## What Changes

- Run the existing re-evaluation unchanged on white wines: 4898 rows with no empty cells, of which 1060 are high (quality 7 and above).
- **Labelling.** Quality 7 and above is high, everything else is low. The white run uses its own recode, so `utils/recode_to_high_low.R` and the red labels are not touched. The five grade-9 white wines count as high.
- **Dataset argument.** The red CSV path is hardcoded in `utils/estimation/fit_combinations.R` and the re-evaluation entry points. Add a dataset argument that defaults to red, so every red command and cache key stays the same.
- **Same protocol, fixed before any white numbers exist:** 10x10 stratified CV, the full existing grids (polynomial and radial SVM included), the same families plus the GAM and the stack, selection on balanced accuracy with high as the event, cut-offs tuned on out-of-fold scores, paired differences against the radial SVM, and the same drop rule (at least 4 of 5 positive paired splits and a mean above their sd).
- **White only.** Only `data/winequality-white.csv` is read. White writes to `results/reevaluation/white/`. The white analysis (write-up, tables and figures) goes in its own files under white paths and reports white results alone, with no red figures, no red-vs-white comparison and no pooling. No red analysis file is edited or overwritten.
- **No shared intermediate files.** Every white intermediate output (per-fit results, caches, tuned grids, out-of-fold scores, cut-offs) is written under a white path, and its cache key includes the dataset. The white run never reads or writes a red intermediate file.
- **Run time is measured, not guessed.** Before the full run, time one split on white and report it. With about three times the rows, kernel SVM training grows faster than linearly. If the full run is slow, it gets more time or compute. The CV and the grids are never reduced.

## Acceptance

- Red is untouched: red results reproduce within 1e-9 and the `reevaluation-tests` suite stays green.
- Red is untouched: checksums of every red intermediate file (results and caches) and every red analysis file (write-up, tables and figures) match before and after the full white run.
- On white, every label is high or low, with 1060 high and 3838 low.
- The white run writes one results file per family, arm and split, and none of them is an `empty_metrics()` failure that went unreported.
- `main.pdf`, the conclusion, `fit.R` and `utils/recipes/step_recipe.R` are byte-identical.

## Sequencing

Built last, after the fix, the tests, the GAM and stack PR (#5) and the README change, because it touches the same fitting code. This change adds its own short white-wine section to `README.md`.

## Impact

- A white-only recode, a dataset argument, `results/reevaluation/white/`, a separate white write-up with its own tables and figures, and one README section.
- No change to red labels, red results or the graded files.
