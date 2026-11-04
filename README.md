# Statistical Learning

[IST](https://tecnico.ulisboa.pt/en/)

[MECD](https://fenix.tecnico.ulisboa.pt/cursos/mecd)

[Statistical Learning](https://fenix.tecnico.ulisboa.pt/disciplinas/AEsta4/2019-2020/2-semestre/)

# Grade

[Project](https://github.com/pedrorio/statistical_learning) : 18.0/20.0

## White-wine re-evaluation

White wines are scored on their own. Results, including per-fit files, `failures.tsv` and the stack decision, are written under `results/reevaluation/white/`. Run one split or the full protocol with `Rscript reevaluation/white/fit_white.R`.

One split took 30910.768 seconds. Family fits wall time was 130757.1 seconds. The GAM and stack step took 1763.336 seconds. `failures.tsv` is header only.

Radial SVM, no resampling: mean balanced accuracy 0.80533117138364785 (sd 0.016825021073961128), mean PR AUC 0.76343665515071568 (sd 0.019735982220084852). Random forest, no resampling: balanced accuracy 0.82319673742138366, PR AUC 0.78762255519006574. Score random forest: balanced accuracy 0.82690398977987423, PR AUC 0.76133556019168169.

Stack balanced accuracies: 0.82267099056603776, 0.82990615172955973, 0.84843012971698117, 0.84867580581761004, 0.82127063679245282. Mean 0.83419074292452833. Drop rule keep is FALSE. The stack beat the radial SVM on 5 of 5 (mean gap 0.028859571540880501, sd 0.013710128634384687) and beat the average on 2 of 5 (mean gap 0.00035623034591192315, sd 0.0024684947289231123).

The 140 white family fits are handed to four workers as four fixed chunks of 35. `parallel::parLapply()` and `parallel::parLapplyLB()` both split the list once per core before any load balancing, so with one chunk per core a worker that finishes has nothing else to take. The fits still queued on the slow chunk stay there. The improvement, not yet made, is to send one fit at a time with `parallel::clusterApplyLB()` from `white_mclapply` in `reevaluation/white/fit_white.R`. That does not change the 10x10 CV, the SVM grids, or the cache keys: `fit_white.R` is not part of the hash stored in each fit, and each job already sets its seed before fitting. The process that is running now will not pick this up; it has already handed out the four chunks.
