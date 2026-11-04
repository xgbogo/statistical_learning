# Statistical Learning

[IST](https://tecnico.ulisboa.pt/en/)

[MECD](https://fenix.tecnico.ulisboa.pt/cursos/mecd)

[Statistical Learning](https://fenix.tecnico.ulisboa.pt/disciplinas/AEsta4/2019-2020/2-semestre/)

# Grade

[Project](https://github.com/pedrorio/statistical_learning) : 18.0/20.0

## White-wine re-evaluation

White wines are scored on their own. Results, including per-fit files, `failures.tsv` and the stack decision, are written under `results/reevaluation/white/`. Run one split or the full protocol with `Rscript reevaluation/white/fit_white.R`.

The 140 white family fits are handed to four workers as four fixed chunks of 35. `parallel::parLapply()` and `parallel::parLapplyLB()` both split the list once per core before any load balancing, so with one chunk per core a worker that finishes has nothing else to take. The fits still queued on the slow chunk stay there. The improvement, not yet made, is to send one fit at a time with `parallel::clusterApplyLB()` from `white_mclapply` in `reevaluation/white/fit_white.R`. That does not change the 10x10 CV, the SVM grids, or the cache keys: `fit_white.R` is not part of the hash stored in each fit, and each job already sets its seed before fitting. The process that is running now will not pick this up; it has already handed out the four chunks.
