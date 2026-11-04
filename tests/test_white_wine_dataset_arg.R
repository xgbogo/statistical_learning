# Acceptance: the dataset argument defaults to red; white paths and keys carry the dataset.
# Spec: "The dataset argument defaults to red", "White paths and cache keys carry the dataset".
source("libraries.R")
source("reevaluation/step_winsorise.R")
source("reevaluation/metrics.R")
source("reevaluation/recipes.R")
source("reevaluation/models.R")
source("reevaluation/fit_reevaluation.R")
source("utils/estimation/fit_combinations.R")

red_sources <- c(
  "reevaluation/step_winsorise.R",
  "reevaluation/metrics.R",
  "reevaluation/recipes.R",
  "reevaluation/models.R",
  "reevaluation/fit_reevaluation.R",
  "data/winequality-red.csv",
  "renv.lock"
)
red_shared <- c(
  "utils/recipes/step_recipe.R",
  "utils/estimation/pipeline.R",
  "utils/load_raw_data.R",
  "utils/load_recoded_data.R",
  "utils/recode_to_high_low.R",
  "data/winequality-red.csv",
  "renv.lock"
)
job <- list(seed = 2021L, arm = "none", model = list(tag = "lr"))
md5 <- c(a = "0123")
red_job_key <- list(
  seed = 2021L, arm = "none", model = "lr", rng = "L'Ecuyer-CMRG",
  split_prop = 0.8, split_strata = TRUE, v = 10L, repeats = 10L,
  fold_strata = "quality", metric = "bal_accuracy", event = "high", sources = md5
)
red_combination_key <- list(
  seed = 2020L, rng = "L'Ecuyer-CMRG", split_prop = 0.8, split_strata = FALSE,
  v = 10L, repeats = 10L, fold_strata = "quality", metric = "accuracy",
  model = file_md5("utils/estimation/models/logistic_regression.R"),
  recipe = file_md5("utils/estimation/recipes/filtered_dataset.R"),
  shared = "s"
)

# Red, as today (passes today).
stopifnot(identical(source_files, red_sources))
stopifnot(identical(shared_source_files, red_shared))
stopifnot(identical(job_key(job, md5, 10, 10), red_job_key))
stopifnot(identical(combination_key("lr", "fd", "s"), red_combination_key))
stopifnot(isTRUE(protected_fit_path("results/reevaluation/lr__none__2021.rds")))
stopifnot(isTRUE(protected_fit_path("results/reevaluation/svm_radial__weights__2025.rds")))
stopifnot(isFALSE(protected_fit_path("results/reevaluation/gam__none__2021.rds")))

# Dataset paths.
stopifnot(identical(dataset_csv(), "data/winequality-red.csv"))
stopifnot(identical(dataset_csv("red"), "data/winequality-red.csv"))
stopifnot(identical(dataset_csv("white"), "data/winequality-white.csv"))
stopifnot(inherits(try(dataset_csv("rose"), silent = TRUE), "try-error"))
stopifnot(identical(reevaluation_dir(), "results/reevaluation"))
stopifnot(identical(reevaluation_dir("red"), "results/reevaluation"))
stopifnot(identical(reevaluation_dir("white"), "results/reevaluation/white"))
stopifnot(identical(combination_fits_dir(), "results/fits"))
stopifnot(identical(combination_fits_dir("red"), "results/fits"))
stopifnot(identical(combination_fits_dir("white"), "results/reevaluation/white/fits"))

# Defaults.
for (fn in list(load_recode_and_score, job_key, fit_reevaluation, combination_key, fit_combinations)) {
  stopifnot(identical(formals(fn)$dataset, "red"))
}

# Source lists.
stopifnot(identical(reevaluation_source_files(), red_sources))
stopifnot(identical(reevaluation_source_files("red"), red_sources))
stopifnot(identical(reevaluation_source_files("white"), c(
  "reevaluation/step_winsorise.R",
  "reevaluation/metrics.R",
  "reevaluation/recipes.R",
  "reevaluation/models.R",
  "reevaluation/fit_reevaluation.R",
  "utils/white_wine.R",
  "data/winequality-white.csv",
  "renv.lock"
)))
stopifnot(identical(combination_shared_files(), red_shared))
stopifnot(identical(combination_shared_files("red"), red_shared))
stopifnot(identical(combination_shared_files("white"), c(
  "utils/recipes/step_recipe.R",
  "utils/estimation/pipeline.R",
  "utils/load_raw_data.R",
  "utils/white_wine.R",
  "data/winequality-white.csv",
  "renv.lock"
)))

# Keys: red unchanged with an explicit dataset; white appends the dataset last.
stopifnot(identical(job_key(job, md5, 10, 10, dataset = "red"), red_job_key))
stopifnot(identical(job_key(job, md5, 10, 10, dataset = "white"), c(red_job_key, list(dataset = "white"))))
stopifnot(identical(combination_key("lr", "fd", "s", dataset = "red"), red_combination_key))
stopifnot(identical(
  combination_key("lr", "fd", "s", dataset = "white"),
  c(red_combination_key, list(dataset = "white"))
))

# White files for the original models are not protected; red ones still are.
stopifnot(isFALSE(protected_fit_path("results/reevaluation/white/lr__none__2021.rds")))
stopifnot(isFALSE(protected_fit_path("results/reevaluation/white/svm_radial__weights__2025.rds")))
stopifnot(isTRUE(protected_fit_path("results/reevaluation/lr__none__2021.rds")))

cat("white dataset argument: ok\n")
