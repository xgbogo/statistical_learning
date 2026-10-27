# Acceptance: red entry points write where they do today; the white entry point
# reads only the white CSV and writes only under results/reevaluation/white/.
# Spec: "The dataset argument defaults to red", "The white run reads only white inputs".
source("libraries.R")
source("reevaluation/step_winsorise.R")
source("reevaluation/metrics.R")
source("reevaluation/recipes.R")
source("reevaluation/models.R")
source("reevaluation/fit_reevaluation.R")
source("utils/estimation/fit_combinations.R")

repo <- normalizePath(".")

# A scratch copy with the code, the lockfile and the chosen CSVs, and no results.
scratch <- function(csvs) {
  dir <- tempfile("white-entry-")
  dir.create(file.path(dir, "data"), recursive = TRUE)
  for (csv in csvs) {
    stopifnot(file.copy(file.path(repo, "data", csv), file.path(dir, "data", csv)))
  }
  stopifnot(file.copy(file.path(repo, "reevaluation"), dir, recursive = TRUE))
  stopifnot(file.copy(file.path(repo, "utils"), dir, recursive = TRUE))
  stopifnot(file.copy(file.path(repo, "renv.lock"), dir))
  dir
}
results_in <- function(dir) sort(list.files(file.path(dir, "results"), recursive = TRUE))

# Red, as today (passes today).
red_dir <- scratch(c("winequality-red.csv", "winequality-white.csv"))
old <- setwd(red_dir)
red_run <- fit_reevaluation(seeds = 2021L, model_tags = character(0))
setwd(old)
stopifnot(identical(results_in(red_dir), c("reevaluation/baseline.rds", "reevaluation/metrics.rds", "reevaluation/run.rds")))
red_baseline <- readRDS(file.path(red_dir, "results/reevaluation/baseline.rds"))
stopifnot(nrow(red_baseline) == 1L, red_baseline$n_test < 400L)

# White, with no red CSV and no red results on disk.
white_dir <- scratch("winequality-white.csv")
old <- setwd(white_dir)
white_run <- tryCatch(
  fit_reevaluation(dataset = "white", seeds = 2021L, model_tags = character(0)),
  finally = setwd(old)
)
stopifnot(identical(
  results_in(white_dir),
  c("reevaluation/white/baseline.rds", "reevaluation/white/metrics.rds", "reevaluation/white/run.rds")
))
white_baseline <- readRDS(file.path(white_dir, "results/reevaluation/white/baseline.rds"))
stopifnot(nrow(white_baseline) == 1L, white_baseline$n_test > 900L)
stopifnot(with(white_baseline, tp + fp + tn + fn == n_test))

# The white loader reads the white CSV with no red CSV present.
old <- setwd(white_dir)
loaded <- tryCatch(load_recode_and_score(dataset = "white"), finally = setwd(old))
stopifnot(nrow(loaded$data) == 4898L, sum(loaded$data$quality == "high") == 1060L)
stopifnot(identical(loaded$score, as.integer(as.character(load_raw_data("data/winequality-white.csv")$quality))))

# The GAM and stack run takes the dataset too.
source("reevaluation/gam_and_stack.R")
stopifnot(identical(formals(run_gam_and_stack)$dataset, "red"))

unlink(c(red_dir, white_dir), recursive = TRUE)
cat("white entry points: ok\n")
