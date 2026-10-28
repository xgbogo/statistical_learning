# White-wine re-evaluation. Same 10 by 10 CV and the same grids as red.
#   Rscript reevaluation/white/fit_white.R one_split
#   Rscript reevaluation/white/fit_white.R full
#
# OpenMP is pinned to one thread before any package loads. mgcv (via
# gam_and_stack.R) is sourced only after the family fits: loading it first
# initialises libgomp, and a later fork deadlocks inside xgboost.

Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1"
)
source("libraries.R")
source("reevaluation/step_winsorise.R")
source("reevaluation/metrics.R")
source("reevaluation/recipes.R")
source("reevaluation/models.R")
source("reevaluation/fit_reevaluation.R")

white_families <- c(
  "lr", "svm_poly", "svm_radial", "lda", "fda", "rf", "xgb",
  "lgb", "xgb_full", "score_rf", "score_lgb", "score_lin"
)

# Forking after xgboost has loaded deadlocks inside libgomp. Fresh PSOCK
# workers load the packages themselves, then run the same fit_job().
white_mclapply <- function(X, FUN, ..., mc.cores = 1L, mc.preschedule = TRUE, mc.set.seed = FALSE) {
  if (length(X) == 0) {
    return(list())
  }
  cores <- as.integer(mc.cores)
  if (is.na(cores) || cores < 1L) {
    cores <- 1L
  }
  cores <- min(cores, length(X))
  dots <- list(...)
  cl <- parallel::makeCluster(cores, type = "PSOCK", outfile = "")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    Sys.setenv(
      OMP_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1"
    )
    source("libraries.R")
    source("reevaluation/step_winsorise.R")
    source("reevaluation/metrics.R")
    source("reevaluation/recipes.R")
    source("reevaluation/models.R")
    source("reevaluation/fit_reevaluation.R")
    white_fit_one <- function(payload) {
      RNGkind("L'Ecuyer-CMRG")
      set.seed(payload$job$seed)
      try(do.call(fit_job, c(list(payload$job), payload$args)), silent = TRUE)
    }
    NULL
  })
  call_worker <- function(payload) white_fit_one(payload)
  environment(call_worker) <- globalenv()
  payloads <- lapply(X, function(job) list(job = job, args = dots))
  if (isTRUE(mc.preschedule)) {
    parallel::parLapply(cl, payloads, call_worker)
  } else {
    parallel::parLapplyLB(cl, payloads, call_worker)
  }
}

write_white_failures <- function(fits_dir) {
  files <- list.files(fits_dir, pattern = "^[a-z_]+__[a-z]+__[0-9]{4}\\.rds$")
  rows <- lapply(files, function(file) {
    metrics <- readRDS(file.path(fits_dir, file))$metrics
    if (identical(metrics$status, "ok")) {
      return(NULL)
    }
    detail <- metrics$detail
    if (length(detail) != 1 || is.na(detail)) {
      detail <- ""
    }
    data.frame(
      model = as.character(metrics$model),
      arm = as.character(metrics$arm),
      seed = as.character(metrics$seed),
      status = as.character(metrics$status),
      detail = gsub("[\r\n]+", " ", as.character(detail)),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    out <- data.frame(
      model = character(),
      arm = character(),
      seed = character(),
      status = character(),
      detail = character(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    out <- out[order(out$model, out$arm, out$seed), , drop = FALSE]
  }
  utils::write.table(
    out,
    file.path(fits_dir, "failures.tsv"),
    sep = "\t",
    quote = TRUE,
    row.names = FALSE
  )
  invisible(out)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1 || !args[[1]] %in% c("one_split", "full")) {
    stop("Usage: Rscript reevaluation/white/fit_white.R one_split|full")
  }
  # Family fits only. Restored before the GAM step, which keeps the red fork path.
  original_mclapply <- getFromNamespace("mclapply", "parallel")
  assignInNamespace("mclapply", white_mclapply, ns = "parallel")
  if (identical(args[[1]], "one_split")) {
    fit_reevaluation(
      dataset = "white",
      seeds = 2021L,
      model_tags = white_families,
      run_file = "run_one_split.rds"
    )
  } else {
    fit_reevaluation(
      dataset = "white",
      seeds = 2021:2025,
      model_tags = white_families,
      run_file = "run.rds"
    )
    assignInNamespace("mclapply", original_mclapply, ns = "parallel")
    source("reevaluation/gam_and_stack.R")
    gam_run <- run_gam_and_stack(dataset = "white")
    saveRDS(gam_run, "results/reevaluation/white/run_gam_stack.rds")
    write_white_failures("results/reevaluation/white")
  }
}
