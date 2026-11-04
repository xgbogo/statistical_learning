# One refit of each stored workflow, to recover test probabilities for the
# original 105 fits. The 10 by 10 is not run. Existing per-fit files are read
# and never written.
source("libraries.R")
library(ranger, warn.conflicts = FALSE)
library(xgboost, warn.conflicts = FALSE)
source("reevaluation/step_winsorise.R")
source("reevaluation/metrics.R")
source("reevaluation/recipes.R")
source("reevaluation/models.R")
source("reevaluation/fit_reevaluation.R")

original_model_order <- c("lr", "svm_poly", "svm_radial", "lda", "fda", "rf", "xgb")
original_arm_order <- c("rose", "weights", "none")
original_seeds <- 2021:2025

# The original mclapply walked seed, then arm, then model. mc.set.seed advances
# one L'Ecuyer stream per launch, and launches are in that same index order.
# The ten class-weighted SVM files were rewritten later by a separate mclapply
# of svm_poly and svm_radial, weights only, still one stream per launch.
original_job_index <- function(seed, arm, model) {
  seed_i <- match(as.integer(seed), original_seeds)
  if (is.na(seed_i)) {
    stop("Unknown original job")
  }
  if (identical(arm, "weights") && model %in% c("svm_poly", "svm_radial")) {
    tags <- c("svm_poly", "svm_radial")
    return((seed_i - 1L) * length(tags) + match(model, tags))
  }
  arm_i <- match(arm, original_arm_order)
  model_i <- match(model, original_model_order)
  if (anyNA(c(arm_i, model_i))) {
    stop("Unknown original job")
  }
  ((seed_i - 1L) * length(original_arm_order) + (arm_i - 1L)) * length(original_model_order) + model_i
}

# Parent RNG state at the original mclapply: after the five outer splits and
# both 10 by 10 fold assignments. Child k receives nextRNGStream applied k times.
parent_stream_after_splits <- function() {
  RNGkind("L'Ecuyer-CMRG")
  data <- load_recoded_data("data/winequality-red.csv", "quality")
  lapply(original_seeds, function(seed) {
    set.seed(seed)
    split <- rsample::initial_split(data, prop = 0.8, strata = quality)
    train <- rsample::training(split)
    rsample::vfold_cv(train, v = 10, repeats = 10, strata = quality)
    weighted <- train
    weighted$case_weight <- make_case_weights(train)
    rsample::vfold_cv(weighted, v = 10, repeats = 10, strata = quality)
    NULL
  })
  get(".Random.seed", envir = .GlobalEnv)
}

advance_stream <- function(seed, steps) {
  stream <- seed
  if (steps > 0) {
    for (i in seq_len(steps)) {
      stream <- parallel::nextRNGStream(stream)
    }
  }
  stream
}

guard_paths <- function() {
  fits_dir <- reevaluation_dir("red")
  fits <- list.files(
    fits_dir,
    pattern = "^(lr|svm_poly|svm_radial|lda|fda|rf|xgb)__.*\\.rds$",
    full.names = TRUE
  )
  added <- list.files(
    fits_dir,
    pattern = "^(lgb|xgb_full|score_rf|score_lgb|score_lin)__.*\\.rds$",
    full.names = TRUE
  )
  c(
    sort(fits),
    sort(added),
    file.path(fits_dir, "metrics.rds"),
    file.path(fits_dir, "baseline.rds"),
    file.path(fits_dir, "run.rds"),
    file.path(fits_dir, "metrics_added.rds"),
    file.path(fits_dir, "run_added.rds"),
    "main.pdf"
  )
}

position_rng <- function(mode, base_stream, seed, arm, model) {
  if (identical(mode, "first")) {
    RNGkind("Mersenne-Twister")
    set.seed(1)
    return(invisible(NULL))
  }
  if (identical(mode, "lecuyer")) {
    RNGkind("L'Ecuyer-CMRG")
    assign(
      ".Random.seed",
      advance_stream(base_stream, original_job_index(seed, arm, model)),
      envir = .GlobalEnv
    )
    return(invisible(NULL))
  }
  stop("Unknown refit mode")
}

recover_best <- function(row, train, spec) {
  recipe <- filtered_recipe(train, row$arm)
  blank <- workflows::workflow() %>%
    workflows::add_model(spec$model) %>%
    workflows::add_recipe(recipe)
  pset <- hardhat::extract_parameter_set_dials(blank)$id
  if (identical(row$model, "fda")) {
    parts <- strsplit(strsplit(row$params, ", ")[[1]], "=", fixed = TRUE)
    values <- stats::setNames(
      lapply(parts, function(kv) as.integer(kv[[2]])),
      vapply(parts, function(kv) kv[[1]], character(1))
    )
    best <- tibble::as_tibble(values)
    return(list(recipe = recipe, best = best[, pset, drop = FALSE]))
  }
  grid <- if (identical(row$model, "rf")) rf_grid(11) else spec$grid
  if (!is.data.frame(grid)) {
    stop("No grid for ", row$model)
  }
  ordered <- grid[, pset, drop = FALSE]
  texts <- vapply(seq_len(nrow(ordered)), function(i) {
    params_text(ordered[i, , drop = FALSE])
  }, character(1))
  hit <- which(texts == row$params)
  if (length(hit) != 1) {
    stop("Stored parameters do not match one grid row: ", row$model, " ", row$params)
  }
  list(recipe = recipe, best = ordered[hit, , drop = FALSE])
}

fit_stored_workflow <- function(row, outer, spec, mode, base_stream) {
  model <- spec$model
  use_case_weights <- identical(row$arm, "weights") && !(row$model %in% svm_tags)
  if (identical(row$arm, "weights") && row$model %in% svm_tags) {
    model <- with_class_weights(model, outer$class_weights)
  }
  train <- if (use_case_weights) outer$weighted_train else outer$train
  test <- if (use_case_weights) outer$weighted_test else outer$test
  # The recipe draw, including the ROSE seed, has to be the next use of this stream.
  position_rng(mode, base_stream, row$seed, row$arm, row$model)
  found <- recover_best(row, train, spec)
  workflow <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(found$recipe)
  if (use_case_weights) {
    workflow <- workflows::add_case_weights(workflow, case_weight)
  }
  final <- tune::finalize_workflow(workflow, found$best)
  fitted <- generics::fit(final, data = train)
  prob <- predict(fitted, test, type = "prob")$.pred_high
  scored <- score_at_threshold(test$quality, prob, row$threshold)
  list(prob = prob, quality = test$quality, scored = scored)
}

comparison <- function(scored, row) {
  fields <- c("bal_accuracy", "roc_auc", "accuracy", "tp", "fp", "tn", "fn")
  diffs <- vapply(fields, function(field) {
    abs(scored[[field]] - row[[field]])
  }, numeric(1))
  list(max_abs = max(diffs), exact = isTRUE(max(diffs) == 0), within = isTRUE(max(diffs) <= 1e-9))
}

refit_original_pr_auc <- function() {
  Sys.setenv(OMP_NUM_THREADS = "1")
  protected <- list.files(
    reevaluation_dir("red"),
    pattern = "^(lr|svm_poly|svm_radial|lda|fda|rf|xgb)__.*\\.rds$",
    full.names = TRUE
  )
  if (length(protected) != 105) {
    stop("Expected 105 protected fit files, found ", length(protected))
  }
  before <- tools::md5sum(guard_paths())
  fits_dir <- reevaluation_dir("red")
  metrics <- readRDS(file.path(fits_dir, "metrics.rds"))
  if (nrow(metrics) != 105) {
    stop("metrics.rds is not the original 105 rows")
  }
  loaded <- load_recode_and_score()
  outers <- make_outer_splits(loaded$data, loaded$score, original_seeds, 10, 10)
  base_stream <- get(".Random.seed", envir = .GlobalEnv)
  if (!identical(base_stream, parent_stream_after_splits())) {
    stop("The outer-split RNG stream does not match the original driver")
  }
  models <- reevaluation_models()
  outer_by_seed <- stats::setNames(outers, as.character(original_seeds))
  cores <- parallel::detectCores(logical = TRUE)
  if (is.na(cores) || cores < 1) cores <- 1L

  one <- function(i, mode) {
    row <- metrics[i, ]
    outer <- outer_by_seed[[as.character(row$seed)]]
    spec <- models[[row$model]]
    attempt <- tryCatch(
      fit_stored_workflow(row, outer, spec, mode, base_stream),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(attempt$error)) {
      return(list(
        max_abs = Inf,
        exact = FALSE,
        within = FALSE,
        pr_auc = NA_real_,
        detail = attempt$error,
        prob = NULL,
        quality = NULL
      ))
    }
    check <- comparison(attempt$scored, row)
    list(
      max_abs = check$max_abs,
      exact = check$exact,
      within = check$within,
      pr_auc = if (check$within) attempt$scored$pr_auc else NA_real_,
      detail = NA_character_,
      prob = if (check$within) attempt$prob else NULL,
      quality = if (check$within) attempt$quality else NULL
    )
  }

  started <- proc.time()[["elapsed"]]
  message("First refit of 105 stored workflows")
  first <- parallel::mclapply(
    seq_len(nrow(metrics)),
    one,
    mode = "first",
    mc.cores = cores,
    mc.preschedule = FALSE
  )
  failed <- vapply(first, function(x) inherits(x, "try-error"), logical(1))
  if (any(failed)) {
    stop(paste(vapply(first[failed], as.character, character(1)), collapse = "\n"))
  }
  first_ok <- vapply(first, function(x) isTRUE(x$within), logical(1))
  message(sprintf("First refit matched %d of 105", sum(first_ok)))

  second <- vector("list", nrow(metrics))
  retry <- which(!first_ok)
  if (length(retry) > 0) {
    message(sprintf("Retrying %d fits on the original L'Ecuyer stream", length(retry)))
    retried <- parallel::mclapply(
      retry,
      one,
      mode = "lecuyer",
      mc.cores = cores,
      mc.preschedule = FALSE
    )
    failed <- vapply(retried, function(x) inherits(x, "try-error"), logical(1))
    if (any(failed)) {
      stop(paste(vapply(retried[failed], as.character, character(1)), collapse = "\n"))
    }
    for (j in seq_along(retry)) second[[retry[[j]]]] <- retried[[j]]
  }
  wall_seconds <- proc.time()[["elapsed"]] - started

  dir.create(file.path(fits_dir, "pr_original"), recursive = TRUE, showWarnings = FALSE)
  rows <- vector("list", nrow(metrics))
  for (i in seq_len(nrow(metrics))) {
    chosen <- if (isTRUE(first[[i]]$within)) first[[i]] else second[[i]]
    pass <- if (isTRUE(first[[i]]$within)) {
      "first"
    } else if (isTRUE(chosen$within)) {
      "lecuyer"
    } else {
      "not_reproducible"
    }
    row <- metrics[i, ]
    pr_status <- if (identical(pass, "not_reproducible")) "not_reproducible" else "ok"
    pr_value <- if (identical(pr_status, "ok")) chosen$pr_auc else NA_real_
    file <- file.path(
      fits_dir,
      "pr_original",
      sprintf("%s__%s__%s.rds", row$model, row$arm, row$seed)
    )
    payload <- list(
      model = row$model,
      arm = row$arm,
      seed = row$seed,
      pr_status = pr_status,
      pr_auc = pr_value,
      pass = pass,
      max_abs = chosen$max_abs,
      detail = chosen$detail
    )
    if (identical(pr_status, "ok")) {
      payload$test <- tibble::tibble(quality = chosen$quality, .pred_high = chosen$prob)
    }
    saveRDS(payload, file)
    rows[[i]] <- tibble::tibble(
      model = row$model,
      model_name = row$model_name,
      arm = row$arm,
      seed = row$seed,
      pr_auc = pr_value,
      pr_status = pr_status,
      pass = pass,
      max_abs_first = first[[i]]$max_abs,
      exact_first = first[[i]]$exact,
      within_first = first[[i]]$within,
      max_abs = chosen$max_abs,
      detail = chosen$detail
    )
  }
  summary <- dplyr::bind_rows(rows)
  saveRDS(summary, file.path(fits_dir, "metrics_pr_original.rds"))

  baseline <- readRDS(file.path(fits_dir, "baseline.rds"))
  guess_pr <- vapply(seq_len(nrow(baseline)), function(i) {
    n_low <- baseline$tn[[i]] + baseline$fp[[i]]
    n_high <- baseline$tp[[i]] + baseline$fn[[i]]
    # A constant score has no ranking, so its PR AUC is the share of high rows.
    n_high / (n_low + n_high)
  }, numeric(1))

  after <- tools::md5sum(guard_paths())
  if (!identical(before[names(after)], after) || !identical(names(before), names(after))) {
    stop("A protected file changed during the refit")
  }
  run <- list(
    wall_seconds = wall_seconds,
    cores = as.integer(cores),
    n = nrow(summary),
    first_exact = sum(summary$exact_first),
    first_within = sum(summary$within_first),
    lecuyer_matched = sum(summary$pass == "lecuyer"),
    not_reproducible = summary$model[summary$pr_status == "not_reproducible"],
    not_reproducible_arm = summary$arm[summary$pr_status == "not_reproducible"],
    not_reproducible_seed = summary$seed[summary$pr_status == "not_reproducible"],
    guess_low_pr_auc = guess_pr,
    guard_md5 = before
  )
  saveRDS(run, file.path(fits_dir, "run_pr_original.rds"))
  message(sprintf(
    "Refit finished in %.1f seconds. First pass %d exact, %d within 1e-9. L'Ecuyer recovered %d. Not reproducible: %d.",
    wall_seconds,
    run$first_exact,
    run$first_within,
    run$lecuyer_matched,
    length(run$not_reproducible)
  ))
  run
}

if (sys.nframe() == 0) {
  refit_original_pr_auc()
}
