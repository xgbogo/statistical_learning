# Penalised GAM, out-of-fold reruns of the two original bases, and the stack.
# Sourcing this file defines the helpers. The full run starts only when the
# file is executed directly.
source("libraries.R")
library(mgcv, warn.conflicts = FALSE)
library(glmnet, warn.conflicts = FALSE)
source("reevaluation/step_winsorise.R")
source("reevaluation/metrics.R")
source("reevaluation/recipes.R")
source("reevaluation/models.R")
source("reevaluation/fit_reevaluation.R")
source("reevaluation/refit_pr_auc.R")

gam_measurements <- function() {
  c(
    "fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar",
    "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density",
    "pH", "sulphates", "alcohol"
  )
}

gam_formula <- function() {
  smooths <- paste0("s(", gam_measurements(), ", k = 5)", collapse = " + ")
  stats::as.formula(paste("quality ~", smooths))
}

# The smooth formula names every measurement, so this recipe must not drop one.
# Winsorise still refits on the analysis split. Correlation filtering stays on
# the other models.
gam_recipe <- function(train_data) {
  # quality ~ . keeps a case-weight column in its role. type is the red/white
  # tag on this frame, not one of the 11 measurements, so it is removed.
  # Numeric predictors are the 11 measurements; winsorise does not drop them.
  recipes::recipe(quality ~ ., data = train_data) %>%
    recipes::step_rm(type) %>%
    recipes::step_naomit(recipes::all_numeric_predictors()) %>%
    step_winsorise_train(recipes::all_numeric_predictors())
}

gam_workflow <- function(train_data, arm) {
  if (!arm %in% c("weights", "none")) {
    stop("Unknown GAM arm")
  }
  spec <- reevaluation_models()$gam
  workflow <- workflows::workflow() %>%
    workflows::add_recipe(gam_recipe(train_data)) %>%
    workflows::add_model(spec$model, formula = gam_formula())
  if (identical(arm, "weights")) {
    workflow <- workflows::add_case_weights(workflow, case_weight)
  }
  workflow
}

edf_from_fit <- function(fitted) {
  gam <- hardhat::extract_fit_engine(fitted)
  smooths <- summary(gam)$s.table
  if (is.null(smooths)) {
    stop("The GAM fit has no smooth terms")
  }
  edf <- smooths[, "edf"]
  names(edf) <- sub("^s\\((.*)\\)$", "\\1", rownames(smooths))
  expected <- make.names(gam_measurements())
  if (!identical(sort(names(edf)), sort(expected))) {
    stop("GAM edf terms are ", paste(names(edf), collapse = ", "))
  }
  edf[expected]
}

partial_frame <- function(fitted) {
  gam <- hardhat::extract_fit_engine(fitted)
  contributions <- stats::predict(gam, type = "terms")
  frame <- gam$model
  pieces <- lapply(colnames(contributions), function(column) {
    term <- sub("^s\\((.*)\\)$", "\\1", column)
    data.frame(
      term = term,
      x = frame[[term]],
      contribution = contributions[, column],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, pieces)
}

reproduces_stored <- function(stored_row, refit, tol = 1e-9) {
  fields <- c("roc_auc", "bal_accuracy", "accuracy", "tp", "fp", "tn", "fn")
  if (!all(fields %in% names(stored_row)) || !all(fields %in% names(refit))) {
    stop("A reproduction check is missing a metric")
  }
  diffs <- abs(as.numeric(stored_row[1, fields]) - as.numeric(refit[1, fields]))
  isTRUE(max(diffs) <= tol)
}

average_oof_repeats <- function(oof) {
  if (!all(c(".row", ".pred_high") %in% names(oof))) {
    stop("OOF frame needs .row and .pred_high")
  }
  means <- tapply(oof$.pred_high, oof$.row, mean)
  data.frame(
    .row = as.integer(names(means)),
    .pred_high = as.numeric(means),
    stringsAsFactors = FALSE
  )
}

fit_blender <- function(oof_x, oof_y, test_x, seed) {
  if (!is.data.frame(oof_x) || nrow(oof_x) != length(oof_y)) {
    stop("Blender inputs must have one outcome per row")
  }
  outcome <- factor(as.character(oof_y), levels = level_order)
  if (anyNA(outcome)) {
    stop("Blender outcome is not high or low")
  }
  train <- oof_x
  train$quality <- outcome
  # Penalty and cut-off are fixed before test_x is read.
  set.seed(seed)
  spec <- parsnip::logistic_reg(penalty = tune::tune(), mixture = 0) %>%
    parsnip::set_engine("glmnet") %>%
    parsnip::set_mode("classification")
  workflow <- workflows::workflow() %>%
    workflows::add_model(spec) %>%
    workflows::add_formula(quality ~ .)
  folds <- rsample::vfold_cv(train, v = 10, repeats = 10, strata = quality)
  grid <- dials::grid_regular(dials::penalty(), levels = 25)
  tuned <- tune::tune_grid(
    workflow,
    resamples = folds,
    grid = grid,
    metrics = yardstick::metric_set(yardstick::mn_log_loss),
    control = tune::control_grid(
      save_pred = TRUE,
      verbose = FALSE,
      allow_par = FALSE,
      event_level = "second"
    )
  )
  best <- tune::select_best(tuned, metric = "mn_log_loss")
  penalty <- best$penalty[[1]]
  oof <- tune::collect_predictions(tuned)
  oof <- oof[oof$.config == best$.config[[1]], , drop = FALSE]
  averaged <- average_oof_repeats(oof)
  oof_prob <- averaged$.pred_high[match(seq_len(nrow(train)), averaged$.row)]
  if (anyNA(oof_prob)) {
    stop("The blender is missing an out-of-fold probability")
  }
  threshold <- choose_threshold(outcome, oof_prob)
  final <- tune::finalize_workflow(workflow, best)
  fitted <- generics::fit(final, data = train)
  test_pred <- predict(fitted, test_x, type = "prob")$.pred_high
  list(
    penalty = penalty,
    threshold = threshold,
    test_pred = test_pred,
    n_test_scored = 1L
  )
}

stack_drop_rule <- function(stack, svm, average) {
  if (length(stack) != length(svm) || length(stack) != length(average)) {
    stop("Drop-rule inputs must be the same length")
  }
  one <- function(reference) {
    difference <- stack - reference
    list(
      positive = as.integer(sum(difference > 0)),
      mean = mean(difference),
      sd = stats::sd(difference)
    )
  }
  svm_gap <- one(svm)
  average_gap <- one(average)
  clears <- function(gap) {
    isTRUE(gap$positive >= 4L && gap$mean > gap$sd)
  }
  list(
    keep = isTRUE(clears(svm_gap) && clears(average_gap)),
    svm = svm_gap,
    average = average_gap
  )
}

# Inverse-frequency weights are not integer counts. mgcv warns on every
# binomial fit; the fit itself still uses those weights.
quiet_weight_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("non-integer #successes", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

fit_gam_job <- function(outer, arm) {
  message(sprintf("GAM %s seed %d", arm, outer$seed))
  train <- if (identical(arm, "weights")) outer$weighted_train else outer$train
  test <- if (identical(arm, "weights")) outer$weighted_test else outer$test
  folds <- if (identical(arm, "weights")) outer$weighted_folds else outer$folds
  workflow <- gam_workflow(train, arm)
  set.seed(outer$seed)
  resampled <- quiet_weight_warning(tune::fit_resamples(
    workflow,
    resamples = folds,
    metrics = yardstick::metric_set(yardstick::bal_accuracy, yardstick::roc_auc),
    control = tune::control_resamples(
      save_pred = TRUE,
      verbose = FALSE,
      allow_par = FALSE,
      event_level = "second"
    )
  ))
  collected <- tune::collect_predictions(resampled)
  if (!all(c("quality", ".pred_high", ".row") %in% names(collected))) {
    stop("GAM out-of-fold predictions are missing columns: ", paste(names(collected), collapse = ", "))
  }
  oof <- tibble::tibble(
    quality = collected$quality,
    .pred_high = collected$.pred_high,
    .row = collected$.row
  )
  n_repeats <- if ("id2" %in% names(folds)) length(unique(folds$id2)) else 1L
  if (nrow(oof) != nrow(train) * n_repeats) {
    stop(sprintf(
      "GAM seed %d arm %s saved %d out-of-fold rows, expected %d",
      outer$seed, arm, nrow(oof), nrow(train) * n_repeats
    ))
  }
  threshold <- choose_threshold(oof$quality, oof$.pred_high)
  fitted <- quiet_weight_warning(generics::fit(workflow, data = train))
  test_prob <- predict(fitted, test, type = "prob")$.pred_high
  scored <- score_at_threshold(test$quality, test_prob, threshold)
  edf <- edf_from_fit(fitted)
  spec <- reevaluation_models()$gam
  metrics <- tibble::tibble(
    model = spec$tag,
    model_name = spec$name,
    arm = arm,
    seed = outer$seed,
    status = "ok",
    bal_accuracy = scored$bal_accuracy,
    roc_auc = scored$roc_auc,
    pr_auc = scored$pr_auc,
    precision = scored$precision,
    recall = scored$recall,
    accuracy = scored$accuracy,
    tp = scored$tp,
    fp = scored$fp,
    tn = scored$tn,
    fn = scored$fn,
    threshold = threshold,
    n_test = nrow(test),
    detail = NA_character_,
    params = "select=TRUE, method=REML, k=5",
    pred_saved = TRUE,
    edf = list(edf)
  )
  list(
    metrics = metrics,
    oof = oof,
    test = tibble::tibble(quality = test$quality, .pred_high = test_prob),
    train_index = outer$train_index,
    test_index = outer$test_index,
    partial = partial_frame(fitted)
  )
}

rerun_base <- function(outer, model_tag, stored_row, base_stream) {
  message(sprintf("Rerun %s seed %d", model_tag, outer$seed))
  spec <- reevaluation_models()[[model_tag]]
  if (is.null(spec)) {
    stop("Unknown base ", model_tag)
  }
  chosen <- NULL
  last_abs <- NA_real_
  for (mode in c("first", "lecuyer")) {
    attempt <- fit_stored_workflow(stored_row, outer, spec, mode, base_stream)
    check <- comparison(attempt$scored, stored_row)
    last_abs <- check$max_abs
    if (is.null(chosen) && isTRUE(check$within)) {
      chosen <- list(attempt = attempt, check = check, mode = mode)
    }
  }
  if (is.null(chosen)) {
    return(list(
      pass = FALSE,
      status = "not reproducible",
      oof = NULL,
      max_abs = last_abs,
      params = stored_row$params,
      model = model_tag,
      seed = outer$seed
    ))
  }
  found <- recover_best(stored_row, outer$train, spec)
  workflow <- workflows::workflow() %>%
    workflows::add_model(spec$model) %>%
    workflows::add_recipe(found$recipe)
  final <- tune::finalize_workflow(workflow, found$best)
  set.seed(outer$seed)
  resampled <- tune::fit_resamples(
    final,
    resamples = outer$folds,
    metrics = yardstick::metric_set(yardstick::bal_accuracy, yardstick::roc_auc),
    control = tune::control_resamples(
      save_pred = TRUE,
      verbose = FALSE,
      allow_par = FALSE,
      event_level = "second"
    )
  )
  collected <- tune::collect_predictions(resampled)
  if (!all(c("quality", ".pred_high", ".row") %in% names(collected))) {
    stop(
      "Rerun out-of-fold predictions are missing columns: ",
      paste(names(collected), collapse = ", ")
    )
  }
  n_repeats <- if ("id2" %in% names(outer$folds)) length(unique(outer$folds$id2)) else 1L
  if (nrow(collected) != nrow(outer$train) * n_repeats) {
    stop(sprintf(
      "Rerun %s seed %d saved %d out-of-fold rows, expected %d",
      model_tag, outer$seed, nrow(collected), nrow(outer$train) * n_repeats
    ))
  }
  list(
    pass = TRUE,
    status = "ok",
    oof = tibble::tibble(
      quality = collected$quality,
      .pred_high = collected$.pred_high,
      .row = collected$.row
    ),
    test = tibble::tibble(quality = outer$test$quality, .pred_high = chosen$attempt$prob),
    max_abs = chosen$check$max_abs,
    params = stored_row$params,
    mode = chosen$mode,
    model = model_tag,
    seed = outer$seed
  )
}

score_probability_map <- function(oof_score, oof_class) {
  dat <- data.frame(
    high = as.integer(as_quality(oof_class) == "high"),
    score = as.numeric(oof_score)
  )
  if (anyNA(dat$high) || anyNA(dat$score) || length(unique(dat$high)) < 2) {
    stop("The score map needs both classes and finite scores")
  }
  stats::glm(high ~ score, family = stats::binomial(), data = dat)
}

apply_score_map <- function(fit, score) {
  as.numeric(stats::predict(fit, newdata = data.frame(score = as.numeric(score)), type = "response"))
}

mean_by_row <- function(row, value) {
  means <- tapply(value, row, mean)
  data.frame(
    .row = as.integer(names(means)),
    value = as.numeric(means),
    stringsAsFactors = FALSE
  )
}

stack_metrics_from_scores <- function(truth, probability, threshold) {
  scored <- score_at_threshold(truth, probability, threshold)
  tibble::tibble(
    bal_accuracy = scored$bal_accuracy,
    roc_auc = scored$roc_auc,
    pr_auc = scored$pr_auc,
    precision = scored$precision,
    recall = scored$recall,
    accuracy = scored$accuracy,
    tp = scored$tp,
    fp = scored$fp,
    tn = scored$tn,
    fn = scored$fn,
    threshold = scored$threshold
  )
}

assemble_seed <- function(outer, svm_rerun, rf_rerun, fits_dir) {
  seed <- outer$seed
  if (!isTRUE(svm_rerun$pass) || !isTRUE(rf_rerun$pass)) {
    payload <- list(status = "not reproducible", seed = seed)
    saveRDS(payload, file.path(fits_dir, sprintf("stack__%d.rds", seed)))
    saveRDS(payload, file.path(fits_dir, sprintf("average__%d.rds", seed)))
    return(list(seed = seed, status = "not reproducible", stack = NA_real_, svm = NA_real_, average = NA_real_))
  }
  score_fit <- readRDS(file.path(fits_dir, sprintf("score_rf__score__%d.rds", seed)))
  if (!identical(sort(score_fit$train_index), sort(outer$train_index))) {
    stop("Score forest training rows do not match this split for seed ", seed)
  }
  svm_oof <- average_oof_repeats(svm_rerun$oof)
  rf_oof <- average_oof_repeats(rf_rerun$oof)
  score_oof <- mean_by_row(score_fit$oof$.row, score_fit$oof$.pred)
  rows <- sort(unique(svm_oof$.row))
  if (!identical(rows, sort(rf_oof$.row)) || !identical(rows, sort(score_oof$.row))) {
    stop("Out-of-fold rows do not match across bases for seed ", seed)
  }
  class_at <- outer$train$quality[rows]
  svm_p <- svm_oof$.pred_high[match(rows, svm_oof$.row)]
  rf_p <- rf_oof$.pred_high[match(rows, rf_oof$.row)]
  score_at_row <- score_oof$value[match(rows, score_oof$.row)]
  mapped <- score_probability_map(score_at_row, class_at)
  score_p <- apply_score_map(mapped, score_at_row)
  # The blender sees the score on its own scale. The logistic map is only
  # the third input of the simple average.
  oof_x <- data.frame(svm = svm_p, rf = rf_p, score_rf = score_at_row)
  test_score_p <- apply_score_map(mapped, score_fit$test$.pred)
  test_x <- data.frame(
    svm = svm_rerun$test$.pred_high,
    rf = rf_rerun$test$.pred_high,
    score_rf = score_fit$test$.pred
  )
  if (nrow(test_x) != nrow(outer$test)) {
    stop("Test inputs are not one row per test wine for seed ", seed)
  }
  blended <- fit_blender(oof_x, class_at, test_x, seed = seed)
  stack_metrics <- stack_metrics_from_scores(outer$test$quality, blended$test_pred, blended$threshold)
  average_oof <- (svm_p + rf_p + score_p) / 3
  average_threshold <- choose_threshold(class_at, average_oof)
  average_test <- (test_x$svm + test_x$rf + test_score_p) / 3
  average_metrics <- stack_metrics_from_scores(outer$test$quality, average_test, average_threshold)
  oof_rows <- outer$train_index[rows]
  saveRDS(
    list(
      status = "ok",
      seed = seed,
      n_test_scored = blended$n_test_scored,
      inputs = c("svm_radial__none", "rf__none", "score_rf__score"),
      oof_rows = oof_rows,
      train_index = outer$train_index,
      test_index = outer$test_index,
      penalty = blended$penalty,
      metrics = stack_metrics
    ),
    file.path(fits_dir, sprintf("stack__%d.rds", seed))
  )
  saveRDS(
    list(
      status = "ok",
      seed = seed,
      n_test_scored = 1L,
      metrics = average_metrics
    ),
    file.path(fits_dir, sprintf("average__%d.rds", seed))
  )
  list(
    seed = seed,
    status = "ok",
    stack = stack_metrics$bal_accuracy,
    average = average_metrics$bal_accuracy
  )
}

gam_stack_guard_paths <- function() {
  c(
    guard_paths(),
    sort(list.files("results/reevaluation/pr_original", pattern = "\\.rds$", full.names = TRUE)),
    "results/reevaluation/metrics_pr_original.rds",
    "results/reevaluation/run_pr_original.rds",
    "fit.R",
    "utils/recipes/step_recipe.R"
  )
}

run_gam_and_stack <- function(fits_dir = "results/reevaluation") {
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1"
  )
  dir.create(file.path(fits_dir, "oof_rerun"), recursive = TRUE, showWarnings = FALSE)
  before <- tools::md5sum(gam_stack_guard_paths())
  started <- proc.time()[["elapsed"]]
  cores <- parallel::detectCores(logical = TRUE)
  if (is.na(cores) || cores < 1) {
    cores <- 1L
  }
  cores <- as.integer(cores)
  message("Building the original L'Ecuyer stream and the five outer splits")
  # Copy the stream. Later set.seed calls must not move this vector.
  base_stream <- as.integer(parent_stream_after_splits())
  loaded <- load_recode_and_score()
  outers <- make_outer_splits(loaded$data, loaded$score, 2021:2025, 10, 10)
  names(outers) <- as.character(2021:2025)
  stored <- readRDS(file.path(fits_dir, "metrics.rds"))

  jobs <- list()
  for (outer in outers) {
    for (arm in c("weights", "none")) {
      jobs[[length(jobs) + 1]] <- list(kind = "gam", outer = outer, arm = arm)
    }
    for (model_tag in c("svm_radial", "rf")) {
      row <- stored[stored$model == model_tag & stored$arm == "none" & stored$seed == outer$seed, , drop = FALSE]
      if (nrow(row) != 1) {
        stop("Stored metrics are missing for ", model_tag, " seed ", outer$seed)
      }
      jobs[[length(jobs) + 1]] <- list(
        kind = "rerun",
        outer = outer,
        model_tag = model_tag,
        stored_row = row
      )
    }
  }

  run_job <- function(job) {
    if (identical(job$kind, "gam")) {
      payload <- fit_gam_job(job$outer, job$arm)
      saveRDS(
        payload,
        file.path(fits_dir, sprintf("gam__%s__%d.rds", job$arm, job$outer$seed))
      )
      return(list(kind = "gam", seed = job$outer$seed, arm = job$arm, ok = TRUE))
    }
    if (identical(job$kind, "rerun")) {
      payload <- rerun_base(job$outer, job$model_tag, job$stored_row, base_stream)
      saveRDS(
        payload,
        file.path(fits_dir, "oof_rerun", sprintf("%s__none__%d.rds", job$model_tag, job$outer$seed))
      )
      return(list(
        kind = "rerun",
        seed = job$outer$seed,
        model = job$model_tag,
        ok = TRUE,
        pass = isTRUE(payload$pass),
        max_abs = payload$max_abs,
        mode = payload$mode
      ))
    }
    stop("Unknown job")
  }

  message(sprintf("Running %d jobs on %d cores", length(jobs), cores))
  finished <- parallel::mclapply(
    jobs,
    function(job) {
      tryCatch(run_job(job), error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), kind = job$kind, seed = job$outer$seed)
      })
    },
    mc.cores = cores,
    mc.preschedule = FALSE,
    mc.set.seed = TRUE
  )
  bad <- Filter(function(item) !isTRUE(item$ok), finished)
  if (length(bad) > 0) {
    stop(paste(vapply(bad, function(item) item$message, character(1)), collapse = "\n"))
  }

  message("Fitting the blender and the simple average")
  assembled <- parallel::mclapply(
    outers,
    function(outer) {
      tryCatch({
        svm_rerun <- readRDS(file.path(fits_dir, "oof_rerun", sprintf("svm_radial__none__%d.rds", outer$seed)))
        rf_rerun <- readRDS(file.path(fits_dir, "oof_rerun", sprintf("rf__none__%d.rds", outer$seed)))
        piece <- assemble_seed(outer, svm_rerun, rf_rerun, fits_dir)
        stored_svm <- stored$bal_accuracy[
          stored$model == "svm_radial" & stored$arm == "none" & stored$seed == outer$seed
        ]
        piece$svm <- stored_svm[[1]]
        piece
      }, error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), seed = outer$seed)
      })
    },
    mc.cores = min(cores, length(outers)),
    mc.preschedule = FALSE,
    mc.set.seed = TRUE
  )
  assemble_bad <- Filter(function(item) isTRUE(item$ok == FALSE), assembled)
  if (length(assemble_bad) > 0) {
    stop(paste(vapply(assemble_bad, function(item) item$message, character(1)), collapse = "\n"))
  }
  stack_ba <- vapply(assembled, function(item) item$stack, numeric(1))
  svm_ba <- vapply(assembled, function(item) item$svm, numeric(1))
  average_ba <- vapply(assembled, function(item) item$average, numeric(1))
  if (anyNA(stack_ba)) {
    decision <- list(
      stack = stack_ba,
      svm = svm_ba,
      average = average_ba,
      keep = FALSE,
      seeds = vapply(assembled, function(item) item$seed, numeric(1))
    )
  } else {
    rule <- stack_drop_rule(stack_ba, svm_ba, average_ba)
    decision <- list(
      stack = stack_ba,
      svm = svm_ba,
      average = average_ba,
      keep = rule$keep,
      svm_gap = rule$svm,
      average_gap = rule$average,
      seeds = vapply(assembled, function(item) item$seed, numeric(1))
    )
  }
  saveRDS(decision, file.path(fits_dir, "stack_decision.rds"))

  reruns <- Filter(function(item) identical(item$kind, "rerun"), finished)
  wall <- proc.time()[["elapsed"]] - started
  run <- list(
    wall_seconds = wall,
    cores = cores,
    rerun_pass = sum(vapply(reruns, function(item) isTRUE(item$pass), logical(1))),
    rerun_n = length(reruns),
    seeds_pass = sum(vapply(assembled, function(item) identical(item$status, "ok"), logical(1))),
    reruns = reruns
  )
  saveRDS(run, file.path(fits_dir, "run_gam_stack.rds"))

  after <- tools::md5sum(gam_stack_guard_paths())
  if (!identical(before[names(after)], after) || !identical(names(before), names(after))) {
    stop("A protected file changed during the GAM and stack run")
  }
  if (!identical(unname(tools::md5sum("main.pdf")), "2848ef7280e31c430dfd9f3e1fc66ebb")) {
    stop("main.pdf changed")
  }
  message(sprintf(
    "GAM and stack finished in %.1f seconds. Reruns passed %d of %d. Seeds kept %d.",
    wall,
    run$rerun_pass,
    run$rerun_n,
    run$seeds_pass
  ))
  run
}

if (sys.nframe() == 0) {
  run_gam_and_stack()
}
