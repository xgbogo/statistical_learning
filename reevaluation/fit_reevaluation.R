source_files <- c(
  "reevaluation/step_winsorise.R",
  "reevaluation/metrics.R",
  "reevaluation/recipes.R",
  "reevaluation/models.R",
  "reevaluation/fit_reevaluation.R",
  "data/winequality-red.csv",
  "renv.lock"
)

file_md5 <- function(path) {
  sum <- tools::md5sum(path)
  if (is.na(sum[[1]])) {
    stop("Cannot hash ", path)
  }
  unname(sum[[1]])
}

assert_dataset <- function(dataset) {
  if (!identical(dataset, "red") && !identical(dataset, "white")) {
    stop("dataset must be \"red\" or \"white\", not ", dataset)
  }
  dataset
}

dataset_csv <- function(dataset = "red") {
  dataset <- assert_dataset(dataset)
  if (identical(dataset, "white")) {
    "data/winequality-white.csv"
  } else {
    "data/winequality-red.csv"
  }
}

reevaluation_dir <- function(dataset = "red") {
  dataset <- assert_dataset(dataset)
  if (identical(dataset, "white")) {
    "results/reevaluation/white"
  } else {
    "results/reevaluation"
  }
}

reevaluation_source_files <- function(dataset = "red") {
  dataset <- assert_dataset(dataset)
  if (identical(dataset, "red")) {
    return(source_files)
  }
  c(
    "reevaluation/step_winsorise.R",
    "reevaluation/metrics.R",
    "reevaluation/recipes.R",
    "reevaluation/models.R",
    "reevaluation/fit_reevaluation.R",
    "utils/white_wine.R",
    "data/winequality-white.csv",
    "renv.lock"
  )
}

make_case_weights <- function(train) {
  n <- nrow(train)
  n_high <- sum(train$quality == "high")
  n_low <- sum(train$quality == "low")
  weights <- ifelse(train$quality == "high", n / (2 * n_high), n / (2 * n_low))
  hardhat::importance_weights(weights)
}

class_weight_vector <- function(train) {
  n <- nrow(train)
  n_high <- sum(train$quality == "high")
  n_low <- sum(train$quality == "low")
  c(low = unname(n / (2 * n_low)), high = unname(n / (2 * n_high)))
}

predictor_count <- function(recipe, data) {
  baked <- recipes::bake(recipes::prep(recipe, training = data), new_data = NULL)
  sum(!names(baked) %in% c("quality", "case_weight"))
}

rf_grid <- function(n_predictors) {
  dials::grid_regular(
    dials::mtry(c(1L, as.integer(n_predictors))),
    dials::min_n(),
    levels = 3
  )
}

params_text <- function(params) {
  if (is.null(params) || !is.data.frame(params) || nrow(params) != 1) {
    return(NA_character_)
  }
  keep <- setdiff(names(params), ".config")
  if (length(keep) == 0) {
    return(NA_character_)
  }
  pieces <- vapply(keep, function(name) {
    value <- params[[name]][[1]]
    if (is.numeric(value)) {
      value <- signif(value, 4)
    }
    paste0(name, "=", value)
  }, character(1))
  paste(pieces, collapse = ", ")
}

original_tags <- c("lr", "svm_poly", "svm_radial", "lda", "fda", "rf", "xgb")

protected_fit_path <- function(path) {
  normalized <- gsub("\\\\", "/", path)
  if (grepl("reevaluation/white/", normalized, fixed = TRUE)) {
    return(FALSE)
  }
  grepl(
    "^(lr|svm_poly|svm_radial|lda|fda|rf|xgb)__",
    basename(path)
  )
}

empty_metrics <- function(model, arm, seed, status, detail) {
  # Read these first: inside tibble() the new `model` column masks the argument.
  tag <- model$tag
  name <- model$name
  tibble::tibble(
    model = tag,
    model_name = name,
    arm = arm,
    seed = seed,
    status = status,
    bal_accuracy = NA_real_,
    roc_auc = NA_real_,
    pr_auc = NA_real_,
    precision = NA_real_,
    recall = NA_real_,
    accuracy = NA_real_,
    tp = NA_real_,
    fp = NA_real_,
    tn = NA_real_,
    fn = NA_real_,
    threshold = NA_real_,
    n_test = NA_integer_,
    detail = detail,
    params = NA_character_,
    pred_saved = FALSE
  )
}

load_recode_and_score <- function(path = dataset_csv(dataset), dataset = "red") {
  dataset <- assert_dataset(dataset)
  if (identical(dataset, "white")) {
    data <- load_white_recoded_data(path, "quality")
  } else {
    data <- load_recoded_data(path, "quality")
  }
  raw <- load_raw_data(path)
  score <- as.integer(as.character(raw$quality))
  # load_raw_data stamps every row as red. On white, that stamp is not a
  # predictor and is not part of the recoded-vs-raw check.
  drop <- if (identical(dataset, "white")) c("quality", "type") else "quality"
  predictors <- setdiff(names(data), drop)
  if (!identical(data[predictors], raw[predictors])) {
    stop("The raw and recoded predictor rows do not match")
  }
  if (!identical(as.character(data$quality), ifelse(score >= 7, "high", "low"))) {
    stop("The raw score and the high/low labels disagree")
  }
  list(data = data, score = score)
}

retarget_rset <- function(folds, new_data) {
  if (nrow(new_data) != nrow(folds$splits[[1]]$data)) {
    stop("The score data and the fold data have different numbers of rows")
  }
  splits <- lapply(folds$splits, function(spl) {
    rsample::make_splits(
      list(
        analysis = as.integer(spl, data = "analysis"),
        assessment = as.integer(spl, data = "assessment")
      ),
      data = new_data
    )
  })
  ids <- folds[, setdiff(names(folds), "splits"), drop = FALSE]
  rsample::manual_rset(splits, ids)
}

resolve_grid <- function(job, recipe) {
  if (!is.null(job$grid_override)) {
    return(job$grid_override)
  }
  if (!is.null(job$model$grid_fn)) {
    return(job$model$grid_fn(predictor_count(recipe, job$train)))
  }
  if (identical(job$model$tag, "rf")) {
    return(rf_grid(predictor_count(recipe, job$train)))
  }
  job$model$grid
}

attach_predictions <- function(metrics, oof, test_pred, train_index, test_index) {
  attr(metrics, "oof") <- oof
  attr(metrics, "test_pred") <- test_pred
  attr(metrics, "train_index") <- train_index
  attr(metrics, "test_index") <- test_index
  metrics
}

assert_oof_excludes_test <- function(oof_row, train_index, test_index, n_train) {
  if (any(oof_row < 1L | oof_row > n_train)) {
    stop("Out-of-fold row ids are outside the training split")
  }
  if (length(intersect(train_index, test_index)) != 0) {
    stop("The outer test rows are inside the training split")
  }
  if (length(intersect(train_index[oof_row], test_index)) != 0) {
    stop("Out-of-fold predictions include outer test rows")
  }
  invisible(NULL)
}

engine_honors_case_weights <- function(engine) {
  if (!engine %in% c("lightgbm", "xgboost")) {
    stop("No class-weight probe for engine ", engine)
  }
  n <- 160
  set.seed(1)
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  quality <- factor(
    ifelse(x1 + stats::rnorm(n) > 0, "high", "low"),
    levels = level_order
  )
  base <- tibble::tibble(x1 = x1, x2 = x2, quality = quality)
  weighted <- base
  weighted$case_weight <- hardhat::importance_weights(rep(1, n))
  fixed <- parsnip::boost_tree(
    trees = 20,
    tree_depth = 2,
    learn_rate = 0.2,
    min_n = 5,
    mtry = 2
  )
  if (identical(engine, "lightgbm")) {
    fixed <- fixed %>%
      parsnip::set_engine("lightgbm", num_threads = 1, seed = 1, deterministic = TRUE) %>%
      parsnip::set_mode("classification")
  } else {
    fixed <- fixed %>%
      parsnip::set_engine("xgboost", nthread = 1) %>%
      parsnip::set_mode("classification")
  }
  recipe <- recipes::recipe(quality ~ ., data = weighted)
  workflow <- workflows::workflow() %>%
    workflows::add_model(fixed) %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_case_weights(case_weight)

  fit_once <- function(weights) {
    data <- base
    data$case_weight <- hardhat::importance_weights(weights)
    if (identical(engine, "xgboost")) {
      set.seed(1)
    }
    fitted <- generics::fit(workflow, data = data)
    predict(fitted, data, type = "prob")$.pred_high
  }

  first <- fit_once(rep(1, n))
  second <- fit_once(rep(1, n))
  if (max(abs(first - second)) > 1e-8) {
    stop("The ", engine, " weight probe is not deterministic")
  }
  changed <- fit_once(ifelse(base$quality == "high", 50, 1))
  max(abs(first - changed)) > 1e-6
}

job_key <- function(job, source_md5, v, repeats, dataset = "red") {
  dataset <- assert_dataset(dataset)
  key <- list(
    seed = job$seed,
    arm = job$arm,
    model = job$model$tag,
    rng = "L'Ecuyer-CMRG",
    split_prop = 0.8,
    split_strata = TRUE,
    v = as.integer(v),
    repeats = as.integer(repeats),
    fold_strata = "quality",
    metric = "bal_accuracy",
    event = "high",
    sources = source_md5
  )
  if (identical(dataset, "white")) {
    key$dataset <- "white"
  }
  key
}

fit_job <- function(job, source_md5, v, repeats, dataset = "red") {
  dataset <- assert_dataset(dataset)
  if (protected_fit_path(job$path)) {
    if (!file.exists(job$path)) {
      stop("Refusing to create a new fit for an existing model: ", job$path)
    }
    loaded <- readRDS(job$path)
    message(sprintf("Kept existing %s / %s / %d", job$model$tag, job$arm, job$seed))
    return(loaded$metrics)
  }

  key <- job_key(job, source_md5, v, repeats, dataset = dataset)
  if (file.exists(job$path)) {
    loaded <- tryCatch(readRDS(job$path), error = function(e) NULL)
    if (is.list(loaded) && identical(loaded$key, key)) {
      message(sprintf("Cached %s / %s / %d", job$model$tag, job$arm, job$seed))
      return(loaded$metrics)
    }
  }

  message(sprintf("Fitting %s / %s / %d", job$model$tag, job$arm, job$seed))
  metrics <- tryCatch(
    score_job(job),
    error = function(e) {
      message(sprintf(
        "No test score for %s / %s / %d: %s",
        job$model$tag, job$arm, job$seed, conditionMessage(e)
      ))
      empty_metrics(job$model, job$arm, job$seed, "na", conditionMessage(e))
    }
  )

  oof <- attr(metrics, "oof")
  test_pred <- attr(metrics, "test_pred")
  train_index <- attr(metrics, "train_index")
  test_index <- attr(metrics, "test_index")
  if (isTRUE(job$model$fresh) && identical(metrics$status, "ok")) {
    if (is.null(oof) || is.null(test_pred) || nrow(oof) == 0 || nrow(test_pred) == 0) {
      stop("The new fit did not save out-of-fold and test predictions")
    }
  }
  metrics_out <- metrics
  attr(metrics_out, "oof") <- NULL
  attr(metrics_out, "test_pred") <- NULL
  attr(metrics_out, "train_index") <- NULL
  attr(metrics_out, "test_index") <- NULL

  tmp <- paste0(job$path, ".tmp.", Sys.getpid())
  saveRDS(
    list(
      key = key,
      metrics = metrics_out,
      oof = oof,
      test = test_pred,
      train_index = train_index,
      test_index = test_index
    ),
    tmp
  )
  file.rename(tmp, job$path)
  metrics_out
}

score_job <- function(job) {
  if (isFALSE(job$weights_supported) && identical(job$arm, "weights")) {
    return(empty_metrics(
      job$model,
      job$arm,
      job$seed,
      "unsupported",
      "The engine ignored class weights on a probe fit."
    ))
  }
  if (isTRUE(job$model$fresh)) {
    set.seed(job$seed)
  }
  if (isTRUE(job$model$score)) {
    return(score_regression_job(job))
  }
  score_classification_job(job)
}

score_classification_job <- function(job) {
  model <- job$model$model
  if (job$arm == "weights" && job$model$tag %in% svm_tags) {
    model <- with_class_weights(model, job$class_weights)
  }
  recipe <- filtered_recipe(job$train, job$arm)
  workflow <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)
  if (job$arm == "weights" && !(job$model$tag %in% svm_tags)) {
    workflow <- workflows::add_case_weights(workflow, case_weight)
  }

  grid <- resolve_grid(job, recipe)

  selection <- yardstick::metric_set(yardstick::bal_accuracy)
  tuned <- tune::tune_grid(
    workflow,
    resamples = job$folds,
    grid = grid,
    metrics = selection,
    control = tune::control_grid(
      save_pred = FALSE,
      verbose = FALSE,
      allow_par = FALSE,
      event_level = "second"
    )
  )
  best <- tune::select_best(tuned, metric = "bal_accuracy")
  final_workflow <- tune::finalize_workflow(workflow, best)
  resampled <- tune::fit_resamples(
    final_workflow,
    resamples = job$folds,
    metrics = yardstick::metric_set(yardstick::bal_accuracy, yardstick::roc_auc),
    control = tune::control_resamples(
      save_pred = TRUE,
      verbose = FALSE,
      allow_par = FALSE,
      event_level = "second"
    )
  )
  oof <- tune::collect_predictions(resampled)
  if (!".pred_high" %in% names(oof)) {
    stop("The selected workflow produced no P(high)")
  }
  threshold <- choose_threshold(oof$quality, oof$.pred_high)

  fitted <- generics::fit(final_workflow, data = job$train)
  test_prob <- predict(fitted, job$test, type = "prob")
  scored <- score_at_threshold(job$test$quality, test_prob$.pred_high, threshold)
  if (!is.finite(scored$bal_accuracy)) {
    stop("The final fit produced no predictions")
  }
  keep_predictions <- isTRUE(job$model$fresh) || isTRUE(job$save_oof)
  if (keep_predictions) {
    assert_oof_excludes_test(oof$.row, job$train_index, job$test_index, nrow(job$train))
  }

  metrics <- tibble::tibble(
    model = job$model$tag,
    model_name = job$model$name,
    arm = job$arm,
    seed = job$seed,
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
    threshold = scored$threshold,
    n_test = nrow(job$test),
    detail = NA_character_,
    params = params_text(best),
    pred_saved = keep_predictions
  )
  if (!keep_predictions) {
    return(metrics)
  }
  attach_predictions(
    metrics,
    oof = tibble::tibble(
      quality = oof$quality,
      .pred_high = oof$.pred_high,
      .row = oof$.row
    ),
    test_pred = tibble::tibble(
      quality = job$test$quality,
      .pred_high = test_prob$.pred_high
    ),
    train_index = job$train_index,
    test_index = job$test_index
  )
}

score_regression_job <- function(job) {
  model <- job$model$model
  recipe <- score_recipe(job$train)
  workflow <- workflows::workflow() %>%
    workflows::add_model(model) %>%
    workflows::add_recipe(recipe)
  grid <- resolve_grid(job, recipe)
  selection <- yardstick::metric_set(bal_accuracy_at_6_5)
  tuned <- tune::tune_grid(
    workflow,
    resamples = job$folds,
    grid = grid,
    metrics = selection,
    control = tune::control_grid(
      save_pred = FALSE,
      verbose = FALSE,
      allow_par = FALSE
    )
  )
  best <- tune::select_best(tuned, metric = "bal_accuracy_at_6_5")
  final_workflow <- tune::finalize_workflow(workflow, best)
  resampled <- tune::fit_resamples(
    final_workflow,
    resamples = job$folds,
    metrics = selection,
    control = tune::control_resamples(
      save_pred = TRUE,
      verbose = FALSE,
      allow_par = FALSE
    )
  )
  oof <- tune::collect_predictions(resampled)
  if (!".pred" %in% names(oof)) {
    stop("The selected score model produced no numeric prediction")
  }
  # The cut search sees only these out-of-fold scores. The test rows are not here.
  assert_oof_excludes_test(oof$.row, job$train_index, job$test_index, nrow(job$train))
  threshold <- choose_score_threshold(oof$quality, oof$.pred)

  fitted <- generics::fit(final_workflow, data = job$train)
  test_score <- predict(fitted, job$test, type = "numeric")
  scored <- score_at_threshold(job$test_class, test_score$.pred, threshold)
  if (!is.finite(scored$bal_accuracy)) {
    stop("The final fit produced no predictions")
  }

  metrics <- tibble::tibble(
    model = job$model$tag,
    model_name = job$model$name,
    arm = job$arm,
    seed = job$seed,
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
    threshold = scored$threshold,
    n_test = nrow(job$test),
    detail = NA_character_,
    params = params_text(best),
    pred_saved = TRUE
  )
  attach_predictions(
    metrics,
    oof = tibble::tibble(
      quality = oof$quality,
      .pred = oof$.pred,
      .row = oof$.row
    ),
    test_pred = tibble::tibble(
      quality = job$test_class,
      score = job$test$quality,
      .pred = test_score$.pred
    ),
    train_index = job$train_index,
    test_index = job$test_index
  )
}

make_outer_splits <- function(data, score, seeds, v, repeats) {
  if (length(score) != nrow(data)) {
    stop("The score vector does not match the data")
  }
  RNGkind("L'Ecuyer-CMRG")
  lapply(seeds, function(seed) {
    set.seed(seed)
    split <- rsample::initial_split(data, prop = 0.8, strata = quality)
    train <- rsample::training(split)
    test <- rsample::testing(split)
    train_index <- as.integer(split, data = "analysis")
    test_index <- as.integer(split, data = "assessment")
    if (!identical(as.character(train$quality), as.character(data$quality[train_index]))) {
      stop("Training rows are not aligned with the split index for seed ", seed)
    }
    if (!identical(as.character(test$quality), as.character(data$quality[test_index]))) {
      stop("Test rows are not aligned with the split index for seed ", seed)
    }
    if (length(intersect(train_index, test_index)) != 0) {
      stop("Outer split overlaps for seed ", seed)
    }
    plain_folds <- rsample::vfold_cv(train, v = v, repeats = repeats, strata = quality)
    weighted_train <- train
    weighted_train$case_weight <- make_case_weights(train)
    weighted_test <- test
    weighted_test$case_weight <- hardhat::importance_weights(rep(1, nrow(test)))
    weighted_folds <- rsample::vfold_cv(
      weighted_train, v = v, repeats = repeats, strata = quality
    )
    score_train <- train
    score_train$quality <- score[train_index]
    score_test <- test
    score_test$quality <- score[test_index]
    if (!identical(as.character(train$quality), ifelse(score_train$quality >= 7, "high", "low"))) {
      stop("Score labels do not match the training classes for seed ", seed)
    }
    if (!identical(as.character(test$quality), ifelse(score_test$quality >= 7, "high", "low"))) {
      stop("Score labels do not match the test classes for seed ", seed)
    }
    list(
      seed = seed,
      train = train,
      test = test,
      folds = plain_folds,
      weighted_train = weighted_train,
      weighted_test = weighted_test,
      weighted_folds = weighted_folds,
      class_weights = class_weight_vector(train),
      train_index = train_index,
      test_index = test_index,
      score_train = score_train,
      score_test = score_test,
      score_folds = retarget_rset(plain_folds, score_train)
    )
  })
}

fit_reevaluation <- function(
  fits_dir = reevaluation_dir(dataset),
  seeds = 2021:2025,
  v = 10L,
  repeats = 10L,
  model_tags = NULL,
  arms = c("rose", "weights", "none"),
  metrics_file = "metrics.rds",
  run_file = "run.rds",
  write_baseline = TRUE,
  dataset = "red"
) {
  dataset <- assert_dataset(dataset)
  dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(OMP_NUM_THREADS = "1")
  cores <- parallel::detectCores(logical = TRUE)
  if (is.na(cores) || cores < 1) {
    cores <- 1L
  }
  cores <- as.integer(cores)

  loaded <- load_recode_and_score(dataset = dataset)
  data <- loaded$data
  outers <- make_outer_splits(data, loaded$score, seeds, v, repeats)
  models <- reevaluation_models()
  if (is.null(model_tags)) {
    model_tags <- original_tags
  }
  source_md5 <- vapply(reevaluation_source_files(dataset), file_md5, character(1))

  probe <- list()
  jobs <- list()
  for (outer in outers) {
    for (tag in model_tags) {
      model <- models[[tag]]
      if (is.null(model)) {
        stop("Unknown model ", tag)
      }
      model_arms <- model$arms
      if (is.null(model_arms)) {
        model_arms <- arms
      }
      weights_supported <- TRUE
      if (isTRUE(model$probe_weights)) {
        engine <- model$model$engine
        if (is.null(probe[[engine]])) {
          probe[[engine]] <- engine_honors_case_weights(engine)
          message(sprintf(
            "Class weights for %s: %s",
            engine,
            if (isTRUE(probe[[engine]])) "honored" else "unsupported"
          ))
        }
        weights_supported <- isTRUE(probe[[engine]])
      }
      for (arm in model_arms) {
        file <- sprintf("%s__%s__%s.rds", tag, arm, outer$seed)
        use_case_weights <- identical(arm, "weights") && !(tag %in% svm_tags)
        use_score <- isTRUE(model$score)
        jobs[[length(jobs) + 1]] <- list(
          model = model,
          arm = arm,
          seed = outer$seed,
          train = if (use_score) outer$score_train else if (use_case_weights) outer$weighted_train else outer$train,
          test = if (use_score) outer$score_test else if (use_case_weights) outer$weighted_test else outer$test,
          folds = if (use_score) outer$score_folds else if (use_case_weights) outer$weighted_folds else outer$folds,
          class_weights = outer$class_weights,
          test_class = if (use_score) outer$test$quality else NULL,
          train_index = outer$train_index,
          test_index = outer$test_index,
          weights_supported = weights_supported,
          grid_override = NULL,
          save_oof = identical(dataset, "white") &&
            identical(arm, "none") &&
            tag %in% c("svm_radial", "rf"),
          path = file.path(fits_dir, file)
        )
      }
    }
  }

  baseline <- dplyr::bind_rows(lapply(outers, function(outer) {
    scored <- guess_low_metrics(outer$test$quality)
    tibble::tibble(
      model = "guess_low",
      model_name = "Guess low",
      arm = "baseline",
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
      threshold = scored$threshold,
      n_test = nrow(outer$test),
      detail = NA_character_,
      params = NA_character_,
      pred_saved = FALSE
    )
  }))
  if (isTRUE(write_baseline)) {
    saveRDS(baseline, file.path(fits_dir, "baseline.rds"))
  }

  message(sprintf(
    "Fitting %d combinations on %d cores, seeds %s, %d x %d CV",
    length(jobs), cores, paste(seeds, collapse = ","), v, repeats
  ))
  started <- proc.time()[["elapsed"]]
  out <- parallel::mclapply(
    jobs,
    fit_job,
    source_md5 = source_md5,
    v = v,
    repeats = repeats,
    dataset = dataset,
    mc.cores = cores,
    mc.preschedule = FALSE,
    mc.set.seed = TRUE
  )
  failed <- vapply(out, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop(paste(vapply(out[failed], as.character, character(1)), collapse = "\n"))
  }

  metrics <- dplyr::bind_rows(out)
  saveRDS(metrics, file.path(fits_dir, metrics_file))
  wall_seconds <- proc.time()[["elapsed"]] - started
  run <- list(
    cores = cores,
    wall_seconds = wall_seconds,
    seeds = seeds,
    v = as.integer(v),
    repeats = as.integer(repeats),
    combinations = length(jobs),
    model_tags = model_tags
  )
  saveRDS(run, file.path(fits_dir, run_file))
  message(sprintf("Fit finished in %.1f seconds on %d cores", wall_seconds, cores))
  run
}
