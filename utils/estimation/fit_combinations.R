project_seed <- 2020L

model_source_files <- c(
  lr = "utils/estimation/models/logistic_regression.R",
  svm_poly = "utils/estimation/models/support_vector_machine_with_polynomial_kernel.R",
  svm_radial = "utils/estimation/models/support_vector_machine_with_radial_kernel.R",
  lda = "utils/estimation/models/linear_discriminant_analysis.R",
  fda = "utils/estimation/models/flexible_discriminant_analysis.R"
)

recipe_source_files <- c(
  fd = "utils/estimation/recipes/filtered_dataset.R",
  sfd = "utils/estimation/recipes/standardised_filtered_dataset.R",
  sfpc = "utils/estimation/recipes/standardised_filtered_principal_components.R"
)

shared_source_files <- c(
  "utils/recipes/step_recipe.R",
  "utils/estimation/pipeline.R",
  "utils/load_raw_data.R",
  "utils/load_recoded_data.R",
  "utils/recode_to_high_low.R",
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

check_dataset <- function(dataset) {
  if (!identical(dataset, "red") && !identical(dataset, "white")) {
    stop("dataset must be \"red\" or \"white\", not ", dataset)
  }
  dataset
}

combination_fits_dir <- function(dataset = "red") {
  dataset <- check_dataset(dataset)
  if (identical(dataset, "white")) {
    "results/reevaluation/white/fits"
  } else {
    "results/fits"
  }
}

combination_shared_files <- function(dataset = "red") {
  dataset <- check_dataset(dataset)
  if (identical(dataset, "red")) {
    return(shared_source_files)
  }
  c(
    "utils/recipes/step_recipe.R",
    "utils/estimation/pipeline.R",
    "utils/load_raw_data.R",
    "utils/white_wine.R",
    "data/winequality-white.csv",
    "renv.lock"
  )
}

combination_key <- function(model_tag, recipe_tag, shared_md5, dataset = "red") {
  dataset <- check_dataset(dataset)
  key <- list(
    seed = project_seed,
    rng = "L'Ecuyer-CMRG",
    split_prop = 0.8,
    split_strata = FALSE,
    v = 10L,
    repeats = 10L,
    fold_strata = "quality",
    metric = "accuracy",
    model = file_md5(model_source_files[[model_tag]]),
    recipe = file_md5(recipe_source_files[[recipe_tag]]),
    shared = shared_md5
  )
  if (identical(dataset, "white")) {
    key$dataset <- "white"
  }
  key
}

fit_combinations <- function(dataset = "red") {
  dataset <- check_dataset(dataset)
  fits_dir <- combination_fits_dir(dataset)
  dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)

  cores <- parallel::detectCores(logical = TRUE)
  if (is.na(cores) || cores < 1) {
    cores <- 1L
  }
  cores <- as.integer(cores)

  RNGkind("L'Ecuyer-CMRG")
  set.seed(project_seed)

  if (identical(dataset, "white")) {
    data <- load_white_recoded_data("data/winequality-white.csv", "quality")
  } else {
    data <- load_recoded_data("data/winequality-red.csv", "quality")
  }
  data_split <- rsample::initial_split(data, prop = .8)
  train_data <- rsample::training(data_split)
  training_folds <- rsample::vfold_cv(train_data, v = 10, repeats = 10, strata = quality)
  metrics <- yardstick::metric_set(yardstick::accuracy)
  selection_metric <- "accuracy"

  combos <- list(
    model_config = model_specs,
    recipe_config = recipe_specs(train_data, "quality")
  ) %>% purrr::cross()

  shared_md5 <- vapply(combination_shared_files(dataset), file_md5, character(1), USE.NAMES = FALSE)

  jobs <- lapply(combos, function(combo) {
    model_tag <- combo$model_config$tag
    recipe_tag <- combo$recipe_config$tag
    file <- sprintf("%s__%s.rds", model_tag, recipe_tag)
    list(
      combo = combo,
      key = combination_key(model_tag, recipe_tag, shared_md5, dataset = dataset),
      path = file.path(fits_dir, file),
      file = file
    )
  })

  message(sprintf(
    "Fitting %d combinations on %d cores, L'Ecuyer-CMRG seed %d",
    length(jobs), cores, project_seed
  ))

  started <- proc.time()[["elapsed"]]

  fit_or_load <- function(job) {
    if (file.exists(job$path)) {
      loaded <- tryCatch(readRDS(job$path), error = function(e) NULL)
      if (is.list(loaded) && identical(loaded$key, job$key)) {
        message(sprintf("Cached %s / %s", job$combo$model_config$tag, job$combo$recipe_config$tag))
        return(loaded$summary)
      }
    }

    full <- pipeline(
      job$combo$model_config,
      job$combo$recipe_config,
      metrics,
      selection_metric,
      training_folds,
      train_data,
      data_split
    )
    metrics <- full$test$metrics
    accuracy <- if (is.null(metrics)) numeric() else metrics$.estimate[metrics$.metric == "accuracy"]
    if (length(accuracy) != 1 || !is.finite(accuracy[[1]])) {
      message(sprintf(
        "No test accuracy for %s / %s; the final fit produced no predictions",
        job$combo$model_config$tag,
        job$combo$recipe_config$tag
      ))
      metrics <- tibble::tibble(
        .metric = "accuracy",
        .estimator = "binary",
        .estimate = NA_real_,
        .config = NA_character_
      )
    }
    summary <- list(
      model_config = list(tag = full$model_config$tag, name = full$model_config$name),
      recipe_config = list(tag = full$recipe_config$tag, name = full$recipe_config$name),
      test = list(metrics = metrics),
      best = list(result = full$best$result),
      plot = full$plot
    )
    tmp <- paste0(job$path, ".tmp.", Sys.getpid())
    saveRDS(list(key = job$key, summary = summary), tmp)
    file.rename(tmp, job$path)
    summary
  }

  out <- parallel::mclapply(
    jobs,
    fit_or_load,
    mc.cores = cores,
    mc.preschedule = FALSE,
    mc.set.seed = TRUE
  )

  failed <- vapply(out, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop(paste(vapply(out[failed], as.character, character(1)), collapse = "\n"))
  }

  saveRDS(vapply(jobs, function(job) job$file, character(1)), file.path(fits_dir, "manifest.rds"))

  seconds <- proc.time()[["elapsed"]] - started
  message(sprintf("Fit finished in %.1f seconds on %d cores", seconds, cores))
  list(cores = cores, seconds = seconds, n = length(jobs))
}
