library(bonsai)
library(lightgbm)

read_original_models <- function() {
  env <- new.env(parent = globalenv())
  files <- c(
    "utils/estimation/models/logistic_regression.R",
    "utils/estimation/models/support_vector_machine_with_polynomial_kernel.R",
    "utils/estimation/models/support_vector_machine_with_radial_kernel.R",
    "utils/estimation/models/linear_discriminant_analysis.R",
    "utils/estimation/models/flexible_discriminant_analysis.R"
  )
  for (file in files) {
    source(file, local = env)
  }
  list(
    lr = env$lr,
    svm_poly = env$svm_poly,
    svm_radial = env$svm_radial,
    lda = env$lda,
    fda = env$fda
  )
}

reevaluation_models <- function() {
  models <- read_original_models()
  models$lr$name <- "Logistic regression"
  models$rf <- list(
    model = parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune(), trees = 500) %>%
      parsnip::set_engine("ranger", num.threads = 1) %>%
      parsnip::set_mode("classification"),
    name = "Random forest",
    tag = "rf",
    grid = NULL
  )
  models$xgb <- list(
    model = parsnip::boost_tree(
      trees = 500,
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      min_n = 10
    ) %>%
      parsnip::set_engine("xgboost", nthread = 1) %>%
      parsnip::set_mode("classification"),
    name = "Gradient boosting",
    tag = "xgb",
    grid = dials::grid_regular(dials::tree_depth(), dials::learn_rate(), levels = 3)
  )

  # Forty space-filling candidates. learn_rate is log10 from -2 to -0.5
  # (0.01 to about 0.316), which covers the rate the nine-point grid selects.
  # Trees start at 50 so the fixed early-stopping patience of 20 is legal.
  # num_leaves is the lightgbm capacity parameter; max_depth stays at the
  # engine default. The grid is deterministic for a given predictor count.
  models$lgb <- list(
    model = lightgbm_spec("classification"),
    name = "LightGBM",
    tag = "lgb",
    grid = NULL,
    grid_fn = lightgbm_grid,
    arms = c("weights", "none"),
    fresh = TRUE,
    probe_weights = TRUE,
    score = FALSE
  )
  models$xgb_full <- list(
    model = parsnip::boost_tree(
      trees = 500,
      tree_depth = tune::tune(),
      learn_rate = tune::tune(),
      min_n = tune::tune(),
      mtry = tune::tune(),
      loss_reduction = tune::tune(),
      sample_size = tune::tune()
    ) %>%
      parsnip::set_engine("xgboost", nthread = 1) %>%
      parsnip::set_mode("classification"),
    name = "Gradient boosting (full search)",
    tag = "xgb_full",
    grid = NULL,
    grid_fn = xgb_full_grid,
    arms = c("weights", "none"),
    fresh = TRUE,
    probe_weights = TRUE,
    score = FALSE
  )
  models$score_rf <- list(
    model = parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune(), trees = 500) %>%
      parsnip::set_engine("ranger", num.threads = 1, seed = 1L) %>%
      parsnip::set_mode("regression"),
    name = "Random forest on the score",
    tag = "score_rf",
    grid = NULL,
    grid_fn = rf_grid,
    arms = "score",
    fresh = TRUE,
    probe_weights = FALSE,
    score = TRUE
  )
  models$score_lgb <- list(
    model = lightgbm_spec("regression"),
    name = "LightGBM on the score",
    tag = "score_lgb",
    grid = NULL,
    grid_fn = lightgbm_grid,
    arms = "score",
    fresh = TRUE,
    probe_weights = FALSE,
    score = TRUE
  )
  models$score_lin <- list(
    model = parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
      parsnip::set_engine("glmnet"),
    name = "Penalised regression on the score",
    tag = "score_lin",
    grid = dials::grid_regular(dials::penalty(), dials::mixture(), levels = 5),
    arms = "score",
    fresh = TRUE,
    probe_weights = FALSE,
    score = TRUE
  )
  # select_features = TRUE is mgcv's select = TRUE. REML picks the smoothness
  # on the likelihood. There is no tuning grid.
  models$gam <- list(
    model = parsnip::gen_additive_mod(select_features = TRUE) %>%
      parsnip::set_engine("mgcv", method = "REML") %>%
      parsnip::set_mode("classification"),
    name = "Penalised GAM",
    tag = "gam",
    grid = NULL,
    grid_fn = NULL,
    arms = c("weights", "none"),
    fresh = TRUE,
    probe_weights = FALSE,
    score = FALSE
  )
  models
}

num_leaves_param <- function(range = c(7L, 127L)) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    label = c(num_leaves = "Number of Leaves"),
    finalize = NULL
  )
}

# validation = 0.2 makes bonsai sample a hold-out from the matrix it is given.
# tune_grid gives it the analysis split, so the assessment fold is not in that draw.
lightgbm_spec <- function(mode) {
  parsnip::boost_tree(
    trees = tune::tune(),
    learn_rate = tune::tune(),
    min_n = tune::tune(),
    mtry = tune::tune(),
    stop_iter = 20
  ) %>%
    parsnip::set_engine(
      "lightgbm",
      num_threads = 1,
      validation = 0.2,
      num_leaves = tune::tune()
    ) %>%
    parsnip::set_mode(mode)
}

lightgbm_grid <- function(n_predictors) {
  if (!is.numeric(n_predictors) || n_predictors < 1) {
    stop("LightGBM needs at least one predictor")
  }
  grid <- withr::with_seed(20210321, {
    dials::grid_space_filling(
      dials::trees(range = c(50L, 1000L)),
      dials::learn_rate(range = c(-2, -0.5)),
      dials::min_n(),
      dials::mtry(c(1L, as.integer(n_predictors))),
      num_leaves_param(),
      size = 40
    )
  })
  expected <- c("trees", "learn_rate", "min_n", "mtry", "num_leaves")
  if (nrow(grid) != 40 || !all(expected %in% names(grid))) {
    stop("LightGBM grid is not the 40-candidate search")
  }
  grid
}

xgb_full_grid <- function(n_predictors) {
  if (!is.numeric(n_predictors) || n_predictors < 1) {
    stop("XGBoost needs at least one predictor")
  }
  grid <- withr::with_seed(20210321, {
    dials::grid_space_filling(
      dials::tree_depth(),
      dials::learn_rate(range = c(-2, -0.5)),
      dials::min_n(),
      dials::mtry(c(1L, as.integer(n_predictors))),
      dials::loss_reduction(),
      dials::sample_prop(),
      size = 40
    )
  })
  expected <- c("tree_depth", "learn_rate", "min_n", "mtry", "loss_reduction", "sample_size")
  if (nrow(grid) != 40 || !all(expected %in% names(grid))) {
    stop("Full XGBoost grid is not the 40-candidate search")
  }
  grid
}

svm_tags <- c("svm_poly", "svm_radial")

with_class_weights <- function(model, weights) {
  parsnip::set_engine(model, "kernlab", class.weights = !!weights)
}

# kernlab's class-weighted probability model refits Platt scaling on
# as.factor(level names). as.factor sorts those names, so low/high comes
# back with the columns swapped relative to the decision value. The decision
# value itself stays oriented: a positive value is the second factor level.
# Correct the two columns here, before any metric is computed.
kernlab_class_weight_prob_post <- function(result, object) {
  result <- tibble::as_tibble(result)
  weights <- object$fit@kcall$class.weights
  levels <- kernlab::lev(object$fit)
  flipped <- !is.null(weights) && length(levels) == 2 && !identical(sort(levels), levels)
  if (flipped) {
    values <- as.matrix(result[, levels, drop = FALSE])
    result[[levels[[1]]]] <- values[, 2]
    result[[levels[[2]]]] <- values[, 1]
  }
  result
}

install_kernlab_class_weight_prob_fix <- function() {
  model_env <- parsnip:::get_model_env()
  for (model in c("svm_poly", "svm_rbf")) {
    key <- paste0(model, "_predict")
    pred <- model_env[[key]]
    hit <- which(
      pred$engine == "kernlab" &
        pred$mode == "classification" &
        pred$type == "prob"
    )
    if (length(hit) != 1) {
      stop("Expected one kernlab probability module for ", model)
    }
    value <- pred$value[[hit]]
    value$post <- kernlab_class_weight_prob_post
    pred$value[[hit]] <- value
    model_env[[key]] <- pred
  }
  invisible(NULL)
}

install_kernlab_class_weight_prob_fix()
