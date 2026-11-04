# Shared pieces for the white and red re-evaluation reports.
# The caller loads libraries.R and passes the sample and the results directory.

model_order <- function() {
  c(
    "lr", "svm_poly", "svm_radial", "lda", "fda",
    "rf", "xgb", "lgb", "xgb_full", "score_rf", "score_lgb", "score_lin",
    "gam", "stack", "average"
  )
}

arm_order <- function() {
  c("none", "rose", "weights", "score", "blend", "mean")
}

arm_labels <- function() {
  c(
    none = "No resampling",
    rose = "ROSE",
    weights = "Class weights",
    score = "Score",
    blend = "Out-of-fold blend",
    mean = "Out-of-fold mean"
  )
}

short_model_names <- function() {
  c(
    lr = "Logistic",
    svm_poly = "Polynomial SVM",
    svm_radial = "Radial SVM",
    lda = "LDA",
    fda = "FDA",
    rf = "Random forest",
    xgb = "Boosting",
    lgb = "LightGBM",
    xgb_full = "Full boosting",
    score_rf = "Score forest",
    score_lgb = "Score LightGBM",
    score_lin = "Score regression",
    gam = "GAM",
    stack = "Stack",
    average = "Average"
  )
}

pretty_name <- function(x) {
  gsub(".", " ", x, fixed = TRUE)
}

num17 <- function(x) {
  sprintf("%.17g", x)
}

chemistry <- function(data) {
  data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
}

metric_cols <- function(df) {
  if (!"pr_auc" %in% names(df)) {
    df$pr_auc <- NA_real_
  }
  df[, c("model", "model_name", "arm", "seed", "status", "bal_accuracy", "pr_auc")]
}

read_gam_metrics <- function(results_dir) {
  files <- list.files(
    results_dir,
    pattern = "^gam__[a-z]+__[0-9]{4}\\.rds$",
    full.names = TRUE
  )
  if (length(files) == 0) {
    stop("No GAM fits were found in ", results_dir)
  }
  rows <- lapply(files, function(file) metric_cols(readRDS(file)$metrics))
  do.call(rbind, rows)
}

read_blend_metrics <- function(results_dir, prefix, model, model_name, arm) {
  rows <- lapply(2021:2025, function(seed) {
    item <- readRDS(file.path(results_dir, sprintf("%s__%d.rds", prefix, seed)))
    ok <- identical(item$status, "ok")
    data.frame(
      model = model,
      model_name = model_name,
      arm = arm,
      seed = seed,
      status = item$status,
      bal_accuracy = if (ok) item$metrics$bal_accuracy[[1]] else NA_real_,
      pr_auc = if (ok) item$metrics$pr_auc[[1]] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

load_report_metrics <- function(results_dir) {
  metrics <- metric_cols(readRDS(file.path(results_dir, "metrics.rds")))
  added_path <- file.path(results_dir, "metrics_added.rds")
  if (file.exists(added_path)) {
    metrics <- rbind(metrics, metric_cols(readRDS(added_path)))
  }
  pr_path <- file.path(results_dir, "metrics_pr_original.rds")
  if (file.exists(pr_path)) {
    pr <- readRDS(pr_path)
    key <- paste(metrics$model, metrics$arm, metrics$seed, sep = "\r")
    pr_key <- paste(pr$model, pr$arm, pr$seed, sep = "\r")
    hit <- match(key, pr_key)
    fill <- is.na(metrics$pr_auc) & !is.na(hit)
    metrics$pr_auc[fill] <- pr$pr_auc[hit[fill]]
  }
  if (any(is.na(metrics$pr_auc))) {
    stop("A stored fit is missing PR AUC")
  }
  if (!any(metrics$model == "gam")) {
    metrics <- rbind(metrics, read_gam_metrics(results_dir))
  }
  metrics <- rbind(
    metrics,
    read_blend_metrics(results_dir, "stack", "stack", "Stack", "blend"),
    read_blend_metrics(results_dir, "average", "average", "Simple average", "mean")
  )
  missing <- setdiff(model_order(), unique(metrics$model))
  if (length(missing) > 0) {
    stop("Missing models: ", paste(missing, collapse = ", "))
  }
  counts <- tapply(metrics$seed, paste(metrics$model, metrics$arm), length)
  if (any(counts != 5L)) {
    stop("A model arm does not have five splits")
  }
  metrics$arm <- as.character(metrics$arm)
  metrics$seed <- as.integer(metrics$seed)
  metrics
}

align_named <- function(named, seeds) {
  unname(named[as.character(seeds)])
}

load_stack_record <- function(results_dir, metrics) {
  decision <- readRDS(file.path(results_dir, "stack_decision.rds"))
  svm <- metrics[metrics$model == "svm_radial" & metrics$arm == "none", ]
  svm <- svm[order(svm$seed), ]
  average <- metrics[metrics$model == "average" & metrics$arm == "mean", ]
  average <- average[order(average$seed), ]
  stack <- metrics[metrics$model == "stack" & metrics$arm == "blend", ]
  stack <- stack[order(stack$seed), ]
  if (!isTRUE(all.equal(align_named(decision$svm, svm$seed), svm$bal_accuracy))) {
    stop("The stack decision does not match the stored radial SVM")
  }
  if (!isTRUE(all.equal(align_named(decision$average, average$seed), average$bal_accuracy))) {
    stop("The stack decision does not match the stored average")
  }
  if (!isTRUE(all.equal(align_named(decision$stack, stack$seed), stack$bal_accuracy))) {
    stop("The stack decision does not match the stored stack")
  }
  d_svm <- stack$bal_accuracy - svm$bal_accuracy
  d_avg <- stack$bal_accuracy - average$bal_accuracy
  if (!isTRUE(all.equal(mean(d_svm), decision$svm_gap$mean))) {
    stop("The SVM gap mean does not match the five differences")
  }
  if (!isTRUE(all.equal(stats::sd(d_svm), decision$svm_gap$sd))) {
    stop("The SVM gap sd does not match the five differences")
  }
  if (!identical(as.integer(sum(d_svm > 0)), as.integer(decision$svm_gap$positive))) {
    stop("The SVM gap count does not match the five differences")
  }
  if (!isTRUE(all.equal(mean(d_avg), decision$average_gap$mean))) {
    stop("The average gap mean does not match the five differences")
  }
  if (!isTRUE(all.equal(stats::sd(d_avg), decision$average_gap$sd))) {
    stop("The average gap sd does not match the five differences")
  }
  if (!identical(as.integer(sum(d_avg > 0)), as.integer(decision$average_gap$positive))) {
    stop("The average gap count does not match the five differences")
  }
  penalties <- vapply(2021:2025, function(seed) {
    readRDS(file.path(results_dir, sprintf("stack__%d.rds", seed)))$penalty[[1]]
  }, numeric(1))
  floor_penalty <- min(dials::grid_regular(dials::penalty(), levels = 25)$penalty)
  decision$gaps_svm <- d_svm
  decision$gaps_average <- d_avg
  decision$seeds_in_order <- svm$seed
  list(
    decision = decision,
    penalties = penalties,
    penalty_floor = floor_penalty
  )
}

metric_means <- function(metrics) {
  keys <- unique(metrics[, c("model", "model_name", "arm")])
  keys <- keys[order(match(keys$model, model_order()), match(keys$arm, arm_order())), , drop = FALSE]
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    sub <- metrics[metrics$model == keys$model[[i]] & metrics$arm == keys$arm[[i]], ]
    data.frame(
      model = keys$model[[i]],
      model_name = keys$model_name[[i]],
      arm = keys$arm[[i]],
      ba = mean(sub$bal_accuracy),
      pr = mean(sub$pr_auc),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

mean_of <- function(metrics, model, arm) {
  rows <- metric_means(metrics)
  hit <- rows[rows$model == model & rows$arm == arm, ]
  if (nrow(hit) != 1) {
    stop("Expected one mean for ", model, " / ", arm)
  }
  hit
}

arm_sentence <- function(metrics, model) {
  rows <- metric_means(metrics)
  rows <- rows[rows$model == model, ]
  rows <- rows[order(match(rows$arm, arm_order())), ]
  labels <- arm_labels()
  pieces <- sprintf(
    "%s: mean balanced accuracy %.4f, mean PR AUC %.4f",
    unname(labels[rows$arm]),
    rows$ba,
    rows$pr
  )
  paste0(paste(pieces, collapse = ". "), ".\n")
}

leader_sentence <- function(metrics) {
  rows <- metric_means(metrics)
  rows <- rows[!rows$model %in% c("stack", "average"), ]
  labels <- arm_labels()
  ba <- rows[which.max(rows$ba), ]
  pr <- rows[which.max(rows$pr), ]
  ref <- mean_of(metrics, "svm_radial", "none")
  if (identical(ba$model, "svm_radial") && identical(ba$arm, "none") &&
      identical(pr$model, "svm_radial") && identical(pr$arm, "none")) {
    sprintf(
      "Among the individual fits, the radial SVM with no resampling has both the highest mean balanced accuracy, %s, and the highest mean PR AUC, %s. That fit is the reference for the paired gaps.\n",
      num17(ba$ba),
      num17(pr$pr)
    )
  } else {
    sprintf(
      paste0(
        "Among the individual fits, the highest mean balanced accuracy is %s for the %s (%s), ",
        "and the highest mean PR AUC is %s for the %s (%s). ",
        "The radial SVM with no resampling is the reference for the paired gaps, ",
        "with mean balanced accuracy %s and mean PR AUC %s.\n"
      ),
      num17(ba$ba), tolower(ba$model_name), unname(labels[ba$arm]),
      num17(pr$pr), tolower(pr$model_name), unname(labels[pr$arm]),
      num17(ref$ba), num17(ref$pr)
    )
  }
}

return_kable <- function(data, caption) {
  knitr::kable(
    data,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = caption,
    linesep = ""
  )
}

mean_metric_kable <- function(metrics) {
  rows <- metric_means(metrics)
  labels <- arm_labels()
  shown <- data.frame(
    Model = rows$model_name,
    Arm = unname(labels[rows$arm]),
    `Balanced accuracy` = sprintf("%.4f", rows$ba),
    `PR AUC` = sprintf("%.4f", rows$pr),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  return_kable(
    shown,
    "Mean test balanced accuracy and PR AUC over seeds 2021 to 2025."
  )
}

gap_frame <- function(metrics, reference_ba, seeds) {
  rows <- metric_means(metrics)
  labels <- arm_labels()
  short <- short_model_names()
  diffs <- lapply(seq_len(nrow(rows)), function(i) {
    sub <- metrics[metrics$model == rows$model[[i]] & metrics$arm == rows$arm[[i]], ]
    sub <- sub[match(seeds, sub$seed), ]
    sub$bal_accuracy - reference_ba
  })
  mat <- do.call(rbind, diffs)
  shown <- data.frame(
    Model = unname(short[rows$model]),
    Arm = unname(labels[rows$arm]),
    stringsAsFactors = FALSE
  )
  for (j in seq_along(seeds)) {
    shown[[as.character(seeds[[j]])]] <- sprintf("%+.4f", mat[, j])
  }
  shown$Mean <- sprintf("%+.4f", rowMeans(mat))
  shown$SD <- sprintf("%.4f", apply(mat, 1, stats::sd))
  shown
}

gap_kable <- function(metrics, reference_ba, seeds, caption) {
  return_kable(gap_frame(metrics, reference_ba, seeds), caption)
}

stack_gap_kable <- function(record) {
  decision <- record$decision
  seeds <- decision$seeds_in_order
  shown <- data.frame(
    Reference = c("Radial SVM, no resampling", "Simple average"),
    stringsAsFactors = FALSE
  )
  for (j in seq_along(seeds)) {
    shown[[as.character(seeds[[j]])]] <- sprintf(
      "%+.4f",
      c(decision$gaps_svm[[j]], decision$gaps_average[[j]])
    )
  }
  shown$Mean <- sprintf("%+.4f", c(mean(decision$gaps_svm), mean(decision$gaps_average)))
  shown$SD <- sprintf("%.4f", c(stats::sd(decision$gaps_svm), stats::sd(decision$gaps_average)))
  return_kable(
    shown,
    "Five signed paired gaps in test balanced accuracy for the stack. Positive means the stack is higher on that split."
  )
}

gap_clears <- function(gap) {
  isTRUE(gap$positive >= 4L && gap$mean > gap$sd)
}

stack_prose <- function(record) {
  decision <- record$decision
  penalties <- record$penalties
  floor_penalty <- record$penalty_floor
  gap_bit <- function(gap, label) {
    c(
      sprintf("Against %s, the mean gap is %s.", label, num17(gap$mean)),
      sprintf("The sample sd is %s.", num17(gap$sd)),
      sprintf(
        "%d of 5 gaps are strictly positive, so that gap %s the rule.",
        gap$positive,
        if (gap_clears(gap)) "clears" else "does not clear"
      )
    )
  }
  if (length(unique(penalties)) == 1 && isTRUE(all.equal(penalties[[1]], floor_penalty))) {
    pen_sentence <- sprintf(
      "The penalty is %s on every seed, the floor of the 25-point grid.",
      sprintf("%.0e", penalties[[1]])
    )
  } else {
    pen_sentence <- sprintf(
      "The penalties are %s. The floor of the 25-point grid is %s.",
      paste(sprintf("%.0e", penalties), collapse = ", "),
      sprintf("%.0e", floor_penalty)
    )
  }
  keep_sentence <- if (isTRUE(decision$keep)) {
    "The stored keep flag is TRUE, so the rule keeps the stack."
  } else {
    "The stored keep flag is FALSE, so the rule does not keep the stack."
  }
  paste(
    c(
      gap_bit(decision$svm_gap, "the radial SVM with no resampling"),
      gap_bit(decision$average_gap, "the simple average"),
      pen_sentence,
      keep_sentence
    ),
    collapse = "\n\n"
  )
}

gam_edf_kable <- function(results_dir) {
  files <- list.files(
    results_dir,
    pattern = "^gam__[a-z]+__[0-9]{4}\\.rds$",
    full.names = TRUE
  )
  rows <- lapply(files, function(file) {
    item <- readRDS(file)$metrics
    edf <- item$edf[[1]]
    data.frame(
      arm = item$arm[[1]],
      term = names(edf),
      edf = as.numeric(edf),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, rows)
  terms <- unique(long$term)
  cell <- function(arm_name) {
    vapply(terms, function(term) {
      mean(long$edf[long$arm == arm_name & long$term == term])
    }, numeric(1))
  }
  labels <- arm_labels()
  shown <- data.frame(
    Term = pretty_name(terms),
    stringsAsFactors = FALSE
  )
  shown[[unname(labels[["weights"]])]] <- sprintf("%.3f", cell("weights"))
  shown[[unname(labels[["none"]])]] <- sprintf("%.3f", cell("none"))
  return_kable(
    shown,
    "Mean effective degrees of freedom of the penalised GAM across the five final training fits."
  )
}

fit_class_maps <- function(data, k) {
  nums <- as.data.frame(chemistry(data))
  maps <- lapply(c("low", "high"), function(cls) {
    set.seed(2021)
    rrcov::PcaHubert(
      nums[data$quality == cls, , drop = FALSE],
      k = k,
      kmax = max(10L, ncol(nums)),
      alpha = 1,
      scale = TRUE,
      crit.pca.distances = 0.999,
      trace = FALSE
    )
  })
  names(maps) <- c("low", "high")
  maps
}

outlier_counts <- function(maps) {
  vapply(maps, function(fit) {
    sum(as.numeric(fit@flag) == 0)
  }, integer(1))
}

draw_saved_maps <- function(maps) {
  colors <- c(low = "#4C78A8", high = "#F58518")
  for (cls in c("low", "high")) {
    plot(maps[[cls]], col = unname(colors[[cls]]), pch = 16, main = NULL)
    legend("topleft", legend = cls, pch = 16, col = unname(colors[[cls]]))
    title(sprintf("Outlier detection in wines with %s quality", cls))
  }
  invisible(maps)
}

prepare_eda <- function(sample, scores, train_index, test_index) {
  if (!exists("step_winsorise_train", mode = "function")) {
    source("reevaluation/step_winsorise.R")
  }
  if (length(scores) != nrow(sample)) {
    stop("The scores do not match the sample")
  }
  if (length(intersect(train_index, test_index)) != 0) {
    stop("The training rows and the test rows overlap")
  }
  if (!identical(sort(c(train_index, test_index)), seq_len(nrow(sample)))) {
    stop("The stored split does not cover the sample")
  }
  if (anyNA(sample)) {
    stop("The sample has a missing value")
  }
  n_high <- sum(sample$quality == "high")
  n_low <- sum(sample$quality == "low")
  train <- sample[train_index, , drop = FALSE]
  recipe <- recipes::recipe(quality ~ ., data = train) %>%
    step_winsorise_train(recipes::all_numeric(), prob = 0.95)
  prepped <- recipes::prep(recipe, training = train)
  step <- prepped$steps[[1]]
  if (!isTRUE(all.equal(unname(step$prob), 0.95))) {
    stop("The trimming probability is not 0.95")
  }
  if (!isTRUE(all.equal(unname(step$const), 2))) {
    stop("The Huber constant is not 2")
  }
  if (!isTRUE(all.equal(unname(step$center[["alcohol"]]), stats::median(train$alcohol)))) {
    stop("The alcohol centre is not the training median")
  }
  if (!isTRUE(all.equal(
    unname(step$scale[["alcohol"]]),
    stats::mad(train$alcohol - stats::median(train$alcohol), center = 0)
  ))) {
    stop("The alcohol scale is not the training MAD")
  }
  baked <- recipes::bake(prepped, new_data = sample)
  values <- as.matrix(sample[, step$columns, drop = FALSE])
  standardized <- sweep(values, 2, step$center[step$columns], "-")
  standardized <- sweep(standardized, 2, step$scale[step$columns], "/")
  distance <- rowSums(
    (standardized %*% step$inv_cor[step$columns, step$columns, drop = FALSE]) * standardized
  )
  moved <- distance > step$cutoff

  chem <- chemistry(sample)
  high <- sample$quality == "high"
  shifts <- vapply(chem, function(col) mean(col[high]) - mean(col[!high]), numeric(1))
  standardised <- shifts / vapply(chem, stats::sd, numeric(1))
  shift_df <- data.frame(
    variable = names(standardised),
    difference = as.numeric(shifts),
    standardised = as.numeric(standardised),
    low_mean = vapply(chem, function(col) mean(col[!high]), numeric(1)),
    high_mean = vapply(chem, function(col) mean(col[high]), numeric(1)),
    stringsAsFactors = FALSE
  )
  shift_df <- shift_df[order(-abs(shift_df$standardised)), ]
  keep <- shift_df[abs(shift_df$standardised) >= 0.2, ]
  if (nrow(keep) < 3) {
    keep <- shift_df[seq_len(min(3, nrow(shift_df))), ]
  }
  shift_sentence <- sprintf(
    "The largest standardised differences, high minus low, are %s.",
    paste(sprintf("%s (%+.2f)", pretty_name(keep$variable), keep$standardised), collapse = ", ")
  )
  corr <- stats::cor(chem)
  upper <- which(upper.tri(corr), arr.ind = TRUE)
  abs_corr <- abs(corr[upper])
  winner <- upper[which.max(abs_corr), ]
  pair_a <- colnames(corr)[[winner[[1]]]]
  pair_b <- colnames(corr)[[winner[[2]]]]
  correlation_sentence <- sprintf(
    "The strongest correlation is %.2f, between %s and %s. %d pairs have absolute correlation above 0.5.",
    corr[winner[[1]], winner[[2]]],
    pretty_name(pair_a),
    pretty_name(pair_b),
    sum(abs_corr > 0.5)
  )

  trimmed_chem <- chemistry(baked)
  pc <- stats::prcomp(trimmed_chem, center = TRUE, scale. = TRUE)
  share <- pc$sdev^2 / sum(pc$sdev^2)
  cumulative <- cumsum(share)
  k <- which(cumulative >= 0.8)[[1]]
  rot <- pc$rotation[, seq_len(k), drop = FALSE]
  pca_table <- data.frame(
    Statistic = c("Standard deviation", "Proportion of variance", "Cumulative proportion"),
    stringsAsFactors = FALSE
  )
  for (j in seq_len(k)) {
    pca_table[[colnames(rot)[[j]]]] <- sprintf(
      "%.4f",
      c(pc$sdev[[j]], share[[j]], cumulative[[j]])
    )
  }
  pc1 <- pc$rotation[, 1]
  top <- order(abs(pc1), decreasing = TRUE)[1:3]
  loading_sentence <- sprintf(
    "The largest absolute loadings on the first component are %s (%+.2f), %s (%+.2f) and %s (%+.2f).",
    pretty_name(names(pc1)[top[[1]]]), pc1[[top[[1]]]],
    pretty_name(names(pc1)[top[[2]]]), pc1[[top[[2]]]],
    pretty_name(names(pc1)[top[[3]]]), pc1[[top[[3]]]]
  )
  long <- do.call(rbind, lapply(seq_len(k), function(j) {
    data.frame(
      variable = pretty_name(rownames(rot)),
      component = colnames(rot)[[j]],
      value = rot[, j],
      stringsAsFactors = FALSE
    )
  }))
  loadings <- do.call(rbind, lapply(split(long, long$component), function(piece) {
    piece <- piece[order(-abs(piece$value)), , drop = FALSE]
    piece[seq_len(min(4L, nrow(piece))), , drop = FALSE]
  }))
  loadings$term <- tidytext::reorder_within(
    loadings$variable,
    abs(loadings$value),
    loadings$component
  )
  loadings$abs_value <- abs(loadings$value)
  loadings$sign <- ifelse(loadings$value > 0, "Positive", "Negative")

  before_maps <- fit_class_maps(sample, k)
  after_maps <- fit_class_maps(baked, k)
  means_table <- data.frame(
    Measurement = pretty_name(shift_df$variable),
    Low = sprintf("%.4g", shift_df$low_mean),
    High = sprintf("%.4g", shift_df$high_mean),
    `Difference in SD` = sprintf("%+.2f", shift_df$standardised),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  list(
    n = nrow(sample),
    n_high = n_high,
    n_low = n_low,
    n_train = length(train_index),
    n_test = length(test_index),
    n_moved = sum(moved),
    n_moved_high = sum(moved & high),
    n_moved_low = sum(moved & !high),
    score_min = min(scores),
    score_max = max(scores),
    range_so2 = diff(range(sample$total.sulfur.dioxide)),
    range_density = diff(range(sample$density)),
    k = k,
    cum = cumulative[[k]],
    out_before = outlier_counts(before_maps),
    out_after = outlier_counts(after_maps),
    shift_sentence = shift_sentence,
    correlation_sentence = correlation_sentence,
    loading_sentence = loading_sentence,
    means_table = means_table,
    pca_table = pca_table,
    raw = sample,
    trimmed = baked,
    before_maps = before_maps,
    after_maps = after_maps,
    top_variables = shift_df$variable[1:3],
    loadings = loadings,
    prob = unname(step$prob)
  )
}

class_means_kable <- function(eda) {
  return_kable(
    eda$means_table,
    "Class means. The last column is the high mean minus the low mean, divided by the sample standard deviation."
  )
}

pca_kable <- function(eda) {
  return_kable(
    eda$pca_table,
    "Standardised principal components of the trimmed sample, kept through the component that reaches 80 percent of the variance."
  )
}

score_count_plot <- function(scores) {
  df <- data.frame(score = factor(scores, levels = sort(unique(scores))))
  ggplot2::ggplot(df, ggplot2::aes(x = score)) +
    ggplot2::geom_bar(fill = "#4C78A8") +
    ggplot2::labs(x = "Quality score", y = "Wines") +
    ggplot2::theme_bw()
}

label_count_plot <- function(quality) {
  df <- data.frame(quality = factor(as.character(quality), levels = c("low", "high")))
  ggplot2::ggplot(df, ggplot2::aes(x = quality)) +
    ggplot2::geom_bar(fill = "#F58518") +
    ggplot2::labs(x = "Label", y = "Wines") +
    ggplot2::theme_bw()
}

density_plot <- function(data, variables) {
  pieces <- lapply(variables, function(variable) {
    data.frame(
      quality = data$quality,
      variable = pretty_name(variable),
      value = data[[variable]],
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, pieces)
  long$variable <- factor(long$variable, levels = pretty_name(variables))
  ggplot2::ggplot(long, ggplot2::aes(x = value, fill = quality, colour = quality)) +
    ggplot2::geom_density(alpha = 0.35) +
    ggplot2::facet_wrap(~ variable, scales = "free") +
    ggplot2::labs(x = NULL, y = "Density", fill = NULL, colour = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}

correlation_plot <- function(data) {
  GGally::ggcorr(chemistry(data), label = TRUE, label_round = 2, hjust = 0.85)
}

loadings_plot <- function(eda) {
  ggplot2::ggplot(eda$loadings, ggplot2::aes(x = abs_value, y = term, fill = sign)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ component, scales = "free_y") +
    tidytext::scale_y_reordered() +
    ggplot2::labs(x = "Absolute loading", y = NULL, fill = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
