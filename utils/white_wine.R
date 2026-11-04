# White wines have their own high/low cut. Quality 9 is high. This file does
# not change the red recode.

recode_white_to_high_low <- function(data, factor_name) {
  score <- suppressWarnings(as.integer(as.character(data[[factor_name]])))
  if (anyNA(score)) {
    stop("White quality scores are not all integers")
  }
  factor(ifelse(score >= 7L, "high", "low"), levels = c("low", "high"))
}

load_white_recoded_data <- function(
  location = "data/winequality-white.csv",
  factor_name = "quality"
) {
  data <- load_raw_data(location)
  data$type <- factor(rep("white", nrow(data)))
  data[[factor_name]] <- recode_white_to_high_low(data, factor_name)
  data
}
