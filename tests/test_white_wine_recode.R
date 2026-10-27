# Acceptance: white labels use their own recode; red labels and the red recode stay.
# Spec: "White labels come from their own recode", "Red labels and the red recode stay as they are".
source("libraries.R")

blob <- function(path) system2("git", c("hash-object", path), stdout = TRUE)

# Red, unchanged (passes today).
stopifnot(identical(blob("utils/recode_to_high_low.R"), "1a43af38e03ac974a7df242793c6faf4f8c2a5ec"))
red <- load_recoded_data("data/winequality-red.csv", "quality")
red_raw <- load_raw_data("data/winequality-red.csv")
stopifnot(identical(levels(red$quality), c("low", "high")))
stopifnot(sum(red$quality == "high") == 217L, sum(red$quality == "low") == 1382L)
stopifnot(identical(
  as.character(red$quality),
  ifelse(as.integer(as.character(red_raw$quality)) >= 7, "high", "low")
))

# White, new.
white_raw <- load_raw_data("data/winequality-white.csv")
white_score <- as.integer(as.character(white_raw$quality))
labels <- recode_white_to_high_low(white_raw, "quality")
stopifnot(is.factor(labels), identical(levels(labels), c("low", "high")))
stopifnot(length(labels) == 4898L, !anyNA(labels))
stopifnot(all(as.character(labels) %in% c("high", "low")))
stopifnot(sum(labels == "high") == 1060L, sum(labels == "low") == 3838L)
stopifnot(sum(white_score == 9L) == 5L, all(labels[white_score == 9L] == "high"))
stopifnot(identical(as.character(labels), ifelse(white_score >= 7, "high", "low")))

stopifnot(identical(formals(load_white_recoded_data)$location, "data/winequality-white.csv"))
stopifnot(identical(formals(load_white_recoded_data)$factor_name, "quality"))
white <- load_white_recoded_data()
stopifnot(nrow(white) == 4898L, sum(is.na(white)) == 0L)
stopifnot(identical(names(white), names(red)))
stopifnot(is.factor(white$type), all(as.character(white$type) == "white"))
stopifnot(identical(white$quality, labels))
predictors <- setdiff(names(white), c("quality", "type"))
stopifnot(identical(white[predictors], white_raw[predictors]))

cat("white recode: ok\n")
