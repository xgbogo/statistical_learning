# Acceptance: the white write-up is in its own files and reads only white results.
# Spec: "The white analysis lives in its own files".
report <- "reevaluation/white/report.Rnw"
if (!file.exists(report)) {
  stop("Missing ", report)
}
text <- paste(readLines(report), collapse = "\n")
stopifnot(!grepl("results/reevaluation(?!/white/)", text, perl = TRUE))
stopifnot(!grepl("winequality-red", text, fixed = TRUE))
stopifnot(!grepl("build/figures/", text, fixed = TRUE))
stopifnot(grepl("build/white/figures/", text, fixed = TRUE))
stopifnot(grepl("run_one_split.rds", text, fixed = TRUE))
stopifnot(grepl("failures.tsv", text, fixed = TRUE))

source("reevaluation/white/render.R")
stopifnot(is.function(render_white_reevaluation))
stopifnot(any(grepl("build/white/reevaluation_white.pdf", deparse(render_white_reevaluation), fixed = TRUE)))

cat("white report: ok\n")
