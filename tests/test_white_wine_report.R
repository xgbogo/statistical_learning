# Acceptance: the white write-up is in its own files and reads only white results;
# the README has its own white-wine section.
# Spec: "The white analysis lives in its own files", "The README gains a white-wine section".
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

readme <- readLines("README.md")
heading <- which(readme == "## White-wine re-evaluation")
stopifnot(length(heading) == 1L)
rest <- readme[-seq_len(heading)]
end <- which(grepl("^#{1,2} ", rest))
section <- paste(if (length(end)) rest[seq_len(end[[1]] - 1L)] else rest, collapse = "\n")
stopifnot(grepl("results/reevaluation/white/", section, fixed = TRUE))
stopifnot(grepl("Rscript reevaluation/white/fit_white.R", section, fixed = TRUE))

cat("white report: ok\n")
