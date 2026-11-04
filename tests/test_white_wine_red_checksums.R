# Acceptance: the red checksum manifest tool, checked in a scratch tree.
# Spec: "Red files are checksummed before and after the white run".
tool <- normalizePath("reevaluation/white/red_checksums.R", mustWork = FALSE)
if (!file.exists(tool)) {
  stop("Missing reevaluation/white/red_checksums.R")
}
source(tool)
stopifnot(is.function(red_checksum_paths), is.function(write_red_checksums))

tree <- tempfile("red-checksums-")
put <- function(path, text = path) {
  dir.create(dirname(file.path(tree, path)), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, file.path(tree, path))
}
red <- c(
  "results/reevaluation/metrics.rds",
  "results/reevaluation/lr__none__2021.rds",
  "results/reevaluation/oof_rerun/rf__none__2021.rds",
  "results/fits/lr__fd.rds",
  "build/reevaluation.pdf",
  "build/figures/reeval-a.pdf",
  "reevaluation/report.Rnw",
  "reevaluation/gam_stack_decisions.tsv",
  "data/winequality-red.csv",
  "main.pdf"
)
other <- c(
  "results/reevaluation/white/lr__none__2021.rds",
  "results/reevaluation/white/fits/lr__fd.rds",
  "build/white/reevaluation_white.pdf",
  "build/white/figures/a.pdf",
  "reevaluation/fit_reevaluation.R",
  "reevaluation/white/report.Rnw",
  "data/winequality-white.csv",
  "README.md"
)
for (path in c(red, other)) put(path)

old <- setwd(tree)
paths <- red_checksum_paths()
manifest <- tempfile(fileext = ".txt")
write_red_checksums(manifest)
setwd(old)
stopifnot(identical(paths, sort(red, method = "radix")))
expected <- paste0(unname(tools::md5sum(file.path(tree, paths))), "  ", paths)
stopifnot(identical(readLines(manifest), expected))

# Optional directories may be absent.
unlink(file.path(tree, c("build", "results/fits")), recursive = TRUE)
old <- setwd(tree)
fewer <- red_checksum_paths()
setwd(old)
stopifnot(identical(fewer, sort(red[!grepl("^(build|results/fits)/", red)], method = "radix")))
for (path in c("build/reevaluation.pdf", "results/fits/lr__fd.rds")) put(path)

# Command line: before, after on an unchanged tree, after on a changed tree.
rscript <- file.path(R.home("bin"), "Rscript")
run <- function(mode) {
  old <- setwd(tree)
  on.exit(setwd(old))
  system2(rscript, c("--vanilla", tool, mode), stdout = FALSE, stderr = FALSE)
}
stopifnot(run("before") == 0)
before_file <- file.path(tree, "results/reevaluation/white/red_checksums_before.txt")
after_file <- file.path(tree, "results/reevaluation/white/red_checksums_after.txt")
stopifnot(file.exists(before_file))
put("results/reevaluation/white/new_white_fit.rds")
stopifnot(run("after") == 0)
stopifnot(identical(readLines(before_file), readLines(after_file)))
put("results/reevaluation/metrics.rds", "changed")
stopifnot(run("after") != 0)

unlink(tree, recursive = TRUE)
cat("red checksum tool: ok\n")
