# Checksums of red results, caches and analysis files.
# White outputs under results/reevaluation/white/ and build/white/ are omitted.
# Sourcing this file defines the helpers. The before/after commands run only
# when the file is executed directly.

red_checksum_paths <- function() {
  paths <- character()
  if (dir.exists("results/reevaluation")) {
    found <- list.files("results/reevaluation", recursive = TRUE, full.names = TRUE)
    found <- found[!grepl("^results/reevaluation/white/", found)]
    paths <- c(paths, found)
  }
  if (dir.exists("results/fits")) {
    paths <- c(paths, list.files("results/fits", recursive = TRUE, full.names = TRUE))
  }
  if (dir.exists("build")) {
    found <- list.files("build", recursive = TRUE, full.names = TRUE)
    found <- found[!grepl("^build/white/", found)]
    paths <- c(paths, found)
  }
  extra <- c(
    "reevaluation/report.Rnw",
    "reevaluation/gam_stack_decisions.tsv",
    "data/winequality-red.csv",
    "main.pdf"
  )
  extra <- extra[file.exists(extra)]
  sort(unique(c(paths, extra)), method = "radix")
}

write_red_checksums <- function(file) {
  paths <- red_checksum_paths()
  sums <- unname(tools::md5sum(paths))
  lines <- paste0(sums, "  ", paths)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file)
  invisible(file)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1 || !args[[1]] %in% c("before", "after")) {
    stop("Usage: Rscript reevaluation/white/red_checksums.R before|after")
  }
  before_file <- "results/reevaluation/white/red_checksums_before.txt"
  after_file <- "results/reevaluation/white/red_checksums_after.txt"
  if (identical(args[[1]], "before")) {
    write_red_checksums(before_file)
  } else {
    write_red_checksums(after_file)
    same <- file.exists(before_file) && identical(readLines(before_file), readLines(after_file))
    if (!same) {
      quit(save = "no", status = 1)
    }
  }
}
