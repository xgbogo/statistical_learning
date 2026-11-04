# Acceptance: after the full white run, graded files are byte-identical and every
# red file has the checksum it had before the run.
# Spec: "Graded files are byte-identical", "Red files are checksummed before and after the white run".
blob <- function(path) system2("git", c("hash-object", path), stdout = TRUE)

# Graded files (pass today).
stopifnot(identical(unname(tools::md5sum("main.pdf")), "2848ef7280e31c430dfd9f3e1fc66ebb"))
stopifnot(identical(blob("conclusion/conclusion.Rnw"), "9a2c4f4078efa5dbe557fa81452c9a6b3140159a"))
stopifnot(identical(blob("fit.R"), "9b6d84a119dcf78d7ece361380ae5fecb5bd7095"))
stopifnot(identical(blob("utils/recipes/step_recipe.R"), "4e7c2c1b5d40268db23211460afc162c8680c2dc"))
stopifnot(identical(blob("utils/recode_to_high_low.R"), "1a43af38e03ac974a7df242793c6faf4f8c2a5ec"))

# Manifests written around the full white run.
before_file <- "results/reevaluation/white/red_checksums_before.txt"
after_file <- "results/reevaluation/white/red_checksums_after.txt"
for (file in c(before_file, after_file)) {
  if (!file.exists(file)) {
    stop("Missing ", file, ": the full white run has not been checksummed")
  }
}
before <- readLines(before_file)
after <- readLines(after_file)
stopifnot(length(before) > 0, identical(before, after))
stopifnot(all(grepl("^[0-9a-f]{32}  [^ ]", before)))
paths <- sub("^[0-9a-f]{32}  ", "", before)
stopifnot(!any(grepl("^(results/reevaluation/white|build/white)/", paths)))
stopifnot(!"data/winequality-white.csv" %in% paths)
stopifnot(all(c(
  "main.pdf",
  "data/winequality-red.csv",
  "reevaluation/report.Rnw",
  "results/reevaluation/metrics.rds",
  "results/reevaluation/stack_decision.rds"
) %in% paths))
red_fits <- file.path(
  "results/reevaluation",
  list.files("results/reevaluation", pattern = "^[a-z_]+__[a-z]+__[0-9]{4}\\.rds$")
)
stopifnot(length(red_fits) == 150L, all(red_fits %in% paths))

cat("red untouched: ok\n")
