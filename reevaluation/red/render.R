render_red_reevaluation <- function() {
  source("libraries.R")
  dir.create("build/red/figures", recursive = TRUE, showWarnings = FALSE)
  old_root <- knitr::opts_knit$get("root.dir")
  knitr::opts_knit$set(root.dir = normalizePath("."))
  on.exit(knitr::opts_knit$set(root.dir = old_root), add = TRUE)
  knitr::knit(
    "reevaluation/red/report.Rnw",
    output = "build/red/reevaluation_red.tex"
  )
  tex <- readLines("build/red/reevaluation_red.tex")
  tex <- gsub("build/red/figures/", "figures/", tex, fixed = TRUE)
  writeLines(tex, "build/red/reevaluation_red.tex")
  status <- system2("latexmk", c(
    "-pdf",
    "-interaction=nonstopmode",
    "-halt-on-error",
    "-outdir=build/red",
    "build/red/reevaluation_red.tex"
  ))
  if (!identical(status, 0L)) {
    stop("latexmk failed")
  }
  "build/red/reevaluation_red.pdf"
}
