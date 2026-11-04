render_white_reevaluation <- function() {
  source("libraries.R")
  dir.create("build/white/figures", recursive = TRUE, showWarnings = FALSE)
  old_root <- knitr::opts_knit$get("root.dir")
  knitr::opts_knit$set(root.dir = normalizePath("."))
  on.exit(knitr::opts_knit$set(root.dir = old_root), add = TRUE)
  knitr::knit(
    "reevaluation/white/report.Rnw",
    output = "build/white/reevaluation_white.tex"
  )
  tex <- readLines("build/white/reevaluation_white.tex")
  tex <- gsub("build/white/figures/", "figures/", tex, fixed = TRUE)
  writeLines(tex, "build/white/reevaluation_white.tex")
  status <- system2("latexmk", c(
    "-pdf",
    "-interaction=nonstopmode",
    "-halt-on-error",
    "-outdir=build/white",
    "build/white/reevaluation_white.tex"
  ))
  if (!identical(status, 0L)) {
    stop("latexmk failed")
  }
  "build/white/reevaluation_white.pdf"
}
