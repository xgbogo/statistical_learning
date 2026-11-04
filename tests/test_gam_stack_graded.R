# Acceptance: graded artifacts are untouched by gam-and-stack.
stopifnot(identical(unname(tools::md5sum("main.pdf")), "2848ef7280e31c430dfd9f3e1fc66ebb"))
stopifnot(system("git diff --quiet b86768e -- fit.R utils/recipes/step_recipe.R main.pdf conclusion", ignore.stdout = TRUE) == 0)
cat("graded: ok\n")
