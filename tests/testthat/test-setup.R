test_that("required packages are installed in the project library", {
  pkgs <- c("readr", "dplyr", "tidyr", "tibble", "ggplot2", "patchwork", "MASS", "glmmTMB",
            "DHARMa", "jsonlite", "png", "knitr", "rmarkdown", "testthat", "digest")
  expect_true(all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)))
})
