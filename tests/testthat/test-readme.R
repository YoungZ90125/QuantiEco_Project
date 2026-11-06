test_that("abstract word count reads only the text under the Extended abstract heading", {
  f <- tempfile()
  writeLines(c("# Title", "not counted", "## Extended abstract", "### Objective", "one two", "three",
               "## Hypotheses", "not counted"), f)
  expect_equal(abstract_word_count(f), 3L)
})

test_that("the README has the required sections in order", {
  h <- grep("^## ", readLines(file.path("..", "..", "README.md"), encoding = "UTF-8", warn = FALSE), value = TRUE)
  required <- c("## Extended abstract", "## Hypotheses and Predictions", "## Data", "## Reproducing the analysis",
                "## Generative-AI declaration", "## References")
  expect_equal(h[h %in% required], required)
})

test_that("figure captions do not repeat Quarto's automatic numbering", {
  q <- readLines(file.path("..", "..", "analysis.qmd"), encoding = "UTF-8")
  expect_false(any(grepl('fig-cap: "Figure [0-9]', q)))
  expect_false(any(grepl("label: fig-(diagnostics|robustness)", q)))
})

test_that("the README abstract is within the 500-word limit", {
  expect_lte(abstract_word_count(file.path("..", "..", "README.md")), 500L)
})
