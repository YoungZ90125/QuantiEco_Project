test_that("classification follows the decision rules", {
  expect_equal(classify_effect(0.59, 0.87, "below"), "supported")
  expect_equal(classify_effect(1.02, 1.60, "below"), "contradicted")
  expect_equal(classify_effect(0.90, 1.10, "above"), "not supported")
})

test_that("robust means every check lies on the predicted side of the null", {
  expect_true(robust_direction(c(0.7, 0.8, 0.95), "below"))
  expect_false(robust_direction(c(0.7, 1.01), "below"))
  expect_true(robust_direction(c(0.1, 0.3), "above", null = 0))
})

test_that("core models give the expected rate ratios (wood stork D 0.72, W 1.46; white ibis D 0.69, W 1.28)", {
  rr <- function(sp) rate_ratios(fit_core(prepare_counts(fx_counts(), fx_water(), sp)))
  w <- rr("wost")
  i <- rr("whib")
  expect_equal(round(w$rr, 3), c(0.721, 1.461))
  expect_equal(round(w$lo, 3), c(0.594, 1.128))
  expect_equal(round(i$rr, 3), c(0.691, 1.279))
  expect_equal(round(i$hi, 3), c(0.819, 1.608))
})

test_that("diagnostics are reproducible with the seed; robustness has R1-R3", {
  df <- prepare_counts(fx_counts(), fx_water(), "wost")
  m <- fit_core(df)
  expect_identical(core_diagnostics(m), core_diagnostics(m))
  expect_setequal(unique(core_robustness(df)$check), c("R1", "R2", "R3_min", "R3_max"))
})
