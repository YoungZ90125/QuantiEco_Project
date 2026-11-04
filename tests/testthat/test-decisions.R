fx_decision_inputs <- function() {
  list(
    core_dec = tibble::tibble(species = c("wost", "wost", "whib", "whib"), term = c("D10", "W10", "D10", "W10"),
                              rr = c(0.7, 1.5, 0.9, 1.3), lo = c(0.6, 1.1, 0.8, 0.95), hi = c(0.8, 1.9, 1.05, 1.7),
                              robust = c(TRUE, TRUE, FALSE, TRUE)),
    g_eff = tibble::tibble(quantity = c("D_high", "D_expl", "D_ratio", "W_high", "W_expl"),
                           estimate = c(0.7, 1.0, 0.7, 1.3, 1.2), lo = c(0.6, 0.9, 0.6, 1.1, 1.05),
                           hi = c(0.85, 1.1, 0.85, 1.6, 1.4), p = c(NA, NA, 0.003, NA, NA)),
    rsn_dec = tibble::tibble(species = c("wost", "wost", "whib", "whib"), term = c("D10_next", "W10_next", "D10_next", "W10_next"),
                             estimate = c(-0.3, 0.5, -0.2, 0.3), lo = c(-0.7, 0.04, -0.5, -0.01), hi = c(0.01, 0.96, 0.03, 0.68),
                             robust = c(TRUE, TRUE, TRUE, TRUE))
  )
}

test_that("the 1c combined outcome: contradicted first, then both, one or neither supported", {
  expect_equal(combine_outcomes(c("supported", "supported")), "supported")
  expect_equal(combine_outcomes(c("supported", "not supported")), "partly supported")
  expect_equal(combine_outcomes(c("not supported", "not supported")), "not supported")
  expect_equal(combine_outcomes(c("contradicted", "supported")), "contradicted")
})

test_that("each row is labelled by hypothesis and prediction", {
  d <- do.call(decision_summary, fx_decision_inputs())
  expect_named(d, c("hypothesis", "prediction", "unit", "estimate", "lo", "hi", "outcome", "robust"))
  map <- unique(d[c("hypothesis", "prediction")])
  expect_equal(map$prediction, c("1a", "1b", "1c", "2a", "2b"))
  expect_equal(map$hypothesis, c("1", "1", "1", "2", "2"))
})

test_that("each prediction is classified in its predicted direction, species in input order", {
  d <- do.call(decision_summary, fx_decision_inputs())
  expect_equal(d$outcome[d$prediction == "1a"], c("supported", "not supported"))
  expect_equal(d$outcome[d$prediction == "1b"], c("supported", "not supported"))
  expect_equal(d$unit[d$prediction == "1c"],
               c("wost: bD", "wost: bW", "whib: bD", "whib: bW", "wost: combined", "whib: combined"))
  expect_equal(d$outcome[d$prediction == "1c"],
               c("not supported", "supported", "not supported", "not supported", "partly supported", "not supported"))
  expect_equal(d$outcome[d$prediction %in% c("2a", "2b")], rep("supported", 3))
  expect_equal(d$robust[d$prediction == "1a"], c(TRUE, FALSE))
  expect_true(all(is.na(d$robust[d$prediction %in% c("2a", "2b")])))
})
