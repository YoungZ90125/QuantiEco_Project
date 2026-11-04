test_that("transitions: per-capita rate in the differential form", {
  tr <- make_transitions(tibble::tibble(year = 2000:2002, pairs = c(10000, 20000, 10000), D10 = 0:2, W10 = 0))
  expect_equal(tr$g, c(log(2), log(0.5)))
  expect_equal(tr$N_t, c(10, 20))
  expect_equal(tr$D10_next, 1:2)
})

test_that("equilibrium K = -(r + bD*D + bW*W)/s", {
  expect_equal(k_value(r = 1, s = -0.1, bD = -0.5, bW = 0, D10 = 1, W10 = 0), 5)
})

test_that("K is NA when the CI of s includes 0, and draws are seeded", {
  cf <- c("(Intercept)" = 1, N_t = -0.01, D10_next = -0.3, W10_next = 0.2)
  scen <- tibble::tibble(scenario = "median", D10 = 0, W10 = 0)
  expect_true(is.na(k_scenarios(cf, diag(c(0.01, 0.01, 0.01, 0.01)), scen)$K))
  v <- diag(c(0.01, 1e-6, 0.01, 0.01))
  expect_identical(k_scenarios(cf, v, scen), k_scenarios(cf, v, scen))
  expect_false(is.na(k_scenarios(cf, v, scen)$K))
})

test_that("the CI of s used by the K rule is t-based when df is given", {
  cf <- c("(Intercept)" = 1, N_t = -0.01, D10_next = 0, W10_next = 0)
  v <- diag(c(0.01, 0.005^2, 0.01, 0.01))
  scen <- tibble::tibble(scenario = "median", D10 = 0, W10 = 0)
  expect_false(is.na(k_scenarios(cf, v, scen)$K))           # normal: upper limit -0.0002
  expect_true(is.na(k_scenarios(cf, v, scen, df = 3)$K))    # t with 3 df: upper limit +0.0059
})

test_that("K is the plug-in value of the formula; the draws give only its interval", {
  cf <- c("(Intercept)" = 1, N_t = -0.1, D10_next = -0.5, W10_next = 0.2)
  scen <- tibble::tibble(scenario = "x", D10 = 1, W10 = 1)
  k <- k_scenarios(cf, diag(c(0.01, 1e-4, 0.01, 0.01)), scen)
  expect_equal(k$K, k_value(1, -0.1, -0.5, 0.2, 1, 1))
  expect_true(k$K_lo < k$K && k$K < k$K_hi)
})

test_that("real data: 32 transitions per core species; scenarios ordered", {
  expect_equal(nrow(make_transitions(prepare_counts(fx_counts(), fx_water(), "wost"))), 32)
  sc <- water_scenarios(fx_water())
  expect_equal(sc$scenario, c("favourable", "median", "unfavourable"))
  expect_true(sc$D10[1] < sc$D10[3] && sc$W10[1] > sc$W10[3])
})

test_that("r + sN fit, diagnostics and leave-one-out ranges have the expected terms", {
  tr <- make_transitions(prepare_counts(fx_counts(), fx_water(), "whib"))
  m <- fit_rsn(tr)
  expect_setequal(names(stats::coef(m)), c("(Intercept)", "N_t", "D10_next", "W10_next"))
  d <- rsn_diagnostics(m)
  expect_length(d$acf, 5)
  expect_equal(rsn_robustness(tr)$term, c("N_t", "D10_next", "W10_next"))
})
