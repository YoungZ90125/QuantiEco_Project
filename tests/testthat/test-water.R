test_that("depth = stage minus ground (ft -> cm)", {
  st <- tibble::tibble(date = as.Date("2000-03-01"), eden_name = "G1", stage_cm = 100)
  g  <- tibble::tibble(eden_name = "G1", region = "WCA", ground_ft = 1)
  expect_equal(daily_depth(st, g)$depth_cm, 100 - 30.48)
})

test_that("90 % coverage rule: 110 of 122 days kept, 109 dropped", {
  d <- function(n) tibble::tibble(date = seq(as.Date("2000-03-01"), by = "day", length.out = n),
                                  eden_name = "G1", region = "WCA", depth_cm = 1)
  expect_equal(window_mean(d(110), "03-01", "06-30")$value, 1)
  expect_true(is.na(window_mean(d(109), "03-01", "06-30")$value))
})

test_that("wet season Jun-Nov 2000 is assigned to 2001", {
  d <- tibble::tibble(date = seq(as.Date("2000-06-01"), as.Date("2000-11-30"), by = "day"),
                      eden_name = "G1", region = "ENP", depth_cm = 5)
  expect_equal(window_mean(d, "06-01", "11-30", year_shift = 1L)$year, 2001)
})

test_that("region means first, then across regions; years missing a region dropped", {
  gv <- tibble::tibble(eden_name = c("a", "b", "c", "a"), region = c("WCA", "WCA", "ENP", "WCA"),
                       year = c(2000, 2000, 2000, 2001), value = c(10, 20, 40, 10))
  expect_equal(regional_index(gv), tibble::tibble(year = 2000, value = 27.5))
})

test_that("a gauge-year below 90 % coverage drops out of its region mean; the year is kept", {
  gv <- tibble::tibble(eden_name = c("a", "b", "c"), region = c("WCA", "WCA", "ENP"), year = 2000, value = c(10, NA, 40))
  expect_equal(regional_index(gv), tibble::tibble(year = 2000, value = 25))
})

test_that("real index: 33 complete years, cor(D, W) = 0.19", {
  wi <- fx_water()
  expect_equal(wi$year, 1992:2024)
  expect_false(anyNA(wi))
  expect_equal(round(cor(wi$D, wi$W), 2), 0.19)
})
