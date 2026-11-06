test_that("kite data: Everglades nests for every year 1996-2021", {
  k <- fx_kite()
  expect_equal(k$year, 1996:2021)
  expect_true(all(k$nests >= 0 & k$nests == round(k$nests)))
})

test_that("kite nests by wetland add up to the annual totals", {
  w <- readr::read_csv(raw("kite_nests_by_wetland.csv"), show_col_types = FALSE)
  totals <- stats::aggregate(nests ~ year, w, sum)
  expect_equal(totals$nests, fx_kite()$nests)
})

test_that("kite comparison data: kite, wood stork and white ibis over the 26 kite years", {
  d3 <- kite_data(fx_counts(), fx_kite(), fx_water())
  expect_setequal(unique(d3$species), c("snki", "wost", "whib"))
  expect_equal(nrow(d3), 3 * 26)
  expect_equal(range(d3$year), c(1996, 2021))
})

test_that("the joint model gives each species' effects and the kite's ratios to the two wading birds", {
  d3 <- kite_data(fx_counts(), fx_kite(), fx_water())
  fit <- fit_kite_joint(d3)
  expect_true(fit$spec_used %in% names(kite_formulas))
  ke <- kite_effects(fit)
  expect_equal(nrow(ke), 2 * 5)
  expect_true(all(is.finite(ke$estimate) & ke$lo < ke$estimate & ke$estimate < ke$hi))
  b <- glmmTMB::fixef(fit$model)$cond
  expect_equal(ke$estimate[ke$term == "D10" & ke$quantity == "kite / wood stork"],
               exp(b[["speciessnki:D10"]] - b[["specieswost:D10"]]))
})

test_that("the kite figure builds with its two panels stacked and aligned", {
  d3 <- kite_data(fx_counts(), fx_kite(), fx_water())
  p <- fig_kite(kite_effects(fit_kite_joint(d3)), data_dir = raw(""))
  g <- patchwork::patchworkGrob(p)
  panels <- g$layout[grepl("^panel-[0-9]+$", g$layout$name), ]
  expect_equal(nrow(panels), 2)
  expect_true(all(panels$l == panels$r) && length(unique(panels$l)) == 1)   # one above the other, same column
})
