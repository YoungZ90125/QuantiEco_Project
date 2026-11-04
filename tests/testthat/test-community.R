test_that("community data: eight species over 33 years, grouped as in Hypothesis 2", {
  cd <- community_data(fx_counts(), fx_water(), fx_species())
  expect_setequal(unique(cd$species), c("whib", "wost", "sneg", "greg", "trhe", "gbhe", "lbhe", "bcnh"))
  expect_setequal(unique(cd$species[cd$group %in% "high grader"]), c("whib", "wost", "sneg"))
  expect_setequal(unique(cd$species[cd$group %in% "exploiter"]), c("greg", "trhe", "gbhe"))
  expect_equal(nrow(cd), 8 * 33)
})

test_that("per-species effects cover all eight species and both water terms", {
  se <- species_effects(community_data(fx_counts(), fx_water(), fx_species()))
  expect_equal(nrow(se), 16)
  expect_setequal(unique(se$term), c("D10", "W10"))
})

test_that("a GLMM counts as converged only with code 0 and a positive-definite Hessian", {
  ok <- function(conv, pd) glmm_ok(list(fit = list(convergence = conv), sdr = list(pdHess = pd)))
  expect_true(ok(0L, TRUE))
  expect_false(ok(1L, TRUE))
  expect_false(ok(0L, FALSE))
  expect_false(glmm_ok(NULL))
})

test_that("group effects are reported under all three GLMM specifications", {
  cd6 <- subset(community_data(fx_counts(), fx_water(), fx_species()), !is.na(group))
  s <- group_effects_by_spec(cd6)
  expect_setequal(unique(s$spec), names(glmm_formulas))
  expect_equal(nrow(s), 3 * 5)
})

test_that("GLMM always returns a usable fit and a consistent ratio", {
  cd6 <- subset(community_data(fx_counts(), fx_water(), fx_species()), !is.na(group))
  fit <- fit_group_glmm(cd6)
  expect_true(fit$spec_used %in% c("full", "no_random_slopes", "no_year_re"))
  ge <- group_effects(fit, cd6)
  expect_true(all(is.finite(ge$estimate)))
  b <- glmmTMB::fixef(fit$model)$cond
  expect_equal(ge$estimate[ge$quantity == "D_ratio"],
               exp(b[["grouphigh grader:D10"]] - b[["groupexploiter:D10"]]))
})
