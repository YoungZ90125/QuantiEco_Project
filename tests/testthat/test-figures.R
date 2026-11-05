is_plot <- function(p) inherits(p, "ggplot") || inherits(p, "patchwork")

test_that("the map shows all 33 depth-index gauges", {
  expect_equal(nrow(map_gauges(raw(""))), 33)
})

test_that("subtitle labels give the rate ratio, its 95% CI and the p-value", {
  expect_equal(rr_label(0.7206, 0.5942, 0.8740, 0.00367), "RR per +10 cm = 0.72 (95% CI 0.59–0.87), p = 0.004")
  expect_equal(rr_label(0.69, 0.58, 0.82, 0.0004), "RR per +10 cm = 0.69 (95% CI 0.58–0.82), p < 0.001")
})

test_that("the map's colonies were counted in at least 3 years up to 2024 and average at least 50 nests", {
  col <- map_colonies(raw(""))
  expect_equal(nrow(col), 37)
  expect_true(all(col$n_years >= 3 & col$mean_nests >= 50))
})

test_that("negative numbers in figure labels use a true minus sign", {
  expect_equal(num_label(c(-0.3427, 0.5)), c("−0.34", "0.50"))
})

test_that("the PDF device falls back to pdf() where cairo is unavailable", {
  expect_identical(pdf_device(FALSE), grDevices::pdf)
  expect_identical(pdf_device(TRUE), grDevices::cairo_pdf)
})

test_that("map and time series build; save_figure writes PNG and PDF", {
  p <- fig_map(data_dir = raw(""))
  expect_true(is_plot(p))
  core <- prepare_counts(fx_counts(), fx_water(), c("wost", "whib"))
  expect_true(is_plot(fig_timeseries(fx_water(), core, data_dir = raw(""))))
  out <- tempfile()
  dir.create(out)
  save_figure(p, "f1", 8, 7, dir = out)
  expect_true(all(file.size(file.path(out, c("f1.png", "f1.pdf"))) > 10e3))
})

test_that("analysis figures build from the analysis objects", {
  wi <- fx_water()
  mc <- fx_counts()
  core <- prepare_counts(mc, wi, c("wost", "whib"))
  models <- lapply(split(core, core$species), fit_core)
  expect_true(is_plot(fig_core_effects(models, core, wi, data_dir = raw(""))))

  cd <- community_data(mc, wi, fx_species())
  cd6 <- subset(cd, !is.na(group))
  fp <- fig_forest(species_effects(cd), group_effects(fit_group_glmm(cd6), cd6))
  expect_true(is_plot(fp))
  expect_false(any(fp$data$grp == "unlabelled"))   # only the six species with a foraging label, as in the prediction

  trs <- lapply(split(core, core$species), make_transitions)
  rms <- lapply(trs, fit_rsn)
  kt <- dplyr::bind_rows(lapply(names(rms), function(s)
    dplyr::mutate(k_scenarios(stats::coef(rms[[s]]), stats::vcov(rms[[s]]), water_scenarios(wi)), species = s)))
  expect_true(is_plot(fig_rsn(trs, rms, kt, data_dir = raw(""))))
  expect_true(is_plot(fig_diagnostics(models, rms)))

  cr <- dplyr::bind_rows(lapply(names(models), function(s)
    dplyr::mutate(dplyr::bind_rows(dplyr::mutate(wald_rr(models[[s]]), check = "Main"),
                                   core_robustness(core[core$species == s, ])), species = s)))
  rr <- dplyr::bind_rows(lapply(names(trs), function(s)
    dplyr::mutate(dplyr::left_join(rsn_robustness(trs[[s]]),
                                   tibble::tibble(term = names(stats::coef(rms[[s]])), estimate = unname(stats::coef(rms[[s]]))),
                                   by = "term"), species = s)))
  expect_true(is_plot(fig_robustness(cr, rr)))
})
