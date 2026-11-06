is_plot <- function(p) inherits(p, "ggplot") || inherits(p, "patchwork")

test_that("the map shows all 33 depth-index gauges", {
  expect_equal(nrow(map_gauges(raw(""))), 33)
})

test_that("the map frame leaves a margin around every colony and gauge and shows the whole of Lake Okeechobee", {
  col <- map_colonies(raw(""))
  g <- map_gauges(raw(""))
  lon <- c(col$longitude, g$lon)
  lat <- c(col$latitude, g$lat)
  expect_true(min(lon) - map_xlim[1] >= 0.1 && map_xlim[2] - max(lon) >= 0.1)
  expect_true(min(lat) - map_ylim[1] >= 0.1 && map_ylim[2] - max(lat) >= 0.1)
  lake <- readr::read_csv(raw("lake_okeechobee_outline.csv"), show_col_types = FALSE)
  expect_true(max(lake$lat) < map_ylim[2])
})

test_that("the three panels of the data overview share one aligned year axis", {
  core <- prepare_counts(fx_counts(), fx_water(), c("wost", "whib"))
  g <- patchwork::patchworkGrob(fig_timeseries(fx_water(), core, data_dir = raw("")))
  panels <- g$layout[grepl("^panel-[0-9]+$", g$layout$name), ]
  expect_equal(nrow(panels), 3)
  # each panel fills exactly one column of the layout, and it is the same column for all three
  expect_true(all(panels$l == panels$r) && length(unique(panels$l)) == 1)
})

test_that("with the kite counts, the data overview has four panels on one aligned year axis", {
  core <- prepare_counts(fx_counts(), fx_water(), c("wost", "whib"))
  g <- patchwork::patchworkGrob(fig_timeseries(fx_water(), core, fx_kite(), data_dir = raw("")))
  panels <- g$layout[grepl("^panel-[0-9]+$", g$layout$name), ]
  expect_equal(nrow(panels), 4)
  expect_true(all(panels$l == panels$r) && length(unique(panels$l)) == 1)
})

test_that("kite points: one per wetland with nests, at the centre of its survey region and inside the map", {
  k <- map_kite(raw(""))
  expect_setequal(k$wetland, c("ENP", "Loxahatchee", "WCA2B", "WCA3A", "WCA3B"))
  expect_true(all(k$mean_nests > 0))
  expect_true(all(k$lon > map_xlim[1] & k$lon < map_xlim[2] & k$lat > map_ylim[1] & k$lat < map_ylim[2]))
  reg <- geo_polys(raw("everglades_subregions.geojson"))
  wca3 <- reg[reg$id %in% c("3an", "3as", "3ase"), ]
  k3 <- k[k$wetland == "WCA3A", ]
  expect_true(k3$lon > min(wca3$lon) && k3$lon < max(wca3$lon) && k3$lat > min(wca3$lat) && k3$lat < max(wca3$lat))
})

# the three species' models and counts, as in the analysis (kite: exploratory comparison)
fx_effects <- function() {
  core <- prepare_counts(fx_counts(), fx_water(), c("wost", "whib"))
  d3 <- kite_data(fx_counts(), fx_kite(), fx_water())
  kite_rows <- dplyr::mutate(d3[d3$species == "snki", ], species = "snki")
  list(models = c(lapply(split(core, core$species), fit_core)[c("wost", "whib")], list(snki = fit_core(kite_rows))),
       counts = dplyr::bind_rows(core, kite_rows))
}

test_that("the core-effects figure: wood stork and white ibis, one row each, the two seasons in separate aligned panels", {
  fx <- fx_effects()
  p <- fig_core_effects(fx$models[c("wost", "whib")], fx$counts, fx_water(), data_dir = raw(""))
  g <- patchwork::patchworkGrob(p)
  panels <- g$layout[grepl("^panel-[0-9]+$", g$layout$name), ]
  expect_equal(nrow(panels), 4)
  expect_true(all(panels$l == panels$r) && length(unique(panels$l)) == 2 && length(unique(panels$t)) == 2)
})

test_that("the kite GLM figure has its two season panels side by side", {
  fx <- fx_effects()
  g <- patchwork::patchworkGrob(fig_core_effects(fx$models["snki"], fx$counts, fx_water(), data_dir = raw("")))
  panels <- g$layout[grepl("^panel-[0-9]+$", g$layout$name), ]
  expect_equal(nrow(panels), 2)
  expect_true(length(unique(panels$t)) == 1 && length(unique(panels$l)) == 2)
})

test_that("the core-effects panels carry no statistics text (it goes in the caption)", {
  fx <- fx_effects()
  p <- fig_core_effects(fx$models["snki"], fx$counts, fx_water(), data_dir = raw(""))
  expect_true(all(vapply(p$patches$plots, function(x) is.null(x$labels$subtitle), logical(1))) && is.null(p$labels$subtitle))
})

test_that("each row's fitted curves pass through the model's prediction at median water", {
  fx <- fx_effects()
  cur <- core_curves(fx$models$wost, dplyr::filter(fx$counts, species == "wost"), fx_water())
  expect_setequal(unique(cur$season), c("Breeding season", "Previous wet season"))
  at_median <- exp(stats::predict(fx$models$wost, newdata = data.frame(D10 = 0, W10 = 0, year_c = 0)))
  d <- cur[cur$season == "Breeding season", ]
  expect_equal(stats::approx(d$depth, d$fit, xout = stats::median(fx_water()$D))$y, unname(at_median), tolerance = 1e-3)
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

  cd <- community_data(mc, wi, fx_species())
  cd6 <- subset(cd, !is.na(group))
  fp <- fig_forest(species_effects(cd), group_effects(fit_group_glmm(cd6), cd6))
  expect_true(is_plot(fp))
  y_labels <- ggplot2::ggplot_build(fp[[2]])$layout$panel_params[[1]]$y$get_labels()   # the forest, right of the brackets
  expect_false(any(grepl("GLMM", y_labels)))
  expect_equal(sum(y_labels == "Group estimate"), 2)
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
