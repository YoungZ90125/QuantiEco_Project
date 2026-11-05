# figures.R — poster figures F1-F5 and supplementary figures S1-S2 (factual labels only).
# Same style as the presentation figures: species colours, PhyloPic silhouettes, statistics in grey
# subtitles and credits in a grey caption.
library(ggplot2)
library(patchwork)   # provides +, / and & for combining ggplots

col_species <- c(kite = "#2a78d6", stork = "#1baf7a", ibis = "#c0398f", sparrow = "#eb6834")
col_core <- c(wost = "#1baf7a", whib = "#c0398f")
col_group <- c("high grader" = "#2e3440", "exploiter" = "#8f9aa8")
ink2 <- "#52514e"   # secondary text: subtitles and captions
icon_key <- c(wost = "stork", whib = "ibis")
species_names <- c(wost = "Wood stork", whib = "White ibis", sneg = "Snowy egret", greg = "Great egret",
                   trhe = "Tricolored heron", gbhe = "Great blue heron", lbhe = "Little blue heron",
                   bcnh = "Black-crowned night-heron")

theme_poster <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          strip.background = element_blank(), strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = ink2, size = rel(0.85)),
          plot.caption = element_text(colour = ink2, size = rel(0.7), hjust = 0))
}

p_text <- function(p) ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p))

# Number for figure labels, with a true minus sign (U+2212) for negative values.
num_label <- function(x, digits = 2) sub("^-", "−", formatC(x, format = "f", digits = digits))
minus_axis <- scales::label_number(style_negative = "minus")   # axis labels with the same minus sign

# Effect label for figure subtitles: rate ratio per +10 cm with its 95 % CI and p-value.
rr_label <- function(rr, lo, hi, p) {
  sprintf("RR per +10 cm = %.2f (95%% CI %.2f–%.2f), %s", rr, lo, hi, p_text(p))
}

# Species silhouette (PhyloPic, already recoloured to the species colour) as a grob for inset_element().
icon_grob <- function(key, data_dir = "data/raw") {
  grid::rasterGrob(png::readPNG(file.path(data_dir, "icons", paste0("icon_", key, ".png"))), interpolate = TRUE)
}

# PDF device: cairo where available (renders symbols such as en dashes), otherwise the standard pdf().
pdf_device <- function(has_cairo = isTRUE(capabilities("cairo"))) {
  if (has_cairo) grDevices::cairo_pdf else grDevices::pdf
}

# Writes <dir>/<name>.png (300 dpi) and <dir>/<name>.pdf. If the cairo PDF device fails on this
# machine, the PDF is written with the standard pdf() device instead.
save_figure <- function(p, name, width, height, dir = "figures") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(dir, paste0(name, ".png")), p, width = width, height = height, dpi = 300, bg = "white")
  pdf_file <- file.path(dir, paste0(name, ".pdf"))
  tryCatch(ggsave(pdf_file, p, width = width, height = height, bg = "white", device = pdf_device()),
           error = function(e) ggsave(pdf_file, p, width = width, height = height, bg = "white", device = grDevices::pdf))
  invisible(file.path(dir, paste0(name, c(".png", ".pdf"))))
}

# ---- map helpers --------------------------------------------------------------------------------

# GeoJSON (Multi)Polygon features -> data frame of outer rings for geom_polygon.
geo_polys <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  out <- list()
  for (fi in seq_along(j$features)) {
    g <- j$features[[fi]]$geometry
    id <- j$features[[fi]]$properties$id %||% j$features[[fi]]$properties$Name
    polys <- if (g$type == "Polygon") list(g$coordinates) else g$coordinates
    for (pi in seq_along(polys)) {
      ring <- polys[[pi]][[1]]
      out[[length(out) + 1]] <- data.frame(id = id, group = paste(fi, pi),
                                           lon = sapply(ring, `[[`, 1), lat = sapply(ring, `[[`, 2))
    }
  }
  dplyr::bind_rows(out)
}

# Silhouette drawn on the map, centred at (x, y), `height` degrees of latitude tall. The width keeps the
# image's aspect ratio under coord_quickmap, where a degree of longitude is cos(latitude) degrees long.
map_icon <- function(key, x, y, height, data_dir = "data/raw", lat0 = 26.1) {
  img <- png::readPNG(file.path(data_dir, "icons", paste0("icon_", key, ".png")))
  w <- height * ncol(img) / nrow(img) / cos(lat0 * pi / 180)
  annotation_raster(img, xmin = x - w / 2, xmax = x + w / 2, ymin = y - height / 2, ymax = y + height / 2, interpolate = TRUE)
}

# Silhouette on the map with the common name (species colour) and Latin name (grey italic) at label_y.
map_species <- function(key, x, y, height, name, latin, label_y, data_dir = "data/raw") {
  list(map_icon(key, x, y, height, data_dir),
       annotate("text", x = x, y = label_y, label = name, colour = col_species[[key]], fontface = "bold", size = 3.1),
       annotate("text", x = x, y = label_y - 0.05, label = latin, colour = ink2, fontface = "italic", size = 2.8))
}

# The 33 EDEN gauges whose daily depths make up the water index.
map_gauges <- function(data_dir = "data/raw") {
  readr::read_csv(file.path(data_dir, "eden_gauges.csv"), show_col_types = FALSE)
}

# Wood stork and white ibis colonies for the map: the mean annual nest count over the years a colony was counted
# up to `last_year` (rows for sub-sites of the same year are summed), kept if the colony was counted in at least
# `min_years` years, so that one-off counts do not stand for a typical year, and averages at least `min_mean` nests.
map_colonies <- function(data_dir = "data/raw", last_year = 2024, min_years = 3, min_mean = 50) {
  rd <- function(f) readr::read_csv(file.path(data_dir, f), show_col_types = FALSE)
  rd("wading_birds_maxcounts_by_colony.csv") |>
    dplyr::filter(species %in% c("wost", "whib"), year <= last_year) |>
    dplyr::group_by(colony, species, year) |>
    dplyr::summarise(nests = sum(count, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(colony, species) |>
    dplyr::summarise(n_years = dplyr::n(), mean_nests = mean(nests), .groups = "drop") |>
    dplyr::filter(n_years >= min_years, mean_nests >= min_mean) |>
    dplyr::inner_join(dplyr::distinct(rd("wading_birds_colonies.csv"), colony, latitude, longitude), by = "colony") |>
    dplyr::mutate(sp = factor(ifelse(species == "wost", "Wood stork", "White ibis"), levels = c("White ibis", "Wood stork"))) |>
    dplyr::arrange(sp)                                   # ibis drawn first, storks on top
}

# ---- F1: study area -----------------------------------------------------------------------------
fig_map <- function(data_dir = "data/raw") {
  rd <- function(f) readr::read_csv(file.path(data_dir, f), show_col_types = FALSE)
  fl <- rd("florida_outline.csv")
  lake <- rd("lake_okeechobee_outline.csv")
  reg <- geo_polys(file.path(data_dir, "everglades_subregions.geojson")) |>
    dplyr::mutate(area = ifelse(id %in% c("coastalenp", "inlandenp"), "Everglades National Park", "Water Conservation Areas"),
                  wca3a = id %in% c("3an", "3as", "3ase"))
  gauges <- map_gauges(data_dir)
  colonies <- map_colonies(data_dir)
  map <- ggplot() +
    geom_polygon(data = fl, aes(long, lat, group = group), fill = "grey95", colour = "grey70", linewidth = 0.3) +
    geom_polygon(data = lake, aes(long, lat, group = group), fill = "#9ec5f4", colour = "#86b6ef", linewidth = 0.3) +
    geom_polygon(data = reg, aes(lon, lat, group = group, fill = area), colour = "white", linewidth = 0.3) +
    geom_polygon(data = dplyr::filter(reg, wca3a), aes(lon, lat, group = group), fill = NA,
                 colour = col_species[["kite"]], linewidth = 0.8) +
    geom_point(data = colonies, aes(longitude, latitude, size = mean_nests, colour = sp), alpha = 0.8) +
    geom_point(data = gauges, aes(lon, lat, shape = "Water-depth gauges"), fill = "grey35", colour = "white", size = 1.6, stroke = 0.3) +
    annotate("text", x = -80.83, y = 26.95, label = "Lake\nOkeechobee", size = 3, colour = "#184f95") +
    annotate("text", x = -80.96, y = 26.37, label = "WCA-3A", size = 3.4, colour = col_species[["kite"]], fontface = "bold") +
    # silhouettes next to the areas each species uses: the kite beside WCA-3A, the stork beside its colonies
    # on the WCA-3A / park boundary, the ibis above its WCA-1 colonies
    map_species("kite", -81.30, 26.30, 0.22, "Everglade snail kite", "Rostrhamus sociabilis plumbeus", 26.15, data_dir) +
    map_species("stork", -81.27, 25.93, 0.15, "Wood stork", "Mycteria americana", 25.81, data_dir) +
    map_species("ibis", -80.33, 26.76, 0.16, "White ibis", "Eudocimus albus", 26.91, data_dir) +
    scale_fill_manual(values = c(`Everglades National Park` = "#d6efe2", `Water Conservation Areas` = "#cde2fb"), name = NULL) +
    scale_shape_manual(values = c(`Water-depth gauges` = 24), name = NULL) +
    scale_colour_manual(values = c(`Wood stork` = col_species[["stork"]], `White ibis` = col_species[["ibis"]]),
                        breaks = c("Wood stork", "White ibis"), name = "Nesting colonies") +
    # legend sizes span the bulk of the colonies (median about 300 nests), not the single largest one
    scale_size(trans = "log10", range = c(1.2, 6), breaks = c(100, 500, 2500), labels = scales::label_comma(),
               name = "Mean nests per year") +
    # narrow legends first, so the wide area labels sit below Lake Okeechobee
    guides(colour = guide_legend(order = 1, override.aes = list(size = 4, alpha = 1)), size = guide_legend(order = 2),
           shape = guide_legend(order = 3, override.aes = list(size = 2.6)), fill = guide_legend(order = 4)) +
    coord_quickmap(xlim = c(-81.55, -80.15), ylim = c(25.15, 27.05), expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_poster(13) +
    # a context map without latitude/longitude axes; legends stacked in the empty top-left corner
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
          legend.position = "inside", legend.position.inside = c(0.015, 0.985), legend.justification = c(0, 1),
          legend.box = "vertical", legend.box.just = "left", legend.spacing.y = unit(3, "pt"),
          legend.margin = margin(2, 4, 2, 4), legend.key.size = unit(13, "pt"),
          legend.text = element_text(size = 10), legend.title = element_text(size = 10.5, face = "bold"),
          legend.background = element_blank(), legend.key = element_blank())
  # locator map: Florida with the mapped extent in red, in the empty Gulf corner at the bottom left
  inset <- ggplot() +
    geom_polygon(data = fl, aes(long, lat, group = group), fill = "grey85", colour = "grey55", linewidth = 0.2) +
    annotate("rect", xmin = -81.55, xmax = -80.15, ymin = 25.15, ymax = 27.05, fill = NA, colour = "red", linewidth = 0.5) +
    coord_quickmap() + theme_void() + theme(panel.background = element_rect(fill = "white", colour = "grey60"))
  map + patchwork::inset_element(inset, left = 0, bottom = 0, right = 0.24, top = 0.18, align_to = "panel")
}

# ---- F2: data overview --------------------------------------------------------------------------
fig_timeseries <- function(wi, core_counts, data_dir = "data/raw") {
  w <- tidyr::pivot_longer(wi[c("year", "D", "W")], c("D", "W"), names_to = "metric", values_to = "depth") |>
    dplyr::mutate(metric = factor(metric, levels = c("W", "D"),
                                  labels = c("Previous wet season (Jun–Nov of the year before)", "Breeding season (Mar–Jun)")))
  pw <- ggplot(w, aes(year, depth, colour = metric)) +
    geom_hline(yintercept = 0, colour = "grey75") +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    scale_colour_manual(values = c("#4393c3", "#053061"), name = NULL) +
    scale_y_continuous(labels = minus_axis) +
    labs(title = "Water depth", x = NULL, y = "Mean water depth (cm)") +
    theme_poster() + theme(legend.position = "top", legend.direction = "vertical", legend.justification = "left")
  # nesting pairs of one species, with its silhouette in the empty top-left corner (low counts in the 1990s)
  counts <- function(sp) {
    p <- ggplot(dplyr::filter(core_counts, species == sp), aes(year, pairs)) +
      geom_line(colour = col_core[[sp]]) + geom_point(colour = col_core[[sp]], size = 1.6) +
      scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
      labs(title = species_names[[sp]], x = NULL, y = "Nesting pairs") +
      theme_poster()
    patchwork::wrap_elements(full = p + patchwork::inset_element(icon_grob(icon_key[[sp]], data_dir),
                                                                 left = 0.02, bottom = 0.45, right = 0.22, top = 1))
  }
  (patchwork::wrap_elements(full = pw) / counts("wost") / counts("whib")) +
    patchwork::plot_layout(heights = c(1.15, 0.85, 0.85))
}

# ---- F3: core-species effects (predictions 1a, 1b) ------------------------------------------------
# Fitted curves vary one water variable and hold the other at its median and the year at 2008 (median).
fig_core_effects <- function(models, core_counts, wi, data_dir = "data/raw") {
  med <- c(D10 = stats::median(wi$D), W10 = stats::median(wi$W))
  xlab <- c(D10 = "Breeding-season depth, Mar–Jun (cm)", W10 = "Previous wet-season depth, Jun–Nov (cm)")
  # horizontal position (left, right) of the silhouette in the empty top band of each panel: falling curves
  # leave the top right free, except for the ibis, whose largest year sits there; rising curves the top left
  icon_x <- list(wost = list(D10 = c(0.62, 1), W10 = c(0.02, 0.4)), whib = list(D10 = c(0.34, 0.72), W10 = c(0.02, 0.4)))
  z <- stats::qnorm(0.975)
  panels <- list()
  for (sp in c("wost", "whib")) {
    m <- models[[sp]]
    d <- dplyr::filter(core_counts, species == sp)
    rr <- rate_ratios(m)
    for (term in c("D10", "W10")) {
      grid <- tibble::tibble(D10 = 0, W10 = 0, year_c = 0, .rows = 100)
      grid[[term]] <- seq(min(d[[term]]), max(d[[term]]), length.out = 100)
      pr <- stats::predict(m, newdata = grid, type = "link", se.fit = TRUE)
      grid <- dplyr::mutate(grid, x = .data[[term]] * 10 + med[[term]], fit = exp(pr$fit),
                            lo = exp(pr$fit - z * pr$se.fit), hi = exp(pr$fit + z * pr$se.fit))
      pts <- tibble::tibble(x = d[[term]] * 10 + med[[term]], pairs = d$pairs)
      r <- rr[rr$term == term, ]
      p <- ggplot() +
        geom_ribbon(data = grid, aes(x, ymin = lo, ymax = hi), fill = col_core[[sp]], alpha = 0.15) +
        geom_line(data = grid, aes(x, fit), colour = col_core[[sp]], linewidth = 1) +
        geom_point(data = pts, aes(x, pairs), colour = col_core[[sp]], size = 2) +
        coord_cartesian(ylim = c(0, max(pts$pairs) * 1.4)) +   # headroom for the silhouette; CI band clipped
        scale_x_continuous(labels = minus_axis) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(title = species_names[[sp]], subtitle = rr_label(r$rr, r$lo, r$hi, r$p_lrt), x = xlab[[term]], y = "Nesting pairs") +
        theme_poster(13)
      ix <- icon_x[[sp]][[term]]
      panels[[paste(sp, term)]] <- patchwork::wrap_elements(
        full = p + patchwork::inset_element(icon_grob(icon_key[[sp]], data_dir), left = ix[1], bottom = 0.64, right = ix[2], top = 1))
    }
  }
  patchwork::wrap_plots(panels, ncol = 2)
}

# ---- F4: community forest plot (predictions 2a, 2b) ------------------------------------------------
fig_forest <- function(species_eff, group_eff) {
  sp <- species_eff |>
    dplyr::filter(!is.na(group)) |>                        # the six species with a foraging label
    dplyr::mutate(label = unname(species_names[species]), grp = as.character(group), kind = "Species (GLM)")
  ge <- group_eff |>
    dplyr::filter(quantity != "D_ratio") |>
    dplyr::transmute(term = ifelse(startsWith(quantity, "D"), "D10", "W10"),
                     grp = ifelse(endsWith(quantity, "high"), "high grader", "exploiter"),
                     label = ifelse(grp == "high grader", "High graders (GLMM)", "Exploiters (GLMM)"),
                     rr = estimate, lo, hi, kind = "Group (GLMM)")
  # rows from the top: each group's GLMM estimate, then its species from the most to the least negative D effect
  ord <- function(g) dplyr::pull(dplyr::arrange(dplyr::filter(sp, term == "D10", grp == g), rr), label)
  top_down <- c("High graders (GLMM)", ord("high grader"), "Exploiters (GLMM)", ord("exploiter"))
  dat <- dplyr::bind_rows(sp, ge) |>
    dplyr::mutate(label = factor(label, levels = rev(top_down)),
                  term = factor(term, levels = c("D10", "W10"),
                                labels = c("Breeding-season depth (Mar–Jun)", "Previous wet-season depth (Jun–Nov)")))
  ratio <- dplyr::filter(group_eff, quantity == "D_ratio")
  ggplot(dat, aes(rr, label, colour = grp, shape = kind)) +
    geom_vline(xintercept = 1, colour = "grey60") +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.25, linewidth = 0.8, orientation = "y") +
    geom_point(size = 3.2) +
    facet_wrap(~ term) +
    scale_x_log10() +
    scale_colour_manual(values = col_group, breaks = names(col_group),
                        labels = c("High graders (need concentrated prey)", "Exploiters (flexible foragers)"), name = NULL) +
    # diamonds mark the group estimates, which the row labels already name: no separate shape legend
    scale_shape_manual(values = c(`Species (GLM)` = 16, `Group (GLMM)` = 18), guide = "none") +
    labs(subtitle = sprintf("Breeding season, high graders ÷ exploiters: %.2f (95%% CI %.2f–%.2f), %s",
                            ratio$estimate, ratio$lo, ratio$hi, p_text(ratio$p)),
         x = "Rate ratio per +10 cm (95% CI, log scale)", y = NULL) +
    theme_poster(13) + theme(legend.position = "bottom")
}

# ---- F5: r + sN dynamics (prediction 1c) -----------------------------------------------------------
# Lines: g = r + s*N + bD*D + bW*W under the three water scenarios, labelled at their right ends;
# open circles: K (g = 0) where K > 0. The subtitle gives the water coefficients (t-based 95 % CIs).
fig_rsn <- function(transitions, rsn_models, k_tab, data_dir = "data/raw") {
  lty <- c(favourable = "solid", median = "22", unfavourable = "11")
  panels <- lapply(c("wost", "whib"), function(sp) {
    tr <- transitions[[sp]]
    m <- rsn_models[[sp]]
    b <- unname(stats::coef(m))
    ci <- unname(stats::confint(m))
    kt <- dplyr::filter(k_tab, species == sp)
    xs <- seq(0, max(tr$N_t) * 1.1, length.out = 100)
    lines <- dplyr::bind_rows(lapply(seq_len(nrow(kt)), function(i)
      tibble::tibble(scenario = kt$scenario[i], N = xs, g = b[1] + b[2] * xs + b[3] * kt$D10[i] + b[4] * kt$W10[i])))
    ends <- dplyr::filter(lines, N == max(N))
    kpts <- dplyr::filter(kt, !is.na(K), K > 0, K <= max(xs))
    p <- ggplot() +
      geom_hline(yintercept = 0, colour = "grey60") +
      geom_point(data = tr, aes(N_t, g), colour = col_core[[sp]], size = 2, alpha = 0.55) +
      geom_line(data = lines, aes(N, g, linetype = scenario), colour = col_core[[sp]], linewidth = 1) +
      geom_text(data = ends, aes(N, g, label = scenario), hjust = -0.1, size = 3.4, colour = col_core[[sp]]) +
      geom_point(data = kpts, aes(K, 0), shape = 21, colour = col_core[[sp]], fill = "white", size = 3.5, stroke = 1.3) +
      scale_linetype_manual(values = lty, guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0.03, 0.3))) +   # room for the line labels
      scale_y_continuous(labels = minus_axis) +
      labs(title = species_names[[sp]],
           subtitle = sprintf("bD = %s [%s, %s]; bW = %s [%s, %s] (95%% CI)",
                              num_label(b[3]), num_label(ci[3, 1]), num_label(ci[3, 2]),
                              num_label(b[4]), num_label(ci[4, 1]), num_label(ci[4, 2])),
           x = "Nesting pairs in year t (thousands)", y = "Per-capita rate, ln(N[t+1] / N[t])") +
      theme_poster(13)
    patchwork::wrap_elements(full = p + patchwork::inset_element(icon_grob(icon_key[[sp]], data_dir),
                                                                 left = 0.68, bottom = 0.66, right = 1, top = 1))
  })
  patchwork::wrap_plots(panels, ncol = 2)
}

# ---- S1: diagnostics -----------------------------------------------------------------------------
fig_diagnostics <- function(core_models, rsn_models, seed = 20261104) {
  qq_uniform <- function(m, title) {
    r <- sort(DHARMa::simulateResiduals(m, n = 1000, seed = seed)$scaledResiduals)
    ggplot(tibble::tibble(expected = (seq_along(r) - 0.5) / length(r), observed = r), aes(expected, observed)) +
      geom_abline(colour = "grey60") + geom_point() +
      labs(title = title, x = "Expected (uniform)", y = "Scaled residual") + theme_poster(10)
  }
  qq_normal <- function(m, title) {
    ggplot(tibble::tibble(r = stats::residuals(m)), aes(sample = r)) + stat_qq() + stat_qq_line(colour = "grey60") +
      labs(title = title, x = "Theoretical quantile", y = "Residual") + theme_poster(10)
  }
  acf_plot <- function(r, title) {
    a <- as.numeric(stats::acf(r, lag.max = 5, plot = FALSE)$acf[2:6])
    ggplot(tibble::tibble(lag = 1:5, acf = a), aes(lag, acf)) +
      geom_hline(yintercept = c(-1, 1) * 1.96 / sqrt(length(r)), linetype = 2, colour = "grey60") +
      geom_hline(yintercept = 0) + geom_col(width = 0.15) +
      labs(title = title, x = "Lag (years)", y = "ACF") + theme_poster(10)
  }
  ps <- list()
  for (sp in c("wost", "whib")) {
    nm <- species_names[[sp]]
    ps <- c(ps, list(qq_uniform(core_models[[sp]], paste(nm, "GLM: DHARMa QQ")),
                     acf_plot(stats::residuals(core_models[[sp]], type = "pearson"), paste(nm, "GLM: residual ACF")),
                     qq_normal(rsn_models[[sp]], paste(nm, "r + sN: residual QQ")),
                     acf_plot(stats::residuals(rsn_models[[sp]]), paste(nm, "r + sN: residual ACF"))))
  }
  patchwork::wrap_plots(ps, ncol = 4)
}

# ---- S2: robustness ------------------------------------------------------------------------------
# core_robust: species, check (Main, R1, R2, R3_min, R3_max), term, rr, lo, hi.
# rsn_robust: species, term, estimate, min, max (leave-one-transition-out range).
fig_robustness <- function(core_robust, rsn_robust) {
  lv <- c("R3: leave one year out", "R2: + density dependence", "R1: no trend", "Main model")
  core_order <- unname(species_names[c("wost", "whib")])
  cr <- dplyr::mutate(core_robust, sp = factor(unname(species_names[species]), levels = core_order),
                      term = factor(term, c("D10", "W10"), c("Breeding-season depth", "Previous wet-season depth")))
  pts <- cr |>
    dplyr::filter(check %in% c("Main", "R1", "R2")) |>
    dplyr::mutate(check = factor(c(Main = "Main model", R1 = "R1: no trend", R2 = "R2: + density dependence")[check], levels = lv))
  loo <- cr |>
    dplyr::filter(check %in% c("R3_min", "R3_max")) |>
    dplyr::select(sp, term, check, rr) |>
    tidyr::pivot_wider(names_from = check, values_from = rr) |>
    dplyr::mutate(check = factor("R3: leave one year out", levels = lv))
  p1 <- ggplot() +
    geom_vline(xintercept = 1, colour = "grey60") +
    geom_segment(data = loo, aes(x = R3_min, xend = R3_max, y = check, yend = check), linewidth = 2, colour = "grey55") +
    geom_errorbar(data = pts, aes(xmin = lo, xmax = hi, y = check), width = 0.2, orientation = "y") +
    geom_point(data = pts, aes(rr, check), size = 2.5) +
    facet_grid(sp ~ term) + scale_x_log10() + scale_y_discrete(drop = FALSE) +
    labs(title = "Core GLMs", x = "Rate ratio per +10 cm (log scale)", y = NULL) + theme_poster(11)
  rr <- dplyr::mutate(rsn_robust, sp = factor(unname(species_names[species]), levels = core_order),
                      term = factor(term, c("N_t", "D10_next", "W10_next"),
                                    c("s (density, per 1000 pairs)", "bD (breeding depth, per 10 cm)", "bW (wet-season depth, per 10 cm)")))
  p2 <- ggplot(rr, aes(y = term)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_segment(aes(x = min, xend = max, yend = term), linewidth = 2, colour = "grey55") +
    geom_point(aes(x = estimate), size = 2.5) +
    facet_wrap(~ sp, scales = "free_x") +
    labs(title = "r + sN model: leave-one-transition-out range (bar) and full-data estimate (point)",
         x = "Coefficient", y = NULL) + theme_poster(11)
  p1 / p2 + patchwork::plot_layout(heights = c(1.4, 1))
}
