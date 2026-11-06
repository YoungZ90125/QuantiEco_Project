# figures.R — poster figures F1-F5 and supplementary figures S1-S2 (factual labels only).
# Same style as the presentation figures: species colours, PhyloPic silhouettes, statistics in grey
# subtitles and credits in a grey caption.
library(ggplot2)
library(patchwork)   # provides +, / and & for combining ggplots

col_species <- c(kite = "#2a78d6", stork = "#1baf7a", ibis = "#c0398f")
col_core <- c(wost = "#1baf7a", whib = "#c0398f", snki = "#2a78d6")
col_water <- "#a9cbe9"   # open water on the map: sea and Lake Okeechobee
col_wca3a <- "#2a78d6"   # WCA-3A outline and label
col_group <- c("high grader" = "#d35400", "exploiter" = "#5d6d7e")   # orange and slate: no species colour
ink2 <- "#52514e"   # secondary text: subtitles and captions
icon_key <- c(wost = "stork", whib = "ibis", snki = "kite")
species_names <- c(wost = "Wood stork", whib = "White ibis", sneg = "Snowy egret", greg = "Great egret",
                   trhe = "Tricolored heron", gbhe = "Great blue heron", lbhe = "Little blue heron",
                   bcnh = "Black-crowned night-heron", snki = "Snail kite")

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

# Silhouette on the map with the common name (species colour) at label_y and the Latin name (grey italic)
# at latin_y; a name may run over several lines ("\n").
map_species <- function(key, x, y, height, name, latin, label_y, latin_y = label_y - 0.06, data_dir = "data/raw") {
  list(map_icon(key, x, y, height, data_dir),
       annotate("text", x = x, y = label_y, label = name, colour = col_species[[key]], fontface = "bold", size = 3.6,
                lineheight = 0.9),
       annotate("text", x = x, y = latin_y, label = latin, colour = ink2, fontface = "italic", size = 3.2, lineheight = 0.9))
}

# Area-weighted centre of polygon rings (columns lon, lat, group), by the shoelace formula on longitude and
# latitude, which is close enough at this scale.
ring_centre <- function(p) {
  parts <- t(vapply(split(p, p$group), function(r) {
    x <- r$lon; y <- r$lat; x2 <- c(x[-1], x[1]); y2 <- c(y[-1], y[1])
    cross <- x * y2 - x2 * y
    a <- sum(cross) / 2
    c(a = abs(a), lon = sum((x + x2) * cross) / (6 * a), lat = sum((y + y2) * cross) / (6 * a))
  }, numeric(3)))
  c(lon = sum(parts[, "lon"] * parts[, "a"]) / sum(parts[, "a"]), lat = sum(parts[, "lat"] * parts[, "a"]) / sum(parts[, "a"]))
}

# Survey regions of the wetlands in the kite data (Everglades National Park: its inland, freshwater region).
kite_regions <- list(Loxahatchee = "1", WCA2A = "2a", WCA2B = "2b", WCA3A = c("3an", "3as", "3ase"), WCA3B = "3b",
                     ENP = "inlandenp")

# Snail kite nests for the map: the mean number of nests per year (1996-2021) in each wetland with nests,
# placed at the centre of the wetland's survey region. Kite nests are spread through a wetland, and the
# data give no colony coordinates.
map_kite <- function(data_dir = "data/raw") {
  reg <- geo_polys(file.path(data_dir, "everglades_subregions.geojson"))
  k <- readr::read_csv(file.path(data_dir, "kite_nests_by_wetland.csv"), show_col_types = FALSE) |>
    dplyr::group_by(wetland) |>
    dplyr::summarise(mean_nests = mean(nests), .groups = "drop") |>
    dplyr::filter(mean_nests > 0)
  centres <- t(vapply(k$wetland, function(w) ring_centre(reg[reg$id %in% kite_regions[[w]], ]), numeric(2)))
  dplyr::mutate(k, lon = unname(centres[, "lon"]), lat = unname(centres[, "lat"]))
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
# Map frame (degrees): a margin of at least 0.1 degrees around every colony and gauge, and all of Lake Okeechobee.
map_xlim <- c(-81.75, -79.95)
map_ylim <- c(25.00, 27.31)   # with map_xlim, the shape of the 5.6 x 8 inch figure

fig_map <- function(data_dir = "data/raw") {
  rd <- function(f) readr::read_csv(file.path(data_dir, f), show_col_types = FALSE)
  fl <- rd("florida_outline.csv")
  lake <- rd("lake_okeechobee_outline.csv")
  reg <- geo_polys(file.path(data_dir, "everglades_subregions.geojson")) |>
    dplyr::mutate(area = ifelse(id %in% c("coastalenp", "inlandenp"), "Everglades National Park", "Water Conservation Areas"),
                  wca3a = id %in% c("3an", "3as", "3ase"))
  gauges <- map_gauges(data_dir)
  colonies <- map_colonies(data_dir)
  kite <- map_kite(data_dir)
  # the two kinds of area in neutral colours that no species uses, named on the map instead of in a legend;
  # the point colours are those of the species names beside the silhouettes
  map <- ggplot() +
    geom_polygon(data = fl, aes(long, lat, group = group), fill = "grey95", colour = "grey70", linewidth = 0.3) +
    geom_polygon(data = lake, aes(long, lat, group = group), fill = col_water, colour = NA) +
    geom_polygon(data = reg, aes(lon, lat, group = group, fill = area), colour = "white", linewidth = 0.3) +
    geom_polygon(data = dplyr::filter(reg, wca3a), aes(lon, lat, group = group), fill = NA,
                 colour = col_wca3a, linewidth = 0.8) +
    geom_point(data = colonies, aes(longitude, latitude, size = mean_nests, colour = sp), alpha = 0.8) +
    geom_point(data = gauges, aes(lon, lat, shape = "Water-depth gauges"), fill = "grey35", colour = "white", size = 2.4, stroke = 0.4) +
    # kite wetlands on top, with a white rim, so that the small ones stay visible beside gauges and colonies
    geom_point(data = kite, aes(lon, lat, size = mean_nests), shape = 21, fill = col_species[["kite"]], colour = "white", stroke = 0.6) +
    annotate("text", x = -80.85, y = 26.94, label = "Lake\nOkeechobee", size = 3, colour = "#184f95") +
    annotate("text", x = c(-80.08, -81.62, -80.75), y = c(25.55, 25.55, 25.05), label = c("Atlantic\nOcean", "Gulf of\nMexico", "Florida Bay"),
             size = 3, colour = "#184f95", fontface = "italic") +
    annotate("text", x = c(-81.03, -80.25), y = c(25.30, 26.07), label = c("Everglades\nNational Park", "Water\nConservation\nAreas"),
             size = 3.4, colour = c("#7d6f3f", "#5f5a4e"), fontface = "bold", lineheight = 0.9) +
    annotate("text", x = -80.96, y = 26.37, label = "WCA-3A", size = 3.4, colour = col_wca3a, fontface = "bold") +
    # silhouettes next to the areas each species uses: the kite beside WCA-3A, the stork beside its colonies
    # on the WCA-3A / park boundary, the ibis above its WCA-1 colonies
    map_species("kite", -81.30, 26.55, 0.26, "Everglade\nsnail kite", "Rostrhamus sociabilis\nplumbeus", 26.385, 26.27, data_dir) +
    map_species("stork", -81.35, 25.94, 0.24, "Wood stork", "Mycteria americana", 25.79, data_dir = data_dir) +
    map_species("ibis", -80.33, 26.84, 0.26, "White ibis", "Eudocimus albus", 27.04, data_dir = data_dir) +
    scale_fill_manual(values = c(`Everglades National Park` = "#ebe3c6", `Water Conservation Areas` = "#d8d5cc"), guide = "none") +
    scale_shape_manual(values = c(`Water-depth gauges` = 24), name = NULL) +
    scale_colour_manual(values = c(`Wood stork` = col_species[["stork"]], `White ibis` = col_species[["ibis"]]), guide = "none") +
    # one size scale for the colonies and the kite wetlands
    scale_size(trans = "log10", range = c(2, 9), breaks = c(10, 100, 1000), labels = scales::label_comma(),
               name = "Mean nests per year") +
    guides(size = guide_legend(order = 1, override.aes = list(shape = 16, colour = "grey30", alpha = 1)),
           shape = guide_legend(order = 2, override.aes = list(size = 3.4))) +
    coord_quickmap(xlim = map_xlim, ylim = map_ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_poster(13) +
    # a context map without latitude/longitude axes, the sea as the background; legends stacked in the
    # empty top-left corner
    theme(panel.background = element_rect(fill = col_water, colour = NA),
          axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
          legend.position = "inside", legend.position.inside = c(0.015, 0.985), legend.justification = c(0, 1),
          legend.box = "vertical", legend.box.just = "left", legend.spacing.y = unit(3, "pt"),
          legend.margin = margin(2, 4, 2, 4), legend.key.size = unit(13, "pt"),
          legend.text = element_text(size = 10), legend.title = element_text(size = 10.5, face = "bold"),
          legend.background = element_blank(), legend.key = element_blank())
  # locator map: Florida with the mapped extent in red, in the empty Gulf corner at the bottom left
  inset <- ggplot() +
    geom_polygon(data = fl, aes(long, lat, group = group), fill = "grey85", colour = "grey55", linewidth = 0.2) +
    annotate("rect", xmin = map_xlim[1], xmax = map_xlim[2], ymin = map_ylim[1], ymax = map_ylim[2], fill = NA, colour = "red", linewidth = 0.5) +
    coord_quickmap() + theme_void() + theme(panel.background = element_rect(fill = "white", colour = "grey60"))
  map + patchwork::inset_element(inset, left = 0, bottom = 0, right = 0.24, top = 0.18, align_to = "panel")
}

# ---- F2: data overview --------------------------------------------------------------------------
# The month ranges of the two seasons are given here, where the seasons first appear; later figures use the
# short names (breeding-season depth, previous wet-season depth). With `kite` (year, nests), a fourth panel
# shows the snail kite nests of the exploratory comparison.
fig_timeseries <- function(wi, core_counts, kite = NULL, data_dir = "data/raw") {
  years <- scale_x_continuous(limits = range(wi$year))   # one year axis for all three panels
  w <- tidyr::pivot_longer(wi[c("year", "D", "W")], c("D", "W"), names_to = "metric", values_to = "depth") |>
    dplyr::mutate(metric = factor(metric, levels = c("W", "D"),
                                  labels = c("Previous wet season (Jun–Nov of the year before)", "Breeding season (Mar–Jun)")))
  pw <- ggplot(w, aes(year, depth, colour = metric)) +
    geom_hline(yintercept = 0, colour = "grey75") +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    scale_colour_manual(values = c("#4393c3", "#053061"), name = NULL) +
    years + scale_y_continuous(labels = minus_axis) +
    labs(title = "Water depth", x = NULL, y = "Mean water depth (cm)") +
    theme_poster() + theme(legend.position = "top", legend.direction = "vertical", legend.justification = "left")
  # nesting pairs of one species, with its silhouette in the empty top-left corner (low counts in the 1990s);
  # the silhouette is drawn inside the plot, so that the three panels stay aligned
  counts <- function(sp) {
    d <- dplyr::filter(core_counts, species == sp)
    ggplot(d, aes(year, pairs)) +
      annotation_custom(icon_grob(icon_key[[sp]], data_dir), xmin = 1992.5, xmax = 1998.5,
                        ymin = 0.5 * max(d$pairs), ymax = max(d$pairs)) +
      geom_line(colour = col_core[[sp]]) + geom_point(colour = col_core[[sp]], size = 1.6) +
      years + scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
      labs(title = species_names[[sp]], x = NULL, y = "Nesting pairs") +
      theme_poster()
  }
  panels <- list(pw, counts("wost"), counts("whib"))
  if (!is.null(kite)) {
    # the kite's high counts are in the late 1990s, so its silhouette goes in the empty top-right corner
    panels[[4]] <- ggplot(kite, aes(year, nests)) +
      annotation_custom(icon_grob("kite", data_dir), xmin = 2016.5, xmax = 2022.5,
                        ymin = 0.45 * max(kite$nests), ymax = max(kite$nests)) +
      geom_line(colour = col_species[["kite"]]) + geom_point(colour = col_species[["kite"]], size = 1.6) +
      years + scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
      labs(title = "Snail kite (exploratory comparison)", x = NULL, y = "Nests") +
      theme_poster()
  }
  patchwork::wrap_plots(panels, ncol = 1, heights = c(1.15, rep(0.85, length(panels) - 1)))
}

# ---- F3: core-species effects (predictions 1a, 1b; snail kite row: exploratory comparison) ----------
# Fitted nesting for one species, one curve per season: each varies its own depth over the observed range
# and holds the other at its median and the year at 2008 (median); with the model's 95 % CI.
core_curves <- function(model, counts, wi) {
  med <- c(D10 = stats::median(wi$D), W10 = stats::median(wi$W))
  season <- c(D10 = "Breeding season", W10 = "Previous wet season")
  z <- stats::qnorm(0.975)
  dplyr::bind_rows(lapply(c("D10", "W10"), function(term) {
    grid <- tibble::tibble(D10 = 0, W10 = 0, year_c = 0, .rows = 100)
    grid[[term]] <- seq(min(counts[[term]]), max(counts[[term]]), length.out = 100)
    pr <- stats::predict(model, newdata = grid, type = "link", se.fit = TRUE)
    tibble::tibble(season = season[[term]], depth = grid[[term]] * 10 + med[[term]], fit = exp(pr$fit),
                   lo = exp(pr$fit - z * pr$se.fit), hi = exp(pr$fit + z * pr$se.fit))
  }))
}

# One row per species (models: named list in display order, e.g. wost, whib), one panel per season: the
# fitted curve with its 95 % CI and the observed years. The rate ratios go in the caption, not the figure.
# Shared axes: the species name and the y axis only in the left column, the x axis only in the bottom row.
fig_core_effects <- function(models, core_counts, wi, data_dir = "data/raw") {
  med <- c(D10 = stats::median(wi$D), W10 = stats::median(wi$W))
  season <- c(D10 = "Breeding season", W10 = "Previous wet season")
  xlab <- c(D10 = "Breeding-season depth (cm)", W10 = "Previous wet-season depth (cm)")
  title <- species_names
  title[["snki"]] <- "Snail kite (exploratory)"
  y_name <- c(wost = "Nesting pairs", whib = "Nesting pairs", snki = "Nests")
  # horizontal position (left, right) of the silhouette in the empty top band of each panel: falling curves
  # leave the top right free, except for the ibis, whose largest year sits there; rising curves the top left
  icon_x <- list(wost = list(D10 = c(0.62, 1), W10 = c(0.02, 0.4)), whib = list(D10 = c(0.34, 0.72), W10 = c(0.02, 0.4)),
                 snki = list(D10 = c(0.02, 0.4), W10 = c(0.02, 0.4)))
  panels <- list()
  for (sp in names(models)) {
    d <- dplyr::filter(core_counts, species == sp)
    cur <- core_curves(models[[sp]], d, wi)
    for (term in c("D10", "W10")) {
      cu <- cur[cur$season == season[[term]], ]
      pts <- tibble::tibble(x = d[[term]] * 10 + med[[term]], pairs = d$pairs)
      # silhouette box: fractions of the panel (icon_x across, the top third down) turned into data units, so
      # that the silhouette is drawn inside the plot and the panels stay aligned
      x_range <- range(cu$depth) + c(-1, 1) * 0.05 * diff(range(cu$depth))   # the default 5 % axis expansion
      y_top <- max(pts$pairs) * 1.4                                          # headroom for the silhouette
      y_range <- c(0, y_top) + c(-1, 1) * 0.05 * y_top
      ix <- icon_x[[sp]][[term]]
      p <- ggplot() +
        annotation_custom(icon_grob(icon_key[[sp]], data_dir),
                          xmin = x_range[1] + ix[1] * diff(x_range), xmax = x_range[1] + ix[2] * diff(x_range),
                          ymin = y_range[1] + 0.64 * diff(y_range), ymax = y_range[2]) +
        geom_ribbon(data = cu, aes(depth, ymin = lo, ymax = hi), fill = col_core[[sp]], alpha = 0.15) +
        geom_line(data = cu, aes(depth, fit), colour = col_core[[sp]], linewidth = 1.2) +
        geom_point(data = pts, aes(x, pairs), colour = col_core[[sp]], size = 2.2) +
        coord_cartesian(ylim = c(0, y_top)) +   # CI band clipped at the top
        scale_x_continuous(labels = minus_axis) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(title = if (term == "D10") title[[sp]] else NULL, x = xlab[[term]], y = y_name[[sp]]) +
        theme_poster(17) + theme(axis.title.x = element_text(size = rel(0.9)))
      if (term == "W10") {   # right column: shares the left column's y axis (same species, same scale)
        p <- p + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
      }
      if (sp != utils::tail(names(models), 1)) {   # upper rows: share the bottom row's x axis
        p <- p + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
      }
      panels[[paste(sp, term)]] <- p
    }
  }
  patchwork::wrap_plots(panels, ncol = 2)
}

# ---- F4: community forest plot (predictions 2a, 2b) ------------------------------------------------
# Circles for the six labelled species (per-species GLMs), diamonds for the two foraging-group estimates
# (GLMM, rows "Group estimate"). Each foraging group has its own colour and block of rows, marked by a
# bracket with the group's name at the far left, so no legend; each panel's axis spans its own estimates.
# Sized for a poster: large text and marks.
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
  # rows from the top: each group's GLMM estimate, then its species from the most to the least negative D
  # effect; a small gap between the two groups
  ord <- function(g) dplyr::pull(dplyr::arrange(dplyr::filter(sp, term == "D10", grp == g), rr), label)
  hg <- c("High graders (GLMM)", ord("high grader"))
  ex <- c("Exploiters (GLMM)", ord("exploiter"))
  ypos <- c(stats::setNames(rev(seq_along(hg)) + length(ex) + 0.6, hg), stats::setNames(rev(seq_along(ex)), ex))
  dat <- dplyr::bind_rows(sp, ge) |>
    dplyr::mutate(y = unname(ypos[label]),
                  term = factor(term, levels = c("D10", "W10"),
                                labels = c("Breeding-season depth", "Previous wet-season depth")))
  shown <- ifelse(grepl("GLMM", names(ypos)), "Group estimate", names(ypos))   # row labels as printed
  forest <- ggplot(dat, aes(rr, y, colour = grp, shape = kind, size = kind)) +
    geom_vline(xintercept = 1, colour = "grey60", linewidth = 0.8) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.3, linewidth = 1.3, orientation = "y", show.legend = FALSE) +
    geom_point() +
    facet_wrap(~ term, scales = "free_x") +
    scale_x_log10(breaks = c(0.5, 0.6, 0.8, 1, 1.25, 1.5, 2), labels = scales::label_number(accuracy = 0.01, drop0trailing = TRUE)) +
    scale_y_continuous(breaks = unname(ypos), labels = shown, expand = expansion(add = 0.5)) +
    scale_colour_manual(values = col_group, guide = "none") +
    # diamonds (larger) mark the group estimates
    scale_shape_manual(values = c(`Species (GLM)` = 16, `Group (GLMM)` = 18), guide = "none") +
    scale_size_manual(values = c(`Species (GLM)` = 4.2, `Group (GLMM)` = 6.5), guide = "none") +
    labs(x = "Rate ratio per +10 cm (95% CI, log scale)", y = NULL) +   # statistics go in the caption
    theme_poster(16) + theme(panel.spacing.x = unit(16, "pt"))
  # brackets at the far left, one per foraging group, on the same vertical scale as the forest rows
  brackets <- tibble::tibble(grp = c("high grader", "exploiter"), label = c("High graders", "Exploiters"),
                             y0 = c(min(ypos[hg]), min(ypos[ex])) - 0.3, y1 = c(max(ypos[hg]), max(ypos[ex])) + 0.3)
  bracket_col <- ggplot(brackets, aes(colour = grp)) +
    geom_segment(aes(x = 1, xend = 1, y = y0, yend = y1), linewidth = 1.3) +
    geom_segment(aes(x = 1, xend = 1.6, y = y0, yend = y0), linewidth = 1.3) +
    geom_segment(aes(x = 1, xend = 1.6, y = y1, yend = y1), linewidth = 1.3) +
    geom_text(aes(x = 0.4, y = (y0 + y1) / 2, label = label), angle = 90, size = 5.8, fontface = "bold") +
    scale_colour_manual(values = col_group, guide = "none") +
    scale_x_continuous(limits = c(0, 1.6), expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(add = 0.5)) +
    coord_cartesian(ylim = range(ypos)) +
    theme_void()
  bracket_col + forest + patchwork::plot_layout(widths = c(0.07, 1))
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
      scale_x_continuous(expand = expansion(mult = c(0.03, 0.62))) +   # room for the line labels
      scale_y_continuous(labels = minus_axis) +
      labs(title = species_names[[sp]],
           subtitle = sprintf("bD = %s [%s, %s]\nbW = %s [%s, %s] (95%% CI)",
                              num_label(b[3]), num_label(ci[3, 1]), num_label(ci[3, 2]),
                              num_label(b[4]), num_label(ci[4, 1]), num_label(ci[4, 2])),
           x = "Nesting pairs in year t (thousands)", y = "Per-capita rate, ln(N[t+1] / N[t])") +
      theme_poster(13)
    patchwork::wrap_elements(full = p + patchwork::inset_element(icon_grob(icon_key[[sp]], data_dir),
                                                                 left = 0.68, bottom = 0.66, right = 1, top = 1))
  })
  patchwork::wrap_plots(panels, ncol = 2)
}

# ---- F6: exploratory comparison with the snail kite ----------------------------------------------
# Rate ratios per +10 cm from the joint model (kite_effects()), one panel per season; the subtitles give
# the kite's rate ratio divided by each wading bird's. Silhouettes beside the rows of the left panel.
fig_kite <- function(kite_eff, data_dir = "data/raw") {
  sp <- c(kite = "Snail kite", `wood stork` = "Wood stork", `white ibis` = "White ibis")
  key <- c(kite = "kite", `wood stork` = "stork", `white ibis` = "ibis")
  d <- kite_eff |>
    dplyr::filter(quantity %in% names(sp)) |>
    dplyr::mutate(label = factor(sp[quantity], levels = rev(sp)))
  lims <- range(c(d$lo, d$hi)) * c(1 / 1.7, 1.1)   # room on the left for the silhouettes
  ratio_line <- function(tm, wb) {
    r <- dplyr::filter(kite_eff, term == tm, quantity == paste("kite /", wb))
    sprintf("Kite ÷ %s: %.2f (95%% CI %.2f–%.2f), %s", wb, r$estimate, r$lo, r$hi, p_text(r$p))
  }
  panel <- function(tm, title, icons) {
    p <- ggplot(dplyr::filter(d, term == tm), aes(estimate, label, colour = quantity)) +
      geom_vline(xintercept = 1, colour = "grey60") +
      geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.2, linewidth = 1, orientation = "y") +
      geom_point(size = 3.6) +
      scale_x_log10(limits = lims) +
      scale_colour_manual(values = c(kite = col_species[["kite"]], `wood stork` = col_species[["stork"]],
                                     `white ibis` = col_species[["ibis"]]), guide = "none") +
      labs(title = title, subtitle = paste(ratio_line(tm, "wood stork"), ratio_line(tm, "white ibis"), sep = "\n"),
           x = "Rate ratio per +10 cm (95% CI, log scale)", y = NULL) +
      theme_poster(13)
    if (icons) {
      for (q in names(sp)) {   # positions on the rate-ratio scale (the log scale transforms them)
        y <- match(sp[[q]], levels(d$label))
        p <- p + annotation_custom(icon_grob(key[[q]], data_dir), xmin = lims[1], xmax = lims[1] * 1.5,
                                   ymin = y - 0.42, ymax = y + 0.42)
      }
    } else {
      p <- p + theme(axis.text.y = element_blank())
    }
    p
  }
  panel("D10", "Breeding-season depth", TRUE) / panel("W10", "Previous wet-season depth", TRUE)   # stacked, for a poster column
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
