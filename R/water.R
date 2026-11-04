# water.R — water-depth index from EDEN daily stage.
# D = breeding-season (March-June) mean depth; W = previous wet-season (June-November) mean depth.

# Daily water depth (cm) at each gauge: stage (cm NAVD88) minus ground elevation (ft NAVD88, converted to cm).
daily_depth <- function(stage, gauges) {
  dplyr::inner_join(stage, gauges[c("eden_name", "region", "ground_ft")], by = "eden_name") |>
    dplyr::transmute(date, eden_name, region, depth_cm = stage_cm - ground_ft * 30.48)
}

# Mean depth per gauge and year over a calendar window such as "03-01" to "06-30".
# A gauge-year keeps its value only if at least `min_frac` of the window's days have data.
# year_shift = 1L assigns the window to the following year (used for the previous wet season).
window_mean <- function(depth, start_md, end_md, year_shift = 0L, min_frac = 0.9) {
  depth |>
    dplyr::mutate(md = format(date, "%m-%d"), year = as.integer(format(date, "%Y"))) |>
    dplyr::filter(md >= start_md, md <= end_md) |>
    dplyr::group_by(eden_name, region, year) |>
    dplyr::summarise(n = sum(!is.na(depth_cm)), value = mean(depth_cm, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(n_expected = as.integer(as.Date(paste0(year, "-", end_md)) - as.Date(paste0(year, "-", start_md))) + 1L,
                  value = ifelse(n >= min_frac * n_expected, value, NA_real_),
                  year = year + year_shift) |>
    dplyr::select(eden_name, region, year, value)
}

# Annual index: mean over gauges within each region, then the mean of the two regions.
# Gauge-years that failed the coverage rule are dropped first; years lacking either region are dropped.
regional_index <- function(gauge_values) {
  gauge_values |>
    dplyr::filter(!is.na(value)) |>
    dplyr::group_by(year, region) |>
    dplyr::summarise(value = mean(value), .groups = "drop") |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_regions = dplyr::n(), value = mean(value), .groups = "drop") |>
    dplyr::filter(n_regions == 2) |>
    dplyr::select(year, value)
}

# D and W per year, plus versions centred at their medians and scaled per 10 cm (D10, W10).
water_index <- function(stage, gauges, years = 1992:2024) {
  dd <- daily_depth(stage, gauges)
  D <- regional_index(window_mean(dd, "03-01", "06-30"))
  W <- regional_index(window_mean(dd, "06-01", "11-30", year_shift = 1L))
  dplyr::inner_join(dplyr::rename(D, D = value), dplyr::rename(W, W = value), by = "year") |>
    dplyr::filter(year %in% years) |>
    dplyr::arrange(year) |>
    dplyr::mutate(D10 = (D - stats::median(D)) / 10, W10 = (W - stats::median(W)) / 10)
}
