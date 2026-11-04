# Test helpers: source the project functions and provide shared fixtures.
# testthat runs with the working directory set to tests/testthat, so paths go up two levels.
proj <- file.path("..", "..")
for (f in list.files(file.path(proj, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

raw <- function(f) file.path(proj, "data", "raw", f)

fx_counts  <- function() readr::read_csv(raw("wading_birds_max_count_all.csv"), show_col_types = FALSE)
fx_species <- function() readr::read_csv(raw("wading_birds_species_list.csv"), show_col_types = FALSE)

# The water index is slow to build from daily data, so it is computed once per test run.
fx_water <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- water_index(readr::read_csv(raw("eden_daily_stage.csv.gz"), show_col_types = FALSE),
                            readr::read_csv(raw("eden_gauges.csv"), show_col_types = FALSE))
    }
    cache
  }
})
