test_that("EDEN subset has the 33 gauges, the full date range and stage in cm", {
  st <- readr::read_csv(raw("eden_daily_stage.csv.gz"), show_col_types = FALSE)
  g  <- readr::read_csv(raw("eden_gauges.csv"), show_col_types = FALSE)
  expect_equal(sort(unique(st$eden_name)), sort(g$eden_name))
  expect_equal(as.vector(table(g$region)[c("ENP", "WCA")]), c(21, 12))
  expect_equal(range(st$date), as.Date(c("1991-01-01", "2024-09-30")))
  expect_false(anyNA(g$ground_ft))
})

test_that("core counts are complete and positive for 1992-2024", {
  core <- subset(fx_counts(), region == "all" & species %in% c("wost", "whib") & year %in% 1992:2024)
  expect_equal(nrow(core), 66)
  expect_true(all(core$count > 0))
})

test_that("map outlines are stored as data, so the maps package is not needed", {
  fl <- readr::read_csv(raw("florida_outline.csv"), show_col_types = FALSE)
  lo <- readr::read_csv(raw("lake_okeechobee_outline.csv"), show_col_types = FALSE)
  expect_true(all(c("long", "lat", "group") %in% names(fl)) && nrow(fl) > 100)
  expect_true(all(c("long", "lat", "group") %in% names(lo)) && nrow(lo) > 10)
  lock <- jsonlite::fromJSON(file.path(proj, "renv.lock"), simplifyVector = FALSE)$Packages
  expect_false("maps" %in% names(lock))
  code <- unlist(lapply(list.files(file.path(proj, "R"), full.names = TRUE), readLines))
  expect_false(any(grepl("map_data\\(|maps::", code)))
})

test_that("PROVENANCE.md lists every data file with its current SHA-256", {
  files <- list.files(raw(""), recursive = TRUE)
  prov <- readLines(file.path(proj, "data", "PROVENANCE.md"), encoding = "UTF-8")
  rows <- grep("^\\| `", prov, value = TRUE)
  listed <- sub("^\\| `([^`]+)`.*", "\\1", rows)
  expect_setequal(listed, files)
  sha <- sub(".*`([0-9a-f]{64})` \\|$", "\\1", rows)
  expect_equal(sha, unname(vapply(file.path(raw(""), listed), digest::digest, "", file = TRUE, algo = "sha256")))
})

test_that("foraging labels match the two species groups of Hypothesis 2", {
  sl <- fx_species()
  lab <- setNames(sl$foraging_type, sl$species)
  expect_true(all(lab[c("whib", "wost", "sneg")] == "high grader"))
  expect_true(all(lab[c("greg", "trhe", "gbhe")] == "exploiter"))
  expect_true(all(is.na(lab[c("lbhe", "bcnh")]) | lab[c("lbhe", "bcnh")] == ""))
})
