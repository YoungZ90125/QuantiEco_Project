# get_data.R — provenance script: how every file in data/raw/ was derived.
#
# The analysis (analysis.qmd) reads only data/raw/ and does not run this script.
# The public files were downloaded on 2026-10-28 into a separate working folder
# (../everglades_ecology_project/, not part of this repository), so this script documents the
# derivation but cannot run from this repository alone. It:
#   - copies the wading-bird files and the (already recoloured) silhouettes unchanged;
#   - extracts the 33 analysis gauges from the EDEN daily stage file and assembles the gauge table;
#   - saves the Florida and Lake Okeechobee outlines from the R package maps, so that rendering the
#     analysis does not need that package (install it with install.packages("maps") to re-run this script);
#   - counts annual snail kite nests in the Everglades wetlands from the kite nest-survival file;
#   - writes data/PROVENANCE.md with SHA-256 checksums.
#
# Original public sources:
#   Wading-bird counts, species table, colonies, region polygons (CC0):
#     Frederick et al. 2026, weecology/EvergladesWadingBird release 4.41.0, doi:10.5281/zenodo.7734553
#     https://raw.githubusercontent.com/weecology/EvergladesWadingBird/4.41.0/<path>
#   EDEN daily median water stage (cm NAVD88), quarterly zips 1991 Q1 - 2024 Q3:
#     https://sofia.usgs.gov/eden/models/watersurfacemod_download.php
#     (files https://sofia.usgs.gov/eden/data/dailymedian/{v2,v3}/YYYY_qN_DM_<release>.zip)
#   Gauge ground elevations: Haider et al. 2024, USGS data release, doi:10.5066/P13M7CRD
#   Gauge coordinates: Haider et al. 2026, USGS data release, doi:10.5066/P13BBYUJ
#   Species silhouettes: PhyloPic (CC0); see data/raw/icons/icons_attribution.csv
#   Florida and Lake Okeechobee outlines: R package maps 3.4.4 (GPL-2), databases "state" and "lakes"
#   Snail kite nests (CC BY 4.0): Fletcher 2024, figshare, doi:10.6084/m9.figshare.23540790.v3,
#     file nest_survival_1996_2021.csv
#
# Run from the project root:  Rscript data/get_data.R

suppressPackageStartupMessages({ library(readr); library(dplyr) })

src <- "../everglades_ecology_project"
stopifnot(dir.exists(src))
out <- "data/raw"
dir.create(file.path(out, "icons"), recursive = TRUE, showWarnings = FALSE)

# ---- wading birds (unchanged copies) -----------------------------------------------------
wb <- file.path(src, "data/raw/wadingbirds/WB-27")
copies <- c(
  "wading_birds_max_count_all.csv"       = "Indicators/max_count_all.csv",
  "wading_birds_species_list.csv"        = "SiteandMethods/species_list.csv",
  "wading_birds_colonies.csv"            = "SiteandMethods/colonies.csv",
  "wading_birds_maxcounts_by_colony.csv" = "Counts/maxcounts.csv",
  "everglades_subregions.geojson"        = "SiteandMethods/regions/subregions.geojson",
  "WB-27_LICENSE.txt"                    = "LICENSE"
)
for (f in names(copies)) stopifnot(file.copy(file.path(wb, copies[[f]]), file.path(out, f), overwrite = TRUE))

# ---- EDEN: 33 gauges, daily stage, with region and ground elevation ----------------------
wca <- c("SITE_7", "SITE_8T", "SITE_9", "SITE_17", "SITE_19", "SITE_99",
         "SITE_62", "SITE_63", "SITE_64", "SITE_65", "SITE_71", "SITE_76")
enp <- c("A13", "E112", "NP201", "NP202", "NP203", "NP205", "NP206", "NP44", "NP46", "NP62", "NP67",
         "NP72", "P33", "P34", "P35", "P36", "P37", "P38", "R3110", "RG1", "TSB")

meta <- read_csv(file.path(src, "data/extracted/hydrology_station_metadata.csv"), show_col_types = FALSE) |>
  filter(source_id == "HY-02", eden_name %in% c(wca, enp)) |>
  distinct(eden_name, .keep_all = TRUE)
gauges <- meta |>
  transmute(eden_name, region = ifelse(eden_name %in% wca, "WCA", "ENP"), lat, lon,
            ground_ft = ground_elev_major_ft_navd88) |>
  arrange(region, eden_name)
stopifnot(nrow(gauges) == 33, !anyNA(gauges$ground_ft))
write_csv(gauges, file.path(out, "eden_gauges.csv"))

stage <- read_csv(file.path(src, "data/extracted/hydrology_HY-02.csv"),
                  col_types = cols(.default = "c"), show_col_types = FALSE) |>
  filter(variable == "stage", units == "cm", vertical_datum == "NAVD88") |>
  inner_join(distinct(meta, station, eden_name), by = "station") |>
  transmute(date = as.Date(date), eden_name, stage_cm = as.numeric(value)) |>
  arrange(eden_name, date)
stopifnot(!anyDuplicated(stage[c("eden_name", "date")]), !anyNA(stage$stage_cm))
write_csv(stage, file.path(out, "eden_daily_stage.csv.gz"))   # readr gzips on the .gz extension

# ---- map outlines (maps package databases) ------------------------------------------------------
fl <- ggplot2::map_data("state", region = "florida")
lake <- dplyr::filter(ggplot2::map_data("lakes"), region == "Lake Okeechobee")
write_csv(dplyr::select(fl, long, lat, group, order), file.path(out, "florida_outline.csv"))
write_csv(dplyr::select(lake, long, lat, group, order), file.path(out, "lake_okeechobee_outline.csv"))

# ---- snail kite: annual nests in the Everglades wetlands (exploratory comparison) ----------
# Unique nests per breeding-season year in the wetlands of the wading-bird survey area: Loxahatchee NWR
# (WCA-1), WCA-2A, WCA-2B, WCA-3A, WCA-3B and Everglades National Park. Years without a nest there are 0.
kite_areas <- c("Loxahatchee", "WCA2A", "WCA2B", "WCA3A", "WCA3B", "ENP")
kite_nests <- read_csv(file.path(src, "data/raw/kite/KI-51/nest_survival_1996_2021.csv"), show_col_types = FALSE) |>
  filter(Area %in% kite_areas)
kite_nests |>
  distinct(Year, Nest_ID) |>
  count(Year, name = "nests") |>
  right_join(tibble(Year = 1996:2021), by = "Year") |>
  transmute(year = Year, nests = coalesce(nests, 0L)) |>
  arrange(year) |>
  write_csv(file.path(out, "kite_nests_everglades.csv"))
# the same nests by wetland (for the map); wetland-years without a nest are 0
kite_nests |>
  distinct(Area, Year, Nest_ID) |>
  count(Area, Year, name = "nests") |>
  right_join(tidyr::expand_grid(Area = sort(unique(kite_nests$Area)), Year = 1996:2021), by = c("Area", "Year")) |>
  transmute(wetland = Area, year = Year, nests = coalesce(nests, 0L)) |>
  arrange(wetland, year) |>
  write_csv(file.path(out, "kite_nests_by_wetland.csv"))

# ---- species silhouettes (PhyloPic, CC0; recoloured copies) -------------------------------
icon_src <- c(kite = "kite", stork = "stork1", ibis = "ibis1")
attr_all <- read_csv(file.path(src, "data/raw/icons/icons_attribution.csv"), show_col_types = FALSE)
for (k in names(icon_src))
  stopifnot(file.copy(file.path(src, "results/slide_figs", paste0("icon_", k, ".png")),
                      file.path(out, "icons", paste0("icon_", k, ".png")), overwrite = TRUE))
attr_all |>
  filter(icon %in% icon_src) |>
  mutate(file = paste0("icon_", names(icon_src)[match(icon, icon_src)], ".png"),
         note = "recoloured") |>
  select(file, uuid, contributor, license, license_url, source, note) |>
  write_csv(file.path(out, "icons", "icons_attribution.csv"))

# ---- PROVENANCE.md --------------------------------------------------------------------------
cite <- list(
  wb   = "Frederick P, Rickershauser M, Garner L, Yenni GM, White EP, Ernest SKM (2026) weecology/EvergladesWadingBird: data from the Everglades breeding bird monitoring project, release 4.41.0. Zenodo. https://doi.org/10.5281/zenodo.7734553",
  eden = "U.S. Geological Survey (2026) Everglades Depth Estimation Network (EDEN) daily median water-level files, 1991-2024. https://sofia.usgs.gov/eden (accessed 28 October 2026)",
  elev = "Haider SM, McCloskey B, Telis PA (2024) Ground-surface elevation, vegetation, and land type within approximately 10 and 400 meters of 176 water-level gaging stations in the Greater Everglades, Florida, 2005-10. U.S. Geological Survey data release. https://doi.org/10.5066/P13M7CRD",
  xy   = "Haider SM, Christesson KR, McCloskey B (2026) Gaging station coordinates and subareas for the Everglades Depth Estimation Network. U.S. Geological Survey data release. https://doi.org/10.5066/P13BBYUJ",
  icon = "PhyloPic silhouettes; contributors and UUIDs in data/raw/icons/icons_attribution.csv",
  maps = "Becker RA, Wilks AR, Brownrigg R, Minka TP, Deckmyn A (2026) maps: Draw Geographical Maps. R package version 3.4.4. https://doi.org/10.32614/CRAN.package.maps",
  kite = "Fletcher RJ Jr (2024) Data for Fletcher et al. 2024: An invasive prey and changing climate interact to shape the breeding phenology of an endangered predator. figshare, version 3. https://doi.org/10.6084/m9.figshare.23540790.v3"
)
lic <- list(
  cc0  = "CC0 1.0",
  ccby = "CC BY 4.0",
  gpl  = "GPL-2 (licence of the maps package that distributes these outlines)",
  eden = "EDEN data use terms: free of charge, may be shown, distributed or published at will; acknowledge EDEN/USGS (and the operating agency, NPS or SFWMD, for their gauges); some recent values are provisional",
  usgs = "U.S. Public Domain (USGS); credit requested"
)
who <- list(
  wb   = "University of Florida Everglades wading-bird monitoring programme (Frederick, Ernest and colleagues)",
  eden = "USGS Everglades Depth Estimation Network; gauges operated by USGS, NPS and SFWMD",
  usgs = "U.S. Geological Survey",
  icon = "PhyloPic contributors",
  maps = "Authors of the R package maps (Becker, Wilks, Brownrigg, Minka, Deckmyn)",
  kite = "University of Florida snail kite monitoring programme (Fletcher and colleagues)"
)
prov <- tribble(
  ~file,                                   ~content,                                                                  ~collected_by, ~licence,  ~citation,
  "wading_birds_max_count_all.csv",        "Annual maximum nesting pairs by species, 1986-2026; 1992-2024 used (region 'all' = WCA-1, WCA-2, WCA-3, Everglades NP)", who$wb, lic$cc0, cite$wb,
  "wading_birds_species_list.csv",         "Species table incl. foraging_type labels",                               who$wb,   lic$cc0,  cite$wb,
  "wading_birds_colonies.csv",             "Colony names and coordinates",                                           who$wb,   lic$cc0,  cite$wb,
  "wading_birds_maxcounts_by_colony.csv",  "Maximum nest counts by colony, species and year (map only)",             who$wb,   lic$cc0,  cite$wb,
  "everglades_subregions.geojson",         "Survey subregion polygons (map only)",                                   who$wb,   lic$cc0,  cite$wb,
  "WB-27_LICENSE.txt",                     "Licence file of the wading-bird data release",                          who$wb,   lic$cc0,  cite$wb,
  "eden_daily_stage.csv.gz",               "Daily median stage (cm NAVD88) at 33 EDEN gauges, 1991-01-01 to 2024-09-30 (subset of the EDEN daily median files; releases v2prov 1991-1999, v3 2000-2022 Q3, v3prov 2022 Q4-2024 Q3)", who$eden, lic$eden, cite$eden,
  "eden_gauges.csv",                       "Gauge table assembled for the 33 analysis gauges: region (WCA/ENP), coordinates and ground elevation (ft NAVD88)", who$usgs, lic$usgs, paste(cite$elev, cite$xy, sep = "; "),
  "florida_outline.csv",                   "Florida state outline (map only), from the maps database 'state'",       who$maps, lic$gpl,  cite$maps,
  "lake_okeechobee_outline.csv",           "Lake Okeechobee outline (map only), from the maps database 'lakes'",     who$maps, lic$gpl,  cite$maps,
  "kite_nests_everglades.csv",             "Annual snail kite nests (unique nests) in Loxahatchee NWR (WCA-1), WCA-2, WCA-3 and Everglades NP, 1996-2021, counted from the file nest_survival_1996_2021.csv (exploratory comparison only)", who$kite, lic$ccby, cite$kite,
  "kite_nests_by_wetland.csv",             "The same snail kite nests by wetland and year (map only)",              who$kite, lic$ccby, cite$kite,
  "icons/icon_kite.png",                   "Silhouette, recoloured during scoping",                                  who$icon, lic$cc0,  cite$icon,
  "icons/icon_stork.png",                  "Silhouette, recoloured during scoping",                                  who$icon, lic$cc0,  cite$icon,
  "icons/icon_ibis.png",                   "Silhouette, recoloured during scoping",                                  who$icon, lic$cc0,  cite$icon,
  "icons/icons_attribution.csv",           "Contributor, UUID and licence of each silhouette",                       who$icon, lic$cc0,  cite$icon
) |>
  mutate(sha256 = vapply(file.path(out, file), digest::digest, character(1), file = TRUE, algo = "sha256"))

md <- c(
  "# Data provenance",
  "",
  "All files in `data/raw/` come from public sources and are included under their licences.",
  "They are copies or subsets of public downloads made on 2026-10-28, derived as documented in `data/get_data.R`",
  "(which reads the original downloads, not included in this repository, and is not needed to render the analysis). The analysis reads them unchanged.",
  "",
  "| File | Content | Collected by | Licence | Cite as | SHA-256 |",
  "|---|---|---|---|---|---|",
  sprintf("| `%s` | %s | %s | %s | %s | `%s` |", prov$file, prov$content, prov$collected_by, prov$licence, prov$citation, prov$sha256),
  "",
  "Notes:",
  "",
  "- EDEN values from late 2022 onward are provisional (release v3prov) and may be revised by USGS."
)
writeLines(md, "data/PROVENANCE.md")
cat("data/raw written:", nrow(prov), "files; data/PROVENANCE.md updated\n")
