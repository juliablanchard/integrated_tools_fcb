# Build region-specific freshwater species weights -----------------------------
#
# PURPOSE
# `add_freshwater_aquaculture_pressures()` blends taxa-level pressures into one
# FRSHF intensity, and wants a production weight per taxon, ideally per REGION.
# GLOBIOM does NOT resolve tilapia vs carp vs catfish -- FISHSPEC bottoms out at
# the FRSH aggregate -- so the species split must come from FAO. GLOBIOM does
# however carry freshwater aquaculture production per COUNTRY, which is what
# turns country-level FAO shares into region-level weights:
#
#     region weight(taxa) = SUM over countries in region of
#                             GLOBIOM country production x FAO country share(taxa)
#
# WHY THIS IS A SMALL ASK
# GLOBIOM 2020 freshwater aquaculture is extremely concentrated: the top 3
# countries (China, India, Indonesia) are 77.4% of global production, the top 6
# are 90.5%, the top 12 are 94.9%. FAO species shares for roughly a dozen
# countries therefore determine almost the whole weighting.
#
# INPUTS
#   fao_shares  data.frame with columns COUNTRY, taxa, share
#               taxa must be one of tilapia / oth_carp / hypoph_carp / catfish.
#               `share` may be shares or raw tonnes; it is normalised per country.
#   production  data.frame with columns COUNTRY, kt  (GLOBIOM freshwater
#               aquaculture production). Use globiom_freshwater_production().
#   region_lookup  the country -> REGION table (data/lookup_regions_countries.csv)
#
# OUTPUT
#   data.frame(REGION, taxa, prod_weight), ready to append to the weights file.
#   Regions whose FAO-covered production falls below `min_coverage` are omitted
#   so they fall back to the global mix rather than being characterised from an
#   unrepresentative minority of their production.

# Normalise country names for matching: GLOBIOM concatenates ("VietNam",
# "SriLanka", "CostaRica") where the region lookup spaces them.
.norm_country <- function(x) tolower(gsub("[^A-Za-z]", "", x))

# Abbreviations that normalisation alone cannot resolve. GLOBIOM name -> region
# lookup name. Verified against both files.
GLOBIOM_COUNTRY_ALIASES <- c(
  "RussianFed"   = "Russian Federation",
  "CzechRep"     = "Czech Republic",
  "KoreaRep"     = "South Korea",
  "KoreaDPRp"    = "Korea DPR",
  "MoldovaRep"   = "Moldova",
  "CongoDemR"    = "Democratic Republic of Congo",
  "Serbia-Monte" = "Serbia-Montenegro",
  "USA"          = "United States of America",
  "UK"           = "United Kingdom"
)

globiom_freshwater_production <- function(fish_compare_path,
                                          scenario = "Scen_CLEAQU_bau",
                                          year = 2020,
                                          syst = "AQUA_F",
                                          spec = "FRSH") {
  f <- readRDS(fish_compare_path)
  for (cc in c("COUNTRY", "FISHSYST", "FISHSPEC", "ALLSCEN3")) {
    f[[cc]] <- as.character(f[[cc]])
  }
  f |>
    dplyr::filter(ALLSCEN3 == scenario, YEAR == year,
                  FISHSYST == syst, FISHSPEC == spec) |>
    dplyr::group_by(COUNTRY) |>
    dplyr::summarise(kt = sum(value), .groups = "drop") |>
    dplyr::filter(kt > 0)
}

build_region_species_weights <- function(fao_shares,
                                         production,
                                         region_lookup,
                                         min_coverage = 0.60) {

  stopifnot(all(c("COUNTRY", "taxa", "share") %in% names(fao_shares)))
  stopifnot(all(c("COUNTRY", "kt") %in% names(production)))

  valid <- c("tilapia", "oth_carp", "hypoph_carp", "catfish")
  bad <- setdiff(unique(fao_shares$taxa), valid)
  if (length(bad) > 0) {
    stop("Unknown taxa in fao_shares: ", paste(bad, collapse = ", "),
         ". Expected: ", paste(valid, collapse = ", "))
  }

  rl <- region_lookup
  names(rl)[1] <- "REGION"
  rl$Country <- trimws(rl$Country)
  rl$REGION  <- toupper(trimws(rl$REGION))
  rl$key <- .norm_country(rl$Country)

  # map GLOBIOM country -> REGION
  prod <- production |>
    dplyr::mutate(alias = ifelse(COUNTRY %in% names(GLOBIOM_COUNTRY_ALIASES),
                                 unname(GLOBIOM_COUNTRY_ALIASES[COUNTRY]), COUNTRY),
                  key = .norm_country(alias)) |>
    dplyr::left_join(rl[, c("key", "REGION")], by = "key")

  unmapped <- prod |> dplyr::filter(is.na(REGION))
  if (nrow(unmapped) > 0) {
    message("Countries with GLOBIOM production but no GLOBIOM region (",
            nrow(unmapped), ", ",
            round(100 * sum(unmapped$kt) / sum(prod$kt), 1),
            "% of production): ",
            paste(utils::head(unmapped$COUNTRY[order(-unmapped$kt)], 8), collapse = ", "),
            if (nrow(unmapped) > 8) ", ..." else "")
  }
  prod <- prod |> dplyr::filter(!is.na(REGION))

  # normalise FAO shares within country
  fs <- fao_shares |>
    dplyr::group_by(COUNTRY) |>
    dplyr::mutate(share = share / sum(share, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(key = .norm_country(
      ifelse(COUNTRY %in% names(GLOBIOM_COUNTRY_ALIASES),
             unname(GLOBIOM_COUNTRY_ALIASES[COUNTRY]), COUNTRY)))

  joined <- prod |> dplyr::inner_join(fs, by = "key", suffix = c("", ".fao"))
  if (nrow(joined) == 0) {
    stop("No country in fao_shares matched GLOBIOM production. Check country names.")
  }

  # how much of each region's production is actually covered by FAO shares
  covered <- joined |> dplyr::distinct(REGION, COUNTRY, kt) |>
    dplyr::group_by(REGION) |> dplyr::summarise(cov_kt = sum(kt), .groups = "drop")
  total <- prod |> dplyr::group_by(REGION) |>
    dplyr::summarise(tot_kt = sum(kt), .groups = "drop")
  cov <- covered |> dplyr::left_join(total, by = "REGION") |>
    dplyr::mutate(coverage = cov_kt / tot_kt)

  out <- joined |>
    dplyr::mutate(tonnes = kt * share) |>
    dplyr::group_by(REGION, taxa) |>
    dplyr::summarise(prod_weight = sum(tonnes), .groups = "drop") |>
    dplyr::inner_join(cov[, c("REGION", "coverage")], by = "REGION")

  dropped <- out |> dplyr::filter(coverage < min_coverage) |> dplyr::distinct(REGION, coverage)
  if (nrow(dropped) > 0) {
    message("Regions below ", round(100 * min_coverage), "% FAO coverage, omitted ",
            "so they fall back to the global mix: ",
            paste0(dropped$REGION, " (", round(100 * dropped$coverage), "%)",
                   collapse = ", "))
  }

  kept <- out |> dplyr::filter(coverage >= min_coverage)
  message("Region-specific weights built for ", dplyr::n_distinct(kept$REGION),
          " regions covering ",
          round(100 * sum(joined$kt[!duplicated(joined$COUNTRY)]) / sum(prod$kt), 1),
          "% of mapped GLOBIOM freshwater production.")

  kept |> dplyr::select(REGION, taxa, prod_weight) |> dplyr::arrange(REGION, taxa)
}

# Merge region weights into the pressures file, preserving the global fallback
# rows (REGION empty) that cover regions without their own mix.
write_weights_file <- function(region_weights, pressures_file, out_file = pressures_file) {
  g <- utils::read.csv(pressures_file, stringsAsFactors = FALSE)
  if (!"REGION" %in% names(g)) g$REGION <- NA_character_
  base <- g |> dplyr::filter(is.na(REGION) | !nzchar(trimws(REGION)))
  pressures <- unique(base$pressure)
  add <- tidyr::expand_grid(region_weights, pressure = pressures) |>
    dplyr::left_join(base |> dplyr::select(taxa, pressure, value_per_t_edible,
                                           unit, edible_pct, source, notes),
                     by = c("taxa", "pressure"))
  out <- dplyr::bind_rows(base, add)
  utils::write.csv(out, out_file, row.names = FALSE, na = "NA")
  message("Wrote ", out_file, ": ", nrow(base), " global rows + ", nrow(add),
          " region-specific rows.")
  invisible(out)
}
