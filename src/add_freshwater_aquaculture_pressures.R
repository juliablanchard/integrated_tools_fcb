# Add freshwater aquaculture pressures from Gephart et al. (2021) ---------------
#
# WHY THIS EXISTS
# Halpern et al. (2022) excludes freshwater aquaculture outright: "we were unable
# to include freshwater aquaculture due to a lack of information on farming
# location" (SI p11, ~73 Mt in 2017). In the GLOBIOM run 3945 projections
# freshwater aquaculture is 76,700 kt in BAU 2050 -- 70.7% of ALL aquaculture --
# and contributes ZERO pressure. Because pond systems are the highest-water and
# nutrient-heavy ones, aquaculture water and nutrient pressures are biased low by
# construction, not just incomplete.
#
# Gephart et al. 2021 (Nature 597:360-365; github.com/jagephart/FishPrint;
# Zenodo 10.5281/zenodo.5338614) covers tilapia, carps and catfish with per-tonne
# stressors on a comparable basis.
#
# SCOPE: ghg, water, nutrient and disturbance.
#
# DISTURBANCE is handled differently from the other three, and comes with a
# material caveat. Halpern SI 5.1.4: "For all categories of mariculture, except
# the unfed and algae fed shellfish, we assume a disruption value of 1, which
# means that natural habitats are fully replaced." For ponds specifically
# (SI 5.1.4.3, shrimp) disturbance is pond surface area in km2, increased by 50%
# "to account for farming infrastructure such as drainage canals, buildings, and
# other facilities". So for a pond system, disturbance IS area:
#
#     disturbance_km2_per_t = on_farm_area_m2_per_t / 1e6 * 1.5
#
# We therefore apply HALPERN'S METHOD using GEPHART'S EMPIRICAL AREAS
# (LCI_compiled_for_SI.csv, Yield_m2_per_t, freshwater pond records). We do NOT
# import Gephart's own land-use metric, because Halpern's pond assumption for
# shrimp (4,000 fry/ha at 30 shrimp/kg = 133 kg/ha) is 171x more extensive than
# the median of Gephart's 136 freshwater pond observations (22.8 t/ha). Mixing
# the two metrics would make freshwater aquaculture appear two orders of
# magnitude less disturbing than the shrimp rows purely by choice of source.
# Yield_m2_per_t is already per tonne LIVE weight, so no edible conversion.
#
# *** ON-FARM ONLY - READ BEFORE REPORTING DISTURBANCE ***
# This yields the on-farm pond footprint and EXCLUDES the feed contribution.
# For fed species that omission is large, not marginal: Halpern's own salmon
# disturbance is 0.0457 km2/t, whereas the cage geometry in SI 5.1.4.1
# (9,000 m3 at 10 m depth = 900 m2 holding 180 t) gives just 5e-06 km2/t --
# cage area explains 1 part in 9,134. The remainder is the forage fishery
# behind the feed (1.73 t forage fish/t salmon x 0.0357 km2/t = 0.062 km2/t,
# the right order). Freshwater aquaculture is largely fed (60,304 of 84,267 kt
# in BAU 2050), so FRSHF disturbance computed here is a LOWER BOUND and is not
# comparable like-for-like with the marine aquaculture rows.
# Adding the feed component would need aquafeed composition per tonne of fish
# crossed with Halpern's crop disturbance intensities, and interacts with how
# GLOBIOM crop production is already counted in this pipeline. Not done here.
#
# UNIT RECONCILIATION (verified against both papers)
#   Halpern pressure_per_tonne units, per tonne of production:
#     ghg      tonnes CO2eq      (Halpern SI: "GHG emissions, tonnes CO2eq")
#     water    m3                (Halpern SI: "blue water withdrawal, m3 freshwater")
#     nutrient tonnes N+P        (Halpern SI: "excess nutrients, tonnes of combined N and P")
#   Gephart reports per tonne EDIBLE weight: kgCO2e, m3, kgNe, kgPe.
#   Conversions applied below:
#     live-weight basis : value_per_t_live = value_per_t_edible * (edible_pct/100)
#     ghg               : kgCO2e -> t CO2e        (/1000)
#     water             : m3 -> m3                (no change)
#     nutrient          : (kgNe + kgPe) -> t      (/1000)
#
# KNOWN APPROXIMATION: Gephart's N and P are characterised EQUIVALENTS (kg N-eq
# for marine eutrophication, kg P-eq for freshwater eutrophication), whereas
# Halpern's nutrient is elemental tonnes N+P. Summing the equivalents to match
# Halpern is an approximation. Flag it wherever the nutrient result is reported.
#
# PRODUCTION WEIGHTING: GLOBIOM's FRSH item does not resolve tilapia vs carp vs
# catfish, so the taxa must be blended into a single FRSHF intensity. Supply
# `prod_weight` in the data file (shares summing to 1, ideally FAO production for
# the modelled regions). If left NA the function stops rather than silently
# using an unweighted mean, which would treat tilapia and carp as equally
# important when carp production is several times larger.


add_freshwater_aquaculture_pressures <- function(pressure_per_tonne,
                                                 data_path,
                                                 file = "gephart_freshwater_aquaculture_pressures.csv",
                                                 item = "FRSHF",
                                                 organism = "freshwater-aquaculture",
                                                 infrastructure_uplift = 1.5) {

  path <- file.path(data_path, file)
  if (!file.exists(path)) {
    warning("Freshwater aquaculture file not found at ", path,
            " - freshwater aquaculture will contribute ZERO pressure.")
    return(pressure_per_tonne)
  }

  g <- utils::read.csv(path, stringsAsFactors = FALSE)

  # Production weights are required for any blend. GLOBIOM's FRSH item does not
  # resolve tilapia vs carp vs catfish, and an unweighted mean would treat them
  # as equally important.
  #
  # REGIONAL weights are strongly preferred over global ones. The freshwater
  # species mix varies enormously between GLOBIOM regions -- China is carp
  # dominated, Egypt (NORTHERNAF) tilapia dominated, Viet Nam (RSEA_OPA)
  # pangasius dominated -- and a single global blend would impose China's mix on
  # every region. Supply an optional REGION column in the weights file to get
  # region-specific blends; rows with REGION = "" or NA are used as the fallback
  # for any region not otherwise covered.
  #
  # Source: FAO FishStatJ, Global Aquaculture Production, species-level tonnes
  # aggregated to the four taxa groups and to GLOBIOM regions.
  wt_all <- g |> dplyr::filter(!is.na(prod_weight))
  if (nrow(wt_all) == 0) {
    message("NOTE: prod_weight is empty in ", file,
            " - freshwater aquaculture contributes ZERO pressure. ",
            "Supply FAO production shares per taxon (optionally per REGION) ",
            "to enable the blend.")
    return(pressure_per_tonne)
  }

  has_region <- "REGION" %in% names(wt_all) &&
    any(!is.na(wt_all$REGION) & nzchar(trimws(wt_all$REGION)))
  if (!has_region) wt_all$REGION <- NA_character_

  wt_all <- wt_all |>
    dplyr::mutate(REGION = ifelse(is.na(REGION) | !nzchar(trimws(REGION)),
                                  NA_character_, toupper(trimws(REGION)))) |>
    dplyr::distinct(REGION, taxa, prod_weight)

  wt_global <- wt_all |> dplyr::filter(is.na(REGION))
  if (nrow(wt_global) == 0) {
    stop("Weights in ", file, " are region-specific only, with no fallback rows. ",
         "Add rows with an empty REGION so regions without their own mix are covered.")
  }

  # normalise within each weight set
  norm <- function(d) dplyr::mutate(d, w = prod_weight / sum(prod_weight))
  wt_global <- norm(wt_global)[, c("taxa", "w")]
  wt_region <- wt_all |> dplyr::filter(!is.na(REGION)) |>
    dplyr::group_by(REGION) |> norm() |> dplyr::ungroup()

  if (has_region) {
    message("Region-specific freshwater species mixes supplied for: ",
            paste(sort(unique(wt_region$REGION)), collapse = ", "),
            ". All other regions use the global fallback.")
  } else {
    message("Using a single GLOBAL freshwater species mix for all regions. ",
            "Region-specific weights would be more accurate - see script header.")
  }

  blend <- function(df, value_col, weights) {
    df |>
      dplyr::select(taxa, pressure, val = dplyr::all_of(value_col)) |>
      dplyr::inner_join(weights, by = "taxa") |>
      dplyr::group_by(pressure) |>
      dplyr::summarise(v = sum(val * w) / sum(w), .groups = "drop")
  }

  gd <- g[g$pressure == "disturbance" & !is.na(g$value_per_t_edible), , drop = FALSE]
  go <- g[g$pressure %in% c("ghg", "water", "N", "P"), , drop = FALSE]

  posteriors_ready <- !all(is.na(go$value_per_t_edible))
  if (!posteriors_ready) {
    message("NOTE: ghg/water/N/P are unpopulated in ", file,
            " - only disturbance will be added. Fill from Nature Source Data ",
            "for Fig 1 (Gephart et al. 2021) or the taxa-level posteriors.")
  } else if (anyNA(go$value_per_t_edible)) {
    stop("Partially filled ", file, ": ", sum(is.na(go$value_per_t_edible)),
         " of ", nrow(go), " ghg/water/N/P values are NA. Fill all four for ",
         "every taxon, or remove the taxon.")
  }
  if (posteriors_ready) {
    go$per_t_live <- go$value_per_t_edible * (go$edible_pct / 100)
  }

  # Blend once per weight set: the global fallback, plus any region-specific mix.
  one_region <- function(weights) {
    out <- list()
    # disturbance: Halpern's pond method on Gephart's empirical areas.
    # Yield_m2_per_t is already per tonne LIVE weight, so no edible conversion.
    # ON-FARM ONLY - see header. Lower bound for fed systems.
    if (nrow(gd) > 0) {
      b <- blend(gd, "value_per_t_edible", weights)
      out$disturbance <- b$v[b$pressure == "disturbance"] / 1e6 * infrastructure_uplift
    }
    if (posteriors_ready) {
      b <- blend(go, "per_t_live", weights)
      v <- stats::setNames(b$v, b$pressure)
      out$ghg      <- v[["ghg"]] / 1000             # kgCO2e -> t CO2eq
      out$water    <- v[["water"]]                  # m3 -> m3
      out$nutrient <- (v[["N"]] + v[["P"]]) / 1000  # kgNe+kgPe -> t (approx, see header)
    }
    if (length(out) == 0) return(NULL)
    data.frame(pressure = names(out),
               pressure_per_tonne = unlist(out, use.names = FALSE),
               stringsAsFactors = FALSE)
  }

  default_vals <- one_region(wt_global)
  if (is.null(default_vals)) return(pressure_per_tonne)

  regions <- unique(pressure_per_tonne$REGION)
  new_rows <- tidyr::expand_grid(REGION = regions, default_vals)

  # overwrite regions that have their own species mix
  for (rg in intersect(unique(wt_region$REGION), regions)) {
    rv <- one_region(wt_region[wt_region$REGION == rg, c("taxa", "w")])
    if (is.null(rv)) next
    new_rows <- new_rows |> dplyr::filter(REGION != rg) |>
      dplyr::bind_rows(tidyr::expand_grid(REGION = rg, rv))
  }

  new_rows <- new_rows |>
    dplyr::mutate(ITEM = item, Organism = organism, SYST = "AQUA_F",
                  System = "aquaculture", n_countries = NA_integer_,
                  pressure_value = NA_real_, tonnes = NA_real_,
                  SOURCE = ifelse(pressure == "disturbance",
                                  "Halpern-method/FishPrint-yield", "Gephart2021"))

  if (!"SOURCE" %in% names(pressure_per_tonne)) {
    pressure_per_tonne$SOURCE <- "Halpern2022"
  }

  message("Added freshwater aquaculture (", item, ") for [",
          paste(unique(new_rows$pressure), collapse = ", "), "] across ",
          length(regions), " regions.",
          if ("disturbance" %in% new_rows$pressure)
            " Disturbance is ON-FARM ONLY and a lower bound - see script header." else "")

  dplyr::bind_rows(pressure_per_tonne, new_rows)
}
