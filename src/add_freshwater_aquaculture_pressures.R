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
  # as equally important. Source: FAO FishStatJ aquaculture production for the
  # modelled regions.
  weights <- g |>
    dplyr::distinct(taxa, prod_weight) |>
    dplyr::filter(!is.na(prod_weight))
  if (nrow(weights) == 0) {
    message("NOTE: prod_weight is empty in ", file,
            " - freshwater aquaculture contributes ZERO pressure. ",
            "Supply FAO production shares per taxon to enable the blend.")
    return(pressure_per_tonne)
  }
  weights <- data.frame(taxa = weights$taxa,
                        w = weights$prod_weight / sum(weights$prod_weight),
                        stringsAsFactors = FALSE)

  blend <- function(df, value_col) {
    df |>
      dplyr::select(taxa, pressure, val = dplyr::all_of(value_col)) |>
      dplyr::inner_join(weights, by = "taxa") |>
      dplyr::group_by(pressure) |>
      dplyr::summarise(v = sum(val * w) / sum(w), .groups = "drop")
  }

  out <- list()

  # --- disturbance: Halpern's pond method on Gephart's empirical areas --------
  # Yield_m2_per_t is per tonne LIVE weight, so no edible conversion.
  # ON-FARM ONLY - see the header. This is a lower bound for fed systems.
  gd <- g[g$pressure == "disturbance" & !is.na(g$value_per_t_edible), , drop = FALSE]
  if (nrow(gd) > 0) {
    b <- blend(gd, "value_per_t_edible")
    out$disturbance <- b$v[b$pressure == "disturbance"] / 1e6 * infrastructure_uplift
  }

  # --- ghg / water / nutrient from the Gephart posteriors --------------------
  go <- g[g$pressure %in% c("ghg", "water", "N", "P"), , drop = FALSE]
  if (all(is.na(go$value_per_t_edible))) {
    message("NOTE: ghg/water/N/P are unpopulated in ", file,
            " - only disturbance will be added. Fill from Nature Source Data ",
            "for Fig 1 (Gephart et al. 2021) or the taxa-level posteriors.")
  } else if (anyNA(go$value_per_t_edible)) {
    stop("Partially filled ", file, ": ", sum(is.na(go$value_per_t_edible)),
         " of ", nrow(go), " ghg/water/N/P values are NA. Fill all four for ",
         "every taxon, or remove the taxon.")
  } else {
    go$per_t_live <- go$value_per_t_edible * (go$edible_pct / 100)
    b <- blend(go, "per_t_live")
    v <- stats::setNames(b$v, b$pressure)
    out$ghg      <- v[["ghg"]] / 1000          # kgCO2e -> t CO2eq
    out$water    <- v[["water"]]               # m3 -> m3
    out$nutrient <- (v[["N"]] + v[["P"]]) / 1000  # kgNe + kgPe -> t (approx, see header)
  }

  if (length(out) == 0) return(pressure_per_tonne)

  vals <- data.frame(pressure = names(out),
                     pressure_per_tonne = unlist(out, use.names = FALSE),
                     stringsAsFactors = FALSE)

  # Gephart taxa-level values are global, not country-resolved, so the same
  # intensity is applied to every region. Regional differentiation would need
  # the country-level posteriors, which are not public.
  regions <- unique(pressure_per_tonne$REGION)
  new_rows <- tidyr::expand_grid(REGION = regions, vals) |>
    dplyr::mutate(ITEM = item, Organism = organism, SYST = "AQUA_F",
                  System = "aquaculture", n_countries = NA_integer_,
                  pressure_value = NA_real_, tonnes = NA_real_,
                  SOURCE = ifelse(pressure == "disturbance",
                                  "Halpern-method/FishPrint-yield", "Gephart2021"))

  if (!"SOURCE" %in% names(pressure_per_tonne)) {
    pressure_per_tonne$SOURCE <- "Halpern2022"
  }

  message("Added freshwater aquaculture (", item, ") for [",
          paste(names(out), collapse = ", "), "] across ", length(regions),
          " regions.",
          if ("disturbance" %in% names(out))
            " Disturbance is ON-FARM ONLY and a lower bound - see script header." else "")

  dplyr::bind_rows(pressure_per_tonne, new_rows)
}
