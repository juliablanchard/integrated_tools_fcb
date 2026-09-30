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
# SCOPE: GHG, water and nutrient ONLY.
# Disturbance is deliberately NOT grafted. Halpern's disturbance is km2eq of
# habitat displacement with a disruption weighting; Gephart's land is m2a of
# terrestrial occupation. These are different quantities and there is no
# defensible conversion. Freshwater aquaculture therefore remains absent from the
# disturbance pressure, and that must be stated wherever disturbance is reported.
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
                                                 organism = "freshwater-aquaculture") {

  path <- file.path(data_path, file)
  if (!file.exists(path)) {
    warning("Gephart freshwater file not found at ", path,
            " - freshwater aquaculture will contribute ZERO pressure.")
    return(pressure_per_tonne)
  }

  g <- utils::read.csv(path, stringsAsFactors = FALSE)

  if (all(is.na(g$value_per_t_edible))) {
    message("NOTE: ", file, " has no values yet. Freshwater aquaculture will ",
            "contribute ZERO pressure - aquaculture water and nutrient results ",
            "remain biased low. See the header of this script for how to fill it.")
    return(pressure_per_tonne)
  }
  if (anyNA(g$value_per_t_edible)) {
    stop("Partially filled ", file, ": ",
         sum(is.na(g$value_per_t_edible)), " of ", nrow(g),
         " values are NA. Fill all four pressures for every taxon, or remove the taxon.")
  }
  if (anyNA(g$prod_weight)) {
    stop("prod_weight is NA in ", file, ". Supply production shares for the taxa ",
         "blend (see header) - an unweighted mean would misrepresent the mix.")
  }

  # convert to live weight, then to Halpern units
  g$per_t_live <- g$value_per_t_edible * (g$edible_pct / 100)

  w <- g |>
    dplyr::distinct(taxa, prod_weight) |>
    dplyr::mutate(prod_weight = prod_weight / sum(prod_weight))

  blend <- g |>
    dplyr::select(taxa, pressure, per_t_live) |>
    dplyr::left_join(w, by = "taxa") |>
    dplyr::group_by(pressure) |>
    dplyr::summarise(per_t_live = sum(per_t_live * prod_weight), .groups = "drop")

  gv <- stats::setNames(blend$per_t_live, blend$pressure)
  out <- data.frame(
    pressure = c("ghg", "water", "nutrient"),
    pressure_per_tonne = c(gv[["ghg"]] / 1000,
                           gv[["water"]],
                           (gv[["N"]] + gv[["P"]]) / 1000),
    stringsAsFactors = FALSE
  )

  # Applied to every region: Gephart taxa-level values are global, not
  # country-resolved. Regional differentiation would need the country-level
  # posteriors, which are not in the public repository.
  regions <- unique(pressure_per_tonne$REGION)
  new_rows <- tidyr::expand_grid(REGION = regions, out) |>
    dplyr::mutate(ITEM = item, Organism = organism, SYST = "AQUA_F",
                  System = "aquaculture", n_countries = NA_integer_,
                  pressure_value = NA_real_, tonnes = NA_real_,
                  SOURCE = "Gephart2021")

  if (!"SOURCE" %in% names(pressure_per_tonne)) {
    pressure_per_tonne$SOURCE <- "Halpern2022"
  }

  message("Added freshwater aquaculture (", item, ") for ghg/water/nutrient across ",
          length(regions), " regions from Gephart et al. 2021. ",
          "Disturbance deliberately excluded - see script header.")

  dplyr::bind_rows(pressure_per_tonne, new_rows)
}
