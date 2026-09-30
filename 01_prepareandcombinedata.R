
source("00_setup.R")


### Step 1. Read Halpern et al. (2022) pressure data ----------------------------

# Country- and food-specific environmental pressures per tonne of production
pressure_per_tonne <- read.csv(
  paste0(data_path, "pressure_per_tonne_data.csv")
)

head(pressure_per_tonne)
unique(pressure_per_tonne$organism)


### Step 2. Match Halpern countries to GLOBIOM regions -------------------------

# Add long country names using ISO3 country codes
lookup_c <- read.csv(paste0(data_path, "all.csv")) |>
  rename(iso3c = alpha.3)

pressure_per_tonne <- pressure_per_tonne |>
  left_join(lookup_c) |>
  rename(Country = name)

# Match countries to GLOBIOM regions
lookup_c <- read.csv(
  paste0(data_path, "lookup_regions_countries.csv")
)

# Diagnostic checks found that EU_BALTIC retains 3 country-level Halpern
# observations, whereas other multi-country GLOBIOM regions currently appear
# to retain only one.
#
# Example:
# EU_CENTRALEAST lookup includes Bulgaria, Czech Republic, Hungary, Poland,
# Romania, Slovakia and Slovenia, but only Bulgaria survived the current match.
#
# One observed naming mismatch:
# all.csv:                       "Czechia"
# lookup_regions_countries.csv:  "Czech Republic"
#
# CHECK BEFORE CHANGING ANYTHING:
# - whether left_join() is matching countries as intended
# - how many countries fail to match because of country-name differences
# - whether the lookup can/should use ISO3 codes instead
# - only then revisit whether REGION-level aggregation is required
#
# Do not aggregate EU_BALTIC until this has been checked.

pressure_per_tonne <- pressure_per_tonne |>
  left_join(lookup_c) |>
  filter(!is.na(REGION))

pressure_per_tonne |>
  filter(iso3c %in% c("BGR", "CZE", "HUN", "POL", "ROU", "SVK", "SVN")) |>
  distinct(iso3c, Country)

### Step 3. Match Halpern food types to GLOBIOM ITEM codes ---------------------

pressure_per_tonne$Organism <- pressure_per_tonne$organism

# --- Livestock aggregation and co-product allocation --------------------------
# GLOBIOM livestock items are broader than Halpern organisms:
#   BVMEAT / BVMILK  = bovine         -> Halpern 'cows'  + 'buffaloes'
#   SGMEAT / SGMILK  = small ruminant -> Halpern 'sheep' + 'goats'
# The old lookup mapped SGMEAT to BOTH 'sheep_meat' and 'goats_meat', which
# produced two pressure rows per SGMEAT x country x pressure and double counted
# on the join. Aggregate the Halpern organisms into GLOBIOM-compatible groups
# instead, summing pressure_value and tonnes and re-dividing so the blend is
# PRODUCTION-WEIGHTED. Averaging pressure_per_tonne directly would be unweighted
# and wrong.
#
# CO-PRODUCT ALLOCATION: Halpern's `tonnes` denominator is the sum of an animal's
# co-products, verified against FAO 2017 -- cows 752.1 Mt vs meat 68 + milk 683;
# sheep 22.1 vs 9.5 + 10.6; goats 25.9 vs 6.0 + 18.7; buffaloes 121.0 vs 4 + 120.
# So pressure_per_tonne is ALREADY a mass-allocated intensity across co-products.
# Applying it to each co-product and summing therefore reproduces the correct
# animal-level total. This is mass allocation, inherited from Halpern; it does
# NOT differentiate beef from milk per tonne. Switching to economic or protein
# allocation would need co-product price/protein factors this dataset lacks.
LIVESTOCK_GROUPS <- c(cows = "bovine", buffaloes = "bovine",
                      sheep = "smallruminants", goats = "smallruminants",
                      pigs = "pigs", chickens = "chickens")

livestock_agg <- pressure_per_tonne |>
  dplyr::filter(Organism %in% names(LIVESTOCK_GROUPS)) |>
  dplyr::mutate(Organism = unname(LIVESTOCK_GROUPS[Organism])) |>
  dplyr::group_by(iso3c, Organism, pressure) |>
  dplyr::summarise(pressure_value = sum(pressure_value, na.rm = TRUE),
                   tonnes         = sum(tonnes, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::mutate(pressure_per_tonne = pressure_value / tonnes) |>
  dplyr::filter(is.finite(pressure_per_tonne))

pressure_per_tonne <- dplyr::bind_rows(
  pressure_per_tonne |> dplyr::filter(!Organism %in% names(LIVESTOCK_GROUPS)),
  livestock_agg
)

message("Livestock aggregated into ", dplyr::n_distinct(livestock_agg$Organism),
        " groups across ", dplyr::n_distinct(livestock_agg$iso3c), " countries.")

lookup <- read.csv(
  paste0(data_path, "lookup_item_foodsystem.csv")
)

# Previous note:
# lookup <- subset(lookup, !is.na(REGION))

# --- Join coverage check ------------------------------------------------------
# The lookup joins to Halpern on `Organism`. The two files use different naming
# conventions, and the filter(!is.na(ITEM)) below discards any failures silently.
# Before the fix, that dropped ALL 7 AQUA_M items and ALL 10 ANIMAL items, so the
# "integrated" (marine-inclusive) result contained no aquaculture and the
# "no_marine" baseline contained no livestock. Fail loudly instead.
#
# EXPECTED unmatched, by design:
#   FEED   (FSHM, FSHO)          - Halpern embeds feed pressures in the fed-animal
#                                  per-tonne values (Halpern SI Fig. S1, S6), so
#                                  giving fishmeal/oil their own rows double counts.
#   CROP   (SUNF, RAPE_ML, SOY_ML) - no Organism assigned in the lookup.
#   ANIMAL (ALMILK only)         - ALMILK = BVMILK + SGMILK exactly, so including
#                                  it alongside its components would double count
#                                  milk. All other ANIMAL items now resolve via
#                                  the livestock aggregation above.
EXPECTED_UNMATCHED <- c("FEED", "CROP")
EXPECTED_UNMATCHED_ITEMS <- c("ALMILK")

lookup_cov <- lookup |>
  dplyr::mutate(matched = trimws(Organism) %in% unique(pressure_per_tonne$Organism))

unmatched <- lookup_cov |> dplyr::filter(!matched)

if (nrow(unmatched) > 0) {
  message("Lookup rows with no Halpern organism match (", nrow(unmatched), "):")
  print(as.data.frame(unmatched[, c("ITEM", "SYST", "Organism")]))
}

bad <- unmatched |>
  dplyr::filter(!SYST %in% EXPECTED_UNMATCHED, !ITEM %in% EXPECTED_UNMATCHED_ITEMS)
if (nrow(bad) > 0) {
  stop("Unexpected unmatched organisms in lookup_item_foodsystem.csv:\n",
       paste0("  ", bad$ITEM, " (", bad$SYST, "): '", bad$Organism, "'",
              collapse = "\n"),
       "\nFix the Organism name to match pressure_per_tonne_data.csv, or add its ",
       "SYST to EXPECTED_UNMATCHED with a documented reason.")
}

# Every aquatic item must carry a pressure estimate - this is the core of the
# integrated (land + marine) calculation, so a silent drop invalidates the result.
aquatic_missing <- unmatched |> dplyr::filter(SYST %in% c("CATCH", "AQUA_M"))
stopifnot("aquatic items lost pressure data in the lookup join" =
            nrow(aquatic_missing) == 0)

pressure_per_tonne <- pressure_per_tonne |>
  left_join(lookup) |>
  filter(!is.na(ITEM))

# Maybe needs more cleaning.
##STEP 3B. check if the matching is clean


region_item_check <- pressure_per_tonne |>
  count(REGION, ITEM, pressure) |>
  filter(n > 1)

region_item_check

# Inspect the duplicated REGION × ITEM × pressure combinations
# to see whether their pressure-per-tonne values differ.

region_item_values <- pressure_per_tonne |>
  semi_join(
    region_item_check,
    by = c("REGION", "ITEM", "pressure")
  ) |>
  select(
    iso3c, REGION, ITEM, pressure,
    pressure_value, tonnes, pressure_per_tonne
  ) |>
  arrange(REGION, ITEM, pressure, iso3c)

region_item_values


region_item_values |>
  group_by(REGION, ITEM, pressure) |>
  summarise(
    n_values = n_distinct(pressure_per_tonne),
    .groups = "drop"
  ) |>
  count(n_values)



unique(pressure_per_tonne$ITEM)

### Step 4. Read and select GLOBIOM scenarios ----------------------------------

scenarios <- readRDS(
  paste0(scenario_path, "output_3945_merged.RDS")
)

# Useful checks when scenario outputs change
nrow(scenarios)
unique(scenarios$ALLSCEN3)
unique(scenarios$REGION)
unique(scenarios$VAR_ID)

# Keep projected production only and the three scenarios being compared
scenarios <- scenarios |>
  filter(
    VAR_ID == "Prod",
    ALLSCEN3 %in% c(
      "Scen_ScottBAU_core_FCAP0",
      "Scen_ScottBarriers_core_FCAP0",
      "Scen_ScottBlueTrans_core_FCAP0"
    )
  )

# Current scenario interpretation:
# Scen_ScottBAU_core_FCAP0       = BAU
# Scen_ScottBarriers_core_FCAP0  = Barriers to Blue Growth
# Scen_ScottBlueTrans_core_FCAP0 = Blue Transformation

# Previous scenario names used in earlier GLOBIOM output:
# SCEN_DIET0_CULTURE0_CAPTURE0          = BAU
# SCEN_DIET+10_CULTURE+50_CAPTURE+10   = Blue Transformation
# SCEN_DIET-10_CULTURE0_CAPTURE-10     = Barriers to Blue Growth

# List foods represented in selected GLOBIOM scenarios
unique(scenarios$ITEM)


### Step 5. Match GLOBIOM production to Halpern pressure intensities -----------

# ITEM and REGION differed in capitalization between the two datasets.
# Standardise both before joining.

scenarios <- scenarios |>
  mutate(
    ITEM = toupper(as.character(ITEM)),
    REGION = toupper(as.character(REGION))
  )

pressure_per_tonne <- pressure_per_tonne |>
  mutate(
    ITEM = toupper(ITEM),
    REGION = toupper(REGION)
  )

# Join environmental pressure intensities to GLOBIOM production.
#
# NOTE: A many-to-many warning is expected here because:
# - GLOBIOM has repeated ITEM-REGION combinations across years/scenarios
# - Halpern has repeated ITEM-REGION combinations across pressure types
#
# A production observation therefore needs to be associated with multiple
# environmental pressure observations.

scenarios <- scenarios |>
  left_join(
    pressure_per_tonne,
    by = c("ITEM", "REGION")
  ) |>
  filter(!is.na(value))

# Keep only GLOBIOM ITEM-REGION combinations for which a Halpern
# pressure-per-tonne value is available.
scenarios_matched <- scenarios |>
  filter(!is.na(pressure_per_tonne))


### Step 6. Calculate integrated environmental pressures -----------------------

# For each matched commodity:
#
# projected pressure =
# GLOBIOM projected production × Halpern pressure per tonne
#
# Then aggregate across commodities/regions for each pressure, year and scenario.

# CHECK WITH SCOTT:
# I don't think VALUE is 1000T - it is SSP 14/09.
# Confirm the units of GLOBIOM 'value' before interpreting absolute pressures.

scen <- scenarios_matched |>
  mutate(
    projected_production_tonnes = returnValue(value),
    total_pressure = value * pressure_per_tonne
  ) |>
  group_by(pressure, YEAR, ALLSCEN3) |>
  summarise(
    Total_projected_pressure = sum(total_pressure, na.rm = TRUE)
  )

scen$method <- "integrated"


### Previous relative-scenario calculation code -------------------------------

# scenarios_bau <- subset(
#   scenarios,
#   ALLSCEN3 == "SCEN_DIET0_CULTURE0_CAPTURE0"
# )
#
# scenarios_blue <- subset(
#   scenarios,
#   ALLSCEN3 == "SCEN_DIET+10_CULTURE+50_CAPTURE+10"
# )
#
# scenarios_yellow <- subset(
#   scenarios,
#   ALLSCEN3 == "SCEN_DIET-10_CULTURE0_CAPTURE-10"
# )
#
# scenarios_blue$BAU_OUTPUT <- scenarios_bau$OUTPUT
# scenarios_yellow$BAU_OUTPUT <- scenarios_bau$OUTPUT
#
# scenarios_blue$relCPI <-
#   (scenarios_blue$Total_projected_cumpressure -
#      scenarios_bau$Total_projected_cumpressure) /
#   scenarios_bau$Total_projected_cumpressure
#
# scenarios_yellow$relCPI <-
#   (scenarios_green$Total_projected_cumpressure -
#      scenarios_bau$Total_projected_cumpressure) /
#   scenarios_bau$Total_projected_cumpressure
#
# scen <- rbind(scenarios_blue, scenarios_green)


### Step 7. Integrated pressures in 2050 ---------------------------------------

scen_2050 <- scen |>
  filter(
    YEAR == "2050",
    !is.na(pressure)
  )

p <- ggplot(
  scen_2050,
  aes(
    x = YEAR,
    y = Total_projected_pressure,
    fill = ALLSCEN3
  )
) +
  geom_col(
    position = position_dodge(0.8),
    width = 0.7
  ) +
  facet_wrap(
    ~pressure,
    scales = "free_y"
  )

p


### Step 8. Terrestrial-only ("no marine") pressure accounting -----------------

# Restrict the same matched dataset to crops and terrestrial animals.
# Comparing this with the integrated calculation isolates the effect of
# including marine food production pressures.

land <- c("CROP", "ANIMAL")

scenarios_nomarine <- scenarios_matched |>
  filter(SYST %in% land)

scen_nomarine <- scenarios_nomarine |>
  mutate(
    projected_production_tonnes = returnValue(value),
    total_pressure = value * pressure_per_tonne
  ) |>
  group_by(pressure, YEAR, ALLSCEN3) |>
  summarise(
    Total_projected_pressure = sum(total_pressure, na.rm = TRUE)
  )

scen_nomarine$method <- "no_marine"

scen_nomarine_2050 <- scen_nomarine |>
  filter(
    YEAR == "2050",
    !is.na(pressure)
  )

p2 <- ggplot(
  scen_nomarine_2050,
  aes(
    x = YEAR,
    y = Total_projected_pressure,
    fill = ALLSCEN3
  )
) +
  geom_col(
    position = position_dodge(0.8),
    width = 0.7
  ) +
  facet_wrap(
    ~pressure,
    scales = "free_y"
  )

p2


### Step 9. Change relative to 2020 BAU ----------------------------------------

scen_2020 <- scen |>
  filter(
    YEAR == "2020",
    !is.na(pressure),
    ALLSCEN3 == "Scen_ScottBAU_core_FCAP0"
  )

scen_2020 <- rbind(
  scen_2020,
  scen_2020,
  scen_2020
)

scen_2050 <- scen_2050 |>
  arrange(ALLSCEN3)

scen_2050$Pressure_rel_change <-
  scen_2050$Total_projected_pressure /
  scen_2020$Total_projected_pressure


scen_nomarine_2020 <- scen_nomarine |>
  filter(
    YEAR == "2020",
    !is.na(pressure),
    ALLSCEN3 == "Scen_ScottBAU_core_FCAP0"
  )

scen_nomarine_2020 <- rbind(
  scen_nomarine_2020,
  scen_nomarine_2020,
  scen_nomarine_2020
)

scen_nomarine_2050 <- scen_nomarine_2050 |>
  arrange(ALLSCEN3)

scen_nomarine_2050$Pressure_rel_change <-
  scen_nomarine_2050$Total_projected_pressure /
  scen_nomarine_2020$Total_projected_pressure


### Step 10. Change relative to BAU in 2050 ------------------------------------

scen_BAU <- scen_2050 |>
  filter(
    ALLSCEN3 == "Scen_ScottBAU_core_FCAP0"
  )

scen_BAU <- rbind(
  scen_BAU,
  scen_BAU,
  scen_BAU
)

scen_2050 <- scen_2050 |>
  arrange(ALLSCEN3)

scen_2050$Pressure_rel_BAU <-
  scen_2050$Total_projected_pressure /
  scen_BAU$Total_projected_pressure


scen_nomarine_BAU <- scen_nomarine_2050 |>
  filter(
    ALLSCEN3 == "Scen_ScottBAU_core_FCAP0"
  )

scen_nomarine_BAU <- rbind(
  scen_nomarine_BAU,
  scen_nomarine_BAU,
  scen_nomarine_BAU
)

scen_nomarine_2050 <- scen_nomarine_2050 |>
  arrange(ALLSCEN3)

scen_nomarine_2050$Pressure_rel_BAU <-
  scen_nomarine_2050$Total_projected_pressure /
  scen_nomarine_BAU$Total_projected_pressure


### Step 11. Compare integrated vs terrestrial-only accounting -----------------

scen_join_2050 <- rbind(
  scen_2050,
  scen_nomarine_2050
)

# Previous plot: relative to BAU in 2020
#
# p2 <- ggplot(
#   filter(
#     scen_join_2050,
#     ALLSCEN3 != "SCEN_DIET0_CULTURE0_CAPTURE0"
#   ),
#   aes(
#     x = method,
#     y = Pressure_rel_change,
#     fill = ALLSCEN3
#   )
# ) +
#   geom_col(
#     position = position_dodge(0.8),
#     width = 0.7
#   ) +
#   facet_wrap(
#     ~pressure,
#     scales = "free_y"
#   )
#
# p2


# Overall differences from including marine food pressures
p3 <- ggplot(
  scen_join_2050,
  aes(
    x = method,
    y = Total_projected_pressure,
    fill = ALLSCEN3
  )
) +
  geom_col(
    position = position_dodge(0.8),
    width = 0.7
  ) +
  facet_wrap(
    ~pressure,
    scales = "free_y"
  )

p3


# Relative to BAU in 2050
#
# There are (slight) differences in directions of change across scenarios.

p4 <- ggplot(
  filter(
    scen_join_2050,
    ALLSCEN3 != "Scen_ScottBAU_core_FCAP0"
  ),
  aes(
    x = method,
    y = Pressure_rel_BAU,
    color = ALLSCEN3
  )
) +
  geom_point(
    position = position_dodge(0.8)
  ) +
  facet_wrap(
    ~pressure,
    scales = "free_y"
  ) +
  theme_minimal()

p4


### Interpretation / feed-pressure clarification -------------------------------

# Despite pressures being higher (obviously), shifts to blue foods still appear
# better for reducing pressures.

# Previous note:
# "not accounting for feed pressures.... how to add these in?"

# SSP note 15/09:
# Halpern's production-pressure estimates for fed livestock and aquaculture
# include pressures associated with feed production. These feed pressures are
# therefore represented in the pressure-per-tonne values used here and do not
# need to be added separately.
