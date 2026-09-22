# Load required libraries
library(dplyr)
library(ggplot2)
library(stringr)

### Step 1.  READ in halpern et al. 2022 data tables
# country and food specific pressures
pressures <- read.csv("halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM(Supplementary Data 1).csv")
#check all groups
unique(pressures$Organism)
# [1] "bana"                "barl"                "buffaloes"           "cass"                "chickens"            "cnut"               
# [7] "coco"                "cows"                "goats"               "maiz"                "ocer"                "oilp"               
# [13] "orts"                "pigs"                "plnt"                "pota"                "rice"                "sheep"              
# [19] "sorg"                "soyb"                "spis"                "sugb"                "sugc"                "swpo"               
# [25] "tnut"                "vege"                "whea"                "xfru"                "xmil"                "xoil"               
# [31] "xpul"                "yams"                "bivalve"             "crustaceans"         "marine-fish-general" "salmon"             
# [37] "shrimp"              "tuna"                "fish"                "benthic"             "demersal"            "fofm"               
# [43] "large-pelagic"       "medium-pelagic"      "reef"                "small-pelagic" 


production <- read.csv("halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM(Supplementary Data 3).csv")
unique(production$Product)
# subset marine products
#marine_products <- c("Benthic", "Demersal", "Forage fish", "Large-pelagic", "Medium-pelagic", "Reef", "Small-pelagic", "Salmon", "Marine-fish-general", "Bivalve", "Shrimp", "Tuna", "Crustaceans")
# first seven are fisheries, followed by aquaculture
# deal with feed separately later on 

production <- production |> mutate(Product = str_to_lower(Product))

# Category is feedfofm?

# mproduction <- subset(production, Product %in% marine_products) 
# mproduction$Product <- as.factor(mproduction$Product)
# subset(mproduction, Product=="Salmon") # where farmed?
# subset(mproduction, Product=="Feed_salmon_meat") #where feed coming from; i think this is to be able to look at the fraction that is from feed?
## Not sure if add these two up by each type of fish not...
## Seems like yes as the salmon production is in countries that are salmon producers not crops etc.
## In paper says env efficiencies calculated separately for animal (on farm) and feed; these pressures would be summed.

# code from paper, for cumulative efficiencies here:
# https://github.com/OHI-Science/global_food_pressures/blob/main/_efficiency/STEP9_calculate_all_efficiencies.Rmd
# could redo for each pressure?

# currently does have cumulative per unit production, per country and food type.
# would need to have pressures by feed for each animal.

# What is the production of each organism in that country
# What is the production of the feed for that organism in country
# calculate feed related pressures associated with use of fofm (marine, land, freshwater)
# calculate farm related pressures (marine only)
# calculate fisheries related pressures, direct consumption

## aggregate country data into GLOBIOM regions
lookup_c <- read.csv("lookup_countries.csv")

production <- production |> 
  left_join(lookup_c) |> 
  subset(!is.na(REGION))

# now add in the GLOBIOM Var ITEMS
production$Organism <- production$Product
lookup <- read.csv("lookup_table_full.csv")
lookup <- subset(lookup, !is.na(Organism))

production <- production |> left_join(lookup)
# remove NAs in ITEM
production <- subset(production, !is.na(production$ITEM))

### Step 2.  READ in GLOBIOM output

scenarios <- readRDS("output 1.RDS")
## SCENARIOs TO COMPARE:
unique(scenarios$ALLSCEN3)
unique(scenarios$REGION)

# match commodities with pressure food types
# for each country & food, multiply pressure by production for each decade
# create a plot of
unique(scenarios$VAR_ID)

scenarios <- scenarios |> 
  subset(VAR_ID == "PROD") |> 
  subset(ALLSCEN3 %in% c("SCEN_DIET0_CULTURE0_CAPTURE0", "SCEN_DIET+10_CULTURE+50_CAPTURE+10", "SCEN_DIET-10_CULTURE0_CAPTURE-10"))

# SCEN_DIET0_CULTURE0_CAPTURE0 - BAU
# SCEN_DIET+10_CULTURE+50_CAPTURE+10 - Blue transformation
# SCEN_DIET-10_CULTURE0_CAPTURE-10 - Barriers to blue growth

# list foods
unique(scenarios$ITEM)

# here compare cumulative pressure per unit production

scenarios <- scenarios |> 
  right_join(production, by = c("ITEM", "REGION")) |> 
  filter(!is.na(OUTPUT))

# merge and remove NAs
# calculate pressures in GLOBIOM scenarios
# CHECK WITH SCOTT: I don't think VALUE is 1000T 
scen <- scenarios |> 
  mutate(
    projected_production_tonnes = OUTPUT,
    projected_cumpressure = OUTPUT * Pressure.per.tonne
  ) |>
  group_by(REGION, YEAR, ALLSCEN3) |>
  summarise(Total_projected_cumpressure = sum(projected_cumpressure, na.rm = TRUE), .groups = "drop")

# scenarios_bau <- subset(scenarios, ALLSCEN3 == "SCEN_DIET0_CULTURE0_CAPTURE0")
# scenarios_blue <- subset(scenarios, ALLSCEN3 == "SCEN_DIET+10_CULTURE+50_CAPTURE+10")
# scenarios_yellow <- subset(scenarios, ALLSCEN3 == "SCEN_DIET-10_CULTURE0_CAPTURE-10")
# 
# scenarios_blue$BAU_OUTPUT <- scenarios_bau$OUTPUT
# scenarios_yellow$BAU_OUTPUT <- scenarios_bau$OUTPUT
# 
# scenarios_blue$relCPI <- (scenarios_blue$Total_projected_cumpressure - scenarios_bau$Total_projected_cumpressure) / scenarios_bau$Total_projected_cumpressure
# scenarios_yellow$relCPI <- (scenarios_green$Total_projected_cumpressure - scenarios_bau$Total_projected_cumpressure) / scenarios_bau$Total_projected_cumpressure
#scen <- rbind(scenarios_blue, scenarios_green)

scen <- filter(scen, YEAR == 2050)
scen$method <- "integrated"
p <- ggplot(scen, aes(x = YEAR, y = Total_projected_cumpressure, fill = ALLSCEN3)) + 
  geom_col(position = position_dodge(0.8), width = 0.7) 
print(p)

scenrel <- scen[1:2, ]
scenrel$deltaCPI <- scenrel$Total_projected_cumpressure / scen[3, ]$Total_projected_cumpressure

land <- c("CROP", "ANIMAL")
scenarios_nomarine <- filter(scenarios, SYST %in% land)
scen_nomarine <- scenarios_nomarine |> 
  mutate(
    projected_production_tonnes = OUTPUT,
    projected_cumpressure = OUTPUT * Pressure.per.tonne
  ) |>
  group_by(REGION, YEAR, ALLSCEN3) |>
  summarise(Total_projected_cumpressure = sum(projected_cumpressure, na.rm = TRUE), .groups = "drop")

scen_nomarine <- filter(scen_nomarine, YEAR == 2050)
scen_nomarine$method <- "no_marine"
scen_join <- rbind(scen, scen_nomarine)
p2 <- ggplot(scen_join, aes(x = YEAR, y = Total_projected_cumpressure, fill = ALLSCEN3)) + 
  geom_col(position = position_dodge(0.8), width = 0.7) + 
  facet_wrap(~method)
print(p2)

#### not accounting for feed pressures....how to add these in?

scenrel_nomarine <- scen_nomarine[1:2, ]
scenrel_nomarine$deltaCPI <- scenrel_nomarine$Total_projected_cumpressure / scen_nomarine[3, ]$Total_projected_cumpressure

scen_join_rel <- rbind(scenrel, scenrel_nomarine)
p2 <- ggplot(scen_join_rel, aes(x = YEAR, y = deltaCPI, fill = ALLSCEN3)) + 
  geom_col(position = position_dodge(0.8), width = 0.7) + 
  facet_wrap(~method)
print(p2)


# Improved publication-quality plot for deltaCPI (relative cumulative pressure)

# Relabel scenario names for clarity
scenario_labels <- c(
  "SCEN_DIET0_CULTURE0_CAPTURE0" = "BAU",
  "SCEN_DIET+10_CULTURE+50_CAPTURE+10" = "Blue transformation",
  "SCEN_DIET-10_CULTURE0_CAPTURE-10" = "Barriers to blue growth"
)

scen_join_rel$Scenario <- factor(scen_join_rel$ALLSCEN3, levels = names(scenario_labels), labels = scenario_labels)

p2_delta_pub <- ggplot(scen_join_rel, aes(x = as.factor(YEAR), y = deltaCPI, fill = Scenario)) +
  geom_col(position = position_dodge(0.8), width = 0.7, color = "black", alpha = 0.95) +
  facet_wrap(~method, labeller = as_labeller(c("integrated" = "Integrated system", "no_marine" = "No marine"))) +
  scale_fill_brewer(palette = "Dark2", name = "Scenario") +
  labs(
    title = "Relative Cumulative Pressure (deltaCPI) by Scenario (2050)",
    subtitle = "Comparison to baseline (BAU) for integrated and no-marine systems",
    x = "Year",
    y = "Relative Cumulative Pressure (deltaCPI)",
    caption = "Data: Halpern et al. 2022, GLOBIOM output"
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15, color = "black"),
    axis.title = element_text(size = 17),
    axis.ticks = element_line(size = 0.8),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(size = 17, face = "bold"),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey80")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

print(p2_delta_pub)

# Save high-resolution version for publication
ggsave("relative_cumulative_pressure_plot_pub.png", p2_delta_pub, width = 11, height = 8, dpi = 400)

###### APPROACH 2.

# mpressures <- mpressures |> group_by(Organism, Pressure) |>
#   summarise(Total_Pressure = sum(Value, na.rm = TRUE))
# 
# lookup <- read.csv("lookup_table_full.csv")
# 
# scen <- scenarios |> left_join(lookup) |> mutate(Country = str_to_title(ALLCOUNTRY)) |>
#   group_by(ALLSCENYEAR, ALLSCEN3, Organism, System) |>
#   summarise(Total_Prod = sum(VALUE, na.rm = TRUE))
# 
# scenarios_bau <- subset(scen, ALLSCEN3 == "SCEN_DIET0_CULTURE0_CAPTURE0")
# scenarios_blue <- subset(scen, ALLSCEN3 == "SCEN_DIET-10_CULTURE+50_CAPTURE+10")
# scenarios_green <- subset(scen, ALLSCEN3 == "SCEN_DIET+10_CULTURE0_CAPTURE-10") 
# 
# scenarios_blue$ProdChange <- (scenarios_blue$Total_Prod - scenarios_bau$Total_Prod) / scenarios_bau$Total_Prod
# scenarios_green$ProdChange <- (scenarios_green$Total_Prod - scenarios_bau$Total_Prod) / scenarios_bau$Total_Prod
# 
# # merge and remove NAs
# scen2 <- scen |> left_join(mpressures) |> filter(!is.na(Total_Prod)) |> pivot_wider(ALLSCEN3)
# scen2 <- rbind(scenarios_blue, scenarios_green)


# Check unique values and summary stats per scenario
scen %>%
  group_by(ALLSCEN3) %>%
  summarise(
    min_pressure = min(Total_projected_cumpressure, na.rm = TRUE),
    max_pressure = max(Total_projected_cumpressure, na.rm = TRUE),
    mean_pressure = mean(Total_projected_cumpressure, na.rm = TRUE),
    n = n()
  )


# Check unique values and summary stats per scenario
scen %>%
  group_by(ALLSCEN3) %>%
  summarise(
    min_pressure = min(Total_projected_cumpressure, na.rm = TRUE),
    max_pressure = max(Total_projected_cumpressure, na.rm = TRUE),
    mean_pressure = mean(Total_projected_cumpressure, na.rm = TRUE),
    n = n()
  )

# Check if Pressure.per.tonne varies by scenario and item
scenarios %>%
  group_by(ALLSCEN3, ITEM) %>%
  summarise(
    mean_pressure = mean(Pressure.per.tonne, na.rm = TRUE),
    mean_output = mean(OUTPUT, na.rm = TRUE),
    n = n()
  ) %>% print(n = 100)

