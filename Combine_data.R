### Step 1.  READ in halpern et al. 2022 data tables
# country and food specific pressures

pressure_per_tonne<-read.csv("pressure_per_tonne_data.csv")
  
pressures<-read.csv("halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM(Supplementary Data 1).csv")
#check all groups
pressures<-read.csv("halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM(Supplementary Data 1).csv")
unique(pressures$Organism)
# [1] "bana"                "barl"                "buffaloes"           "cass"                "chickens"            "cnut"               
# [7] "coco"                "cows"                "goats"               "maiz"                "ocer"                "oilp"               
# [13] "orts"                "pigs"                "plnt"                "pota"                "rice"                "sheep"              
# [19] "sorg"                "soyb"                "spis"                "sugb"                "sugc"                "swpo"               
# [25] "tnut"                "vege"                "whea"                "xfru"                "xmil"                "xoil"               
# [31] "xpul"                "yams"                "bivalve"             "crustaceans"         "marine-fish-general" "salmon"             
# [37] "shrimp"              "tuna"                "fish"                "benthic"             "demersal"            "fofm"               
# [43] "large-pelagic"       "medium-pelagic"      "reef"                "small-pelagic" 


# check marine catch
catch<-subset(pressures, System =="fisheries" & Origin =="marine") 
unique(catch$Organism)
# [1] "benthic"        "demersal"       "fofm"           "large-pelagic"  "medium-pelagic"
# [6] "reef"           "small-pelagic"

aqua_m<-subset(pressures, System ==c("aquaculture") & Origin =="marine") 
unique(aqua_m$Organism)
# [1] "bivalve"             "crustaceans"         "marine-fish-general" "salmon"             
# [5] "shrimp"              "tuna" 

mpressures<-subset(pressures, Origin =="marine") 
mpressures$Organism<-as.factor(mpressures$Organism)
# country and food specific production

production<-read.csv("halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM(Supplementary Data 3).csv")
unique(production$Product)
# subset marine products
#marine_products<-c("Benthic","Demersal","Forage fish", "Large-pelagic", "Medium-pelagic","Reef","Small-pelagic","Salmon","Marine-fish-general","Bivalve","Shrimp","Tuna", "Crustaceans")
# first seven are fisheries, followed by aquaculture
# deal with feed separaetly later on 

production <- production |> mutate(Product=str_to_lower(Product))

# Category is feedfofm?

# mproduction<-subset(production,Product%in%marine_products) 
# mproduction$Product<-as.factor(mproduction$Product)
# subset(mproduction,Product=="Salmon") # where farmed?
# subset(mproduction,Product=="Feed_salmon_meat") #where feed coming from; i think this is to be able to look at the fraction that is form feed?

##Not sure if add these two up by each type of fish not...
##Seems like yes as the salmon production is in countries that are salmon producers not crops etc.
## In paper says env efficiencies calculated separately for animal (on farm) and feed; these pressures would be summed.

# code from paper, for cumulative efficiencies here:
#https://github.com/OHI-Science/global_food_pressures/blob/main/_efficiency/STEP9_calculate_all_efficiencies.Rmd
# could redone for each pressure?

# currently does have cumulative per unit production, per country and food type.
# would need to have pressures by feed for each animal.

#What is the production of each organism in that country
#What is the production of the feed for that organism in country
#calculate feed related pressures associated with use of fofm (marine,land,freshwater)
#calculate farm related pressures (marine only)
#calculate fisheries related pressures, direct consumption


## aggregate country data into GLOBIOM regions
lookup_c<-read.csv("lookup_countries.csv")

production<-production |> left_join(lookup_c) |> subset(!is.na(REGION))


# now add in the GLOBIOM Var ITEMS
production$Organism<-production$Product
lookup<-read.csv("lookup_table_full.csv")
lookup<-subset(lookup, !is.na(Organism))

production<-production |> left_join(lookup)

# remove NAS in ITEM
production<-subset(production,!is.na(production$ITEM))

### Step 2.  READ in GLOBIOM output

scenarios<-readRDS("output 1.RDS")
## SCENARIOs TO COMPARE:
unique(scenarios$ALLSCEN3)
unique(scenarios$REGION)

# match commodities with pressure food types
# for each country & food, multiply pressure by production for each decade
# create a plot of
unique(scenarios$VAR_ID)


scenarios<-scenarios |> subset(VAR_ID=="PROD") |> subset(ALLSCEN3%in% c("SCEN_DIET0_CULTURE0_CAPTURE0","SCEN_DIET+10_CULTURE+50_CAPTURE+10","SCEN_DIET-10_CULTURE0_CAPTURE-10")  )

#SCEN_DIET0_CULTURE0_CAPTURE0 - BAU
#SCEN_DIET+10_CULTURE+50_CAPTURE+10 - Blue transformation
#SCEN_DIET-10_CULTURE0_CAPTURE-10 - Barriers to blue growth

# list foods
unique(scenarios$ITEM)

# [1] "BARL"            "CASS"            "CORN"            "COTT"            "GNUT"            "MILL"            "POTA"           
# [8] "RICE"            "SOYA"            "SRGH"            "SUGC"            "SUNF"            "SWPO"            "WHEA"           
# [15] "FSHO"            "RAPE_ML"         "SOYA_ML"         "BVMEAT"          "SGMEAT"          "PGMEAT"          "PTMEAT"         
# [22] "BVMILK"          "ALMILK"          "PTEGGS"          "CHEMPULP"        "MECHPULP"        "FRSHF"           "SALMF"          
# [29] "MARNF"           "CEPHF"           "CRSTF"           "DMRSF"           "MLSCF"           "PELGF"           "SHRIF"          
# [36] "TUNAF"           "FRSHW"           "SALMW"           "MARNW"           "CEPHW"           "CRSTW"           "DMRSW"          
# [43] "MLSCW"           "PELGW"           "SHRIW"           "TUNAW"           "FSHM"            "BEAD"            "CHKP"           
# [50] "RAPE"            "OPAL"            "SGMILK"          "CORN_DG"         "WHEA_DG"         "SW_BIOMASS"      "PW_BIOMASS"     
# [57] "OW_BIOMASS"      "FW_BIOMASS"      "EW_BIOMASS"      "SAWNWOOD"        "PLYWOOD"         "FIBERBOARD"      "WOODPELLETS"    
# [64] "BARK"            "SAWDUST"         "WOODCHIPS"       "BLACKLIQUOR"     "IP_BIOMASS"      "RECYCLEDWOOD"    "LOGGINGRESIDUES"
# [71] "FAME"            "BIOF"            "W_ETHANOL"       "W_HEAT"          "W_ELECT"         "W_GAS"           "C_ETHANOL"

# here compare cumulative pressure per unit production


scenarios<-scenarios |> right_join(production,by=c("ITEM","REGION")) |> filter(!is.na(OUTPUT))
#merge and remove NAs
#calculate pressures in GLOBIOM scenarios
# CHECK WITH SCOTT: I don't think VLAUE is 1000T 
scen<- scenarios |> mutate(projected_production_tonnes=OUTPUT,projected_cumpressure=OUTPUT*Pressure.per.tonne) |>
  group_by(REGION,YEAR,ALLSCEN3) |>
  summarise(Total_projected_cumpressure=sum(projected_cumpressure,na.rm=T))

# scenarios_bau<-subset(scenarios,ALLSCEN3=="SCEN_DIET0_CULTURE0_CAPTURE0")
# scenarios_blue<-subset(scenarios,ALLSCEN3=="SCEN_DIET+10_CULTURE+50_CAPTURE+10")
# scenarios_yellow<-subset(scenarios,ALLSCEN3=="SCEN_DIET-10_CULTURE0_CAPTURE-10")
# 
# scenarios_blue$BAU_OUTPUT<-scenarios_bau$OUTPUT
# scenarios_yellow$BAU_OUTPUT<-scenarios_bau$OUTPUT
# 
# scenarios_blue$relCPI<-(scenarios_blue$Total_projected_cumpressure-scenarios_bau$Total_projected_cumpressure)/scenarios_bau$Total_projected_cumpressure
# scenarios_yellow$relCPI<-(scenarios_green$Total_projected_cumpressure-scenarios_bau$Total_projected_cumpressure)/scenarios_bau$Total_projected_cumpressure
#scen<-rbind(scenarios_blue,scenarios_green)

scen<-filter(scen,YEAR== 2050)
scen$method<-"integrated"
p<-ggplot(scen,aes(x=YEAR,y=Total_projected_cumpressure,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) 
p 
scenrel<-scen[1:2,]
scenrel$deltaCPI<-scenrel$Total_projected_cumpressure/scen[3,]$Total_projected_cumpressure



land<-c("CROP","ANIMAL")
scenarios_nomarine<-filter(scenarios,SYST%in%land)
scen_nomarine<- scenarios_nomarine|> mutate(projected_production_tonnes=OUTPUT,projected_cumpressure=OUTPUT*Pressure.per.tonne) |>
  group_by(REGION,YEAR,ALLSCEN3) |>
  summarise(Total_projected_cumpressure=sum(projected_cumpressure,na.rm=T))



scen_nomarine<-filter(scen_nomarine,YEAR== 2050)
scen_nomarine$method<-"no_marine"
scen_join<-rbind(scen,scen_nomarine)
p2<-ggplot(scen_join,aes(x=YEAR,y=Total_projected_cumpressure,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) + facet_wrap(~method)
p2


#### not accounting for feed pressures....hw to add these in?

scenrel_nomarine<-scen_nomarine[1:2,]
scenrel_nomarine$deltaCPI<-scenrel_nomarine$Total_projected_cumpressure/scen_nomarine[3,]$Total_projected_cumpressure

scen_join_rel<-rbind(scenrel,scenrel_nomarine)
p2<-ggplot(scen_join_rel,aes(x=YEAR,y= deltaCPI,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) + facet_wrap(~method)
p2


###### APPROACH 2.

# mpressures<-mpressures |> group_by(Organism,Pressure) |>
#   summarise(Total_Pressure=sum(Value,na.rm=T))
# 
# lookup<-read.csv("lookup_table_full.csv")
# 
# scen<-scenarios |> left_join(lookup) |> mutate(Country=str_to_title(ALLCOUNTRY))|>
# group_by(ALLSCENYEAR,ALLSCEN3,Organism,System) |>
#   summarise(Total_Prod=sum(VALUE,na.rm=T))
# 
# scenarios_bau<-subset(scen,ALLSCEN3=="SCEN_DIET0_CULTURE0_CAPTURE0")
# scenarios_blue<-subset(scen,ALLSCEN3=="SCEN_DIET-10_CULTURE+50_CAPTURE+10")
# scenarios_green<-subset(scen,ALLSCEN3=="SCEN_DIET+10_CULTURE0_CAPTURE-10") 
# 
# scenarios_blue$ProdChange<-(scenarios_blue$Total_Prod-scenarios_bau$Total_Prod)/scenarios_bau$Total_Prod
# scenarios_green$ProdChange<-(scenarios_green$Total_Prod-scenarios_bau$Total_Prod)/scenarios_bau$Total_Prod
# 
# # merge and remove NAs
# scen2<-scen |> left_join(mpressures) |> filter(!is.na(Total_Prod)) |> pivot_wider(ALLSCEN3)
# scen2<-rbind(scenarios_blue,scenarios_green)
# 
