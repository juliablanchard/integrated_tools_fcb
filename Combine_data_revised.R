library(tidyverse)


### Step 1.  READ in halpern et al. 2022 data tables
# country and food specific pressures per tonne of production

pressure_per_tonne<-read.csv("pressure_per_tonne_data.csv")
  
### Step 2. match and aggregate country data into GLOBIOM regions

#get long country names to match iso3c
lookup_c<-read.csv("all.csv")
lookup_c<-lookup_c|> rename(iso3c=alpha.3)
pressure_per_tonne<-pressure_per_tonne |> left_join(lookup_c) |> rename(Country=name)
#now regions
lookup_c<-read.csv("lookup_countries.csv")
pressure_per_tonne<-pressure_per_tonne |> left_join(lookup_c) |> subset(!is.na(REGION))

# now add in the GLOBIOM Var ITEMS
pressure_per_tonne$Organism<-pressure_per_tonne$organism
lookup<-read.csv("lookup_table_full.csv")
#lookup<-subset(lookup, !is.na(REGION))

pressure_per_tonne<-pressure_per_tonne |> left_join(lookup)

# remove NAS in ITEM
pressure_per_tonne<-subset(pressure_per_tonne,!is.na(ITEM))

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


scenarios<-scenarios |> left_join(pressure_per_tonne,by=c("ITEM","REGION")) |> filter(!is.na(OUTPUT))
#merge and remove NAs
#calculate pressures in GLOBIOM scenarios
# CHECK WITH SCOTT: I don't think VLAUE is 1000T 
scen<- scenarios |> mutate(projected_production_tonnes=OUTPUT,total_pressure=OUTPUT*pressure_per_tonne) |>
  group_by(pressure,YEAR,ALLSCEN3) |>
  summarise(Total_projected_pressure=sum(total_pressure,na.rm=T))

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
scen$method<-"integrated"



scen_2050<- scen |> filter(YEAR== "2050" & !is.na(pressure))
p<-ggplot(scen_2050,aes(x=YEAR,y=Total_projected_pressure,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) + facet_wrap(~pressure,scales = "free_y")
p 




land<-c("CROP","ANIMAL")
scenarios_nomarine<-filter(scenarios,SYST%in%land)
scen_nomarine<- scenarios_nomarine |> mutate(projected_production_tonnes=OUTPUT,total_pressure=OUTPUT*pressure_per_tonne) |>
  group_by(pressure,YEAR,ALLSCEN3) |>
  summarise(Total_projected_pressure=sum(total_pressure,na.rm=T))
scen_nomarine$method<-"no_marine"

scen_nomarine_2050<-filter(scen_nomarine,YEAR== 2050 & !is.na(pressure))
scen_nomarine$method<-"no_marine"

p2<-ggplot(scen_nomarine_2050,aes(x=YEAR,y=Total_projected_pressure,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) + facet_wrap(~pressure,scales="free_y")
p2

### 

#relative to 2020 BAU
scen_2020<- scen |> filter(YEAR== "2020" & !is.na(pressure) & ALLSCEN3=="SCEN_DIET0_CULTURE0_CAPTURE0")
scen_2020<-rbind(scen_2020,scen_2020,scen_2020)

scen_2050<- scen_2050 |> arrange(ALLSCEN3)
scen_2050$Pressure_rel_change<-scen_2050$Total_projected_pressure/scen_2020$Total_projected_pressure

scen_nomarine_2020<- scen_nomarine |> filter(YEAR== "2020" & !is.na(pressure) & ALLSCEN3=="SCEN_DIET0_CULTURE0_CAPTURE0")
scen_nomarine_2020<-rbind(scen_nomarine_2020,scen_nomarine_2020,scen_nomarine_2020)

scen_nomarine_2050<- scen_nomarine_2050 |> arrange(ALLSCEN3)
scen_nomarine_2050$Pressure_rel_change<-scen_nomarine_2050$Total_projected_pressure/scen_nomarine_2020$Total_projected_pressure


#relative to BAU in 2050
scen_BAU<- scen_2050 |> filter(ALLSCEN3== "SCEN_DIET0_CULTURE0_CAPTURE0")
scen_BAU<-rbind(scen_BAU,scen_BAU,scen_BAU)

scen_2050<- scen_2050 |> arrange(ALLSCEN3)
scen_2050$Pressure_rel_BAU<-scen_2050$Total_projected_pressure/scen_BAU$Total_projected_pressure

scen_nomarine_BAU<- scen_nomarine_2050 |> filter(ALLSCEN3== "SCEN_DIET0_CULTURE0_CAPTURE0")
scen_nomarine_BAU<-rbind(scen_nomarine_BAU,scen_nomarine_BAU,scen_nomarine_BAU)

scen_nomarine_2050<- scen_nomarine_2050 |> arrange(ALLSCEN3)
scen_nomarine_2050$Pressure_rel_BAU<-scen_nomarine_2050$Total_projected_pressure/scen_nomarine_BAU$Total_projected_pressure

scen_join_2050<-rbind(scen_2050,scen_nomarine_2050)



# relative to BAU in 2020
# p2<-ggplot(filter(scen_join_2050, ALLSCEN3!="SCEN_DIET0_CULTURE0_CAPTURE0"),aes(x=method,y=Pressure_rel_change,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) + facet_wrap(~pressure,scales="free_y")
# p2

# overall differences from including marine food pressures.

p2<-ggplot(scen_join_2050,aes(x=method,y=Total_projected_pressure,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) + facet_wrap(~pressure,scales="free_y")
p2


# Relative to BAU in 2050

# There are (slight) differences in directions of change acorss scenarios

p2<-ggplot(filter(scen_join_2050, ALLSCEN3!="SCEN_DIET0_CULTURE0_CAPTURE0"),aes(x=method,y=Pressure_rel_BAU,color=ALLSCEN3)) + geom_point(position = position_dodge(0.8), width = 0.7) + facet_wrap(~pressure,scales="free_y") + theme_minimal()
p2


### despite pressures being higher (obv), shifts to blue foods still better for reducing pressures. 

#### not accounting for feed pressures....hw to add these in?

# scenrel_nomarine<-scen_nomarine[1:2,]
# scenrel_nomarine$deltaCPI<-scenrel_nomarine$Total_projected_cumpressure/scen_nomarine[3,]$Total_projected_cumpressure
# 
# scen_join_rel<-rbind(scenrel,scenrel_nomarine)
# p2<-ggplot(scen_join_rel,aes(x=YEAR,y= deltaCPI,fill=ALLSCEN3)) + geom_col(position = position_dodge(0.8), width = 0.7) + facet_wrap(~method)
# p2
