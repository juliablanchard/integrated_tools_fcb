# integrated_tools_fcb
Example integration of marine food production ecosystems with IAMs

## data files
### Scenario data : contains outputs from IAMs in both gdx and .RDS formats 
for converting gdx to .rds see "some helper codes and functions. R" file

## other data files, sources and context
1. all.csv - 
2. [text to be replaced w/ final file name ](data/halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM.xlsx)
3. [text to be replaced w/ final file name](data/halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM(Supplementary Data 1).csv)
4. [text to be replaced w/ final file name](data/halpern_etal_natsust_2022_41893_2022_965_MOESM2_ESM(Supplementary Data 3).csv)
5. lookup_item_foodsystem.csv - lookup table for matching food items to GLOBIOM item codes (source?)
6. lookup_countries.csv - lookup table for matching Halpern country codes to GLOBIOM regions (source?)
7. lookup_regions_countries.csv - lookup table for matching GLOBIOM regions to countries (source?)
8. looup_table.csv - use, source
9. pressure_per_tonne_data.csv- from Halpern et al 2022; disaggregated pressures per country

## Workflow
00_setup.R - includes all packages, etc.,
01_prepareandcombinedata.R - contains code to extract necessary data from different sources and merge pressures for different scenarios
02_explorescen.R (To be created)- contains code to compare scenarios via summary tables, plots, etc.,

## outputs
contains dataframes and plots made in 02_explorescen.R

