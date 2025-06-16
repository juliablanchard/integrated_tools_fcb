# demquantity <- readRDS(here("_Analysis/dem_compare.RDS"))
# 
# demquantity %>% 
#   filter(REGION == "USAREG") %>% 
#   filter(YEAR == 2050) %>% 
#   filter(PRODUCT %in% c("TUNAF","SALMF","CORN","BVMEAT")) %>% 
#   ggplot() +
#   geom_col(aes(x = ALLSCEN3, y = DEMQUANTITY_COMPARE, fill = PRODUCT),
#            position = "stack") +
#   theme_classic()


  glob.plot <- function(glob_df, 
                        var_id, 
                        colors_plot,
                        scen_baseline = "SCEN_BAU", 
                        year = max(as.numeric(glob_df$YEAR)),
                        title = "Insert Title Here",
                        save = TRUE){

    
       df_dat <- glob_df %>% ####### use 'globiom' or 'land_comp'
  filter(YEAR == year) %>% 
  filter(VAR_ID == var_id) %>% 
      rename("ITEM" = contains("ITEM"),
             "OUTPUT" = contains("OUTPUT"))  %>% 
      {
        if(var_id == "EMIS"){
          filter(.,ITEM %in% c("AFFR","LUC","LSP","CRP"))
        } else {
          if(var_id == "WATR" | str_detect(var_id, "FRT")){
           # mutate(., ITEM = case_when(ITEM == "CER" ~ "Cereals",
           #                            ITEM == "OCR" ~ "Other Crops",
           #                            ITEM == "OSD" ~ "Oilseeds",
           #                            ITEM == "SGC" ~ "Sugarcane",
           #                            TRUE ~ ""))
            mutate(., ITEM = var_id)
          } else {.}
          }
      } %>%  
      filter(REGION != "World") %>% 
    filter(ITEM != "")
    
  df_dat_ <- df_dat %>% group_by(ALLSCEN3,ITEM) %>% 
  summarise(OUTPUT = sum(OUTPUT)) %>% 
  pivot_wider(names_from = ALLSCEN3, values_from = OUTPUT) %>% 
  mutate(across(.cols = starts_with("SCEN"), .fns = list(
    delta_Baseline = function(x) {(x-!!as.symbol(scen_baseline))}),
    .names = "{.fn}.{.col}" )) %>%
  dplyr::select(!starts_with("SCEN")) %>%
  pivot_longer(cols = starts_with("delta_Baseline"), values_to = "delta_Baseline", names_to = "SCEN") %>%
  mutate(SCEN = str_remove(SCEN, "delta_Baseline.") ) %>%
  rename("OUTPUT"="delta_Baseline") %>% 
  filter(SCEN != scen_baseline) %>% 
    left_join(scen_pub_names %>% rename("SCEN" = "ALLSCEN3")) %>% 
    mutate(SCEN = PUBSCEN) %>% 
  filter(OUTPUT != 0) %>% 
  mutate(ITEM = factor(ITEM, levels = unique(ITEM))) %>% 
    mutate(SCEN = factor(SCEN, levels = unique(SCEN) %>% rev))

  unit <- df_dat$VAR_UNIT %>% unique()
  
 print(df_dat_$ITEM %>% unique)
 
pick_col = colors_plot[1:length(df_dat_$ITEM %>% unique)] %>% 
  setNames(df_dat_$ITEM %>% unique)#%>% 

pal = scale_fill_manual(name = paste0(var_id," Type"),
                        values = pick_col, 
                        guide = guide_legend(reverse = FALSE))

print(df_dat_, n=Inf)

plot_f <- ggplot(df_dat_) +
    geom_col(aes(x= SCEN,
                 y= OUTPUT, 
                 fill = ITEM), 
             position = "stack") +
    pal +
    theme_classic() +
    ylab(unit) +
    xlab(NULL) +
    coord_flip() +
    theme(legend.position = "right"
    )+
    geom_hline(yintercept = 0) +
  ggtitle(paste0(title, year))

if(save){message("Saving here: ", here(paste0("Plots/",var_id,"_",year,".png")) )
ggsave(here(paste0("Plots/",var_id,"_",year,".png")), dpi = 300, width = 8, height = 6, device = "png")
}
return(plot_f)
  }
  

# 
# bio_dat <- globiom_ag %>% ####### use 'globiom' or 'bio_comp'
#   filter(YEAR == 2050) %>% 
#   filter(VAR_ID == "ABII",
#          REGION_AG == "World") %>% 
#   #filter(VAR_UNIT == j) %>% 
#   # group_by(ITEM_AG,SW_Scen) %>% 
#   #   summarise(OUTPUT_AG = mean(OUTPUT_AG, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = SW_Scen, values_from = OUTPUT_AG) %>% 
#   mutate(across(.cols = starts_with("SW_"), .fns = list(
#     delta_Baseline = function(x) {(x-SW_Base) # /SW_Base
#     }),
#     .names = "{.fn}.{.col}" )) %>% 
#   dplyr::select(!starts_with("SW_")) %>% 
#   pivot_longer(cols = starts_with("delta_Baseline"), values_to = "delta_Baseline", names_to = "SW_Scen") %>%
#   mutate(SW_Scen = str_remove(SW_Scen, "delta_Baseline.") ) %>%
#   rename("OUTPUT_AG"="delta_Baseline") %>% 
#   filter(SW_Scen != "SW_Base") %>% 
#   #  filter(!(ITEM_AG %in% c("OTHLAND","OTHAGRI","PLANTATION") )) %>% 
#   pub_scen() %>% 
#   filter((SW_Scen %in% sw_do)) 
# # mutate(SW_Scen = factor(SW_Scen, levels = sw_do#names(scen_col)
# #                           )) 
# 
# bio_col = beyonce_palette(39)[c(1)] %>% setNames(bio_dat$ITEM_AG %>% unique)
# 
# 
# #%>% 
# # mutate(ITEM_AG =  factor(ITEM_AG, levels = c("Forest","Other Vegetation","Grassland","Cropland")))
# bio_pal = scale_fill_manual(name = "BII Indicator",values = bio_col, guide = guide_legend(reverse = FALSE)  )
# 
# ( bio_plots <-  ggplot(bio_dat) +
#     geom_col(aes(x= SW_Scen,
#                  y= OUTPUT_AG, 
#                  fill = ITEM_AG), 
#              position = "stack") +
#     bio_pal +
#     # facet_wrap(~REGION_AG,scales = "free" ) +
#     theme_classic() +
#     output_legend +
#     #coord_flip() +
#     #facet_grid(~VAR_ID,  scales = "free") +
#     ylab(paste0(ifelse(i == 1,"BII (Species Presence)","BII / MJ"))) + # in ",
#     #  str_replace_all(,"_","/") %>% 
#     #   str_replace_all("-"," "))) +
#     xlab(NULL) +
#     geom_hline(yintercept = 0) +
#     theme(legend.position = "none") +
#     coord_flip() +
#     ggtitle("A-5. Biodiversity Conserved") )