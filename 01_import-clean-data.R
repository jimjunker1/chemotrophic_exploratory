# 01_import-clean-data.R
load_packages = function(){
  library(tidyverse)
}
load_packages()

import_clean_data = function(){}
#import data files
prod_supp = read.csv(file = "wbt_prod-support.csv",T)
inv_stoic = read.csv(file = "wbt_invert-stoic.csv",T)
#rename some columns
prod_supp <- prod_supp %>% rename(perOM = X.OM)
inv_stoic <- inv_stoic %>% rename(DATE = Date, TAXA_ID = Taxa,
                                  CN = C.N, CP = C.P, NP = N.P)
#break prod_supp into resources and consumers
res_prod <- prod_supp %>% filter(TYPE != "invert") 
inv_prod<- prod_supp %>% filter(TYPE == "invert")
#need to standardize invert names across data frames
unique(droplevels(inv_prod$TAXA_ID))
unique(droplevels(inv_stoic$TAXA_ID))
df_list = list()

#create df of NP production of basal resources
#create perOM from AFDM
res_prod <- res_prod %>%
  mutate(perC = ifelse(is.na(perC), mean(perC, na.rm = T), perC),
         perN = ifelse(is.na(perN), mean(perN, na.rm = T), perN)) %>%
  mutate(PROD_mg_N_m_d = (PROD_mg_AFDM_m_d/perOM)*perN, 
         PROD_mg_P_m_d = (PROD_mg_AFDM_m_d/perOM)*perP) %>%
  group_by(DATE, TYPE) %>%
  summarise(PROD_mg_N = mean(PROD_mg_N_m_d, na.rm = T),
            PROD_mg_P = mean(PROD_mg_P_m_d, na.rm = T))
## quick vis of pools
ggplot(res_prod, aes(x = log10(PROD_mg_N), y = log10(PROD_mg_P), group = TYPE)) + 
  geom_point(aes(colour = TYPE), size = 4) +
  scale_colour_manual(values = c("dark green", "brown"))



