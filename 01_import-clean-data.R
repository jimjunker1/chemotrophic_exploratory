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
#need to standardize names across data frames
unique(droplevels(inv_prod$TAXA_ID))
unique(droplevels(inv_stoic$TAXA_ID))
