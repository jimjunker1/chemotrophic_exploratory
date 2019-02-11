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

# work to create df of invert production

#need to standardize invert names across data frames
unique(droplevels(inv_prod$TAXA_ID))
unique(droplevels(inv_stoic$TAXA_ID))
#fix/compress stoic names
old_names = list(c("Zapada (nem)","Zapada(nem)","Zapada"),
                 c("Elmidae(ad)","elmidae(ad)"),
                 c("Rhyac","rhyac","Rhyac1","Rhyac2","Rhyac3"),
                 c("Limn","limn"),
                 c("Baetis","Baetis1","Baetis2"),
                 c("Suwallia","Suwallia1","Suwallia2"),
                 c("Doddsi","Drunella1","Drunella","Drunella2"),
                 c("Hep","hep","Hep2","Hep3"),
                 c("Oligo","oligo","Oligo2"),
                 c("chironomidae"),
                 c("megaloptera"),
                 c("leptophilidae"))

new_names = list("Zapada",
                 "Elmidae(A)",
                 "Rhyacophilidae",
                 "Limnophilidae",
                 "Baetidae",
                 "Suwallia",
                 "Drunella sp.",
                 "Heptageniidae",
                 "Oligochaeta",
                 "Chironomidae",
                 "Megaloptera",
                 "Leptophlebiidae")

stoic_keyval = setNames(rep(new_names, lengths(old_names)), unlist(old_names))

inv_stoic = inv_stoic %>%
  mutate(TAXA_ID = recode(TAXA_ID, !!!stoic_keyval))

mean_stoic = inv_stoic %>%
  summarise(CN = mean(CN), CP = mean(CP), NP = mean(NP))

#check new names of stoic and prod
#need to standardize invert names across data frames
unique(droplevels(inv_prod$TAXA_ID))
unique(droplevels(inv_stoic$TAXA_ID))

##
inv_prod$stoic_taxa = NA

prod_names = list("Doddsi","Cynigmula","Zapada","Suwallia","Atenella","Acentrella",
                  "Lepto","Epeorus","Elmidae","Sweltza","Gloss","Perlodid","Serratella",
                  "Ameletidae","Brachy","Tipulidae","Rhyac","Pericoma","Grandis","Chiron",
                  "Rhithrogena","Baetis","Barbaetis","Hydro","Classenia","Elmidae (A)",
                  "Limn")

stoic_names = list("Drunella","Serratella","Zapada","Suwallia","Ephemerella","Ephemeralla",
                   "Leptophlebiidae","Epeorus","Elmidae","Suwallia","Limnophilidae","Suwallia","Ephemerella",
                   "Baetis","Limnophilidae","Tipulidae","Rhyacophilidae","Elmidae","Drunella","Chironomidae",
                   "Rhithrogena","Baetis","Baetis","Hydropsyche","Suwallia","Elmidae(A)",
                   "Limnophilidae")

keyval = setNames(as.list(stoic_names), unlist(prod_names))

inv_prod = inv_prod %>%
  mutate(stoic_taxa = recode(TAXA_ID, !!!keyval)) %>%
  select(-c(perC:perP, CN:NP))

inv_prod = inv_prod %>%
  left_join(inv_stoic %>% rename(stoic_taxa = TAXA_ID))

inv_full = inv_prod %>%
  filter(!is.na(CN))
inv_na = inv_prod %>%
  filter(is.na(CN))

inv_taxa_stoic = inv_stoic %>% 
  group_by(DATE) %>% 
  summarise(CN = mean(CN), CP = mean(CP), NP = mean(NP))

inv_na = inv_na %>% select(-c(CN:NP)) %>%
  left_join(inv_taxa_stoic %>% rename(stoic_taxa = TAXA_ID))

inv_prod = bind_rows(inv_full,inv_na) %>%
  mutate(CN = ifelse(is.na(CN), mean_stoic$CN, CN),
         CP = ifelse(is.na(CP), mean_stoic$CP, CP),
         NP = ifelse(is.na(NP), mean_stoic$NP, NP)) %>%
  mutate(PROD_mg_N = ((PROD_mg_AFDM_m_d/perOM) * 0.5)/CN,
         PROD_mg_P = ((PROD_mg_AFDM_m_d/perOM) * 0.5)/CP) %>%
  select(-c(notes,stoic_taxa, perOM, CN:NP))

#create df of NP production of basal resources
#create perOM from AFDM
res_prod <- res_prod %>%
  mutate(perC = ifelse(is.na(perC), mean(perC, na.rm = T), perC),
         perN = ifelse(is.na(perN), mean(perN, na.rm = T), perN),
         perP = ifelse(is.na(perP), mean(perP, na.rm = T), perP)) %>%
  mutate(PROD_mg_N_m_d = (PROD_mg_AFDM_m_d/perOM)*perN, 
         PROD_mg_P_m_d = (PROD_mg_AFDM_m_d/perOM)*perP) %>%
  group_by(DATE, TYPE) %>%
  summarise(allo_sup = mean(allo_sup),
            auto_sup = mean(auto_sup),
            allo_consum = mean(allo_consum),
            auto_consum = mean(auto_consum),
            PROD_mg_AFDM = mean(PROD_mg_AFDM_m_d),
            PROD_mg_N = mean(PROD_mg_N_m_d, na.rm = T),
            PROD_mg_P = mean(PROD_mg_P_m_d, na.rm = T))

full_prod = bind_rows(res_prod, inv_prod %>% select(-TAXA_ID))
## quick vis of pools


ggplot(full_prod, aes(x = log10(PROD_mg_N), y = log10(PROD_mg_P), group = TYPE)) + 
  geom_point(aes(colour = TYPE), size = 3) +
  scale_colour_manual(values = c("dark green", "red", "brown")) +
  theme(panel.background = element_blank()) + facet_wrap(~DATE)

