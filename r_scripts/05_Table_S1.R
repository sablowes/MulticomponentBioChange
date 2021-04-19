# code to produce Table S1: summary of component changes when the signs of abundance and evenness changes are different 
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/predicts_multi4_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/btx_multi4_global_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/spatial_multi4_results.Rdata')

# biotime
bt_study_summary %>% 
  mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)) %>%
  group_by(componentChange, deltaN_ENSPIE_sign) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  # put the zeroes in 
  complete(componentChange, deltaN_ENSPIE_sign,
           fill = list(n = 0)) #%>%
  # write.table(file = '~/Dropbox/1current/multidimensionalChangeMS/Tables/bt_table_S1.txt', sep = ',')


# btx: perturbed time series
btx_study_summary %>% 
  mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)) %>%
  group_by(componentChange, deltaN_ENSPIE_sign) %>% 
  summarise(n = n())  %>% 
  ungroup() %>% 
  # put the zeroes in 
  complete(componentChange, deltaN_ENSPIE_sign,
           fill = list(n = 0)) #%>%
  # write.table(file = '~/Dropbox/1current/multidimensionalChangeMS/Tables/btx_table_S1.txt', sep = ',')

# natural spatial gradients
spat_coefs_multi4_filtered %>% 
  mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)) %>% 
  group_by(componentChange, deltaN_ENSPIE_sign) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  # zeroes in
  complete(componentChange, deltaN_ENSPIE_sign,
           fill = list(n = 0)) #%>%
  # write.table(file = '~/Dropbox/1current/multidimensionalChangeMS/Tables/space_natural_table_S1.txt', sep = ',')

# PREDICTS
SS_LU_multi_filtered %>% 
  # only want relationships among departures from reference
  filter(LU!='Primary vegetation') %>% 
  mutate(deltaN_ENSPIE_sign = sign(N) * sign(S_PIE)) %>% 
  group_by(componentChange, deltaN_ENSPIE_sign) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  # zeroes in
  complete(componentChange, deltaN_ENSPIE_sign,
           fill = list(n = 0)) #%>%
  # write.table(file = '~/Dropbox/1current/multidimensionalChangeMS/Tables/space_lu_table_S1.txt', sep = ',')

