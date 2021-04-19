# code to wrangle the PREDICTs data

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

# these data are available here: https://data.nhm.ac.uk/dataset/the-2016-release-of-the-predicts-database
# data column descriptions
meta <- read_csv('~/Dropbox/1current/PREDICTS/data/resource.csv')
# the data (downloaded Fri 15 May 2020)
predicts <- readRDS('~/Dropbox/1current/PREDICTS/data/database.rds')

maybes <- predicts %>%
  filter(Diversity_metric_type=='Abundance' & Diversity_metric_unit=='individuals') %>%
  group_by(Source_ID, Realm, Biome, Higher_taxon, Sampling_method, Sampling_effort_unit) %>%
  summarise(
    N_study = max(Study_number),
    N_sites = max(Site_number),
    N_landUses = n_distinct(Predominant_land_use),
    n_intensity = n_distinct(Use_intensity)) %>% 
  filter(N_landUses > 1)

multi_LU <- predicts %>% 
  filter(Source_ID %in% unique(maybes$Source_ID))


multi_LU <- multi_LU %>% 
  filter(Measurement>0) %>% 
  # remove observations with no land use code (and the ambiguous secondary vegetation category)
  filter(Predominant_land_use!='Cannot decide') %>%
  filter(Predominant_land_use!='Secondary vegetation (indeterminate age)') %>% 
  mutate(LU = Predominant_land_use)


#- want to calculate MoB metrics for each LU category
alpha <- multi_LU %>% 
  # following the model in Newbold et al 2015 Nature (see supplementary material)
  group_by(SS, SSB,  SSBS, LU) %>% 
  summarise(S = n_distinct(Taxon_name_entered), 
            N_raw = sum(Measurement),
            N_corrected = sum(Effort_corrected_measurement),
            S_PIE = mobr::calc_PIE(Effort_corrected_measurement, ENS = T),
            ENSPIE = vegan::diversity(Effort_corrected_measurement, index = 'invsimpson'),
            singletons = sum(Measurement==1),
            doubletons = sum(Measurement==2), 
            # calculate abundance-based coverage, following Chao et al 2014 Ecol Monogr
            # for communities with no doubletons (this prevents NaN results when singletons==0 & doubletons==0)
            # (from code associated with Chao et al 2014 - see appendix)
            Chat_corrected = ifelse(doubletons>0, 
                                    1 - (singletons/N_raw) * (((N_raw-1)*singletons)/((N_raw-1)*singletons + 2*doubletons)), 
                                    1 - (singletons/N_raw) * (((N_raw-1)*singletons)/((N_raw-1)*singletons + 2)))) %>% 
  ungroup() 

# get N for rarefied richness
minN <- alpha %>% 
  # find min abundance for each study (within data sources)
  group_by(SS) %>% 
  summarise(minN = min(N_raw)) 

Sn_alpha <- multi_LU %>% 
  left_join(minN,
            by = c('SS')) %>% 
  select(SS, SSB, SSBS, LU, minN, Taxon_name_entered, Measurement) %>% 
  # remove studies with minN <= 5
  filter(minN > 5) %>% 
  group_by(SS, SSB, SSBS, LU) %>% 
  nest(data = c(Taxon_name_entered, Measurement, minN)) %>% 
  mutate(Sn_5 = purrr::map(data, ~mobr::rarefaction(.x$Measurement, method = 'indiv', effort = unique(.x$minN)) %>% 
                             as.numeric()))

alpha <- left_join(alpha, 
                   Sn_alpha %>% 
                     unnest(cols = Sn_5) %>% 
                     select(-data))


alpha$LU <- factor(alpha$LU,
                   levels = c('Primary vegetation',
                              'Mature secondary vegetation',      
                              'Intermediate secondary vegetation',
                              'Young secondary vegetation',
                              'Plantation forest',                
                              'Cropland',
                              'Pasture',
                              'Urban'),
                   labels = c('Reference',
                              'Mature_SecVeg',      
                              'Intermediate_SecVeg',
                              'Young_SecVeg',
                              'Plantation',                
                              'Cropland',
                              'Pasture',
                              'Urban')
)

save(alpha, file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/PREDICTS_alpha.Rdata')

# put sourceID into df of model results (to get # of 'studies'): 180
left_join(SS_LU_multi_filtered, multi_LU %>% distinct(Source_ID, SS)) %>% 
  distinct(Source_ID)
