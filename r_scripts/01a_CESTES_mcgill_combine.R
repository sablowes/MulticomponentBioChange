source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

## the data to be combined
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/cestes10.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/mcgill_data_for_analysis.Rdata')

mcgill_sad <- mcgill_sad %>% 
  # remove one data set with eutrophication gradient that slipped through initial screening
  filter(dataset_id!='m27')

# combine cestes and mcgill
spatial_grad <- bind_rows(cestes_10 %>% 
                            select(S, N, Sn, ENSPIE,
                                   dataset_id, newID) %>% 
                            mutate(newID = paste0('s_', newID),
                                   data_source = 'CESTES'),
                          mcgill_sad %>% 
                            ungroup() %>% 
                            mutate(data_source = 'McGill') %>% 
                            select(S, N, Sn, ENSPIE,
                                   dataset_id, newID, data_source))
# set 1st site to be reference
spatial_grad$newID <- factor(spatial_grad$newID,
                             levels = c("s_1",  "s_2",  "s_3",  "s_4",  "s_5",
                                        "s_6",  "s_7",  "s_8",  "s_9",  "s_10"))
# add unique id for each observation (for modelling overdispersion in species richness)
spatial_grad <- spatial_grad %>% 
  mutate(obsID = paste0('obs', 1:n()))

save(spatial_grad, file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/spatial_natural.Rdata')
