# wrangle results from multivariate model fit to CESTES and McGill data
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

load('~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/spatial_natural_fit0.Rdata')

spat_coefs <- coef(spatial_10sites0obsID, robust = TRUE, probs = c(0.05, 0.95))

spat_coefs_multi4 <- bind_rows(
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_2',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_2'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_2'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_2'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_2'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_2'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_2'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_2'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_2'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_2'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_2'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_2'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_2'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_3',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_3'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_3'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_3'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_3'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_3'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_3'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_3'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_3'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_3'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_3'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_3'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_3'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_4',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_4'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_4'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_4'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_4'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_4'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_4'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_4'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_4'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_4'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_4'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_4'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_4'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_5',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_5'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_5'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_5'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_5'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_5'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_5'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'S_newIDs_5'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'S_newIDs_5'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'S_newIDs_5'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_5'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_5'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_5'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_6',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_6'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_6'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_6'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_6'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_6'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_6'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_6'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_6'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_6'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_6'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_6'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_6'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_7',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_7'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_7'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_7'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_7'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_7'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_7'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_7'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_7'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_7'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_7'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_7'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_7'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_8',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_8'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_8'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_8'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_8'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_8'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_8'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_8'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_8'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_8'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_8'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_8'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_8'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_9',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_9'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_9'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_9'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_9'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_9'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_9'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_9'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_9'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_9'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_9'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_9'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_9'][,'Q5']
  ),
  tibble(
    study = rownames(spat_coefs[[1]]),
    newID = 's_10',
    # N
    N = spat_coefs[[1]][,,'N_newIDs_10'][,'Estimate'],
    N_upper = spat_coefs[[1]][,,'N_newIDs_10'][,'Q95'],
    N_lower = spat_coefs[[1]][,,'N_newIDs_10'][,'Q5'],
    # S
    S = spat_coefs[[1]][,,'S_newIDs_10'][,'Estimate'],
    S_upper = spat_coefs[[1]][,,'S_newIDs_10'][,'Q95'],
    S_lower = spat_coefs[[1]][,,'S_newIDs_10'][,'Q5'],
    # Sn
    Sn = spat_coefs[[1]][,,'Sn_newIDs_10'][,'Estimate'],
    Sn_upper = spat_coefs[[1]][,,'Sn_newIDs_10'][,'Q95'],
    Sn_lower = spat_coefs[[1]][,,'Sn_newIDs_10'][,'Q5'],
    # ENSPIE
    ENSPIE = spat_coefs[[1]][,,'ENSPIE_newIDs_10'][,'Estimate'],
    ENSPIE_upper = spat_coefs[[1]][,,'ENSPIE_newIDs_10'][,'Q95'],
    ENSPIE_lower = spat_coefs[[1]][,,'ENSPIE_newIDs_10'][,'Q5']
  )
) %>% 
  mutate(db = 'Spatial gradients')

spat_coefs_multi4 <- spat_coefs_multi4 %>% 
  mutate(N_change = ifelse((N_upper > 0 & N_lower < 0), 0.9, 1),
         S_change = ifelse((S_upper > 0 & S_lower < 0), 0.9, 1),
         Sn_change = ifelse((Sn_upper > 0 & Sn_lower < 0), 0.9, 1),
         ENSPIE_change = ifelse((ENSPIE_upper > 0 & ENSPIE_lower < 0), 0.9, 1)) %>% 
  mutate(strokeS = ifelse(S_change==1, 1.1, 1),
         sizeS = ifelse(S_change==1, 2, .5),
         strokeSn = ifelse(Sn_change==1, 1.1, 1),
         sizeSn = ifelse(Sn_change==1, 2, .5),
         sizeN = ifelse(N_change==1, 2, .5),
         sizeENSPIE = ifelse(ENSPIE_change==1, 2, .5),
         sizeN_only = ifelse((S_change!=1 & N_change==1 & ENSPIE_change!=1 & Sn_change!=1), 2, .5),
         sizeENSPIE_only = ifelse((S_change!=1 & N_change!=1 & Sn_change!=1 & ENSPIE_change==1), 2, .5),
         sizeS_only = ifelse((S_change==1 & N_change!=1 & ENSPIE_change!=1 & Sn_change!=1), 2, .5),
         sizeSn_only = ifelse((S_change!=1 & N_change!=1 & ENSPIE_change!=1 & Sn_change==1), 2, .5),
         sizeN_ENSPIE_only = ifelse((S_change!=1 & N_change==1 & ENSPIE_change==1 & Sn_change!=1), 2, .5),
         sizeS_N_only = ifelse((S_change==1 & N_change==1 & ENSPIE_change!=1 & Sn_change!=1), 2, .5),
         sizeSn_N_only = ifelse((S_change!=1 & N_change==1 & ENSPIE_change!=1 & Sn_change==1), 2, .5),
         sizeS_N_ = ifelse((S_change==1 & N_change==1), 2, .5),
         sizeS_Sn_only = ifelse((S_change==1 & N_change!=1 & ENSPIE_change!=1 & Sn_change==1), 2, .5),
         sizeS_ENSPIE_only = ifelse((S_change==1 & N_change!=1 & ENSPIE_change==1 & Sn_change!=1), 2, .5),
         sizeSn_ENSPIE_only = ifelse((S_change!=1 & N_change!=1 & ENSPIE_change==1 & Sn_change==1), 2, .5),
         sizeS_ENSPIE_ = ifelse((S_change==1 & ENSPIE_change==1), 2, .5),
         sizeS_N_and_ENSPIE = ifelse((S_change==1 & N_change==1 & ENSPIE_change==1 & Sn_change!=1), 2, .5),
         sizeSn_N_and_ENSPIE = ifelse((S_change!=1 & N_change==1 & ENSPIE_change==1 & Sn_change==1), 2, .5),
         sizeS_Sn_N_and_ENSPIE = ifelse((S_change==1 & N_change==1 & ENSPIE_change==1 & Sn_change==1), 2, .5),
         # categorical variable for barplot
         componentChange = ifelse(sizeN_only==2, 'N only',
                                  ifelse(sizeENSPIE_only==2, 'ENSPIE only',
                                         ifelse(sizeS_only==2, 'S only',
                                                ifelse(sizeSn_only==2, 'Sn only',
                                                       ifelse(sizeN_ENSPIE_only==2, 'N & ENSPIE',
                                                              ifelse(sizeS_N_only==2, 'S & N',
                                                                     ifelse(sizeSn_N_only==2, 'Sn & N',
                                                                            ifelse(sizeS_Sn_only==2, 'S & Sn',
                                                                                   ifelse(sizeS_ENSPIE_only==2, 'S & ENSPIE',
                                                                                          ifelse(sizeSn_ENSPIE_only==2, 'Sn & ENSPIE',
                                                                                                 ifelse(sizeSn_N_and_ENSPIE==2, 'Sn, N & ENSPIE',
                                                                                                        ifelse(sizeS_Sn_N_and_ENSPIE==2, 'S, Sn, N & ENSPIE',
                                                                                                               ifelse(sizeS_N_and_ENSPIE==2, 'S, N & ENSPIE', 'No change'))))))))))))),
         # add indicator for whether signs of components changing are the same
         componentChange_sign = ifelse(componentChange=='N & ENSPIE', sign(N) * sign(ENSPIE),
                                       ifelse(componentChange=='S & N', sign(S) * sign(N),
                                              ifelse(componentChange=='Sn & N', sign(Sn) * sign(N),
                                                     ifelse(componentChange=='S & Sn', sign(S) * sign(Sn),
                                                            ifelse(componentChange=='S & ENSPIE', sign(S) * sign(ENSPIE),
                                                                   ifelse(componentChange=='Sn & ENSPIE', sign(Sn) * sign(ENSPIE),
                                                                          ifelse(componentChange=='Sn, N & ENSPIE', sign(Sn) * sign(N) * sign(ENSPIE),
                                                                                 ifelse(componentChange=='S, Sn, N & ENSPIE', sign(S) * sign(Sn) * sign(N) * sign(ENSPIE),
                                                                                        ifelse(componentChange=='S, N & ENSPIE', sign(S) * sign(N) * sign(ENSPIE), NA)))))))))
  )

spat_coefs_multi4$componentChange <- factor(spat_coefs_multi4$componentChange,
                                            levels = c('No change', 
                                                       'N only', 'ENSPIE only', 'Sn only', 'S only', 
                                                       'S & Sn', 'S & ENSPIE', 'S & N',
                                                       'Sn & ENSPIE', 'Sn & N',  
                                                       'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                                                       'S, Sn, N & ENSPIE'))



levels <- spatial_10sites0obsID$data %>% 
  as_tibble() %>% 
  distinct(dataset_id, newID) %>% 
  mutate(level = dataset_id,
         site = ifelse(newID=='s_1', 'Intercept', paste0('newID', newID))) %>% 
  group_by(dataset_id) %>% 
  nest(data = c(level, site)) 

spatial_multi4_posterior <- levels %>%
  mutate(N_study = purrr::map(data, ~posterior_samples(spatial_10sites0obsID, 
                                                       pars = paste('r_dataset_id__N[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         Sn_study = purrr::map(data, ~posterior_samples(spatial_10sites0obsID, 
                                                        pars = paste('r_dataset_id__Sn[', as.character(.x$level), ',', 
                                                                     as.character(.x$site),']', sep=''),
                                                        fixed = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()),
         S_study = purrr::map(data, ~posterior_samples(spatial_10sites0obsID, 
                                                       pars = paste('r_dataset_id__S[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         ENSPIE_study = purrr::map(data, ~posterior_samples(spatial_10sites0obsID, 
                                                            pars = paste('r_dataset_id__ENSPIE[', as.character(.x$level), ',', 
                                                                         as.character(.x$site),']', sep=''),
                                                            fixed = TRUE,
                                                            # want 1000 samples
                                                            subset = seq(1, 4000, by = 4))
                                   %>% unlist() %>% as.numeric()))


spatial_multi4_posterior <- spatial_multi4_posterior %>% 
  # add indicator for database
  mutate(db = 'Spatial gradients') %>% 
  select(-data) %>% 
  unnest(cols = c(N_study, S_study, ENSPIE_study, Sn_study)) %>% 
  ungroup()

spatial_cor <- as.mcmc(spatial_10sites0obsID, combine_chains = TRUE, 
                      pars = "^cor") %>% 
  as_tibble()

spatial_cor_long <- bind_rows(
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_2__N_newIDs_2,
           corS_S_PIE = cor_dataset_id__S_newIDs_2__ENSPIE_newIDs_2,
           corS_Sn = cor_dataset_id__S_newIDs_2__Sn_newIDs_2,
           corN_S_PIE = cor_dataset_id__N_newIDs_2__ENSPIE_newIDs_2,
           corSn_N = cor_dataset_id__N_newIDs_2__Sn_newIDs_2,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_2__ENSPIE_newIDs_2) %>% 
    mutate(newID = '2'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_3__N_newIDs_3,
           corS_S_PIE = cor_dataset_id__S_newIDs_3__ENSPIE_newIDs_3,
           corS_Sn = cor_dataset_id__S_newIDs_3__Sn_newIDs_3,
           corN_S_PIE = cor_dataset_id__N_newIDs_3__ENSPIE_newIDs_3,
           corSn_N = cor_dataset_id__N_newIDs_3__Sn_newIDs_3,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_3__ENSPIE_newIDs_3) %>% 
    mutate(newID = '3'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_4__N_newIDs_4,
           corS_S_PIE = cor_dataset_id__S_newIDs_4__ENSPIE_newIDs_4,
           corS_Sn = cor_dataset_id__S_newIDs_4__Sn_newIDs_4,
           corN_S_PIE = cor_dataset_id__N_newIDs_4__ENSPIE_newIDs_4,
           corSn_N = cor_dataset_id__N_newIDs_4__Sn_newIDs_4,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_4__ENSPIE_newIDs_4) %>% 
    mutate(newID = '4'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_5__N_newIDs_5,
           corS_S_PIE = cor_dataset_id__S_newIDs_5__ENSPIE_newIDs_5,
           corS_Sn = cor_dataset_id__S_newIDs_5__Sn_newIDs_5,
           corN_S_PIE = cor_dataset_id__N_newIDs_5__ENSPIE_newIDs_5,
           corSn_N = cor_dataset_id__N_newIDs_5__Sn_newIDs_5,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_5__ENSPIE_newIDs_5) %>% 
    mutate(newID = '5'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_6__N_newIDs_6,
           corS_S_PIE = cor_dataset_id__S_newIDs_6__ENSPIE_newIDs_6,
           corS_Sn = cor_dataset_id__S_newIDs_6__Sn_newIDs_6,
           corN_S_PIE = cor_dataset_id__N_newIDs_6__ENSPIE_newIDs_6,
           corSn_N = cor_dataset_id__N_newIDs_6__Sn_newIDs_6,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_6__ENSPIE_newIDs_6) %>% 
    mutate(newID = '6'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_7__N_newIDs_7,
           corS_S_PIE = cor_dataset_id__S_newIDs_7__ENSPIE_newIDs_7,
           corS_Sn = cor_dataset_id__S_newIDs_7__Sn_newIDs_7,
           corN_S_PIE = cor_dataset_id__N_newIDs_7__ENSPIE_newIDs_7,
           corSn_N = cor_dataset_id__N_newIDs_7__Sn_newIDs_7,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_7__ENSPIE_newIDs_7) %>% 
    mutate(newID = '7'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_8__N_newIDs_8,
           corS_S_PIE = cor_dataset_id__S_newIDs_8__ENSPIE_newIDs_8,
           corS_Sn = cor_dataset_id__S_newIDs_8__Sn_newIDs_8,
           corN_S_PIE = cor_dataset_id__N_newIDs_8__ENSPIE_newIDs_8,
           corSn_N = cor_dataset_id__N_newIDs_8__Sn_newIDs_8,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_2__ENSPIE_newIDs_2) %>% 
    mutate(newID = '8'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_9__N_newIDs_9,
           corS_S_PIE = cor_dataset_id__S_newIDs_9__ENSPIE_newIDs_9,
           corS_Sn = cor_dataset_id__S_newIDs_9__Sn_newIDs_9,
           corN_S_PIE = cor_dataset_id__N_newIDs_9__ENSPIE_newIDs_9,
           corSn_N = cor_dataset_id__N_newIDs_9__Sn_newIDs_9,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_2__ENSPIE_newIDs_2) %>% 
    mutate(newID = '9'),
  spatial_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_10__N_newIDs_10,
           corS_S_PIE = cor_dataset_id__S_newIDs_10__ENSPIE_newIDs_10,
           corS_Sn = cor_dataset_id__S_newIDs_10__Sn_newIDs_10,
           corN_S_PIE = cor_dataset_id__N_newIDs_10__ENSPIE_newIDs_10,
           corSn_N = cor_dataset_id__N_newIDs_10__Sn_newIDs_10,
           corS_PIE_Sn = cor_dataset_id__Sn_newIDs_2__ENSPIE_newIDs_2) %>% 
    mutate(newID = '10'))



# model estimates coefficients for combinations of study and site not in the data
spatial_filter <- spatial_10sites0obsID$data %>% 
  as_tibble() %>% 
  distinct(dataset_id, newID) %>% 
  unite(filter, c(dataset_id, newID), remove = F)
  
spat_coefs_multi4_filtered <- spat_coefs_multi4 %>% 
  unite(filter, c(study, newID), remove = F) %>% 
  filter(filter %in% spatial_filter$filter)

save(spat_coefs_multi4, spat_coefs_multi4_filtered,
     spatial_multi4_posterior, spatial_cor_long,
     file = paste0(path2wd, 'multiComponentChange/results/spatial_multi4_results.Rdata'))
