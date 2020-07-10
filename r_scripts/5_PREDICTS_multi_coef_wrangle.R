# wrangle coefficients from 'multivariate' model fit to PREDICTS
load(paste0(path2wd, 'results/PREDICTS_multi4a-6502421.Rdata'))

# want to get SS-level effects
all_coef <- coef(PREDICTS_multi4_fit, robust = TRUE, probs = c(0.05, 0.95), group = 'SS') 

# total abundance
N_SS_primary <- all_coef[[1]][,,'Ncorrected_Intercept'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_Intercept']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Primary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_matureSec <- all_coef[[1]][,,'Ncorrected_LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_LUMature_SecVeg']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


N_SS_interSec <- all_coef[[1]][,,'Ncorrected_LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_LUIntermediate_SecVeg']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_youngSec <- all_coef[[1]][,,'Ncorrected_LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_LUYoung_SecVeg']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_plantation <- all_coef[[1]][,,'Ncorrected_LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_LUPlantation_forest']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_cropland <- all_coef[[1]][,,'Ncorrected_LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_LUCropland']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_pasture <- all_coef[[1]][,,'Ncorrected_LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_LUPasture']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_urban <- all_coef[[1]][,,'Ncorrected_LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Ncorrected_LUUrban']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# Sn
Sn_SS_primary <- all_coef[[1]][,,'Sn5_Intercept'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_Intercept']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Primary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_matureSec <- all_coef[[1]][,,'Sn5_LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_LUMature_SecVeg']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


Sn_SS_interSec <- all_coef[[1]][,,'Sn5_LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_LUIntermediate_SecVeg']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_youngSec <- all_coef[[1]][,,'Sn5_LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_LUYoung_SecVeg']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_plantation <- all_coef[[1]][,,'Sn5_LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_LUPlantation_forest']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_cropland <- all_coef[[1]][,,'Sn5_LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_LUCropland']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_pasture <- all_coef[[1]][,,'Sn5_LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_LUPasture']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_urban <- all_coef[[1]][,,'Sn5_LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'Sn5_LUUrban']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# S_PIE
S_PIE_SS_primary <- all_coef[[1]][,,'ENSPIE_Intercept'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_Intercept']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Primary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_matureSec <- all_coef[[1]][,,'ENSPIE_LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_LUMature_SecVeg']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_interSec <- all_coef[[1]][,,'ENSPIE_LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_LUIntermediate_SecVeg']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_youngSec <- all_coef[[1]][,,'ENSPIE_LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_LUYoung_SecVeg']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_plantation <- all_coef[[1]][,,'ENSPIE_LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_LUPlantation_forest']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_cropland <- all_coef[[1]][,,'ENSPIE_LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_LUCropland']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_pasture <- all_coef[[1]][,,'ENSPIE_LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_LUPasture']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_urban <- all_coef[[1]][,,'ENSPIE_LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'ENSPIE_LUUrban']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# Species richness
S_SS_primary <- all_coef[[1]][,,'S_Intercept'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_Intercept']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Primary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_matureSec <- all_coef[[1]][,,'S_LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_LUMature_SecVeg']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


S_SS_interSec <- all_coef[[1]][,,'S_LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_LUIntermediate_SecVeg']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_youngSec <- all_coef[[1]][,,'S_LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_LUYoung_SecVeg']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_plantation <- all_coef[[1]][,,'S_LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_LUPlantation_forest']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_cropland <- all_coef[[1]][,,'S_LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_LUCropland']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_pasture <- all_coef[[1]][,,'S_LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_LUPasture']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_urban <- all_coef[[1]][,,'S_LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(all_coef[[1]][,,'S_LUUrban']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


SS_LU_multi <- bind_rows(
  left_join(S_SS_primary, 
            N_SS_primary,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_primary,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_primary,
              by = c('SS', 'LU')),
  left_join(S_SS_matureSec, 
            N_SS_matureSec,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_matureSec,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_matureSec,
              by = c('SS', 'LU')),
  left_join(S_SS_interSec, 
            N_SS_interSec,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_interSec,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_interSec,
              by = c('SS', 'LU')),
  left_join(S_SS_youngSec, 
            N_SS_youngSec,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_youngSec,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_youngSec,
              by = c('SS', 'LU')),
  left_join(S_SS_plantation, 
            N_SS_plantation,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_plantation,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_plantation,
              by = c('SS', 'LU')),
  left_join(S_SS_cropland,
            N_SS_cropland,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_cropland,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_cropland,
              by = c('SS', 'LU')),
  left_join(S_SS_pasture, 
            N_SS_pasture,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_pasture,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_pasture,
              by = c('SS', 'LU')),
  left_join(S_SS_urban, 
            N_SS_urban,
            by = c('SS', 'LU')) %>% 
    left_join(S_PIE_SS_urban,
              by = c('SS', 'LU')) %>% 
    left_join(Sn_SS_urban,
              by = c('SS', 'LU'))) %>% 
  mutate(alphaS = ifelse((S_lower < 0 & S_upper > 0), 0.95, 1),
         alphaN = ifelse((N_lower < 0 & N_upper > 0), 0.95, 1),
         alphaENSPIE = ifelse((S_PIE_lower < 0 & S_PIE_upper > 0), 0.95, 1),
         alphaSn = ifelse((Sn_lower < 0 & Sn_upper > 0), 0.95, 1))

# add component change indicators
SS_LU_multi <- SS_LU_multi %>% 
  mutate(sizeS = ifelse(alphaS==1, 2, .5),
         # sizeSn = ifelse(alphaSn==1, 2, .5),
         sizeN = ifelse(alphaN==1, 2, .5),
         sizeENSPIE = ifelse(alphaENSPIE==1, 2, .5),
         sizeN_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE!=1 & alphaSn!=1), 2, .5),# 
         sizeENSPIE_only = ifelse((alphaS!=1 & alphaN!=1  & alphaENSPIE==1 & alphaSn!=1), 2, .5),#
         sizeS_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn!=1), 2, .5),# & alphaSn!=1
         sizeSn_only = ifelse((alphaS!=1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeN_ENSPIE_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE==1 & alphaSn!=1), 2, .5),# 
         sizeS_N_only = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE!=1 & alphaSn!=1), 2, .5),# 
         sizeSn_N_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeS_N_ = ifelse((alphaS==1 & alphaN==1), 2, .5),
         sizeS_Sn_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeS_ENSPIE_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE==1 & alphaSn!=1), 2, .5),# 
         sizeSn_ENSPIE_only = ifelse((alphaS!=1 & alphaN!=1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
         sizeS_ENSPIE_ = ifelse((alphaS==1 & alphaENSPIE==1), 2, .5),
         sizeS_N_and_ENSPIE = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE==1 & alphaSn!=1), 2, .5),# 
         sizeSn_N_and_ENSPIE = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
         sizeS_Sn_N_and_ENSPIE = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
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
                                                                                                               ifelse(sizeS_N_and_ENSPIE==2, 'S, N & ENSPIE', 'No change')
                                                                                                        )
                                                                                                 )
                                                                                          )
                                                                                   )
                                                                            )
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
         )
  )


SS_LU_multi <- SS_LU_multi %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'))

SS_LU_multi$LU <- factor(SS_LU_multi$LU,
                   levels = c('Primary vegetation',
                              'Mature secondary vegetation',      
                              'Intermediate secondary vegetation',
                              'Young secondary vegetation',
                              'Plantation forest',                
                              'Cropland',                         
                              'Pasture',
                              'Urban'),
                   labels = c('Primary vegetation',
                              'Mature secondary vegetation',      
                              'Intermediate secondary vegetation',
                              'Young secondary vegetation',
                              'Plantation forest',                
                              'Cropland',                         
                              'Pasture',
                              'Urban')
)

SS_LU_multi$componentChange <- factor(SS_LU_multi$componentChange,
                                           levels = c('No change', 
                                                      'N only', 'ENSPIE only', 'Sn only', 'S only', 
                                                      'S & Sn', 'S & ENSPIE', 'S & N',
                                                      'Sn & ENSPIE', 'Sn & N',  
                                                      'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                                                      'S, Sn, N & ENSPIE'))

LU_shape <- c('Primary vegetation' = 0,
              'Mature secondary vegetation' = 15,      
              'Intermediate secondary vegetation' = 17,
              'Young secondary vegetation' = 18,       
              'Plantation forest' = 8,     
              'Cropland' = 7,               
              'Pasture' = 2,                          
              'Urban' = 5)

componentChange_col = c('No change' = '#666666', 
                        'N only' = '#a6cee3', 
                        'ENSPIE only' = '#1f78b4', 
                        'S only' = '#b2df8a', 
                        'Sn only' = '#33a02c',
                        'N & ENSPIE' = '#fb9a99', 
                        'S & N' = '#e31a1c',
                        'S & Sn' = '#fdbf6f',
                        'Sn & N' = '#ff7f00',
                        'S & ENSPIE' = '#cab2d6', 
                        'Sn & ENSPIE' = '#2d004b',
                        'S, N & ENSPIE' = '#01665e',
                        'Sn, N & ENSPIE' = '#b2182b',
                        'S, Sn, N & ENSPIE' = '#543005'
)

SS_LU_multi <- SS_LU_multi %>% 
  # add indicator for database
  mutate(db = 'PREDICTS')

# posterior distributions of change estimates for SS-levels
study_levels = PREDICTS_multi4_fit$data %>%
  as_tibble() %>%
  distinct(SS, LU) %>%
  mutate(level = gsub(" ", ".", SS, fixed = TRUE),
         LU2 = ifelse(LU=='PriVeg', 'Intercept', paste0('LU', LU))) %>%
  group_by(SS, LU) %>% 
  nest(data = c(level, LU2))

PREDICTS_SS_posterior <- study_levels %>%
  mutate(N_global = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                             pars = paste('b_Ncorrected_', as.character(.x$LU2), sep=''),
                                                             exact = TRUE,
                                                             # want 1000 samples
                                                             subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                          otherwise = NULL)
  ),
  N_SS = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                          pars = paste('r_SS__Ncorrected[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                          exact = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  S_global = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                          pars = paste('b_S_', as.character(.x$LU2), sep=''),
                                                          exact = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  S_SS = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                   pars = paste('r_SS__S[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                   exact = TRUE,
                                                   # want 1000 samples
                                                   subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                otherwise = NULL)
  ),
  ENSPIE_global = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                          pars = paste('b_ENSPIE_', as.character(.x$LU2), sep=''),
                                                          exact = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  ENSPIE_SS = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                        pars = paste('r_SS__ENSPIE[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                        exact = TRUE,
                                                        # want 1000 samples
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                     otherwise = NULL)
  ),
  Sn_global = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                               pars = paste('b_Sn5_', as.character(.x$LU2), sep=''),
                                                               exact = TRUE,
                                                               # want 1000 samples
                                                               subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                            otherwise = NULL)
  ),
  Sn_SS = purrr::map(data, possibly(~posterior_samples(PREDICTS_multi4_fit, 
                                                    pars = paste('r_SS__Sn5[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                    exact = TRUE,
                                                    # want 1000 samples
                                                    subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                 otherwise = NULL)
  )
  )

PREDICTS_SS_posterior$LU <- factor(PREDICTS_SS_posterior$LU,
                                    levels = c('PriVeg',
                                               'Mature_SecVeg',      
                                               'Intermediate_SecVeg',
                                               'Young_SecVeg',
                                               'Plantation_forest',                
                                               'Cropland',                         
                                               'Pasture',
                                               'Urban'),
                                    labels = c('Primary vegetation',
                                               'Mature secondary vegetation',      
                                               'Intermediate secondary vegetation',
                                               'Young secondary vegetation',
                                               'Plantation forest',                
                                               'Cropland',                         
                                               'Pasture',
                                               'Urban')
) 

PREDICTS_SS_posterior <- PREDICTS_SS_posterior %>% 
  # add indicator for database
mutate(db = 'PREDICTS')

PREDICTS_SS_posterior <- PREDICTS_SS_posterior %>% 
  select(-data) %>% 
  unnest(cols = c(N_global, N_SS, 
                  S_global, S_SS, 
                  ENSPIE_global, ENSPIE_SS, 
                  Sn_global, Sn_SS)) 

# predicts_posterior distributions of the correlations estimated by the model
predicts_posterior <- as.mcmc(PREDICTS_multi4_fit, combine_chains = TRUE, 
                              pars = "^cor") %>% 
  as_tibble()

# want to get the components to look at how change was realised
predicts_cor_long <-
  bind_rows(
    # Primary veg (completeness only; reference for change)
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_Intercept__Ncorrected_Intercept ,
             corS_S_PIE = cor_SS__S_Intercept__ENSPIE_Intercept,
             corS_Sn = cor_SS__S_Intercept__Sn5_Intercept,
             corN_S_PIE = cor_SS__Ncorrected_Intercept__ENSPIE_Intercept,
             corSn_N = cor_SS__Ncorrected_Intercept__Sn5_Intercept,
             corS_PIE_Sn = cor_SS__ENSPIE_Intercept__Sn5_Intercept) %>% 
      mutate(LU = 'Primary vegetation'),
    # mature secondary ver
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_LUMature_SecVeg__Ncorrected_LUMature_SecVeg ,
             corS_S_PIE = cor_SS__S_LUMature_SecVeg__ENSPIE_LUMature_SecVeg,
             corS_Sn = cor_SS__S_LUMature_SecVeg__Sn5_LUMature_SecVeg,
             corN_S_PIE = cor_SS__Ncorrected_LUMature_SecVeg__ENSPIE_LUMature_SecVeg,
             corSn_N = cor_SS__Ncorrected_LUMature_SecVeg__Sn5_LUMature_SecVeg,
             corS_PIE_Sn = cor_SS__ENSPIE_LUMature_SecVeg__Sn5_LUMature_SecVeg) %>% 
      mutate(LU = 'Mature secondary vegetation'),
    # intermediate secondary veg
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_LUIntermediate_SecVeg__Ncorrected_LUIntermediate_SecVeg ,
             corS_S_PIE = cor_SS__S_LUIntermediate_SecVeg__ENSPIE_LUIntermediate_SecVeg,
             corS_Sn = cor_SS__S_LUIntermediate_SecVeg__Sn5_LUIntermediate_SecVeg,
             corN_S_PIE = cor_SS__Ncorrected_LUIntermediate_SecVeg__ENSPIE_LUIntermediate_SecVeg,
             corSn_N = cor_SS__Ncorrected_LUIntermediate_SecVeg__Sn5_LUIntermediate_SecVeg,
             corS_PIE_Sn = cor_SS__ENSPIE_LUIntermediate_SecVeg__Sn5_LUIntermediate_SecVeg) %>% 
      mutate(LU = 'Intermediate secondary vegetation'),
    # young secondary veg
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_LUYoung_SecVeg__Ncorrected_LUYoung_SecVeg ,
             corS_S_PIE = cor_SS__S_LUYoung_SecVeg__ENSPIE_LUYoung_SecVeg,
             corS_Sn = cor_SS__S_LUYoung_SecVeg__Sn5_LUYoung_SecVeg,
             corN_S_PIE = cor_SS__Ncorrected_LUYoung_SecVeg__ENSPIE_LUYoung_SecVeg,
             corSn_N = cor_SS__Ncorrected_LUYoung_SecVeg__Sn5_LUYoung_SecVeg,
             corS_PIE_Sn = cor_SS__ENSPIE_LUYoung_SecVeg__Sn5_LUYoung_SecVeg) %>% 
      mutate(LU = 'Young secondary vegetation'),
    # Plantation_forest
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_LUPlantation_forest__Ncorrected_LUPlantation_forest ,
             corS_S_PIE = cor_SS__S_LUPlantation_forest__ENSPIE_LUPlantation_forest,
             corS_Sn = cor_SS__S_LUPlantation_forest__Sn5_LUPlantation_forest,
             corN_S_PIE = cor_SS__Ncorrected_LUPlantation_forest__ENSPIE_LUPlantation_forest,
             corSn_N = cor_SS__Ncorrected_LUPlantation_forest__Sn5_LUPlantation_forest,
             corS_PIE_Sn = cor_SS__ENSPIE_LUPlantation_forest__Sn5_LUPlantation_forest) %>% 
      mutate(LU = 'Plantation forest'),
    # Cropland
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_LUCropland__Ncorrected_LUCropland ,
             corS_S_PIE = cor_SS__S_LUCropland__ENSPIE_LUCropland,
             corS_Sn = cor_SS__S_LUCropland__Sn5_LUCropland,
             corN_S_PIE = cor_SS__Ncorrected_LUCropland__ENSPIE_LUCropland,
             corSn_N = cor_SS__Ncorrected_LUCropland__Sn5_LUCropland,
             corS_PIE_Sn = cor_SS__ENSPIE_LUCropland__Sn5_LUCropland) %>% 
      mutate(LU = 'Cropland'),
    # Urban
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_LUUrban__Ncorrected_LUUrban ,
             corS_S_PIE = cor_SS__S_LUUrban__ENSPIE_LUUrban,
             corS_Sn = cor_SS__S_LUUrban__Sn5_LUUrban,
             corN_S_PIE = cor_SS__Ncorrected_LUUrban__ENSPIE_LUUrban,
             corSn_N = cor_SS__Ncorrected_LUUrban__Sn5_LUUrban,
             corS_PIE_Sn = cor_SS__ENSPIE_LUUrban__Sn5_LUUrban) %>% 
      mutate(LU = 'Urban'),
    # Pasture
    predicts_posterior %>% 
      select(corS_N = cor_SS__S_LUPasture__Ncorrected_LUPasture ,
             corS_S_PIE = cor_SS__S_LUPasture__ENSPIE_LUPasture,
             corS_Sn = cor_SS__S_LUPasture__Sn5_LUPasture,
             corN_S_PIE = cor_SS__Ncorrected_LUPasture__ENSPIE_LUPasture,
             corSn_N = cor_SS__Ncorrected_LUPasture__Sn5_LUPasture,
             corS_PIE_Sn = cor_SS__ENSPIE_LUPasture__Sn5_LUPasture) %>% 
      mutate(LU = 'Pasture'))

predicts_cor_long$LU <- factor(predicts_cor_long$LU,
                               levels = c('Primary vegetation',
                                          'Mature secondary vegetation',      
                                          'Intermediate secondary vegetation',
                                          'Young secondary vegetation',
                                          'Plantation forest',                
                                          'Cropland',                         
                                          'Pasture',
                                          'Urban'))

