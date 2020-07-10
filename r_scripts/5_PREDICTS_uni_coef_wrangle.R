# PREDICTS univariate models
# coefficient wrangle for models fit to each metric independently

load(paste0(path2wd, 'results/PREDICTS_N.Rdata'))
load(paste0(path2wd, 'results/PREDICTS_ENSPIE.Rdata'))
load(paste0(path2wd, 'results/PREDICTS_S.Rdata'))
load(paste0(path2wd, 'results/PREDICTS_Sn.Rdata'))
# want to get SS-level effects
N_lnorm_LU_coef <- coef(N_lnorm_LU2, robust = TRUE, probs = c(0.05, 0.95)) 
S_PIE_lnorm_LU_coef <- coef(ENSPIE_lnorm_LU2, robust = TRUE, probs = c(0.05, 0.95)) 
S_pois_LU_coef <- coef(S_pois_LU, robust = TRUE, probs = c(0.05, 0.95)) 
Sn_lnorm_LU_coef <- coef(Sn_lnorm_LU2, robust = TRUE, probs = c(0.05, 0.95)) 

# total abundance
N_SS_primary <- N_lnorm_LU_coef[[1]][,,'Intercept'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'Intercept']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Primary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_matureSec <- N_lnorm_LU_coef[[1]][,,'LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'LUMature_SecVeg']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


N_SS_interSec <- N_lnorm_LU_coef[[1]][,,'LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'LUIntermediate_SecVeg']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_youngSec <- N_lnorm_LU_coef[[1]][,,'LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'LUYoung_SecVeg']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_plantation <- N_lnorm_LU_coef[[1]][,,'LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'LUPlantation_forest']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_cropland <- N_lnorm_LU_coef[[1]][,,'LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'LUCropland']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_pasture <- N_lnorm_LU_coef[[1]][,,'LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'LUPasture']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_SS_urban <- N_lnorm_LU_coef[[1]][,,'LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(N_lnorm_LU_coef[[1]][,,'LUUrban']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# Sn
Sn_SS_primary <- Sn_lnorm_LU_coef[[1]][,,'Intercept'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'Intercept']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Primary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_matureSec <- Sn_lnorm_LU_coef[[1]][,,'LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'LUMature_SecVeg']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


Sn_SS_interSec <- Sn_lnorm_LU_coef[[1]][,,'LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'LUIntermediate_SecVeg']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_youngSec <- Sn_lnorm_LU_coef[[1]][,,'LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'LUYoung_SecVeg']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_plantation <- Sn_lnorm_LU_coef[[1]][,,'LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'LUPlantation_forest']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_cropland <- Sn_lnorm_LU_coef[[1]][,,'LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'LUCropland']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_pasture <- Sn_lnorm_LU_coef[[1]][,,'LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'LUPasture']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_SS_urban <- Sn_lnorm_LU_coef[[1]][,,'LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(Sn_lnorm_LU_coef[[1]][,,'LUUrban']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# S_PIE
S_PIE_SS_primary <- S_PIE_lnorm_LU_coef[[1]][,,'Intercept'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'Intercept']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Primary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_matureSec <- S_PIE_lnorm_LU_coef[[1]][,,'LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'LUMature_SecVeg']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_interSec <- S_PIE_lnorm_LU_coef[[1]][,,'LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'LUIntermediate_SecVeg']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_youngSec <- S_PIE_lnorm_LU_coef[[1]][,,'LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'LUYoung_SecVeg']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_plantation <- S_PIE_lnorm_LU_coef[[1]][,,'LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'LUPlantation_forest']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_cropland <- S_PIE_lnorm_LU_coef[[1]][,,'LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'LUCropland']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_pasture <- S_PIE_lnorm_LU_coef[[1]][,,'LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'LUPasture']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_SS_urban <- S_PIE_lnorm_LU_coef[[1]][,,'LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_PIE_lnorm_LU_coef[[1]][,,'LUUrban']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# Species richness
S_SS_primary <- S_pois_LU_coef[[1]][,,'Intercept'] %>% 
                                  as_tibble() %>% 
                                  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'Intercept']),
                                         S = Estimate,
                                         S_lower = Q5,
                                         S_upper = Q95,
                                         LU = 'Primary vegetation') %>% 
                                  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_matureSec <- S_pois_LU_coef[[1]][,,'LUMature_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'LUMature_SecVeg']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Mature secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


S_SS_interSec <- S_pois_LU_coef[[1]][,,'LUIntermediate_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'LUIntermediate_SecVeg']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Intermediate secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_youngSec <- S_pois_LU_coef[[1]][,,'LUYoung_SecVeg'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'LUYoung_SecVeg']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Young secondary vegetation') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_plantation <- S_pois_LU_coef[[1]][,,'LUPlantation_forest'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'LUPlantation_forest']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Plantation forest') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_cropland <- S_pois_LU_coef[[1]][,,'LUCropland'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'LUCropland']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Cropland') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_pasture <- S_pois_LU_coef[[1]][,,'LUPasture'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'LUPasture']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Pasture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_SS_urban <- S_pois_LU_coef[[1]][,,'LUUrban'] %>% 
  as_tibble() %>% 
  mutate(SS = rownames(S_pois_LU_coef[[1]][,,'LUUrban']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


SS_LU_univariate <- bind_rows(
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
SS_LU_univariate_partial <- SS_LU_univariate %>% 
  # remove rows where we don't have an estimate of Sn
  filter(!is.na(alphaSn)) %>% 
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

                                                                   
missing_alphaSn_rows <- SS_LU_univariate %>% 
  filter(is.na(alphaSn)) %>% 
  select(-Sn, -Sn_lower, -Sn_upper, -alphaSn) %>% 
  mutate(sizeS = ifelse(alphaS==1, 2, .5),
         # sizeSn = ifelse(alphaSn==1, 2, .5),
         sizeN = ifelse(alphaN==1, 2, .5),
         sizeENSPIE = ifelse(alphaENSPIE==1, 2, .5),
         sizeN_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE!=1), 2, .5),# 
         sizeENSPIE_only = ifelse((alphaS!=1 & alphaN!=1  & alphaENSPIE==1), 2, .5),#
         sizeS_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE!=1), 2, .5),# & alphaSn!=1
         # sizeSn_only = ifelse((alphaS!=1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeN_ENSPIE_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE==1), 2, .5),# 
         sizeS_N_only = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE!=1), 2, .5),# 
         # sizeSn_N_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeS_N_ = ifelse((alphaS==1 & alphaN==1), 2, .5),
         # sizeS_Sn_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeS_ENSPIE_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE==1), 2, .5),# 
         # sizeSn_ENSPIE_only = ifelse((alphaS!=1 & alphaN!=1 & alphaENSPIE==1), 2, .5),
         sizeS_ENSPIE_ = ifelse((alphaS==1 & alphaENSPIE==1), 2, .5),
         sizeS_N_and_ENSPIE = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE==1), 2, .5),# 
         # sizeSn_N_and_ENSPIE = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
         # sizeS_Sn_N_and_ENSPIE = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
         # categorical variable for barplot
         componentChange = ifelse(sizeN_only==2, 'N only',
                                  ifelse(sizeENSPIE_only==2, 'ENSPIE only',
                                         ifelse(sizeS_only==2, 'S only',
                                                # ifelse(sizeSn_only==2, 'Sn only',
                                                       ifelse(sizeN_ENSPIE_only==2, 'N & ENSPIE',
                                                              ifelse(sizeS_N_only==2, 'S & N',
                                                                     # ifelse(sizeSn_N_only==2, 'Sn & N',
                                                                            # ifelse(sizeS_Sn_only==2, 'S & Sn',
                                                                                   ifelse(sizeS_ENSPIE_only==2, 'S & ENSPIE',
                                                                                          # ifelse(sizeSn_ENSPIE_only==2, 'Sn & ENSPIE',
                                                                                                 # ifelse(sizeSn_N_and_ENSPIE==2, 'Sn, N & ENSPIE',
                                                                                                        # ifelse(sizeS_Sn_N_and_ENSPIE==2, 'S, Sn, N & ENSPIE',
                                                                                                               ifelse(sizeS_N_and_ENSPIE==2, 'S, N & ENSPIE', 'No change')
                                                                                                        )
                                                                                                 )
                                                                                          )
                                                                                   )
                                                                            )
                                                                     )
                                                              )

 
  

SS_LU_univariate <- bind_rows(SS_LU_univariate_partial,
                              missing_alphaSn_rows)

SS_LU_univariate <- SS_LU_univariate %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'))

SS_LU_univariate$LU <- factor(SS_LU_univariate$LU,
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

SS_LU_univariate <- SS_LU_univariate %>% 
  mutate(db = 'PREDICTS')

SS_LU_univariate$componentChange <- factor(SS_LU_univariate$componentChange,
                                        levels = c('No change', 
                                                   'N only', 'ENSPIE only', 'Sn only', 'S only', 
                                                   'S & Sn', 'S & ENSPIE', 'S & N',
                                                   'Sn & ENSPIE', 'Sn & N',  
                                                   'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                                                   'S, Sn, N & ENSPIE'))
