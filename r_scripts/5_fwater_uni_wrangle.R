# wrangle coefficients from univariate fits to Dani's data 

library(brms)
library(tidyverse)

Sys.setlocale('LC_ALL','C') # for the portuguese (I think) funkiness!

load(paste0(path2wd, 'results/fwater_uni_fits.Rdata'))

# want to get SS-level effects
N_lnorm_fw_coef <- coef(fw_N_fit, robust = TRUE, probs = c(0.05, 0.95)) 
S_PIE_lnorm_fw_coef <- coef(fw_S_PIE_fit, robust = TRUE, probs = c(0.05, 0.95)) 
S_pois_fw_coef <- coef(fw_S_fit, robust = TRUE, probs = c(0.05, 0.95)) 
Sn_lnorm_fw_coef <- coef(fw_Sn_fit, robust = TRUE, probs = c(0.05, 0.95)) 

# N -----------------------------------------------------------------------
N_study_reference <- N_lnorm_fw_coef[[1]][,,'Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(N_lnorm_fw_coef[[1]][,,'Intercept']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_study_Agriculture <- N_lnorm_fw_coef[[1]][,,'TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(N_lnorm_fw_coef[[1]][,,'TreatmentAgriculture']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


N_study_Forestry <- N_lnorm_fw_coef[[1]][,,'TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(N_lnorm_fw_coef[[1]][,,'TreatmentForestry']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_study_Urban <- N_lnorm_fw_coef[[1]][,,'TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(N_lnorm_fw_coef[[1]][,,'TreatmentUrbanization']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


# S-------------------------
S_study_reference <- S_pois_fw_coef[[1]][,,'Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_pois_fw_coef[[1]][,,'Intercept']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


S_study_Agriculture <- S_pois_fw_coef[[1]][,,'TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_pois_fw_coef[[1]][,,'TreatmentAgriculture']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


S_study_Forestry <- S_pois_fw_coef[[1]][,,'TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_pois_fw_coef[[1]][,,'TreatmentForestry']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_study_Urban <- S_pois_fw_coef[[1]][,,'TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_pois_fw_coef[[1]][,,'TreatmentUrbanization']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


# Sn--------------------------------
Sn_study_reference <- Sn_lnorm_fw_coef[[1]][,,'Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(Sn_lnorm_fw_coef[[1]][,,'Intercept']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_study_Agriculture <- Sn_lnorm_fw_coef[[1]][,,'TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(Sn_lnorm_fw_coef[[1]][,,'TreatmentAgriculture']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


Sn_study_Forestry <- Sn_lnorm_fw_coef[[1]][,,'TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(Sn_lnorm_fw_coef[[1]][,,'TreatmentForestry']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_study_Urban <- Sn_lnorm_fw_coef[[1]][,,'TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(Sn_lnorm_fw_coef[[1]][,,'TreatmentUrbanization']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


# evenness (S_PIE)---------------------
S_PIE_study_reference <- S_PIE_lnorm_fw_coef[[1]][,,'Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_PIE_lnorm_fw_coef[[1]][,,'Intercept']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_study_Agriculture <- S_PIE_lnorm_fw_coef[[1]][,,'TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_PIE_lnorm_fw_coef[[1]][,,'TreatmentAgriculture']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


S_PIE_study_Forestry <- S_PIE_lnorm_fw_coef[[1]][,,'TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_PIE_lnorm_fw_coef[[1]][,,'TreatmentForestry']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_study_Urban <- S_PIE_lnorm_fw_coef[[1]][,,'TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(S_PIE_lnorm_fw_coef[[1]][,,'TreatmentUrbanization']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


# combine -----------------------
fwater_study_LU_uni <- bind_rows(
  left_join(S_study_reference, 
            N_study_reference,
            by = c('Study', 'LU')) %>% 
    left_join(S_PIE_study_reference,
              by = c('Study', 'LU')) %>% 
    left_join(Sn_study_reference,
              by = c('Study', 'LU')),
  left_join(S_study_Agriculture, 
            N_study_Agriculture,
            by = c('Study', 'LU')) %>% 
    left_join(S_PIE_study_Agriculture,
              by = c('Study', 'LU')) %>% 
    left_join(Sn_study_Agriculture,
              by = c('Study', 'LU')),
  left_join(S_study_Forestry, 
            N_study_Forestry,
            by = c('Study', 'LU')) %>% 
    left_join(S_PIE_study_Forestry,
              by = c('Study', 'LU')) %>% 
    left_join(Sn_study_Forestry,
              by = c('Study', 'LU')),
  left_join(S_study_Urban, 
            N_study_Urban,
            by = c('Study', 'LU')) %>% 
    left_join(S_PIE_study_Urban,
              by = c('Study', 'LU')) %>% 
    left_join(Sn_study_Urban,
              by = c('Study', 'LU'))) %>% 
  mutate(alphaS = ifelse((S_lower < 0 & S_upper > 0), 0.95, 1),
         alphaN = ifelse((N_lower < 0 & N_upper > 0), 0.95, 1),
         alphaENSPIE = ifelse((S_PIE_lower < 0 & S_PIE_upper > 0), 0.95, 1),
         alphaSn = ifelse((Sn_lower < 0 & Sn_upper > 0), 0.95, 1))

# add component change indicators
fwater_study_LU_uni <- fwater_study_LU_uni %>% 
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


fwater_study_LU_uni <- fwater_study_LU_uni %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'))

fwater_study_LU_uni$LU <- factor(fwater_study_LU_uni$LU,
                                   levels = c('Reference stream',
                                              'Agriculture',      
                                              'Forestry',
                                              'Urban'),
                                   labels = c('Reference stream',
                                              'Agriculture',      
                                              'Forestry',
                                              'Urban')
)

LU_shape <- c('Reference stream' = 0,
              'Agriculture' = 15,      
              'Forestry' = 17,
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

fwater_study_LU_uni <- fwater_study_LU_uni %>% 
  # add indicator for database
  mutate(db = 'Freshwater LU')

