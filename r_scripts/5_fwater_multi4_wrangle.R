# posterior wrangles for Dani's freshwater data
Sys.setlocale('LC_ALL','C')

load(paste0(path2wd, 'results/fwater_multi4_fit.Rdata'))

# want to get Study-level effects
all_coef <- coef(fwater_multi4_fit, robust = TRUE, probs = c(0.05, 0.95), group = 'Study') 

# total abundance
N_study_reference <- all_coef[[1]][,,'N_Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'N_Intercept']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_study_Agriculture <- all_coef[[1]][,,'N_TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'N_TreatmentAgriculture']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


N_study_Forestry <- all_coef[[1]][,,'N_TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'N_TreatmentForestry']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

N_study_Urban <- all_coef[[1]][,,'N_TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'N_TreatmentUrbanization']),
         N = Estimate,
         N_lower = Q5,
         N_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# species richness
S_study_reference <- all_coef[[1]][,,'S_Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'S_Intercept']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_study_Agriculture <- all_coef[[1]][,,'S_TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'S_TreatmentAgriculture']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


S_study_Forestry <- all_coef[[1]][,,'S_TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'S_TreatmentForestry']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_study_Urban <- all_coef[[1]][,,'S_TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'S_TreatmentUrbanization']),
         S = Estimate,
         S_lower = Q5,
         S_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# rarefied richness
Sn_study_reference <- all_coef[[1]][,,'Sn_Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Sn_Intercept']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_study_Agriculture <- all_coef[[1]][,,'Sn_TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Sn_TreatmentAgriculture']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


Sn_study_Forestry <- all_coef[[1]][,,'Sn_TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Sn_TreatmentForestry']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

Sn_study_Urban <- all_coef[[1]][,,'Sn_TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Sn_TreatmentUrbanization']),
         Sn = Estimate,
         Sn_lower = Q5,
         Sn_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

# evenness (S_PIE)
S_PIE_study_reference <- all_coef[[1]][,,'Spie_Intercept'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Spie_Intercept']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Reference stream') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_study_Agriculture <- all_coef[[1]][,,'Spie_TreatmentAgriculture'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Spie_TreatmentAgriculture']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Agriculture') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)


S_PIE_study_Forestry <- all_coef[[1]][,,'Spie_TreatmentForestry'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Spie_TreatmentForestry']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Forestry') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)

S_PIE_study_Urban <- all_coef[[1]][,,'Spie_TreatmentUrbanization'] %>% 
  as_tibble() %>% 
  mutate(Study = rownames(all_coef[[1]][,,'Spie_TreatmentUrbanization']),
         S_PIE = Estimate,
         S_PIE_lower = Q5,
         S_PIE_upper = Q95,
         LU = 'Urban') %>% 
  select(-Estimate, -Est.Error, -Q5, -Q95)



fwater_study_LU_multi <- bind_rows(
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
fwater_study_LU_multi <- fwater_study_LU_multi %>% 
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


fwater_study_LU_multi <- fwater_study_LU_multi %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'))

fwater_study_LU_multi$LU <- factor(fwater_study_LU_multi$LU,
                         levels = c('Reference stream',
                                    'Agriculture',      
                                    'Forestry',
                                    'Urban'),
                         labels = c('Reference stream',
                                    'Agriculture',      
                                    'Forestry',
                                    'Urban')
)

fwater_study_LU_multi$componentChange <- factor(fwater_study_LU_multi$componentChange,
                                      levels = c('No change', 
                                                 'N only', 'ENSPIE only', 'Sn only', 'S only', 
                                                 'S & Sn', 'S & ENSPIE', 'S & N',
                                                 'Sn & ENSPIE', 'Sn & N',  
                                                 'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                                                 'S, Sn, N & ENSPIE'))


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

fwater_study_LU_multi <- fwater_study_LU_multi %>% 
  # add indicator for database
  mutate(db = 'Freshwater LU')

# posterior distributions of change estimates for SS-levels
fw_study_levels = fwater_multi4_fit$data %>%
  as_tibble() %>%
  distinct(Study, Treatment) %>%
  mutate(level = Study,
         LU2 = ifelse(Treatment=='Reference', 'Intercept', paste0('Treatment', Treatment))) %>%
  group_by(Study, Treatment) %>% 
  nest(data = c(level, LU2))

fw_study_posterior <- fw_study_levels %>%
  mutate(N_global = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                          pars = paste('b_N_', as.character(.x$LU2), sep=''),
                                                          fixed = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  N = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                          pars = paste('r_Study__N[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                          fixed = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  S_global = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                          pars = paste('b_S_', as.character(.x$LU2), sep=''),
                                                          fixed = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  S = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                   pars = paste('r_Study__S[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                   fixed = TRUE,
                                                   # want 1000 samples
                                                   subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                otherwise = NULL)
  ),
  ENSPIE_global = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                          pars = paste('b_Spie_', as.character(.x$LU2), sep=''),
                                                          fixed = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  ENSPIE = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                        pars = paste('r_Study__Spie[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                        fixed = TRUE,
                                                        # want 1000 samples
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                     otherwise = NULL)
  ),
  Sn_global = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                          pars = paste('b_Sn_', as.character(.x$LU2), sep=''),
                                                          fixed = TRUE,
                                                          # want 1000 samples
                                                          subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                       otherwise = NULL)
  ),
  Sn = purrr::map(data, possibly(~posterior_samples(fwater_multi4_fit, 
                                                    pars = paste('r_Study__Sn[', as.character(.x$level), ',', as.character(.x$LU2),']', sep=''),
                                                    fixed = TRUE,
                                                    # want 1000 samples
                                                    subset = floor(runif(n = 1000, 1, max = 2000))) %>% unlist() %>% as.numeric(),
                                 otherwise = NULL)
  )
)

fw_study_posterior$Treatment <- factor(fw_study_posterior$Treatment,
                                levels = c('Reference',
                                           'Agriculture',      
                                           'Forestry',
                                           'Urbanization'),
                                labels = c('Reference stream',
                                           'Agriculture',      
                                           'Forestry',
                                           'Urban')
)

fw_study_posterior <- fw_study_posterior %>% 
  # add indicator for database
  mutate(db = 'Freshwater LU')

fw_study_posterior <- fw_study_posterior %>% 
  select(-data) %>% 
  unnest(cols = c(N_global, N,
                  S_global, S,
                  ENSPIE_global, ENSPIE,
                  Sn_global, Sn))


# fwater_posterior distributions of the correlations estimated by the model
fwater_posterior <- as.mcmc(fwater_multi4_fit, combine_chains = TRUE, 
                              pars = "^cor") %>% 
  as_tibble()

# want to get the components to look at how change was realised
fwater_cor_long <-
  bind_rows(
    # Primary veg (completeneStudy only; reference for change)
    fwater_posterior %>% 
      select(corS_N = cor_Study__S_Intercept__N_Intercept ,
             corS_S_PIE = cor_Study__S_Intercept__Spie_Intercept,
             corS_Sn = cor_Study__S_Intercept__Sn_Intercept,
             corN_S_PIE = cor_Study__N_Intercept__Spie_Intercept,
             corSn_N = cor_Study__N_Intercept__Sn_Intercept,
             corS_PIE_Sn = cor_Study__Spie_Intercept__Sn_Intercept) %>% 
      mutate(LU = 'Reference stream'),
    # mature secondary ver
    fwater_posterior %>% 
      select(corS_N = cor_Study__S_TreatmentAgriculture__N_TreatmentAgriculture ,
             corS_S_PIE = cor_Study__S_TreatmentAgriculture__Spie_TreatmentAgriculture,
             corS_Sn = cor_Study__S_TreatmentAgriculture__Sn_TreatmentAgriculture,
             corN_S_PIE = cor_Study__N_TreatmentAgriculture__Spie_TreatmentAgriculture,
             corSn_N = cor_Study__N_TreatmentAgriculture__Sn_TreatmentAgriculture,
             corS_PIE_Sn = cor_Study__Spie_TreatmentAgriculture__Sn_TreatmentAgriculture) %>% 
      mutate(LU = 'Agriculture'),
    # intermediate secondary veg
    fwater_posterior %>% 
      select(corS_N = cor_Study__S_TreatmentForestry__N_TreatmentForestry ,
             corS_S_PIE = cor_Study__S_TreatmentForestry__Spie_TreatmentForestry,
             corS_Sn = cor_Study__S_TreatmentForestry__Sn_TreatmentForestry,
             corN_S_PIE = cor_Study__N_TreatmentForestry__Spie_TreatmentForestry,
             corSn_N = cor_Study__N_TreatmentForestry__Sn_TreatmentForestry,
             corS_PIE_Sn = cor_Study__Spie_TreatmentForestry__Sn_TreatmentForestry) %>% 
      mutate(LU = 'Forestry'),
    # young secondary veg
    fwater_posterior %>% 
      select(corS_N = cor_Study__S_TreatmentUrbanization__N_TreatmentUrbanization ,
             corS_S_PIE = cor_Study__S_TreatmentUrbanization__Spie_TreatmentUrbanization,
             corS_Sn = cor_Study__S_TreatmentUrbanization__Sn_TreatmentUrbanization,
             corN_S_PIE = cor_Study__N_TreatmentUrbanization__Spie_TreatmentUrbanization,
             corSn_N = cor_Study__N_TreatmentUrbanization__Sn_TreatmentUrbanization,
             corS_PIE_Sn = cor_Study__Spie_TreatmentUrbanization__Sn_TreatmentUrbanization) %>% 
      mutate(LU = 'Urban'))

fwater_cor_long$LU <- factor(fwater_cor_long$LU,
                               levels = c('Reference stream',
                                          'Agriculture',      
                                          'Forestry',
                                          'Urban'))
