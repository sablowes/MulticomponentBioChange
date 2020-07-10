# wrangle univariate fits to fragSAD data (from Chase et al. analysis)

# code to wrangle the coefficients for the fragment area regressions (main text results)
load('~/Dropbox/1current/fragmentation_synthesis/FragFrame_1/main_results/fragSize_ref.Rdata')

frag <- read_csv('~/Dropbox/1current/fragmentation_synthesis/FragFrame_1/intermediate_results/2_biodiv_frag_fcont_10_mabund_as_is.csv')

# add mean centred (log) fragsize
frag$c.lfs <- log(frag$frag_size_num) - mean(log(frag$frag_size_num))

# load the meta data
meta <- read_delim('~/Dropbox/1current/fragmentation_synthesis/FragFrame_1/data/new_meta_2_merge.csv',  delim =';') %>% 
   dplyr::rename(dataset_label = dataset_id) 

frag <- left_join(frag, 
                  meta,
                  by = 'dataset_label')

#------wrangle for plotting

# for plotting the random-effects----------------
Sstd_lognorm_fragSize_coef <- coef(Sstd_lognorm_fragSize, robust = TRUE, probs = c(0.05, 0.95))
Sn_lognorm_fragSize_coef <- coef(Sn_lognorm_fragSize, robust = TRUE, probs = c(0.05, 0.95))
Scov_lognorm_fragSize_coef <- coef(Scov_lognorm_fragSize, robust = TRUE, probs = c(0.05, 0.95))
Schao_lognorm_fragSize_coef <- coef(S_chao_lognorm_fragSize, robust = TRUE, probs = c(0.05, 0.95))
S_PIE_fS_coef <- coef(S_PIE_lognorm_fragSize, robust = TRUE, probs = c(0.05, 0.95))
Nstd_fS_coef <- coef(Nstd_lognorm_fragSize, robust = TRUE, probs = c(0.05, 0.95))

S_fS_group_coefs <- bind_cols(Sstd_lognorm_fragSize_coef[[1]][,,'c.lfs'] %>% 
                                                  as_tibble() %>% 
                                                  mutate(Study = rownames(Sstd_lognorm_fragSize_coef[[1]][,,'c.lfs']),
                                                         S = Estimate,
                                                         S_lower = Q5,
                                                         S_upper = Q95) %>% 
                                                  dplyr::select(-Estimate, -Est.Error, -Q5, -Q95)) 

Sn_fS_group_coefs <- bind_cols(Sn_lognorm_fragSize_coef[[1]][,,'c.lfs'] %>% 
                                                 as_tibble() %>% 
                                                 mutate(Study = rownames(Sn_lognorm_fragSize_coef[[1]][,,'c.lfs']),
                                                        Sn = Estimate,
                                                        Sn_lower = Q5,
                                                        Sn_upper = Q95) %>% 
                                                 dplyr::select(-Estimate, -Est.Error, -Q5, -Q95)) 

N_fS_group_coefs <- bind_cols(Nstd_fS_coef[[1]][,,'c.lfs'] %>% 
                                                 as_tibble() %>% 
                                                 mutate(Study = rownames(Nstd_fS_coef[[1]][,,'c.lfs']),
                                                        N = Estimate,
                                                        N_lower = Q5,
                                                        N_upper = Q95) %>% 
                                                 dplyr::select(-Estimate, -Est.Error, -Q5, -Q95)) 

S_PIE_fS_group_coefs <- bind_cols(S_PIE_fS_coef[[1]][,,'c.lfs'] %>% 
                                                 as_tibble() %>% 
                                                 mutate(Study = rownames(S_PIE_fS_coef[[1]][,,'c.lfs']),
                                                        S_PIE = Estimate,
                                                        S_PIE_lower = Q5,
                                                        S_PIE_upper = Q95) %>% 
                                                 dplyr::select(-Estimate, -Est.Error, -Q5, -Q95)) 


fSAD_uni_coefs <-  left_join(S_fS_group_coefs, 
            N_fS_group_coefs,
            by = c('Study')) %>% 
    left_join(S_PIE_fS_group_coefs,
              by = c('Study')) %>% 
    left_join(Sn_fS_group_coefs,
              by = c('Study')) %>% 
  mutate(alphaS = ifelse((S_lower < 0 & S_upper > 0), 0.95, 1),
         alphaN = ifelse((N_lower < 0 & N_upper > 0), 0.95, 1),
         alphaENSPIE = ifelse((S_PIE_lower < 0 & S_PIE_upper > 0), 0.95, 1),
         alphaSn = ifelse((Sn_lower < 0 & Sn_upper > 0), 0.95, 1))

# add component change indicators
fSAD_uni_coefs_partial <- fSAD_uni_coefs %>% 
  # remove rows with no estimate for Sn
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


missing_alphaSn_rows_fSAD <- fSAD_uni_coefs %>% 
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


fSAD_uni_coefs <- bind_rows(fSAD_uni_coefs_partial,
                            missing_alphaSn_rows_fSAD)


fSAD_uni_coefs <- fSAD_uni_coefs %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'))

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

fSAD_uni_coefs <- fSAD_uni_coefs %>% 
  # add indicator for database
  mutate(db = 'fragSAD')
