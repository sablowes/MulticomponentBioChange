# wrangle for supp_supp_multi4c_fit
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/supp_multi4a-6560307.Rdata')

# not used...model has no 'global' treatment effect, 
# so we don't need to add the random effect to anything
supp_intercept_posterior <- tibble(
  S_intercept = posterior_samples(supp_multi4c_fit,
                               pars = 'b_S_Intercept',
                               exact = TRUE,
                               # get every 4th draw of the posterior
                               subset = seq(1, 4000, by = 4)) %>% 
    unlist() %>% as.numeric(),
  ENSPIE_intercept = posterior_samples(supp_multi4c_fit,
                                    pars = 'b_ENSPIE_Intercept',
                                    exact = TRUE,
                                    # get every 4th draw of the posterior
                                    subset = seq(1, 4000, by = 4))
  %>% unlist() %>% as.numeric(),
  Sn_intercept = posterior_samples(supp_multi4c_fit,
                                pars = 'b_Sn_Intercept',
                                exact = TRUE,
                                # get every 4th draw of the posterior
                                subset = seq(1, 4000, by = 4))
  %>% unlist() %>% as.numeric(),
  N_intercept = posterior_samples(supp_multi4c_fit,
                               pars = 'b_N_Intercept',
                               exact = TRUE,
                               # get every 4th draw of the posterior
                               subset = seq(1, 4000, by = 4))
  %>% unlist() %>% as.numeric()
)

supp_trt_levels <- supp_multi4c_fit$data %>% 
  as_tibble() %>% 
  distinct(group) %>% 
  mutate(level = group) %>%
  nest(data = c(level)) 

supp_trt_sample_posterior <- supp_trt_levels %>%
  mutate(N_trt = purrr::map(data, ~posterior_samples(supp_multi4c_fit, 
                                                       pars = paste('r_group__N[', as.character(.x$level), ',trttreatment]', sep=''),
                                                       exact = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         Sn_trt = purrr::map(data, ~posterior_samples(supp_multi4c_fit, 
                                                        pars = paste('r_group__Sn[', as.character(.x$level), ',trttreatment]', sep=''),
                                                        exact = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()),
         S_trt = purrr::map(data, ~posterior_samples(supp_multi4c_fit, 
                                                       pars = paste('r_group__S[', as.character(.x$level), ',trttreatment]', sep=''),
                                                       exact = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         ENSPIE_trt = purrr::map(data, ~posterior_samples(supp_multi4c_fit, 
                                                            pars = paste('r_group__ENSPIE[', as.character(.x$level), ',trttreatment]', sep=''),
                                                            exact = TRUE,
                                                            # want 1000 samples
                                                            subset = seq(1, 4000, by = 4))
                                   %>% unlist() %>% as.numeric()))


supp_trt_sample_posterior <- supp_trt_sample_posterior %>% 
  select(-data) %>% 
  unnest(cols = c(N_trt, Sn_trt, S_trt, ENSPIE_trt))


supp_trt_summary <- supp_trt_sample_posterior %>% 
  group_by(group) %>% 
  summarise(N_slope = median(N_trt),
            N_lower = quantile(N_trt, probs = 0.05),
            N_upper = quantile(N_trt, probs = 0.95),
            N_change = ifelse((N_upper > 0 & N_lower < 0), 0.9, 1),
            S_slope = median(S_trt),
            S_lower = quantile(S_trt, probs = 0.05),
            S_upper = quantile(S_trt, probs = 0.95),
            S_change = ifelse((S_upper > 0 & S_lower < 0), 0.9, 1),
            Sn_slope = median(Sn_trt),
            Sn_lower = quantile(Sn_trt, probs = 0.05),
            Sn_upper = quantile(Sn_trt, probs = 0.95),
            Sn_change = ifelse((Sn_upper > 0 & Sn_lower < 0), 0.9, 1),
            ENSPIE_slope = median(ENSPIE_trt),
            ENSPIE_lower = quantile(ENSPIE_trt, probs = 0.05),
            ENSPIE_upper = quantile(ENSPIE_trt, probs = 0.95),
            ENSPIE_change = ifelse((ENSPIE_upper > 0 & ENSPIE_lower < 0), 0.9, 1))



supp_trt_summary <- supp_trt_summary %>% 
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
                                                                                                               ifelse(sizeS_N_and_ENSPIE==2, 'S, N & ENSPIE', 'No change')))))))))))))
  )

supp_trt_summary$componentChange <- factor(supp_trt_summary$componentChange,
                                           levels = c('No change', 
                                                      'N only', 'ENSPIE only', 'Sn only', 'S only', 
                                                      'S & Sn', 'S & ENSPIE', 'S & N',
                                                      'Sn & ENSPIE', 'Sn & N',  
                                                      'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                                                      'S, Sn, N & ENSPIE'))


# colour code for the componentChange covariate
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
                        'S, Sn, N & ENSPIE' = '#543005')


# want to make the 'no change' points smaller
supp_trt_summary <- supp_trt_summary %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'),
         change_stroke = ifelse(componentChange=='No change', 0.5, 1.1))


# get posterior for correlation across studies in slope estiamtes
# posterior distributions of the correlations estimated by the model
supp_posterior_cor <- as.mcmc(supp_multi4c_fit, combine_chains = TRUE, 
                            pars = "^cor") %>% 
  as_tibble()

# only want study-level correlations in slopes 
# (though slope-intercept correlations do show whether rates of change are strongly related
# to richness etc...)
supp_cor_long <- supp_posterior_cor %>% 
  select(corS_N =  cor_group__S_trttreatment__N_trttreatment,
         corS_S_PIE = cor_group__S_trttreatment__ENSPIE_trttreatment,
         corS_Sn = cor_group__S_trttreatment__Sn_trttreatment,
         corN_S_PIE = cor_group__N_trttreatment__ENSPIE_trttreatment,
         corSn_N = cor_group__N_trttreatment__Sn_trttreatment,
         corS_PIE_Sn = cor_group__ENSPIE_trttreatment__Sn_trttreatment
  )

# add indicator for database
supp_trt_summary <- supp_trt_summary %>% 
  mutate(db = 'Experiments (v. controls)')

# want the sigmas for each response for simulating population of change
supp_sigma_post <- as.mcmc(supp_multi4c_fit, combine_chains = TRUE, 
                         pars = "^sd") %>% 
  as_tibble()

supp_sigma_long <- supp_sigma_post %>% 
  select(sigmaN = sd_group__N_trttreatment,
         sigmaENSPIE = sd_group__ENSPIE_trttreatment,
         sigmaSn = sd_group__Sn_trttreatment,
         sigmaS = sd_group__S_trttreatment)
