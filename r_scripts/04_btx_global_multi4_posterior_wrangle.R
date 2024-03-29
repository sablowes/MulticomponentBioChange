# code to extract draws from the posterior distributions for the multivariate model
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

##----biotimeEx
load(paste0(path2wd, 'data_model_fits/btx_multi4_fit_global.Rdata'))

# only study-level (varying) slopes and intercepts in this model; no global estimates needed
btx_study_levels <- btx_multi4_fit_global$data %>% 
  as_tibble() %>% 
  distinct(study_trt) %>% 
  mutate(level = gsub(" ", ".", study_trt)) %>%
  nest(data = c(level)) 

btx_study_sample_posterior <- btx_study_levels %>%
  mutate(N_global = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                                  pars = 'b_N_cYear',
                                                                  fixed = TRUE,
                                                                  # want 1000 samples
                                                                  subset = seq(1, 4000, by = 4))
                                         %>% unlist() %>% as.numeric()),
         N_study = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                       pars = paste('r_study_trt__N[', as.character(.x$level), ',cYear]', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         Sn_global = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                        pars = 'b_Sn_cYear',
                                                        fixed = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()),
         Sn_study = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                        pars = paste('r_study_trt__Sn[', as.character(.x$level), ',cYear]', sep=''),
                                                        fixed = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()),
         S_global = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                         pars = 'b_S_cYear',
                                                         fixed = TRUE,
                                                         # want 1000 samples
                                                         subset = seq(1, 4000, by = 4))
                                %>% unlist() %>% as.numeric()),
         S_study = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                       pars = paste('r_study_trt__S[', as.character(.x$level), ',cYear]', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         ENSPIE_global = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                         pars = 'b_ENSPIE_cYear',
                                                         fixed = TRUE,
                                                         # want 1000 samples
                                                         subset = seq(1, 4000, by = 4))
                                %>% unlist() %>% as.numeric()),
         ENSPIE_study = purrr::map(data, ~posterior_samples(btx_multi4_fit_global, 
                                                            pars = paste('r_study_trt__ENSPIE[', as.character(.x$level), ',cYear]', sep=''),
                                                            fixed = TRUE,
                                                            # want 1000 samples
                                                            subset = seq(1, 4000, by = 4))
                                   %>% unlist() %>% as.numeric()))


btx_study_sample_posterior <- btx_study_sample_posterior %>% 
  select(-data) %>% 
  unnest(cols = c(N_global, N_study, 
                  Sn_global,Sn_study, 
                  S_global, S_study, 
                  ENSPIE_global, ENSPIE_study)) 


btx_study_summary <- btx_study_sample_posterior %>% 
  group_by(study_trt) %>% 
  summarise(N_slope = median(N_global + N_study),
            N_lower = quantile(N_global + N_study, probs = 0.05),
            N_upper = quantile(N_global + N_study, probs = 0.95),
            N_change = ifelse((N_upper > 0 & N_lower < 0), 0.9, 1),
            S_slope = median(S_global + S_study),
            S_lower = quantile(S_global + S_study, probs = 0.05),
            S_upper = quantile(S_global + S_study, probs = 0.95),
            S_change = ifelse((S_upper > 0 & S_lower < 0), 0.9, 1),
            Sn_slope = median(Sn_global + Sn_study),
            Sn_lower = quantile(Sn_global + Sn_study, probs = 0.05),
            Sn_upper = quantile(Sn_global + Sn_study, probs = 0.95),
            Sn_change = ifelse((Sn_upper > 0 & Sn_lower < 0), 0.9, 1),
            ENSPIE_slope = median(ENSPIE_global + ENSPIE_study),
            ENSPIE_lower = quantile(ENSPIE_global + ENSPIE_study, probs = 0.05),
            ENSPIE_upper = quantile(ENSPIE_global + ENSPIE_study, probs = 0.95),
            ENSPIE_change = ifelse((ENSPIE_upper > 0 & ENSPIE_lower < 0), 0.9, 1))



btx_study_summary <- btx_study_summary %>% 
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
         componentChange_sign = ifelse(componentChange=='N & ENSPIE', sign(N_slope) * sign(ENSPIE_slope),
                                       ifelse(componentChange=='S & N', sign(S_slope) * sign(N_slope),
                                              ifelse(componentChange=='Sn & N', sign(Sn_slope) * sign(N_slope),
                                                     ifelse(componentChange=='S & Sn', sign(S_slope) * sign(Sn_slope),
                                                            ifelse(componentChange=='S & ENSPIE', sign(S_slope) * sign(ENSPIE_slope),
                                                                   ifelse(componentChange=='Sn & ENSPIE', sign(Sn_slope) * sign(ENSPIE_slope),
                                                                          ifelse(componentChange=='Sn, N & ENSPIE', sign(Sn_slope) * sign(N_slope) * sign(ENSPIE_slope),
                                                                                 ifelse(componentChange=='S, Sn, N & ENSPIE', sign(S_slope) * sign(Sn_slope) * sign(N_slope) * sign(ENSPIE_slope),
                                                                                        ifelse(componentChange=='S, N & ENSPIE', sign(S_slope) * sign(N_slope) * sign(ENSPIE_slope), NA)))))))))
  )

btx_study_summary$componentChange <- factor(btx_study_summary$componentChange,
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




# get posterior for correlation across studies in slope estiamtes
# posterior distributions of the correlations estimated by the model
btx_posterior_cor <- as.mcmc(btx_multi4_fit_global, combine_chains = TRUE, 
                             pars = "^cor") %>% 
  as_tibble()

# only want study-level correlations in slopes 
# (though slope-intercept correlations do show whether rates of change are strongly related
# to richness etc...)
btx_cor_long <- btx_posterior_cor %>% 
  select(corS_N =  cor_study_trt__S_cYear__N_cYear,
         corS_S_PIE = cor_study_trt__S_cYear__ENSPIE_cYear,
         corS_Sn = cor_study_trt__S_cYear__Sn_cYear,
         corN_S_PIE = cor_study_trt__N_cYear__ENSPIE_cYear,
         corSn_N = cor_study_trt__N_cYear__Sn_cYear,
         corS_PIE_Sn = cor_study_trt__ENSPIE_cYear__Sn_cYear
  )

# add indicator for database
btx_study_summary <- btx_study_summary %>% 
  mutate(db = 'Experimental (time series)')

btx_study_sample_posterior <- btx_study_sample_posterior %>% 
  mutate(db = 'Experimental (time series)')

# want the sigmas for each response for simulating population of change
btx_sigma_post <- as.mcmc(btx_multi4_fit_global, combine_chains = TRUE, 
                          pars = "^sd") %>% 
  as_tibble()

btx_sigma_long <- btx_sigma_post %>% 
  select(sigmaN = sd_study_trt__N_cYear,
         sigmaENSPIE = sd_study_trt__ENSPIE_cYear,
         sigmaSn = sd_study_trt__Sn_cYear,
         sigmaS = sd_study_trt__S_cYear) %>% 
  mutate(db = 'Experimental (time series)')

save(btx_study_summary, btx_study_sample_posterior, btx_cor_long,
     file = paste0(path2wd, 'multiComponentChange/results/btx_multi4_global_results.Rdata'))
