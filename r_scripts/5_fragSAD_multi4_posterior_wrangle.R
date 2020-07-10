# plot correlations from the fit to fragSAD
library(brms)
library(tidyverse)

load(paste0(path2wd, 'results/fragSAD_multi4.Rdata'))

# posterior distributions for the 'global' parameters
fragSAD_global_posterior <- tibble(
  S_global = posterior_samples(fragSAD_multi4_fit,
                               pars = 'b_Sstdmean_c.lfs',
                               fixed = TRUE,
                               # get every 4th draw of the posterior
                               subset = seq(1, 2000, by = 2)) %>% 
    unlist() %>% as.numeric(),
  ENSPIE_global = posterior_samples(fragSAD_multi4_fit,
                                    pars = 'b_SPIEmean_c.lfs',
                                    fixed = TRUE,
                                    # get every 4th draw of the posterior
                                    subset = seq(1, 2000, by = 2))
  %>% unlist() %>% as.numeric(),
  Sn_global = posterior_samples(fragSAD_multi4_fit,
                                pars = 'b_Snmean_c.lfs',
                                fixed = TRUE,
                                # get every 4th draw of the posterior
                                subset = seq(1, 2000, by = 2))
  %>% unlist() %>% as.numeric(),
  N_global = posterior_samples(fragSAD_multi4_fit,
                               pars = 'b_Nstd_c.lfs',
                               fixed = TRUE,
                               # get every 4th draw of the posterior
                               subset = seq(1, 2000, by = 2))
  %>% unlist() %>% as.numeric()
)


fragSAD_study_levels <- fragSAD_multi4_fit$data %>% 
  as_tibble() %>% 
  distinct(dataset_label) %>% 
  mutate(level = dataset_label) %>%
  nest(data = c(level)) 

fragSAD_study_sample_posterior <- fragSAD_study_levels %>%
  mutate(N_study = purrr::map(data, ~posterior_samples(fragSAD_multi4_fit, 
                                                       pars = paste('r_dataset_label__Nstd[', as.character(.x$level), ',c.lfs]', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 2000, by = 2))
                              %>% unlist() %>% as.numeric()),
         Sn_study = purrr::map(data, ~posterior_samples(fragSAD_multi4_fit, 
                                                        pars = paste('r_dataset_label__Snmean[', as.character(.x$level), ',c.lfs]', sep=''),
                                                        fixed = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 2000, by = 2))
                               %>% unlist() %>% as.numeric()),
         S_study = purrr::map(data, ~posterior_samples(fragSAD_multi4_fit, 
                                                       pars = paste('r_dataset_label__Sstdmean[', as.character(.x$level), ',c.lfs]', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 2000, by = 2))
                              %>% unlist() %>% as.numeric()),
         ENSPIE_study = purrr::map(data, ~posterior_samples(fragSAD_multi4_fit, 
                                                            pars = paste('r_dataset_label__SPIEmean[', as.character(.x$level), ',c.lfs]', sep=''),
                                                            fixed = TRUE,
                                                            # want 1000 samples
                                                            subset = seq(1, 2000, by = 2))
                                   %>% unlist() %>% as.numeric()))


fragSAD_study_sample_posterior <- fragSAD_study_sample_posterior %>% 
  select(-data) %>% 
  unnest(cols = c(N_study, Sn_study, S_study, ENSPIE_study))

fragSAD_study_sample_posterior <- bind_cols(fragSAD_study_sample_posterior,
                                            N_global = rep(fragSAD_global_posterior$N_global, 
                                                           times = length(unique(fragSAD_study_sample_posterior$dataset_label))),
                                            S_global = rep(fragSAD_global_posterior$S_global, 
                                                           times = length(unique(fragSAD_study_sample_posterior$dataset_label))),
                                            Sn_global = rep(fragSAD_global_posterior$Sn_global, 
                                                            times = length(unique(fragSAD_study_sample_posterior$dataset_label))),
                                            ENSPIE_global = rep(fragSAD_global_posterior$ENSPIE_global, 
                                                                times = length(unique(fragSAD_study_sample_posterior$dataset_label)))
) %>% 
  mutate(db = 'fragSAD')


fragSAD_study_summary <- fragSAD_study_sample_posterior %>% 
  group_by(dataset_label) %>% 
  summarise(N_slope = median(N_study + N_global),
            N_lower = quantile(N_study + N_global, probs = 0.05),
            N_upper = quantile(N_study + N_global, probs = 0.95),
            N_change = ifelse((N_upper > 0 & N_lower < 0), 0.9, 1),
            S_slope = median(S_study + S_global),
            S_lower = quantile(S_study + S_global, probs = 0.05),
            S_upper = quantile(S_study + S_global, probs = 0.95),
            S_change = ifelse((S_upper > 0 & S_lower < 0), 0.9, 1),
            Sn_slope = median(Sn_study + Sn_global),
            Sn_lower = quantile(Sn_study + Sn_global, probs = 0.05),
            Sn_upper = quantile(Sn_study + Sn_global, probs = 0.95),
            Sn_change = ifelse((Sn_upper > 0 & Sn_lower < 0), 0.9, 1),
            ENSPIE_slope = median(ENSPIE_study + ENSPIE_global),
            ENSPIE_lower = quantile(ENSPIE_study + ENSPIE_global, probs = 0.05),
            ENSPIE_upper = quantile(ENSPIE_study + ENSPIE_global, probs = 0.95),
            ENSPIE_change = ifelse((ENSPIE_upper > 0 & ENSPIE_lower < 0), 0.9, 1))

fragSAD_study_summary <- fragSAD_study_summary %>% 
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

fragSAD_study_summary$componentChange <- factor(fragSAD_study_summary$componentChange,
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

fragSAD_study_summary <- fragSAD_study_summary %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'),
         change_stroke = ifelse(componentChange=='No change', 0.5, 1.1))

fragSAD_study_summary <- fragSAD_study_summary %>% 
  mutate(db = 'fragSAD')

# get posterior for correlation across studies in slope estiamtes
# posterior distributions of the correlations estimated by the model
fS_posterior_cor <- as.mcmc(fragSAD_multi4_fit, combine_chains = TRUE, 
                            pars = "^cor") %>% 
  as_tibble()

# only want study-level correlations in slopes 
# (though slope-intercept correlations do show whether rates of change are strongly related
# to richness etc...)
fS_cor_long <- fS_posterior_cor %>% 
  select(corS_N =  cor_dataset_label__Sstdmean_c.lfs__Nstd_c.lfs,
         corS_S_PIE = cor_dataset_label__Sstdmean_c.lfs__SPIEmean_c.lfs,
         corS_Sn = cor_dataset_label__Sstdmean_c.lfs__Snmean_c.lfs,
         corN_S_PIE = cor_dataset_label__Nstd_c.lfs__SPIEmean_c.lfs,
         corSn_N = cor_dataset_label__Nstd_c.lfs__Snmean_c.lfs,
         corS_PIE_Sn = cor_dataset_label__SPIEmean_c.lfs__Snmean_c.lfs
  )

