# code to extract draws from the posterior distributions for the multivariate model

##----count data only, whole time period--------
# load('~/Documents/work_localOnly/BioTIME_analyses/multiC_alpha/multi4-6484430.Rdata')
# this model has poisson distributions for all responses except total abundance
load('~/Documents/work_localOnly/BioTIME_analyses/multiC_alpha/multi4_pois2-6504373.Rdata')

# S_multi4_pp <- pp_check(multi4_fit, resp = 'S') +
#   scale_x_continuous(name = 'Species richness', trans = 'log')

# posterior distributions for the 'global' parameters
bt_global_posterior <- tibble(
  S_global = posterior_samples(multi4_fit,
                                 pars = 'b_S_cYEAR',
                                 exact = TRUE,
                                 # get every 4th draw of the posterior
                                 subset = seq(1, 4000, by = 4)) %>% 
                                 unlist() %>% as.numeric(),
  ENSPIE_global = posterior_samples(multi4_fit,
                                 pars = 'b_ENSPIE_cYEAR',
                                 exact = TRUE,
                                # get every 4th draw of the posterior
                                 subset = seq(1, 4000, by = 4))
                                 %>% unlist() %>% as.numeric(),
  Sn_global = posterior_samples(multi4_fit,
                                 pars = 'b_Sn_cYEAR',
                                 exact = TRUE,
                                 # get every 4th draw of the posterior
                                 subset = seq(1, 4000, by = 4))
                                 %>% unlist() %>% as.numeric(),
  N_global = posterior_samples(multi4_fit,
                               pars = 'b_N_cYEAR',
                               exact = TRUE,
                               # get every 4th draw of the posterior
                               subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()
)


bt_study_levels <- multi4_fit$data %>% 
  as_tibble() %>% 
  distinct(STUDY_ID) %>% 
  mutate(level = STUDY_ID) %>%
  nest(data = c(level)) 

bt_study_sample_posterior <- bt_study_levels %>%
  mutate(N_study = purrr::map(data, ~posterior_samples(multi4_fit, 
                                                       pars = paste('r_STUDY_ID__N[', as.character(.x$level), ',cYEAR]', sep=''),
                                                       exact = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         Sn_study = purrr::map(data, ~posterior_samples(multi4_fit, 
                                                 pars = paste('r_STUDY_ID__Sn[', as.character(.x$level), ',cYEAR]', sep=''),
                                                 exact = TRUE,
                                                 # want 1000 samples
                                                 subset = seq(1, 4000, by = 4))
                                                 %>% unlist() %>% as.numeric()),
         S_study = purrr::map(data, ~posterior_samples(multi4_fit, 
                                                    pars = paste('r_STUDY_ID__S[', as.character(.x$level), ',cYEAR]', sep=''),
                                                    exact = TRUE,
                                                    # want 1000 samples
                                                    subset = seq(1, 4000, by = 4))
                                                    %>% unlist() %>% as.numeric()),
         ENSPIE_study = purrr::map(data, ~posterior_samples(multi4_fit, 
                                                    pars = paste('r_STUDY_ID__ENSPIE[', as.character(.x$level), ',cYEAR]', sep=''),
                                                    exact = TRUE,
                                                    # want 1000 samples
                                                    subset = seq(1, 4000, by = 4))
                                                    %>% unlist() %>% as.numeric()))


bt_study_sample_posterior <- bt_study_sample_posterior %>% 
  select(-data) %>% 
  unnest()


bt_study_summary <- bind_cols(bt_study_sample_posterior,
                           N_global = rep(bt_global_posterior$N_global, times = length(unique(bt_study_sample_posterior$STUDY_ID))),
                           S_global = rep(bt_global_posterior$S_global, times = length(unique(bt_study_sample_posterior$STUDY_ID))),
                           Sn_global = rep(bt_global_posterior$Sn_global, times = length(unique(bt_study_sample_posterior$STUDY_ID))),
                           ENSPIE_global = rep(bt_global_posterior$ENSPIE_global, times = length(unique(bt_study_sample_posterior$STUDY_ID)))
                           ) %>% 
  group_by(STUDY_ID) %>% 
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



bt_study_summary <- bt_study_summary %>% 
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
# get the raw data for some metadata
# load('~/Dropbox/1current/BioTime/multidimensionalChangeMS/data_model_fits/rarefied_medians_count.Rdata')  
meta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEmetadataSept18.csv')

meta  <-  meta %>% 
  mutate(
    # simplified taxa
    taxa_mod = ifelse((TAXA=='Marine invertebrates' | TAXA=='Terrestrial invertebrates' | TAXA=='Freshwater invertebrates'),
                      'Invertebrates',
                      ifelse((TAXA=='Marine plants' | TAXA=='Terrestrial plants' | TAXA=='Freshwater plants'),
                             'Plant', TAXA)),
    Realm2 = ifelse(REALM!='Marine', 'Terrestrial/Freshwater', 'Marine')) %>% 
    distinct(REALM, Realm2, taxa_mod, STUDY_ID)


bt_study_summary <- left_join(bt_study_summary %>% 
                             mutate(STUDY_ID = as.character(STUDY_ID)),
                           meta %>% distinct(STUDY_ID, REALM, Realm2, taxa_mod) %>% 
                             mutate(STUDY_ID = as.character(STUDY_ID)),
                           by = 'STUDY_ID')

bt_study_summary$taxa_mod2 <- factor(bt_study_summary$taxa_mod, 
                               levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                          "All", "Marine invertebrates/plants", "Mammals", "Reptiles"),
                               labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                          "Multiple taxa", "Marine invertebrates/plants", "Mammals", "Reptiles"))

# shapes for taxa_mod
bt_shapes_mod2 = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
                'Plant' = 5, 'Multiple taxa' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8, 'Reptiles' = 4)

bt_study_summary$componentChange <- factor(bt_study_summary$componentChange,
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
bt_study_summary <- bt_study_summary %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'),
         change_stroke = ifelse(componentChange=='No change', 0.5, 1.1))

bt_study_summary <- bt_study_summary %>%
  mutate(realm_label = ifelse(REALM=='Marine', 'a.  Marine', 'b.   Terrestrial and freshwater'),
         realm_label2 = ifelse(REALM=='Marine', 'c.  Marine', 'd.   Terrestrial and freshwater'),
         realm_label3 = ifelse(REALM=='Marine', 'e.  Marine', 'f.   Terrestrial and freshwater'))


# join meta data to posterior samples
bt_study_sample_posterior <- left_join(bt_study_sample_posterior,
                                    meta %>% 
                                      mutate(STUDY_ID = as.character(STUDY_ID)),
                                    by = 'STUDY_ID')

bt_study_sample_posterior <- bt_study_sample_posterior %>% 
  mutate(N_global = rep(bt_global_posterior$N_global, time = n_distinct(STUDY_ID)),
         Sn_global = rep(bt_global_posterior$Sn_global, time = n_distinct(STUDY_ID)),
         S_global = rep(bt_global_posterior$S_global, time = n_distinct(STUDY_ID)),
         ENSPIE_global = rep(bt_global_posterior$ENSPIE_global, time = n_distinct(STUDY_ID))) %>% 
  mutate(db = 'BioTIME')

bt_study_sample_posterior$taxa_mod2 <- factor(bt_study_sample_posterior$taxa_mod, 
                                           levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                      "All", "Marine invertebrates/plants", "Mammals", "Reptiles"),
                                           labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                                      "Multiple taxa", "Marine invertebrates/plants", "Mammals", "Reptiles"))


# get posterior for correlation across studies in slope estiamtes
# posterior distributions of the correlations estimated by the model
bt_posterior_cor <- as.mcmc(multi4_fit, combine_chains = TRUE, 
                     pars = "^cor") %>% 
  as_tibble()

# only want study-level correlations in slopes 
# (though slope-intercept correlations do show whether rates of change are strongly related
# to richness etc...)
bt_cor_long <- bt_posterior_cor %>% 
  select(corS_N =  cor_STUDY_ID__S_cYEAR__N_cYEAR,
         corS_S_PIE = cor_STUDY_ID__S_cYEAR__ENSPIE_cYEAR,
         corS_Sn = cor_STUDY_ID__S_cYEAR__Sn_cYEAR,
         corN_S_PIE = cor_STUDY_ID__N_cYEAR__ENSPIE_cYEAR,
         corSn_N = cor_STUDY_ID__N_cYEAR__Sn_cYEAR,
         corS_PIE_Sn = cor_STUDY_ID__ENSPIE_cYEAR__Sn_cYEAR
  )

# add indicator for database
bt_study_summary <- bt_study_summary %>% 
  mutate(db = 'BioTIME')

# want the sigmas for each response for simulating population of change
bt_sigma_post <- as.mcmc(multi4_fit, combine_chains = TRUE, 
                            pars = "^sd") %>% 
  as_tibble()

bt_sigma_long <- bt_sigma_post %>% 
  select(sigmaN = sd_STUDY_ID__N_cYEAR,
         sigmaENSPIE = sd_STUDY_ID__ENSPIE_cYEAR,
         sigmaSn = sd_STUDY_ID__Sn_cYEAR,
         sigmaS = sd_STUDY_ID__S_cYEAR)

save(bt_global_posterior, bt_study_sample_posterior, bt_study_summary, 
     bt_cor_long, bt_sigma_long,
     bt_shapes_mod2, componentChange_col,
     file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')
