# fit 'multivariate' model to fragSAD (≥ 4 responses!)

# load the data
temp <- '~/Dropbox/1current/fragmentation_synthesis/FragFrame_1/'

frag <- read_csv(paste0(temp, 'intermediate_results/2_biodiv_frag_fcont_10_mabund_as_is.csv'))

# add mean centred (log) fragsize
frag$c.lfs <- log(frag$frag_size_num) - mean(log(frag$frag_size_num))


#----- simplest model: diversity as a function of fragment size; 
# allow fragment size (slope) to vary by study (varying intercept)----

# results presented in ms are fit with default, weakly regularising priors
# that are informed by the data (see brms documentation)

# sample effort standardised species richness 
Sstd_model <- bf(S_std_mean ~ c.lfs + (c.lfs | p | dataset_label), 
                 family = lognormal())

Sn_model <- bf(S_n_mean ~ c.lfs + (c.lfs | p | dataset_label),
               family = lognormal())

Scov_model <- bf(S_cov_mean ~ c.lfs + (c.lfs | p | dataset_label), 
               family = lognormal())

S_PIE_model <- bf(S_PIE_mean ~ c.lfs + (c.lfs | p | dataset_label), 
               family = lognormal())

Schao_model <- bf(S_chao_mean ~ c.lfs + (c.lfs | p | dataset_label), 
               family = lognormal())

N_model <- bf(N_std ~ c.lfs + (c.lfs | p | dataset_label), 
               family = lognormal())

fragSAD_multi4_fit <- brm(Sstd_model + N_model + S_PIE_model + Sn_model +
                    set_rescor(FALSE),
                  data= frag %>% filter(!is.na(S_PIE_mean) & S_std_mean>0 &
                                          S_n_mean > 0),
                  # prior=hier_prior,
                  # inits = '0',
                  #init_r = 0.001,
                  # control = list(adapt_delta=0.95),
                  cores = 4,
                  chains = 4)

# save(fragSAD_multi4_fit, file='~/Dropbox/1current/BioTime/multidimensionalChangeMS/data_model_fits/fragSAD_multi4.Rdata')

plot(fragSAD_multi4_fit)

fragSAD_S_pp <- pp_check(fragSAD_multi4_fit, resp = 'Sstdmean') +
  scale_x_continuous(name = 'Species richness',
                     trans = 'log') +
  theme(legend.position = c(1,1),
  legend.justification = c(1,1))
  
fragSAD_N_pp <- pp_check(fragSAD_multi4_fit, resp = 'Nstd') +
  scale_x_continuous(name = 'Total abundance',
                     trans = 'log') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

fragSAD_S_PIE_pp <- pp_check(fragSAD_multi4_fit, resp = 'SPIEmean') +
  scale_x_continuous(name = 'Evenness (S_PIE)',
                     trans = 'log') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

fragSAD_Sn_pp <- pp_check(fragSAD_multi4_fit, resp = 'Snmean') +
  scale_x_continuous(name = 'Rarefied richness (Sn)',
                     trans = 'log') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

cowplot::plot_grid(fragSAD_S_pp,
                   fragSAD_Sn_pp,
                   fragSAD_S_PIE_pp,
                   fragSAD_N_pp,
                   nrow = 2, align = 'hv')

ggsave('~/Dropbox/1current/BioTime/multidimensionalChangeMS/Figs/fragSAD_multi4_pp.png',
       width = 200, height = 200, units = 'mm')

fragSAD_S_N_fit <- brm(Sstd_model + N_model +
                            set_rescor(FALSE),
                          data= frag %>% filter(!is.na(S_PIE_mean) & S_std_mean>0 &
                                                  S_n_mean > 0 & S_cov_mean > 0),
                          cores = 4,
                          chains = 4)

fragSAD_S_S_PIE_fit <- brm(Sstd_model + S_PIE_model +
                         set_rescor(FALSE),
                       data= frag %>% filter(!is.na(S_PIE_mean) & S_std_mean>0 &
                                               S_n_mean > 0 & S_cov_mean > 0),
                       cores = 4,
                       chains = 4)

fragSAD_S_Sn_fit <- brm(Sstd_model + Sn_model +
                         set_rescor(FALSE),
                       data= frag %>% filter(!is.na(S_PIE_mean) & S_std_mean>0 &
                                               S_n_mean > 0 & S_cov_mean > 0),
                       cores = 4,
                       chains = 4)

fragSAD_S_PIE_Sn_fit <- brm(S_PIE_model + Sn_model +
                              set_rescor(FALSE),
                            data= frag %>% filter(!is.na(S_PIE_mean) & S_std_mean>0 &
                                                    S_n_mean > 0),
                            cores = 4,
                            chains = 4)

fragSAD_N_Snfit <- brm(N_model + Sn_model +
                         set_rescor(FALSE),
                       data= frag %>% filter(!is.na(S_PIE_mean) & S_std_mean>0 &
                                               S_n_mean > 0 & S_cov_mean > 0),
                       cores = 4,
                       chains = 4)

fragSAD_N_S_PIE_fit <- brm(N_model + S_PIE_model +
                         set_rescor(FALSE),
                       data= frag %>% filter(!is.na(S_PIE_mean) & S_std_mean>0 &
                                               S_n_mean > 0 & S_cov_mean > 0),
                       cores = 4,
                       chains = 4)

save(fragSAD_S_N_fit,
     fragSAD_S_S_PIE_fit,
     fragSAD_S_Sn_fit,
     fragSAD_S_PIE_Sn_fit,
     fragSAD_N_Snfit,
     fragSAD_N_S_PIE_fit,
     file = '~/Documents/work_localOnly/BioTIME_analyses/multiC_alpha/PREDICTS_mult2_fits.Rdata')
