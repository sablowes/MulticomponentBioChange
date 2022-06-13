# fit multivariate model to experimental and perturbed time series
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

# these data were compiled and wrangled by Dr Alban Sagious
# code to download and wrangle data available at: https://github.com/MulticomponentBioChange/temporal_comparison_data_preparation
dat <- read_csv(paste0(path2wd, 'multiComponentChange/data/long_table.csv'))

good_dat <- dat %>% 
  filter(minN > 5) %>% 
  unite(study_trt, c(dataset_id, treatment), remove = F) %>% 
  mutate(cYear = year - mean(year)) 

# remove treatments with only a single observation (i.e., not time series)
remove <- good_dat %>% 
  group_by(study_trt) %>% 
  summarise(nyrs = n_distinct(cYear)) %>% 
  filter(nyrs==1)

good_dat <- good_dat %>% 
  filter(!study_trt %in% remove$study_trt) %>% 
  # combine site and block to a single covariate
  mutate(site = str_replace(site, ' ', '_')) %>% 
  unite(sb, c(site, block))


#  model with non-varying intercept and slope
S_model_s2 <- bf(S ~ cYear + (1 | dataset_id) + (cYear | p | study_trt) + (1| sb),
                family = lognormal())

N_model_s2 <- bf(N ~ cYear + (1 | dataset_id) +  (cYear | p | study_trt) + (1| sb),
                family = lognormal())

Sn_model_s2 <- bf(Sn ~ cYear + (1 | dataset_id) + (cYear | p | study_trt) + (1| sb),
                 family = lognormal())

S_PIE_model_s2 <- bf(ENSPIE ~ cYear + (1 | dataset_id) + (cYear | p | study_trt) + (1| sb),
                    family = lognormal())

btx_multi4_fit_global <- brm(S_model_s2 + N_model_s2 + S_PIE_model_s2 + Sn_model_s2 + 
                        set_rescor(FALSE),
                      data= good_dat,
                      prior = c(prior('normal(0,1)', class = 'sigma', resp = 'S'),
                                prior('normal(0,1)', class = 'sigma', resp = 'N'),
                                prior('normal(0,1)', class = 'sigma', resp = 'ENSPIE'),
                                prior('normal(0,1)', class = 'sigma', resp = 'Sn'),
                                # sd of varying intercepts and slopes
                                prior('normal(0,1)', class = 'sd', resp = 'S'),
                                prior('normal(0,1)', class = 'sd', resp = 'N'),
                                prior('normal(0,1)', class = 'sd', resp = 'ENSPIE'),
                                prior('normal(0,1)', class = 'sd', resp = 'Sn'),
                                # priors for non-varying slopes 
                                prior('normal(0,0.2)', class = 'b', coef = 'cYear', resp = 'S'),
                                prior('normal(0,0.2)', class = 'b', coef = 'cYear',  resp = 'N'),
                                prior('normal(0,0.2)', class = 'b', coef = 'cYear',  resp = 'ENSPIE'),
                                prior('normal(0,0.2)', class = 'b', coef = 'cYear',  resp = 'Sn'),
                                # non-varying intercepts 
                                prior('normal(0,1)', class = 'Intercept', resp = 'S'),
                                prior('normal(0,1)', class = 'Intercept', resp = 'N'),
                                prior('normal(0,1)', class = 'Intercept', resp = 'ENSPIE'),
                                prior('normal(0,1)', class = 'Intercept', resp = 'Sn')),
                      # control = list(adapt_delta=0.99),
                      # prior predictive
                      # sample_prior = 'only',
                      # init_r = 0.5,
                      iter = 3000,
                      cores = 4,
                      chains = 4)


save(btx_multi4_fit_global, 
     file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/btx_multi4_fit_global.Rdata')
