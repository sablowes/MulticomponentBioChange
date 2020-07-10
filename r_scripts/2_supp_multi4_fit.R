# fit multivariate model to supp data on cluster
rm(list=ls())

##	
#withr::with_libpaths(new = '/home/blowes/R/x86_64-pc-linux-gnu-library/3.3/', library(brms))
library(brms)
library(dplyr)
library(tidyr)

## the data
# load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/intermediate_results/supp_alpha.Rdata')
load('/data/idiv_chase/sablowes/multiC_alpha/data/supp_alpha.Rdata')


S_formula <- bf(S ~ 1 + (1 |referenceID) + (trt | p | group) + (1 | siteID)) +
  poisson()

N_formula <- bf(N ~ 1 + (1 | referenceID) + (trt | p | group)) +
  lognormal()

ENSPIE_formula <- bf(ENSPIE ~ 1 + (1 | referenceID) + (trt | p | group)) +
  lognormal()

Sn_formula <- bf(Sn ~ 1 + (1 | referenceID) + (trt | p | group)) +
  lognormal()

supp_multi4c_fit <- brm(S_formula + N_formula + ENSPIE_formula + Sn_formula +
                          set_rescor(FALSE),
                        data= supp_alpha,
                        prior = c(prior('normal(0,1)', class = 'sigma', resp = 'N'),
                                  prior('normal(0,1)', class = 'sigma', resp = 'ENSPIE'),
                                  prior('normal(0,1)', class = 'sigma', resp = 'Sn'),
                                  # sd of varying slopes and intercepts
                                  prior('normal(0,1)', class = 'sd', resp = 'S'),
                                  prior('normal(0,1)', class = 'sd', resp = 'N'),
                                  prior('normal(0,1)', class = 'sd', resp = 'ENSPIE'),
                                  prior('normal(0,1)', class = 'sd', resp = 'Sn'),
                                  # non-varying intercepts
                                  prior('normal(0,1)', class = 'Intercept', resp = 'S'),
                                  prior('normal(0,1)', class = 'Intercept', resp = 'N'),
                                  prior('normal(0,1)', class = 'Intercept', resp = 'ENSPIE'),
                                  prior('normal(0,1)', class = 'Intercept', resp = 'Sn')),
                        control = list(adapt_delta=0.9),
                        iter = 8000,
                        warmup = 1000,
                        thin = 7,
                        # inits = '0',
                        cores = 4,
                        chains = 4)

save(supp_multi4c_fit, file=Sys.getenv('OFILE'))