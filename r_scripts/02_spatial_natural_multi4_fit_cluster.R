# fit model to spatial data with natural environmental variation
rm(list=ls())
library(brms)
library(dplyr)
library(tidyr)

## the data
load('/data/idiv_chase/sablowes/multiC_alpha/data/spatial_natural.Rdata')

# no non-varying intercept; varying intercepts and slopes for each sites to the reference within studies
S_model = bf(S ~ 0 + data_source + (newID | p | dataset_id) + (1 | obsID),
             family = 'poisson')
N_model = bf(N ~ 0 + data_source + (newID | p | dataset_id),
             family = lognormal())
Sn_model = bf(Sn ~ 0 + data_source + (newID | p | dataset_id), 
              family = lognormal())
ENSPIE_model = bf(ENSPIE ~ 0 + data_source + (newID | p | dataset_id),
                  family = lognormal())

spatial_10sites0obsID <- brm(S_model + N_model + Sn_model + ENSPIE_model +
                               set_rescor(FALSE),
                             prior = c(prior('normal(0,1)', class = 'sigma', resp = 'N'),
                                       prior('normal(0,1)', class = 'sigma', resp = 'ENSPIE'),
                                       prior('normal(0,1)', class = 'sigma', resp = 'Sn'),
                                       # sd of varying slopes and intercepts
                                       prior('normal(0,1)', class = 'sd', resp = 'S'),
                                       prior('normal(0,1)', class = 'sd', resp = 'N'),
                                       prior('normal(0,1)', class = 'sd', resp = 'ENSPIE'),
                                       prior('normal(0,1)', class = 'sd', resp = 'Sn'),
                                       # non-varying slopes
                                       prior('normal(0,1)', class = 'b', resp = 'S'),
                                       prior('normal(0,1)', class = 'b', resp = 'N'),
                                       prior('normal(0,1)', class = 'b', resp = 'ENSPIE'),
                                       prior('normal(0,1)', class = 'b', resp = 'Sn')),
                             data = spatial_grad,
                             iter = 4000, thin = 2,
                             cores = 4,
                             control = list(adapt_delta=0.995, max_treedepth = 12),
                             init_r = 0.1#,
                             # sample_prior = 'only'
)

save(spatial_10sites0obsID, file=Sys.getenv('OFILE'))