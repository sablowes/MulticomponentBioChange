source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

## the data
load(paste0(path2wd, 'multiComponentChange/data/spatial_natural.Rdata'))

# non-varying intercept for data source; 
# varying intercepts and slopes for each site compared to the reference (intercept) within studies
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
                    iter = 4000,
                    cores = 4,
                    control = list(adapt_delta=0.995, max_treedepth = 12),
                    init_r = 0.1#,
                    # sample_prior = 'only'
                    )

save(spatial_10sites0obsID, file='~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/spatial_natural_fit0.Rdata')

n_pp <- pp_check(spatial_10sites0obsID, 
                 resp = 'N') +
  scale_x_continuous(trans = 'log10',
                     name = 'Number of individuals') +
  labs(tag = 'a') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

s_pp <- pp_check(spatial_10sites0obsID, 
                 resp = 'S') +
  scale_x_continuous(trans = 'log2',
                     name = 'Species richness') +
  labs(tag = 'b') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

sn_pp <- pp_check(spatial_10sites0obsID, 
                  resp = 'Sn') +
  scale_x_continuous(trans = 'log2',
                     name = 'Rarefied richness') +
  labs(tag = 'c') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

enspie_pp <- pp_check(spatial_10sites0obsID, 
                      resp = 'ENSPIE') +
  scale_x_continuous(trans = 'log2',
                     name = expression(paste('Evenness (', italic(S)[PIE], ')'))) +
  labs(tag = 'd') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

plot_grid(n_pp, s_pp, 
          sn_pp, enspie_pp,
          nrow = 2)

ggsave(filename = '~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/spatial_natural_pp0_feb21.png',
       width = 200, height = 200, units = 'mm')

pp_check(spatial_10sites0obsID, type = 'scatter_avg',
         resp = 'N') +
   scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  geom_abline(slope = 1, intercept = 0, lty = 2) 

pp_check(spatial_10sites0obsID, type = 'scatter_avg',
         resp = 'S') +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  geom_abline(slope = 1, intercept = 0, lty = 2) 

pp_check(spatial_10sites0obsID, type = 'scatter_avg',
         resp = 'Sn') +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  geom_abline(slope = 1, intercept = 0, lty = 2) 

pp_check(spatial_10sites0obsID, type = 'scatter_avg',
         resp = 'ENSPIE') +
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') +
  geom_abline(slope = 1, intercept = 0, lty = 2) 

