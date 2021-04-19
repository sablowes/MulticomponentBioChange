# posterior predictive checks
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

# spatial comparisons: land use change
load('~/Documents/work_localOnly/BioTIME_analyses/multiC_alpha/predicts_multi4a-6502421.Rdata')

pp_check(PREDICTS_multi4_fit, resp = 'S', type = 'stat_freqpoly_grouped', group = 'SS') 
ggsave('~/Desktop/S_pp_SS_frq_poly.png', width = 400, height = 300, units = 'mm')

pp_check(btx_multi4_fit_global, resp = 'Sn', type = 'stat_grouped', group = 'study_trt')
pp_check(PREDICTS_multi4_fit, resp = 'Ncorrected', type = 'stat_grouped', group = 'SS') + 
  scale_x_continuous(trans = 'log10')
ggsave('~/Desktop/N_pp_SS.png', width = 400, height = 300, units = 'mm')
pp_check(btx_multi4_fit_global, resp = 'ENSPIE', type = 'stat_grouped', group = 'study_trt')


n_pp <- pp_check(PREDICTS_multi4_fit, 
                 resp = 'Ncorrected') +
  scale_x_continuous(trans = 'log10',
                     name = '# individuals') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

s_pp <- pp_check(PREDICTS_multi4_fit, 
                 resp = 'S') +
  scale_x_continuous(trans = 'log2',
                     name = 'species richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

sn_pp <- pp_check(PREDICTS_multi4_fit,
                  resp = 'Sn5') +
  scale_x_continuous(trans = 'log2',
                     name = 'rarefied richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

enspie_pp <- pp_check(PREDICTS_multi4_fit,
                      resp = 'ENSPIE') +
  scale_x_continuous(trans = 'log2',
                     name = 'evenness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

plot_grid(n_pp, s_pp, 
          sn_pp, enspie_pp,
          nrow = 2)

ggsave(filename = '~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/predicts_postpred.png',
       width = 200, height = 200, units = 'mm')

rm(PREDICTS_multi4_fit)

# spatial comparison: natural
load('~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/spatial_natural_fit0_rPurschke.Rdata')

n_pp <- pp_check(spatial_10sites0obsID, 
                 resp = 'N') +
  scale_x_continuous(trans = 'log10',
                     name = '# individuals') +
  labs(tag = 'a') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15,0.9),
        legend.justification = c(1,1))

s_pp <- pp_check(spatial_10sites0obsID, 
                 resp = 'S') +
  scale_x_continuous(trans = 'log2',
                     name = 'species richness') +
  labs(tag = 'b') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15,0.9),
        legend.justification = c(1,1))

sn_pp <- pp_check(spatial_10sites0obsID, 
                  resp = 'Sn') +
  scale_x_continuous(trans = 'log2',
                     name = 'rarefied richness') +
  labs(tag = 'c') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15,0.9),
        legend.justification = c(1,1))

enspie_pp <- pp_check(spatial_10sites0obsID, 
                      resp = 'ENSPIE') +
  scale_x_continuous(trans = 'log2',
                     name = 'evenness') +
  labs(tag = 'd') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15,0.9),
        legend.justification = c(1,1))

plot_grid(n_pp, s_pp, 
          sn_pp, enspie_pp,
          nrow = 2) +
  draw_figure_label(label = 'Spatial comparisons: natural variation')

ggsave(filename = '~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/spatial_natural_pp0_feb21.png',
       width = 200, height = 200, units = 'mm')

# biotime pp_checks done on cluster 
# biotime
load('~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/bt_pp_plots-7140579.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/bt_pp_plots-7141495.Rdata')

plot_grid(N_pp_dens, S_pp_dens,
          Sn_pp_dens, ENSPIE_pp_dens, 
          nrow = 2, align = 'hv')
ggsave(filename = '~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/bt_pp.png',
       width = 200, height = 200, units = 'mm')

rm(N_pp_dens, S_pp_dens,
          Sn_pp_dens, ENSPIE_pp_dens)

load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/btx_multi4_fit_global.Rdata')

S_btx_pp <- pp_check(btx_multi4_fit_global, resp = 'S') +
  scale_x_continuous(name = 'Species richness (S)', trans = 'log2') +
  labs(tag = 'b') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

Sn_btx_pp <- pp_check(btx_multi4_fit_global, resp = 'Sn') +
  scale_x_continuous(name = expression(paste('Rarefied richness (', italic(S)[n],')')),, trans = 'log2') +
  labs(tag = 'c') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

N_btx_pp <- pp_check(btx_multi4_fit_global, resp = 'N') +
  scale_x_continuous(name = 'Number of individuals (N)', trans = 'log10') +
  labs(tag = 'a') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

ENSPIE_btx_pp <- pp_check(btx_multi4_fit_global, resp = 'ENSPIE') +
  scale_x_continuous(name = expression(paste('Evenness (', italic(S)[PIE],')')),
                     trans = 'log2') +
  labs(tag = 'd') +
  theme(legend.position = c(1,1),
        plot.tag.position = c(0.15, 0.9),
        legend.justification = c(1,1))

cowplot::plot_grid(N_btx_pp, S_btx_pp, Sn_btx_pp, ENSPIE_btx_pp)

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/btx_multi4_postpred_global.png',
       width = 200, height = 200, units = 'mm')