# posterior distribution of correlation between change in components
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/predicts_multi4_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/btx_multi4_global_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/spatial_multi4_results.Rdata')

#--------colour for plots--------------
reln_colours = c('S_N' = '#f0027f',
                 'S_S_PIE' = '#f2df35',
                 'S_Sn' = '#386cb0',
                 'Sn_N' = '#beaed4',
                 'S_PIE_Sn' = '#fdc086',
                 'N_S_PIE' = '#7fc97f')


bt_cor_plot <-
ggplot() +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_density(data = bt_cor_long,
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.58, y = 9,
             label = expression(paste('S & N')),
             size = 2,
             aes(fill = 'S_N')) +
  geom_density(data = bt_cor_long,
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = 0, y = 9,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(fill = 'N_S_PIE')) +
  geom_density(data = bt_cor_long,
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_label(x = 0.375, y = 9,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(fill = 'Sn_N')) +
  geom_density(data = bt_cor_long,
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_label(x = 0.75, y = 18,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(fill = 'S_S_PIE')) +
  geom_density(data = bt_cor_long,
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.9, y = 41,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(fill = 'S_Sn'),
             colour = 'white') +
  geom_density(data = bt_cor_long,
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.85, y = 24,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(fill = 'S_PIE_Sn')) +
  scale_colour_manual(guide = FALSE,
                    name = 'Components',
                    values = c('S_N' = '#f0027f',
                               'S_S_PIE' = '#f2df35',
                               'S_Sn' = '#386cb0',
                               'Sn_N' = '#beaed4',
                               'S_PIE_Sn' = '#fdc086',
                               'N_S_PIE' = '#7fc97f')) +
  scale_fill_manual(guide = FALSE,
                      name = 'Components',
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'Temporal changes: natural variation',
       # subtitle = 'Strength of relationships between components changing through time',
       tag = 'a') +
  theme_minimal() +
  theme(legend.position = 'none',
        legend.justification = c(1,1),
        legend.background = element_blank(),
        plot.tag.position = c(0.15,0.85),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.title = element_text(size = 12, hjust = 0)) 

btx_cor_plot <-
  ggplot() +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_density(data = btx_cor_long,
               aes(x = corS_N, colour = 'S_N')) +
  geom_density(data = btx_cor_long,
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_density(data = btx_cor_long,
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_density(data = btx_cor_long,
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_density(data = btx_cor_long,
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_density(data = btx_cor_long,
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.75, y = 4.5,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(fill = 'S_PIE_Sn')) +
  geom_label(x = 0.8, y = 6,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(fill = 'S_Sn'),
             colour = 'white') +
  geom_label(x = -0.4, y = 3,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(fill = 'N_S_PIE')) +
  geom_label(x = 0.5, y = 4,
             label = expression(paste('S & N')),
             size = 2,
             aes(fill = 'S_N')) +
  geom_label(x = 0.4, y = 3.4,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(fill = 'S_S_PIE')) +
  geom_label(x = 0.1, y = 3,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(fill = 'Sn_N')) +
  scale_colour_manual(guide = FALSE,
                    name = 'Components',
                    values = c('S_N' = '#f0027f',
                               'S_S_PIE' = '#f2df35',
                               'S_Sn' = '#386cb0',
                               'Sn_N' = '#beaed4',
                               'S_PIE_Sn' = '#fdc086',
                               'N_S_PIE' = '#7fc97f')) +
  scale_fill_manual(guide = FALSE,
                      name = 'Components',
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'Temporal changes: perturbed environments',
       # subtitle = 'Strength of relationships between components changing through time',
       tag = 'b') +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.15,0.85),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.title = element_text(size = 12, hjust = 0)) +
  guides(fill = guide_legend(ncol = 3, label.hjust = 0))

predicts_cor_plot_simple <-
ggplot() +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.65, y = 2.25,
             label = expression(paste('S & N')),
             size = 2,
             aes(fill = 'S_N')) +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_label(x = 0.25, y = 2.25,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(fill = 'S_S_PIE')) +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.4, y = 1.2,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(fill = 'S_Sn'),
             colour = 'white') +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_label(x = -0.05, y = 2.5,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(fill = 'Sn_N')) +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.7, y = 2,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(fill = 'S_PIE_Sn')) +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = 0, y = 1.9,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(fill = 'N_S_PIE')) +
  scale_colour_manual(guide = FALSE,
                    values = c('S_N' = '#f0027f',
                               'S_S_PIE' = '#f2df35',
                               'S_Sn' = '#386cb0',
                               'Sn_N' = '#beaed4',
                               'S_PIE_Sn' = '#fdc086',
                               'N_S_PIE' = '#7fc97f')) +
  scale_fill_manual(guide = FALSE,
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'Spatial comparisons: land use change',
       tag = 'd') +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0),
        plot.tag.position = c(0.15,0.85),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.title = element_text(size = 12, hjust = 0))

spatial_gradient_cor <-
  ggplot() +
  stat_density(data = spatial_cor_long,
               aes(x = corS_N, colour = 'S_N'),
               geom='line', position='identity') +
    stat_density(data = spatial_cor_long,
               aes(x = corS_S_PIE, colour = 'S_S_PIE'),
               geom='line', position='identity') +
    stat_density(data = spatial_cor_long,
               aes(x = corS_Sn, colour = 'S_Sn'),
               geom='line', position='identity') +
    stat_density(data = spatial_cor_long,
               aes(x = corSn_N, colour = 'Sn_N'),
               geom='line', position='identity') +
    stat_density(data = spatial_cor_long,
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn'),
               geom='line', position='identity') +
    stat_density(data = spatial_cor_long,
               aes(x = corN_S_PIE, colour = 'N_S_PIE'),
               geom='line', position='identity') +
    geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_label(x = 0.15, y = 2.5,
             label = expression(paste('S & N')),
             size = 2,
             aes(fill = 'S_N')) +
  geom_label(x = 0.18, y = 2.1,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(fill = 'S_S_PIE')) +
  geom_label(x = 0.3, y = 2.3,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(fill = 'S_Sn'),
             colour = 'white') +
  geom_label(x = 0, y = 2,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(fill = 'Sn_N')) +
  geom_label(x = 0.4, y = 2,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(fill = 'S_PIE_Sn')) +
  geom_label(x = 0.0, y = 2.3,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(fill = 'N_S_PIE')) +
  scale_linetype_manual(name = '',
                        values = c('CESTES' = 1,
                                     'McGill' = 2)) +
  scale_colour_manual(guide = FALSE,
                    values = c('S_N' = '#f0027f',
                               'S_S_PIE' = '#f2df35',
                               'S_Sn' = '#386cb0',
                               'Sn_N' = '#beaed4',
                               'S_PIE_Sn' = '#fdc086',
                               'N_S_PIE' = '#7fc97f')) +
  scale_fill_manual(guide = FALSE,
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'Spatial comparisons: natural variation',
       tag = 'c') +
  theme_minimal() +
  theme(legend.position = c(0,0.6),
        legend.justification = c(0,1),
        plot.tag.position = c(0.15,0.85),
        plot.tag = element_text(size = 10, face = 'bold'),
        plot.title = element_text(size = 12, hjust = 0)) 



plot_grid(NULL,
          plot_grid(NULL,
                    plot_grid(bt_cor_plot, btx_cor_plot, nrow = 1),
                    plot_grid(spatial_gradient_cor, predicts_cor_plot_simple, nrow = 1),
                    NULL,
                    nrow = 4, rel_heights = c(0.1, 1, 1, 0.1), align = 'hv'),
          NULL, nrow = 1, rel_widths = c(0.1, 1, 0.1)) +
  draw_label(y = 0.05, label = 'Correlation')

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/revision/Fig4.pdf',
       width = 220, height = 200, units = 'mm')
