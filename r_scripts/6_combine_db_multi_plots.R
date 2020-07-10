# combined plot of models fit to BioTIME and PREDICTS
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_PREDICTS_multi_coef_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_fragSAD_multi4_posterior_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_fwater_multi4_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_supp_multi4_posterior_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_btx_multi4_posterior_wrangle.R')

# colour scheme for multiple databases
db_colour <- c('PREDICTS' = '#1b9e77',
               'BioTIME' = '#d95f02',
               'fragSAD' = '#7570b3',
               'Freshwater LU' = '#e7298a',
               'Experiments (v. controls)' = '#66a61e')

db_shape <- c('PREDICTS' = 20,
              'BioTIME' = 15,
              'fragSAD' = 3,
              'Freshwater LU' = 19,
              'Experiments (v. controls)' = 19)

# NB: coefficients estimated allowing for correlations between the response of different components
S_N_combined <-
  ggplot() +
    # facet_wrap(~db) +
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),# 
             aes(x = N, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             # alpha = 0.5
             size = 0.5
             ) +
  geom_point(data = supp_trt_summary,
             aes(x = N_slope, y = S_slope,
                 shape = db,
                 colour = db),
             size = 0.5) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),# 
             aes(x = N, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             alpha = 0.75,
             size = 0.5
  ) +
    geom_point(data = bt_study_summary, 
               aes(x = N_slope, S_slope, 
                   shape = db,
                   colour = db,
                   # size = change_size, stroke = change_stroke
                   ),
               size = 0.5,
               alpha = 1) +#size = sizeS,
  geom_point(data = fragSAD_study_summary, 
             aes(x = -N_slope, -S_slope, 
                 shape = db,
                 colour = db,
                 # size = change_size, stroke = change_stroke
             ),
             # shape = 3,
             size = 0.5,
             alpha = 0.25) +#size = sizeS,
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_shape_manual(name = '', values = db_shape) +
  scale_colour_manual(name = '',
                      values = db_colour) +
  labs(y = expression(paste('Change in species richness [log(S)]', sep = '')),
       x = expression(paste('Change in total abundance [log(N)]', sep = '')),
       tag = 'a') +
  theme_minimal() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.background = element_blank(),
        text = element_text(size = 6)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

S_Sn_combined <-
  ggplot() +
  # facet_wrap(~db) +
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),# 
             aes(x = Sn, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             # alpha = 0.5
             size = 0.5
             ) +
  geom_point(data = supp_trt_summary,
             aes(x = Sn_slope, y = S_slope,
                 shape = db,
                 colour = db),
             size = 0.5) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),# 
             aes(x = Sn, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             alpha = 0.75,
             size = 0.5
  ) +
  geom_point(data = bt_study_summary, 
             aes(x = Sn_slope, S_slope, 
                 shape = db,
                 colour = db,
                 # size = change_size, 
                 # stroke = change_stroke
                 ),
             alpha = 1,
             size = 0.5) +#size = sizeS,
  geom_point(data = fragSAD_study_summary, 
             aes(x = -Sn_slope, -S_slope, 
                 shape = db,
                 colour = db,
                 # size = change_size, stroke = change_stroke
             ),
             shape = 3,
             size = 0.5,
             alpha = 0.25) +#size = sizeS,
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_shape_manual(name = '', values = db_shape, guide = F) +
  scale_colour_manual(values = db_colour, guide = F) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5)) +
  labs(y = '', #expression(paste('Change in species richness [log(S), relative to primary vegetation]', sep = '')),
       x = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = '')),
       tag = 'b') +
  theme_minimal() +
  theme(legend.position = 'none',
        text = element_text(size = 6))

S_S_PIE_combined <-
  ggplot() +
  # facet_wrap(~db) +
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = S_PIE, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             # alpha = 0.5
             size = 0.5
             ) +
  geom_point(data = supp_trt_summary,
             aes(x = ENSPIE_slope, y = S_slope,
                 shape = db,
                 colour = db),
             size = 0.5) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),# 
             aes(x = S_PIE, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             alpha = 0.75,
             size = 0.5
  ) +
  geom_point(data = bt_study_summary, 
               aes(x = ENSPIE_slope, S_slope, 
                   shape = db,
                   colour = db,
                   # size = change_size, stroke = change_stroke
                   ),
               alpha = 1,
               size = 0.5) +#size = sizeS,
  geom_point(data = fragSAD_study_summary, 
             aes(x = -ENSPIE_slope, -S_slope, 
                 shape = db,
                 colour = db,
                 # size = change_size, stroke = change_stroke
             ),
             shape = 3,
             size = 0.5,
             alpha = 0.25) +#size = sizeS,
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_shape_manual(name = '', values = db_shape, guide = F) +
  scale_colour_manual(values = db_colour, guide = F) +
  labs(y = '', #expression(paste('Change in species richness [log(S), relative to primary vegetation]', sep = '')),
       x = expression(paste('Change in evenness [log(',S[PIE],')]', sep = '')),
       tag = 'c') +
  # coord_fixed() +
  theme_minimal() +
  theme(legend.position = 'none',
        text = element_text(size = 6))

combined_title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Component changes allowed to covary",
    # fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

bottom <- cowplot::plot_grid(S_N_combined, 
                   S_Sn_combined,
                   S_S_PIE_combined,
                   nrow = 1)

cowplot::plot_grid(combined_title,
                   bottom,
                   nrow = 2,
                   rel_heights = c(0.1,1))

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/combined_multi_deltaDelta4_noTitle.png',
       width = 240, height = 80, units = 'mm')


bt_cor_plot <-
ggplot() +
  geom_density(data = bt_cor_long,
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.58, y = 9,
             label = expression(paste('S & N')),
             size = 2,
             aes(colour = 'S_N')) +
  geom_density(data = bt_cor_long,
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = 0, y = 9,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(colour = 'N_S_PIE')) +
  geom_density(data = bt_cor_long,
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_label(x = 0.375, y = 9,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(colour = 'Sn_N')) +
  geom_density(data = bt_cor_long,
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_label(x = 0.75, y = 18,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(colour = 'S_S_PIE')) +
  geom_density(data = bt_cor_long,
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.9, y = 41,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(colour = 'S_Sn')) +
  geom_density(data = bt_cor_long,
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.85, y = 24,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(colour = 'S_PIE_Sn')) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  scale_colour_manual(guide = FALSE,
                      name = 'Components',
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'BioTIME',
       # subtitle = 'Strength of relationships between components changing through time',
       tag = 'a') +
  theme_minimal() +
  theme(legend.position = 'none',
        legend.justification = c(1,1),
        legend.background = element_blank()) +
  guides(colour = guide_legend(ncol = 3, label.hjust = 0))

btx_cor_plot <-
  ggplot() +
  geom_density(data = btx_cor_long,
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.6, y = 3.5,
             label = expression(paste('S & N')),
             size = 2,
             aes(colour = 'S_N')) +
  geom_density(data = btx_cor_long,
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = -0.7, y = 4.5,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(colour = 'N_S_PIE')) +
  geom_density(data = btx_cor_long,
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_density(data = btx_cor_long,
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_density(data = btx_cor_long,
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.8, y = 6,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(colour = 'S_Sn')) +
  geom_density(data = btx_cor_long,
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.35, y = 3.5,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(colour = 'S_PIE_Sn')) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_label(x = 0.05, y = 3.5,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(colour = 'S_S_PIE')) +
  geom_label(x = 0.05, y = 4.5,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(colour = 'Sn_N')) +
  scale_colour_manual(guide = FALSE,
                      name = 'Components',
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'Experimental (time series)',
       # subtitle = 'Strength of relationships between components changing through time',
       tag = 'b') +
  theme_minimal() +
  theme(legend.position = 'none',
        legend.justification = c(1,1),
        legend.background = element_blank()) +
  guides(colour = guide_legend(ncol = 3, label.hjust = 0))

supp_cor_plot <-
  ggplot() +
  geom_density(data = supp_cor_long,
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.2, y = 2,
             label = expression(paste('S & N')),
             size = 2,
             aes(colour = 'S_N')) +
  geom_density(data = supp_cor_long,
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = -0.2, y = 2,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(colour = 'N_S_PIE')) +
  geom_density(data = supp_cor_long,
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_label(x = -0.2, y = 2.5,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(colour = 'Sn_N')) +
  geom_density(data = supp_cor_long,
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_density(data = supp_cor_long,
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.85, y = 3.1,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(colour = 'S_Sn')) +
  geom_density(data = supp_cor_long,
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.35, y = 3.1,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(colour = 'S_PIE_Sn')) +
  geom_label(x = 0.55, y = 2.75,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(colour = 'S_S_PIE')) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_colour_manual(guide = FALSE,
                      name = 'Components',
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'Experiments (v. controls)',
       # subtitle = 'Strength of relationships between components changing through time',
       tag = 'd') +
  theme_minimal() +
  theme(legend.position = 'none',
        legend.justification = c(1,1),
        legend.background = element_blank()) +
  guides(colour = guide_legend(ncol = 3, label.hjust = 0))

predicts_cor_plot_simple <-
ggplot() +
  geom_density(data = predicts_cor_long %>% 
                 filter(LU!='Primary vegetation'),
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.65, y = 2.25,
             label = expression(paste('S & N')),
             size = 2,
             aes(colour = 'S_N')) +
  geom_density(data = predicts_cor_long %>% 
                 filter(LU!='Primary vegetation'),
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_label(x = 0.25, y = 2.25,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(colour = 'S_S_PIE')) +
  geom_density(data = predicts_cor_long %>% 
                 filter(LU!='Primary vegetation'),
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.8, y = 1.2,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(colour = 'S_Sn')) +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_label(x = -0.2, y = 2.5,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(colour = 'Sn_N')) +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.7, y = 2,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(colour = 'S_PIE_Sn')) +
  geom_density(data = predicts_cor_long %>%
                 filter(LU!='Primary vegetation'),
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = -0.2, y = 1.5,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(colour = 'N_S_PIE')) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_colour_manual(guide = FALSE,
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = '',
       title = 'PREDICTS',
       tag = 'e') +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0)) 

fwater_cor_plot_simple <-
  ggplot() +
  geom_density(data = fwater_cor_long %>% 
                 filter(LU!='Reference stream'),
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.4, y = 1.8,
             label = expression(paste('S & N')),
             size = 2,
             aes(colour = 'S_N')) +
  geom_density(data = fwater_cor_long %>% 
                 filter(LU!='Reference stream'),
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_label(x = 0.6, y = 1.6,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(colour = 'S_S_PIE')) +
  geom_density(data = fwater_cor_long %>% 
                 filter(LU!='Reference stream'),
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.4, y = 1.4,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(colour = 'S_Sn')) +
  geom_density(data = fwater_cor_long %>%
                 filter(LU!='Reference stream'),
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_label(x = -0.1, y = 1.8,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(colour = 'Sn_N')) +
  geom_density(data = fwater_cor_long %>%
                 filter(LU!='Reference stream'),
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.6, y = 1.1,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(colour = 'S_PIE_Sn')) +
  geom_density(data = fwater_cor_long %>%
                 filter(LU!='Reference stream'),
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = 0.16, y = 1.85,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(colour = 'N_S_PIE')) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_colour_manual(guide = FALSE,
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = 'Correlation',
       title = 'Freshwater LU',
       tag = 'f') +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0)) 

fragSAD_cor_plot <- ggplot() +
  geom_density(data = fS_cor_long,
               aes(x = corS_N, colour = 'S_N')) +
  geom_label(x = 0.7, y = 4.5,
             label = expression(paste('S & N')),
             size = 2,
             aes(colour = 'S_N')) +
  geom_density(data = fS_cor_long,
               aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
  geom_label(x = 0.15, y = 2.5,
             label = expression(paste('N & ', S[PIE])),
             size = 2,
             aes(colour = 'N_S_PIE')) +
  geom_density(data = fS_cor_long,
               aes(x = corSn_N, colour = 'Sn_N')) +
  geom_label(x = 0.45, y = 3.5,
             label = expression(paste('N & ', S[n])),
             size = 2,
             aes(colour = 'Sn_N')) +
  geom_density(data = fS_cor_long,
               aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
  geom_label(x = 0.75, y = 6.25,
             label = expression(paste('S & ', S[PIE])),
             size = 2,
             aes(colour = 'S_S_PIE')) +
  geom_density(data = fS_cor_long,
               aes(x = corS_Sn, colour = 'S_Sn')) +
  geom_label(x = 0.8, y = 10,
             label = expression(paste('S & ', S[n])),
             size = 2,
             aes(colour = 'S_Sn')) +
  geom_density(data = fS_cor_long,
               aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
  geom_label(x = 0.8, y = 8.5,
             label = expression(paste(S[PIE], ' & ', S[n])),
             size = 2,
             aes(colour = 'S_PIE_Sn')) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_colour_manual(guide = FALSE,
                      name = 'Components',
                      values = c('S_N' = '#f0027f',
                                 'S_S_PIE' = '#f2df35',
                                 'S_Sn' = '#386cb0',
                                 'Sn_N' = '#beaed4',
                                 'S_PIE_Sn' = '#fdc086',
                                 'N_S_PIE' = '#7fc97f')) +
  labs(x = 'Correlation',
       title = 'fragSAD',
       tag = 'c') +
  theme_minimal() +
  theme(legend.position = 'none',
        legend.justification = c(1,1),
        legend.background = element_blank()) +
  guides(colour = guide_legend(ncol = 3, label.hjust = 0))


right <- cowplot::plot_grid(supp_cor_plot,
                           predicts_cor_plot_simple,
                           fwater_cor_plot_simple,
                           ncol = 1,
                           align = 'v')

left <- cowplot::plot_grid(bt_cor_plot,
                            btx_cor_plot,
                            fragSAD_cor_plot,
                            ncol = 1, 
                            align = 'v')

cowplot::plot_grid(left,
                   right,
                   nrow = 1)

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/combined_multi_cor4.png',
       width = 220, height = 290, units = 'mm')


# predicts_cor_plot <- ggplot() +
#   facet_wrap(~LU) +
#   geom_density(data = predicts_cor_long %>% 
#                  filter(LU!='Primary vegetation'),
#                aes(x = corS_N, colour = 'S_N')) +
#   geom_density(data = predicts_cor_long %>% 
#                  filter(LU!='Primary vegetation'),
#                aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
#   geom_density(data = predicts_cor_long %>% 
#                  filter(LU!='Primary vegetation'),
#                aes(x = corS_Sn, colour = 'S_Sn')) +
#   geom_density(data = predicts_cor_long %>%
#                  filter(LU!='Primary vegetation'),
#                aes(x = corSn_N, colour = 'Sn_N')) +
#   geom_density(data = predicts_cor_long %>%
#                  filter(LU!='Primary vegetation'),
#                aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
#   geom_density(data = predicts_cor_long %>%
#                  filter(LU!='Primary vegetation'),
#                aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
#   geom_vline(xintercept = 0, lty = 2) +
#   scale_colour_manual(name = '',
#                       values = c('S_N' = '#f0027f',
#                                  'S_S_PIE' = '#f2df35',
#                                  'S_Sn' = '#386cb0',
#                                  'Sn_N' = '#beaed4',
#                                  'S_PIE_Sn' = '#fdc086',
#                                  'N_S_PIE' = '#7fc97f'),
#                       # careful here!
#                       labels = c(expression(paste('N & ', S[PIE])),
#                                  'S & N',
#                                  expression(paste(S[PIE], ' & ', S[n])),
#                                  expression(paste('S & ', S[PIE])),
#                                  expression(paste('S & ', S[n])),
#                                  expression(paste('N & ', S[n]))                                 )
#   ) +
#   labs(x = '',
#        title = 'PREDICTS',
#        tag = 'a') +
#   theme_minimal() +
#   theme(legend.position = c(0.8, 0.25),
#         legend.justification = c(1,1),
#         legend.background = element_blank(),
#         strip.text = element_text(hjust = 0)) +
#   guides(colour = guide_legend(ncol = 3, label.hjust = 0))

# fwater_cor_plot <-
#   ggplot() +
#   facet_wrap(~LU) +
#   geom_density(data = fwater_cor_long %>% 
#                  filter(LU!='Reference stream'),
#                aes(x = corS_N, colour = 'S_N')) +
#   
#   geom_density(data = fwater_cor_long %>% 
#                  filter(LU!='Reference stream'),
#                aes(x = corS_S_PIE, colour = 'S_S_PIE')) +
#   geom_density(data = fwater_cor_long %>% 
#                  filter(LU!='Reference stream'),
#                aes(x = corS_Sn, colour = 'S_Sn')) +
#   geom_density(data = fwater_cor_long %>%
#                  filter(LU!='Reference stream'),
#                aes(x = corSn_N, colour = 'Sn_N')) +
#   geom_density(data = fwater_cor_long %>%
#                  filter(LU!='Reference stream'),
#                aes(x = corS_PIE_Sn, colour = 'S_PIE_Sn')) +
#   geom_density(data = fwater_cor_long %>%
#                  filter(LU!='Reference stream'),
#                aes(x = corN_S_PIE, colour = 'N_S_PIE')) +
#   geom_vline(xintercept = 0, lty = 2) +
#   scale_colour_manual(name = '',
#                       values = c('S_N' = '#f0027f',
#                                  'S_S_PIE' = '#f2df35',
#                                  'S_Sn' = '#386cb0',
#                                  'Sn_N' = '#beaed4',
#                                  'S_PIE_Sn' = '#fdc086',
#                                  'N_S_PIE' = '#7fc97f'),
#                       # careful here!
#                       labels = c(expression(paste('N & ', S[PIE])),
#                                  'S & N',
#                                  expression(paste(S[PIE], ' & ', S[n])),
#                                  expression(paste('S & ', S[PIE])),
#                                  expression(paste('S & ', S[n])),
#                                  expression(paste('N & ', S[n]))                                 )
#   ) +
#   labs(x = 'Correlation',
#        title = 'Freshwater LU',
#        tag = 'b') +
#   theme_minimal() +
#   theme(legend.position = 'none',#c(1,1),
#         legend.justification = c(1,1),
#         legend.background = element_blank(),
#         strip.text = element_text(hjust = 0)) +
#   guides(colour = guide_legend(ncol = 3, label.hjust = 0))
