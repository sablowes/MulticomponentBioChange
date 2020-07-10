source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/0_init_dirs_load_packages.R')

load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_PREDICTS_multi_coef_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_fragSAD_multi4_posterior_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_fwater_multi4_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_supp_multi4_posterior_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_btx_multi4_posterior_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_cestes_multi4_posterior_wrangle.R')
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_mcgill_multi4_posterior_wrangle.R')

#---combine predicts and freshwater db, and CESTES and McGill
PREDICTS_SS_posterior <- PREDICTS_SS_posterior %>% 
  mutate(db = 'Space-for-time (land use)',
         N = N_SS,
         S = S_SS,
         ENSPIE = ENSPIE_SS,
         Sn = Sn_SS) %>% 
  select(-N_SS, -S_SS, -Sn_SS, -ENSPIE_SS)
SS_LU_multi <- SS_LU_multi %>% mutate(db = 'Space-for-time (land use)')
fw_study_posterior <- fw_study_posterior %>% mutate(db = 'Space-for-time (land use)')
fwater_study_LU_multi <- fwater_study_LU_multi %>% mutate(db = 'Space-for-time (land use)')

cestes_coefs_multi4 <- cestes_coefs_multi4 %>% mutate(db = 'Spatial gradient (natural)')
cestes_multi4_posterior <- cestes_multi4_posterior %>% mutate(db = 'Spatial gradient (natural)')
mcgill_coefs_multi4 <- mcgill_coefs_multi4 %>% mutate(db = 'Spatial gradient (natural)')
mcgill_multi4_posterior <- mcgill_multi4_posterior %>% mutate(db = 'Spatial gradient (natural)')

#--------density plots--------------
reln_colours = c('S_N' = '#f0027f',
                 'S_S_PIE' = '#f2df35',
                 'S_Sn' = '#386cb0',
                 'Sn_N' = '#beaed4',
                 'S_PIE_Sn' = '#fdc086',
                 'N_S_PIE' = '#7fc97f')

#----S_N x2 ----
S_N_2d_post_density_impact <-
  ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('Experiments (v. controls)', 
                                'Space-for-time (land use)', 'Spatial gradient (natural)'),
                     labels = c('(d) Experiments (v. controls)', 
                                '(e) Space-for-time (land use)', '(f) Spatial gradient (natural)')), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                   filter(LU!='Primary vegetation'),
                   fw_study_posterior %>% 
                     ungroup() %>% 
                     filter(Treatment!='Reference stream')),
                 aes(x = N_global + N,
                     y = S_global + S,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = N,
                 y = S, shape = 'Terrestrial', colour = 'Terrestrial'),
             size = 0.75) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),
             aes(x = N,
                 y = S, shape = 'Freshwater', colour = 'Freshwater'),
             size = 0.75, alpha = 0.5) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = supp_trt_sample_posterior %>% 
                   mutate(db = 'Experiments (v. controls)'),
                 aes(x = N_trt,
                     y = S_trt,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = supp_trt_summary,
             aes(x = N_slope,
                 y = S_slope),
             size = 0.75) +
  mapply(function(level) {
    stat_ellipse(data = bind_rows(cestes_multi4_posterior %>%
                   filter(newID!='1'),
                   mcgill_multi4_posterior %>%
                     filter(newID!='s_1')),
                 aes(x = N_study,
                     y = S_study,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = cestes_coefs_multi4 %>% 
               filter(N > -4),
             aes(x = N, y = S),
             size = 0.75) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = N, y = S),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in species richness [log(S)]', sep = '')),
       x = expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_color_manual(name = '', 
                     values = c('Terrestrial' = '#252525',
                                'Freshwater' = '#969696')) +
  scale_shape_manual(name = '', 
                     values = c('Terrestrial' = 19,
                                'Freshwater' = 4)) +
  theme_minimal() +
  theme(legend.position = c(1,0.5),
        legend.justification = c(1, 1),
        strip.text = element_text(hjust = 0)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


S_N_2d_post_density_natural <-
  ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('BioTIME', 
                                'Experimental (time series)', 'fragSAD'),
                     labels = c('(a) BioTIME', 
                                '(b) Experimental time series', '(c) fragSAD'),), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(x = N_study + N_global,
                     y = S_study + S_global,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary,
             aes(x = N_slope, y = S_slope),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = fragSAD_study_sample_posterior,
                 aes(x = -(N_study + N_global),
                     y = -(S_study + S_global),
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) + 
  geom_point(data = fragSAD_study_summary, 
             aes(x = -N_slope, y = -S_slope),
             size = 0.75) + 
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(x = N_study,
                     y = S_study,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary,
             aes(x = N_slope,
                 y = S_slope),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in species richness [log(S)]', sep = '')),
       x = ''#expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0))

#----S_Sn x2 ----
S_Sn_2d_post_density_impact <-
  ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('Experiments (v. controls)', 
                                'Space-for-time (land use)', 'Spatial gradient (natural)'),
                     labels = c('(d) Experiments (v. controls)', 
                                '(e) Space-for-time (land use)', '(f) Spatial gradient (natural)')), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                   filter(LU!='Primary vegetation'),
                   fw_study_posterior %>% 
                     ungroup() %>% 
                     filter(Treatment!='Reference stream')),
                 aes(x = Sn_global + Sn,
                     y = S_global + S,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = Sn,
                 y = S, shape = 'Terrestrial', colour = 'Terrestrial'),
             size = 0.75) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),
             aes(x = Sn,
                 y = S, shape = 'Freshwater', colour = 'Freshwater'),
             size = 0.75, alpha = 0.5) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = supp_trt_sample_posterior %>% 
                   mutate(db = 'Experiments (v. controls)'),
                 aes(x = Sn_trt,
                     y = S_trt,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = supp_trt_summary,
             aes(x = Sn_slope,
                 y = S_slope),
             size = 0.75) +
  mapply(function(level) {
    stat_ellipse(data = bind_rows(cestes_multi4_posterior %>%
                   filter(newID!='1'),
                 mcgill_multi4_posterior %>%
                   filter(newID!='s_1')),
                 aes(x = Sn_study,
                     y = S_study,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = cestes_coefs_multi4 %>% 
               filter(N > -4),
             aes(x = Sn, y = S),
             size = 0.75) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = Sn, y = S),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '', #expression(paste('Change in species richness [log(S), relative to primary vegetation]', sep = '')),
       x = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = ''))#,
       # tag = 'b'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_color_manual(name = '', 
                     values = c('Terrestrial' = '#252525',
                                'Freshwater' = '#969696')) +
  scale_shape_manual(name = '', 
                     values = c('Terrestrial' = 19,
                                'Freshwater' = 4)) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())

S_Sn_2d_post_density_natural <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('BioTIME', 
                                'Experimental (time series)', 'fragSAD'),
                     labels = c('(a) BioTIME', 
                                '(b) Experimental time series', '(c) fragSAD'),), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(x = Sn_study + Sn_global,
                     y = S_study + S_global,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary,
             aes(x = Sn_slope, y = S_slope),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = fragSAD_study_sample_posterior,
                 aes(x = -(Sn_study + Sn_global),
                     y = -(S_study + S_global),
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = fragSAD_study_summary,
             aes(x = -Sn_slope,
                 y = -S_slope),
             size = 0.5) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(x = Sn_study,
                     y = S_study,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary,
             aes(x = Sn_slope,
                 y = S_slope),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '', #expression(paste('Change in species richness [log(S), relative to primary vegetation]', sep = '')),
       x = ''#expression(paste('Change in rarefied richness [log(',S[n],')]', sep = ''))#,
       # tag = 'b'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())

#----S_S_PIE x2 ----
S_S_PIE_2d_post_density_impact <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('Experiments (v. controls)', 
                                'Space-for-time (land use)', 'Spatial gradient (natural)'),
                     labels = c('(d) Experiments (v. controls)', 
                                '(e) Space-for-time (land use)', '(f) Spatial gradient (natural)')), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                   filter(LU!='Primary vegetation'),
                 fw_study_posterior %>% 
                   ungroup() %>% 
                   filter(Treatment!='Reference stream')),
    aes(x = ENSPIE_global + ENSPIE,
                     y = S_global + S,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = S_PIE,
                 y = S, shape = 'Terrestrial', colour = 'Terrestrial'),
             size = 0.75) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),
             aes(x = S_PIE,
                 y = S, shape = 'Freshwater', colour = 'Freshwater'),
             size = 0.75, alpha = 0.5) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = supp_trt_sample_posterior %>% 
                   mutate(db = 'Experiments (v. controls)'),
                 aes(x = ENSPIE_trt,
                     y = S_trt,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = supp_trt_summary,
             aes(x = ENSPIE_slope,
                 y = S_slope),
             size = 0.75) +
  # 2d mv-normal posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(cestes_multi4_posterior %>%
                   filter(newID!='1'),
                 mcgill_multi4_posterior %>%
                   filter(newID!='s_1')),
                 aes(x = ENSPIE_study,
                     y = S_study,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = cestes_coefs_multi4 %>% 
               filter(N > -4),
             aes(x = ENSPIE, y = S),
             size = 0.5) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = ENSPIE, y = S),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '', #expression(paste('Change in species richness [log(S), relative to primary vegetation]', sep = '')),
       x = expression(paste('Change in evenness [log(',S[PIE],')]', sep = ''))) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_color_manual(name = '', 
                     values = c('Terrestrial' = '#252525',
                                'Freshwater' = '#969696')) +
  scale_shape_manual(name = '', 
                     values = c('Terrestrial' = 19,
                                'Freshwater' = 4)) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())

S_S_PIE_2d_post_density_natural <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('BioTIME', 
                                'Experimental (time series)', 'fragSAD'),
                     labels = c('(a) BioTIME', 
                                '(b) Experimental time series', '(c) fragSAD'),), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(x = ENSPIE_study + ENSPIE_global,
                     y = S_study + S_global,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary,
             aes(x = ENSPIE_slope, y = S_slope),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = fragSAD_study_sample_posterior,
                 aes(x = -(ENSPIE_study + ENSPIE_global),
                     y = -(S_study + S_global),
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = fragSAD_study_summary,
             aes(x = -ENSPIE_slope,
                 y = -S_slope),
             size = 0.5) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(x = ENSPIE_study,
                     y = S_study,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary,
             aes(x = ENSPIE_slope,
                 y = S_slope),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '', #expression(paste('Change in species richness [log(S), relative to primary vegetation]', sep = '')),
       x = ''#expression(paste('Change in evenness [log(',S[PIE],')]', sep = ''))#,
       # tag = 'b'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())


impact_cols = plot_grid(S_N_2d_post_density_impact,
                        S_Sn_2d_post_density_impact,
                        S_S_PIE_2d_post_density_impact,
                        align = 'hv',
                        ncol = 3)

natural_cols = plot_grid(S_N_2d_post_density_natural,
                         S_Sn_2d_post_density_natural,
                         S_S_PIE_2d_post_density_natural,
                         align = 'hv',
                         ncol = 3)

plot_grid(natural_cols, impact_cols, 
          nrow = 2#, 
          # rel_heights = c(6.2/9,1)
)

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/combined_density_multi4_condensed.png',
       height = 290, width = 220, units = 'mm')

# plot(ellipse::ellipse(bt_vcov, centre = bt_mu, 
#                       which = c(3,4), level = 0.1), type = 'l')

# N vs Sn
Sn_S_PIE_2d_post_density_impact <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('Experiments (v. controls)', 
                                'Space-for-time (land use)', 'Spatial gradient (natural)'),
                     labels = c('(d) Experiments (v. controls)', 
                                '(e) Space-for-time (land use)', '(f) Spatial gradient (natural)')), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                   filter(LU!='Primary vegetation'),
                 fw_study_posterior %>% 
                   ungroup() %>% 
                   filter(Treatment!='Reference stream')),
                 aes(x = ENSPIE_global + ENSPIE,
                     y = Sn_global + Sn,
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = S_PIE,
                 y = S, shape = 'Terrestrial', colour = 'Terrestrial'),
             size = 0.75) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),
             aes(x = S_PIE,
                 y = Sn, shape = 'Freshwater', colour = 'Freshwater'),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = supp_trt_sample_posterior %>% 
                   mutate(db = 'Experiments (v. controls)'),
                 aes(x = ENSPIE_trt,
                     y = Sn_trt,
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = supp_trt_summary,
             aes(x = ENSPIE_slope,
                 y = Sn_slope),
             size = 0.75) +
  mapply(function(level) {
    stat_ellipse(data = bind_rows(cestes_multi4_posterior %>%
                                    filter(newID!='1'),
                                  mcgill_multi4_posterior %>%
                                    filter(newID!='s_1')),
                 aes(x = ENSPIE_study,
                     y = Sn_study,
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = cestes_coefs_multi4 %>% 
               filter(N > -4),
             aes(x = ENSPIE, y = Sn),
             size = 0.75) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = ENSPIE, y = Sn),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = '')),
       x = expression(paste('Change in evenness [log(',S[PIE],')]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_color_manual(name = '', 
                     values = c('Terrestrial' = '#252525',
                                'Freshwater' = '#969696')) +
  scale_shape_manual(name = '', 
                     values = c('Terrestrial' = 19,
                                'Freshwater' = 4)) +
  theme_minimal() +
  theme(legend.position = c(1,0.5),
        legend.justification = c(1, 1),
        strip.text = element_text(hjust = 0)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

Sn_S_PIE_2d_post_density_natural <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('BioTIME', 
                                'Experimental (time series)', 'fragSAD'),
                     labels = c('(a) BioTIME', 
                                '(b) Experimental time series', '(c) fragSAD'),), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(x = ENSPIE_study + ENSPIE_global,
                     y = Sn_study + Sn_global,
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary,
             aes(x = ENSPIE_slope, y = Sn_slope),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = fragSAD_study_sample_posterior,
                 aes(x = -(ENSPIE_study + ENSPIE_global),
                     y = -(Sn_study + Sn_global),
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) + 
  geom_point(data = fragSAD_study_summary, 
             aes(x = -ENSPIE_slope, y = -Sn_slope),
             size = 0.75) + 
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(x = ENSPIE_study,
                     y = Sn_study,
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary,
             aes(x = ENSPIE_slope,
                 y = Sn_slope),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = '')),
       x = ''#expression(paste('Change in evenness [log(',S[PIE],')]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0))

# N vs Sn
Sn_N_2d_post_density_impact <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('Experiments (v. controls)', 
                                'Space-for-time (land use)', 'Spatial gradient (natural)'),
                     labels = c('(d) Experiments (v. controls)', 
                                '(e) Space-for-time (land use)', '(f) Spatial gradient (natural)')), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                   filter(LU!='Primary vegetation'),
                 fw_study_posterior %>% 
                   ungroup() %>% 
                   filter(Treatment!='Reference stream')),
                 aes(x = N_global + N,
                     y = Sn_global + Sn,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = N,
                 y = Sn, shape = 'Terrestrial', colour = 'Terrestrial'),
             size = 0.75) +
    geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),
             aes(x = N,
                 y = Sn, shape = 'Freshwater', colour = 'Freshwater'),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = supp_trt_sample_posterior %>% 
                   mutate(db = 'Experiments (v. controls)'),
                 aes(x = N_trt,
                     y = Sn_trt,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = supp_trt_summary,
             aes(x = N_slope,
                 y = Sn_slope),
             size = 0.75) +
  mapply(function(level) {
    stat_ellipse(data = bind_rows(cestes_multi4_posterior %>%
                                    filter(newID!='1'),
                                  mcgill_multi4_posterior %>%
                                    filter(newID!='s_1')),
                 aes(x = N_study,
                     y = Sn_study,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = cestes_coefs_multi4 %>% 
               filter(N > -4),
             aes(x = N, y = Sn),
             size = 0.75) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = N, y = Sn),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = '')),
       x = expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_color_manual(name = '', 
                     values = c('Terrestrial' = '#252525',
                                'Freshwater' = '#969696')) +
  scale_shape_manual(name = '', 
                     values = c('Terrestrial' = 19,
                                'Freshwater' = 4)) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())

Sn_N_2d_post_density_natural <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('BioTIME', 
                                'Experimental (time series)', 'fragSAD'),
                     labels = c('(a) BioTIME', 
                                '(b) Experimental time series', '(c) fragSAD'),), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(x = N_study + N_global,
                     y = Sn_study + Sn_global,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary,
             aes(x = N_slope, y = Sn_slope),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = fragSAD_study_sample_posterior,
                 aes(x = -(N_study + N_global),
                     y = -(Sn_study + Sn_global),
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) + 
  geom_point(data = fragSAD_study_summary, 
             aes(x = -N_slope, y = -Sn_slope),
             size = 0.75) + 
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(x = N_study,
                     y = Sn_study,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary,
             aes(x = N_slope,
                 y = Sn_slope),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = '')),
       x = ''#expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())

# N vs S_PIE
S_PIE_N_2d_post_density_impact <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('Experiments (v. controls)', 
                                'Space-for-time (land use)', 'Spatial gradient (natural)'),
                     labels = c('(d) Experiments (v. controls)', 
                                '(e) Space-for-time (land use)', '(f) Spatial gradient (natural)')), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                   filter(LU!='Primary vegetation'),
                 fw_study_posterior %>% 
                   ungroup() %>% 
                   filter(Treatment!='Reference stream')),
                 aes(x = N_global + N,
                     y = ENSPIE_global + ENSPIE,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = N,
                 y = S_PIE, shape = 'Terrestrial', colour = 'Terrestrial'),
             size = 0.75) +
  geom_point(data = fwater_study_LU_multi %>% 
               filter(LU!='Reference stream'),
             aes(x = N,
                 y = S_PIE, shape = 'Freshwater', colour = 'Freshwater'),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = supp_trt_sample_posterior %>% 
                   mutate(db = 'Experiments (v. controls)'),
                 aes(x = N_trt,
                     y = ENSPIE_trt,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = supp_trt_summary,
             aes(x = N_slope,
                 y = ENSPIE_slope),
             size = 0.75) +
  mapply(function(level) {
    stat_ellipse(data = bind_rows(cestes_multi4_posterior %>%
                                    filter(newID!='1'),
                                  mcgill_multi4_posterior %>%
                                    filter(newID!='s_1')),
                 aes(x = N_study,
                     y = ENSPIE_study,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = cestes_coefs_multi4 %>% 
               filter(N > -4),
             aes(x = N, y = ENSPIE),
             size = 0.75) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = N, y = ENSPIE),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in evennes [log(', S[PIE], ')]', sep = '')),
       x = expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_color_manual(name = '', 
                     values = c('Terrestrial' = '#252525',
                                'Freshwater' = '#969696')) +
  scale_shape_manual(name = '', 
                     values = c('Terrestrial' = 19,
                                'Freshwater' = 4)) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())

S_PIE_N_2d_post_density_natural <- ggplot() +
  facet_wrap(~factor(db, 
                     levels = c('BioTIME', 
                                'Experimental (time series)', 'fragSAD'),
                     labels = c('(a) BioTIME', 
                                '(b) Experimental time series', '(c) fragSAD'),), 
             ncol = 1) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(x = N_study + N_global,
                     y = ENSPIE_study + ENSPIE_global,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary,
             aes(x = N_slope, y = ENSPIE_slope),
             size = 0.75) +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = fragSAD_study_sample_posterior,
                 aes(x = -(N_study + N_global),
                     y = -(ENSPIE_study + ENSPIE_global),
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) + 
  geom_point(data = fragSAD_study_summary, 
             aes(x = -N_slope, y = -ENSPIE_slope),
             size = 0.75) + 
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(x = N_study,
                     y = ENSPIE_study,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary,
             aes(x = N_slope,
                 y = ENSPIE_slope),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = expression(paste('Change in evennes [log(', S[PIE], ')]', sep = '')),
       x = ''#expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       # tag = 'a'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        strip.text = element_blank())

N_corrs_impact <- plot_grid(Sn_S_PIE_2d_post_density_impact,
                            Sn_N_2d_post_density_impact,
                            S_PIE_N_2d_post_density_impact,
                            align = 'hv',
                            ncol = 3)

N_corrs_natural <- plot_grid(Sn_S_PIE_2d_post_density_natural,
                             Sn_N_2d_post_density_natural,
                             S_PIE_N_2d_post_density_natural,
                             align = 'hv',
                             ncol = 3)

plot_grid(N_corrs_natural,
          N_corrs_impact,
          ncol = 1#,
          # rel_heights = c(2.2/3,1)
)

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/condensed_density_multi4_supp.png',
       height = 290, width = 240, units = 'mm')
