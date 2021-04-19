source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/predicts_multi4_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/btx_multi4_global_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/spatial_multi4_results.Rdata')

#---combine predicts and freshwater db, and CESTES and McGill
PREDICTS_SS_posterior <- PREDICTS_SS_posterior %>% 
  mutate(db = 'Space-for-time (land use)',
         N = N_SS,
         S = S_SS,
         ENSPIE = ENSPIE_SS,
         Sn = Sn_SS) %>% 
  select(-N_SS, -S_SS, -Sn_SS, -ENSPIE_SS)

#--------density plots--------------
reln_colours = c('S_N' = '#f0027f',
                 'S_S_PIE' = '#f2df35',
                 'S_Sn' = '#386cb0',
                 'Sn_N' = '#beaed4',
                 'S_PIE_Sn' = '#fdc086',
                 'N_S_PIE' = '#7fc97f')

#----S_N x6 ----
S_N_2d_post_density_time1 <-
  ggplot() +
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
  geom_point(data = bt_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = N_slope, y = S_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = bt_study_summary %>% 
               filter(componentChange!='No change') %>%
               mutate(deltaN_ENSPIE_sign = sign(N_slope * ENSPIE_slope)),
             aes(x = N_slope, y = S_slope, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'a') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                   '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(#legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_N_2d_post_density_time2 <-
  ggplot() +
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
  geom_point(data = btx_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = N_slope,
                 y = S_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope * ENSPIE_slope)),
             aes(x = N_slope,
                 y = S_slope, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'd') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_N_2d_post_density_space1 <-
  ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                   filter(LU!='Primary vegetation')),
                   # fw_study_posterior %>% 
                   #   ungroup() %>% 
                   #   filter(Treatment!='Reference stream')),
                 aes(x = N_global + N,
                     y = S_global + S,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2 & componentChange=='No change'),
             aes(x = N,
                 y = S, colour = 'No change'),
             size = 0.75) +
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2 & componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(S_PIE)),
             aes(x = N,
                 y = S,  colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = expression(paste('Change in total abundance [log(N)]', sep = '')),
       tag = 'j') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        legend.justification = c(1, 1),
        strip.text = element_blank(),
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold')) 

S_N_2d_post_density_space4 <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = spatial_multi4_posterior %>% 
                   filter(newID!='s_1'),
                 aes(x = N_study,
                     y = S_study,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange=='No change'),
             aes(x = N, y = S, colour = 'No change'),
             size = 0.75) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)),
             aes(x = N, y = S, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',#expression(paste('Change in total abundance [log(N)]', sep = '')),
       tag = 'g') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

#----S_Sn x2 ----
S_Sn_2d_post_density_time1 <-
  ggplot() +
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
  geom_point(data = bt_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = Sn_slope, y = S_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = bt_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(x = Sn_slope, y = S_slope, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'b') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_Sn_2d_post_density_time2 <- ggplot() +
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
  geom_point(data = btx_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = Sn_slope,
                 y = S_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(x = Sn_slope,
                 y = S_slope, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'e') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_Sn_2d_post_density_space1 <- ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                                    filter(LU!='Primary vegetation')),
                                  # fw_study_posterior %>% 
                                  #   ungroup() %>% 
                                  #   filter(Treatment!='Reference stream')),
                 aes(x = Sn_global + Sn,
                     y = S_global + S,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2 & componentChange=='No change'),
             aes(x = Sn,
                 y = S, colour = 'No change'),
             size = 0.75) +
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2 & componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(S_PIE)),
             aes(x = Sn,
                 y = S, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = expression(paste('Change in rarefied richness [log(', S[n], ')]')),
       tag = 'k') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold')) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

S_Sn_2d_post_density_space4 <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = spatial_multi4_posterior %>% 
                   filter(newID!='s_1'),
                 aes(x = Sn_study,
                     y = S_study,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange=='No change'),
             aes(x = Sn, y = S, colour = 'No change'),
             size = 0.75) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)),
             aes(x = Sn, y = S, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',#expression(paste('Change in rarefied richness [log(', S[n], ')]')),
       tag = 'h') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))
#----S_S_PIE x6 ----
S_S_PIE_2d_post_density_time1 <-
  ggplot() +
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
  geom_point(data = bt_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = ENSPIE_slope, y = S_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = bt_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(x = ENSPIE_slope, y = S_slope, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',#expression(paste('Change in species richness [log(S)]', sep = '')),
       x = '',#expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       tag = 'c'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_S_PIE_2d_post_density_time2 <- ggplot() +
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
  geom_point(data = btx_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = ENSPIE_slope,
                 y = S_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(x = ENSPIE_slope,
                 y = S_slope, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'f') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_S_PIE_2d_post_density_space1 <- ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                                    filter(LU!='Primary vegetation')),
                 aes(x = ENSPIE_global + ENSPIE,
                     y = S_global + S,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2 & componentChange=='No change'),
             aes(x = S_PIE,
                 y = S, colour = 'No change'),
             size = 0.75) +
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2 & componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(S_PIE)),
             aes(x = S_PIE,
                 y = S, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = expression(paste('Change in evenness [log(',S[PIE],')]', sep = '')),
       tag = 'l'
  ) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold')) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

S_S_PIE_2d_post_density_space4 <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = spatial_multi4_posterior %>% 
                   filter(newID!='s_1'),
                 aes(x = ENSPIE_study,
                     y = S_study,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange=='No change'),
             aes(x = ENSPIE, y = S, colour = 'No change'),
             size = 0.75) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)),
             aes(x = ENSPIE, y = S, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',#expression(paste('Change in evenness [log(',S[PIE],')]', sep = '')),
       tag = 'i') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

# colour legend
colour_legend <- ggplot() +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange=='No change'),
             aes(x = ENSPIE, y = S, colour = 'No change'),
             size = 0.75) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)),
             aes(x = ENSPIE, y = S, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      name = '',
                      labels = c(expression(paste('One or more component change differs from zero, ', Delta, N, ' & ', Delta, S[PIE], ' have different signs')),
                                 expression(paste('One or more component change differs from zero, ', Delta, N, ' & ', Delta, S[PIE], ' have same signs')),
                                 'No change in any component')) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.text = element_text(size = 10, hjust = 0)) +
  guides(colour = guide_legend(nrow = 2))

source('~/Dropbox/1current/R_random/functions/gg_legend.R')
leg <- gg_legend(colour_legend)

# some titles for each row
time_t1 <- ggdraw() +
  draw_label('Temporal change: natural variation',
             x = 0.05, y = 0.01, hjust = 0, size = 12) +
  theme(plot.margin = margin(0,0,0,5))

time_t2 <- ggdraw() +
  draw_label('Temporal change: perturbed environments',
             x = 0.05, y = 0.01, hjust = 0, size = 12) +
  theme(plot.margin = margin(0,0,0,5))

space_t1 <- ggdraw() +
  draw_label('Spatial change: land use change',
             x = 0.05, y = 0.01, hjust = 0, size = 12) +
  theme(plot.margin = margin(0,0,0,5))


space_t4 <- ggdraw() +
  draw_label('Spatial change: natural variation',
             x = 0.05, y = 0.01, hjust = 0, size = 12) +
  theme(plot.margin = margin(0,0,0,5))



time1_cols = plot_grid(time_t1,
                      plot_grid(S_N_2d_post_density_time1,
                         S_Sn_2d_post_density_time1,
                         S_S_PIE_2d_post_density_time1,
                         align = 'hv',
                         ncol = 3),
                      ncol = 1, rel_heights = c(0.05, 1))

time2_cols = plot_grid(time_t2,
                      plot_grid(S_N_2d_post_density_time2,
                                S_Sn_2d_post_density_time2,
                                S_S_PIE_2d_post_density_time2,
                                align = 'hv',
                                ncol = 3),
                      ncol = 1, rel_heights = c(0.01, 1))

space1_cols = plot_grid(space_t1,
                        plot_grid(S_N_2d_post_density_space1,
                                  S_Sn_2d_post_density_space1,
                                  S_S_PIE_2d_post_density_space1,
                                  align = 'hv',
                                  ncol = 3),
                        ncol = 1, rel_heights = c(0.01, 1))

space4_cols = plot_grid(space_t4,
                        plot_grid(S_N_2d_post_density_space4,
                                  S_Sn_2d_post_density_space4,
                                  S_S_PIE_2d_post_density_space4,
                                  align = 'hv',
                                  ncol = 3),
                        ncol = 1, rel_heights = c(0.01, 1))

plot_grid(leg, time1_cols, time2_cols, 
          space4_cols, space1_cols, 
          #space2_cols, space3_cols, 
          nrow = 5,
          align = 'hv',
          rel_heights = c(0.15, 1,1,1,1)) +
  draw_label(x = 0.015, label = expression(paste('Change in species richness [log(S)]', sep = '')),
             angle = 90)

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/submission/Fig4.pdf',
       height = 290, width = 240, units = 'mm')

# Sn ~ S_PIE
Sn_S_PIE_2d_post_density_time1 <-
  ggplot() +
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
  geom_point(data = bt_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = ENSPIE_slope, y = Sn_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = bt_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(x = ENSPIE_slope, y = Sn_slope, 
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'a') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

Sn_S_PIE_2d_post_density_time2 <-
  ggplot() +
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
  geom_point(data = btx_study_summary %>% 
               filter(componentChange=='No change'),
             aes(x = ENSPIE_slope,
                 y = Sn_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(x = ENSPIE_slope,
                 y = Sn_slope,
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'd') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

Sn_S_PIE_2d_post_density_space1 <-
  ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                                    filter(LU!='Primary vegetation')),
                                  # fw_study_posterior %>% 
                                  #   ungroup() %>% 
                                  #   filter(Treatment!='Reference stream')),
                 aes(x = ENSPIE_global + ENSPIE,
                     y = Sn_global + Sn,
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2) %>% 
               filter(componentChange=='No change'),
             aes(x = S_PIE,
                 y = Sn, colour = 'No change'),
             size = 0.75) +
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2) %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(S_PIE)),
             aes(x = S_PIE,
                 y = Sn, 
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = expression(paste('Change in evenness [log(', S[PIE], ')]', sep = '')),
       tag = 'j') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = c(1,0.55),
        legend.justification = c(1, 1),
        strip.text = element_blank(),
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold')) #+
  # guides(colour = guide_legend(override.aes = list(alpha = 1)))

Sn_S_PIE_2d_post_density_space4 <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = spatial_multi4_posterior %>% 
                   filter(newID!='s_1'),
                 aes(x = ENSPIE_study,
                     y = Sn_study,
                     fill = 'S_PIE_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange=='No change'),
             aes(x = ENSPIE, y = Sn, colour = 'No change'),
             size = 0.75) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)),
             aes(x = ENSPIE, y = Sn,
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'g') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

# Sn ~ N
Sn_N_2d_post_density_time1 <-
  ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(y = Sn_study + Sn_global,
                     x = N_study + N_global,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary %>% 
               filter(componentChange=='No change'),
             aes(y = Sn_slope, x = N_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = bt_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(y = Sn_slope, x = N_slope,
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'b') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

Sn_N_2d_post_density_time2 <-
  ggplot() +
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(y = Sn_study,
                     x = N_study,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange=='No change'),
             aes(y = Sn_slope,
                 x = N_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(y = Sn_slope,
                 x = N_slope,
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'e') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

Sn_N_2d_post_density_space1 <-
  ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                                    filter(LU!='Primary vegetation')),
                                  # fw_study_posterior %>% 
                                  #   ungroup() %>% 
                                  #   filter(Treatment!='Reference stream')),
                 aes(y = Sn_global + Sn,
                     x = N_global + N,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2) %>% 
               filter(componentChange=='No change'),
             aes(y = Sn,
                 x = N, colour = 'No change'),
             size = 0.75) +
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2) %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(S_PIE)),
             aes(y = Sn,
                 x = N, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = expression(paste('Change in total abundance [log(N)]', sep = '')),
       tag = 'k') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = c(1,0.55),
        legend.justification = c(1, 1),
        strip.text = element_blank(),
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold')) #+
  # guides(colour = guide_legend(override.aes = list(alpha = 1)))

Sn_N_2d_post_density_space4 <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = spatial_multi4_posterior %>%
                   filter(newID!='s_1'),
                 aes(y = Sn_study,
                     x = N_study,
                     fill = 'Sn_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange=='No change'),
             aes(y = Sn, x = N, colour = 'No change'),
             size = 0.75) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)),
             aes(y = Sn, x = N, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'h') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

# S_PIE ~ N
S_PIE_N_2d_post_density_time1 <-
  ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bt_study_sample_posterior,
                 aes(y = ENSPIE_study + ENSPIE_global,
                     x = N_study + N_global,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = bt_study_summary %>% 
               filter(componentChange=='No change'),
             aes(y = ENSPIE_slope, x = N_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = bt_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(y = ENSPIE_slope, x = N_slope, 
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'c') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_PIE_N_2d_post_density_time2 <-
  ggplot() +
  mapply(function(level) {
    stat_ellipse(data = btx_study_sample_posterior,
                 aes(y = ENSPIE_study,
                     x = N_study,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange=='No change'),
             aes(y = ENSPIE_slope,
                 x = N_slope, colour = 'No change'),
             size = 0.75) +
  geom_point(data = btx_study_summary %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N_slope) * sign(ENSPIE_slope)),
             aes(y = ENSPIE_slope,
                 x = N_slope,
                 colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',
       tag = 'f') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

S_PIE_N_2d_post_density_space1 <-
  ggplot() +
  # posterior density
  mapply(function(level) {
    stat_ellipse(data = bind_rows(PREDICTS_SS_posterior %>% 
                                    filter(LU!='Primary vegetation')),
                                  # fw_study_posterior %>% 
                                  #   ungroup() %>% 
                                  #   filter(Treatment!='Reference stream')),
                 aes(y = ENSPIE_global + ENSPIE,
                     x = N_global + N,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  }, 
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2) %>% 
               filter(componentChange=='No change'),
             aes(y = S_PIE,
                 x = N, colour = 'No change'),
             size = 0.75) +
  geom_point(data = SS_LU_multi_filtered %>% 
               filter(LU!='Primary vegetation' & Sn > -2) %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(S_PIE)),
             aes(y = S_PIE,
                 x = N, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = expression(paste('Change in total abundance [log(N)]', sep = '')),
       tag = 'l') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = c(1,0.55),
        legend.justification = c(1, 1),
        strip.text = element_blank(),
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold')) #+
  # guides(colour = guide_legend(override.aes = list(alpha = 1)))

S_PIE_N_2d_post_density_space4 <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = spatial_multi4_posterior %>% 
                   filter(newID!='s_1'),
                 aes(y = ENSPIE_study,
                     x = N_study,
                     fill = 'N_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange=='No change'),
             aes(y = ENSPIE, x = N, colour = 'No change'),
             size = 0.75) +
  geom_point(data = spat_coefs_multi4_filtered %>% 
               filter(componentChange!='No change') %>% 
               mutate(deltaN_ENSPIE_sign = sign(N) * sign(ENSPIE)),
             aes(y = ENSPIE, x = N, colour = as.character(deltaN_ENSPIE_sign)),
             size = 0.75) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  labs(y = '',
       x = '',#expression(paste('Change in total abundance [log(N)]', sep = '')),
       tag = 'i') +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  scale_colour_manual(values = c('No change' = '#d9d9d9',
                                 '-1' = '#252525',
                                 '1' = '#737373'),
                      guide = FALSE) +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.tag.position = c(0.25,0.9),
        plot.tag = element_text(size = 10, face = 'bold'))

time1_cols_supp = plot_grid(time_t1,
                       plot_grid(Sn_S_PIE_2d_post_density_time1,
                                 Sn_N_2d_post_density_time1,
                                 # S_PIE_N_2d_post_density_time1,
                                 align = 'hv',
                                 ncol = 2),
                       ncol = 1, rel_heights = c(0.05, 1))

time2_cols_supp = plot_grid(time_t2,
                       plot_grid(Sn_S_PIE_2d_post_density_time2,
                                 Sn_N_2d_post_density_time2,
                                 align = 'hv',
                                 ncol = 2),
                       ncol = 1, rel_heights = c(0.01, 1))

space1_cols_supp = plot_grid(space_t1,
                        plot_grid(Sn_S_PIE_2d_post_density_space1,
                                  Sn_N_2d_post_density_space1,
                                  # S_PIE_N_2d_post_density_space1,
                                  align = 'hv',
                                  ncol = 2),
                        ncol = 1, rel_heights = c(0.01, 1))

space4_cols_supp = plot_grid(space_t4,
                        plot_grid(Sn_S_PIE_2d_post_density_space4,
                                  Sn_N_2d_post_density_space4,
                                  align = 'hv',
                                  ncol = 2),
                        ncol = 1, rel_heights = c(0.01, 1))

two_col = plot_grid(time1_cols_supp, time2_cols_supp, 
                    space4_cols_supp, space1_cols_supp, 
                    # space2_cols_supp, space3_cols_supp,
          nrow = 4,
          align = 'hv') +
  draw_label(x = 0.02, label = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = '')),
             angle = 90)

three_col = plot_grid(S_PIE_N_2d_post_density_time1, 
                      S_PIE_N_2d_post_density_time2, 
                      S_PIE_N_2d_post_density_space4, 
                      S_PIE_N_2d_post_density_space1, 
                      # S_PIE_N_2d_post_density_space2, 
                      # S_PIE_N_2d_post_density_space3,
                      align = 'hv', nrow = 4) +
  draw_label(x = 0.01, label = expression(paste('Change in evenness [log(', S[PIE], ')]')),
             angle = 90)

plot_grid(leg,
          plot_grid(two_col,
          three_col,
          ncol = 2, align = 'hv',
          rel_widths = c(1,0.5)),
          nrow = 2, rel_heights = c(0.05, 1))

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/FigSx_scatter_rPurschke.png',
       height = 290, width = 240, units = 'mm')
