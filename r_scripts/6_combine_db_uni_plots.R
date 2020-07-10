# combined plots (biotime and predicts) for changes estimated
# with univariate models

rm(list=ls())

source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/bt_uni_coef_wrangle.R')
source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/PREDICTS_uni_coef_wrangle.R')
source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/fwater_uni_wrangle.R')
source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/fragSAD_coef_wrangle.R')


# colour scheme for 4 databases
db_colour <- c('PREDICTS' = '#1b9e77',
               'BioTIME' = '#d95f02',
               'fragSAD' = '#7570b3',
               'Freshwater LU' = '#e7298a')

db_shape <- c('PREDICTS' = 20,
               'BioTIME' = 15,
               'fragSAD' = 3,
               'Freshwater LU' = 19)

S_N_uni_combine <-
  ggplot() +
    geom_point(data = SS_LU_univariate %>% 
                 filter(LU!='Primary vegetation' & Sn > -2),
               aes(x = N, y = S, 
                   shape = db,
                   # size = change_size,
                   colour = db),
               size = 0.5,
               # alpha = 0.5
               ) +
  geom_point(data = fwater_study_LU_uni %>% 
               filter(LU!='Reference stream'),
             aes(x = N, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             size = 0.5,
             # alpha = 0.75
  ) +
  geom_point(data = Sn_study_corr,
             aes(x = deltaN, deltaS, 
                 shape = db,
                 colour = db,
                 # size = change_size, 
                 # stroke = change_stroke
                 ),
             size = 0.5,
             alpha = 1) +#size = sizeS,
  geom_point(data = fSAD_uni_coefs,
             aes(x = -N, -S, 
                 shape = db,
                 colour = db,
                 # size = change_size, 
                 # stroke = change_stroke
             ),
             size = 0.5,
             alpha = 0.25,
             #shape = 3
             ) +#size = sizeS,
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
    labs(y = expression(paste('Change in species richness [log(S)]', sep = '')),
       x = expression(paste('Change in total number of individuals [log(N)]', sep = '')),
       tag = 'a') +
  scale_alpha(guide = FALSE) +
  scale_colour_manual(name = '',
                      values = db_colour, 
                      # guide = F
                      ) +
  scale_shape_manual(name = '', values = db_shape) +
  theme_minimal() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.spacing.y = unit(0, 'mm'),
        legend.text = element_text(margin = margin(t = 0, l = 0), hjust = 0),
        legend.background = element_blank(),
        text = element_text(size = 6)) +
    guides(colour = guide_legend())


S_Sn_uni_combine <-
  ggplot() +
  geom_point(data = SS_LU_univariate %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = Sn, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             # alpha = 0.5
             size = 0.5
             ) +
  geom_point(data = fwater_study_LU_uni %>% 
               filter(LU!='Reference stream'),
             aes(x = Sn, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             size = 0.5,
             # alpha = 0.75
  ) +
  geom_point(data = Sn_study_corr,
             aes(x = deltaSn, deltaS, 
                 shape = db,
                 colour = db,
                 # size = change_size, stroke = change_stroke
                 ),
             alpha = 1,
             size = 0.5) +
  geom_point(data = fSAD_uni_coefs,
             aes(x = -Sn, -S, 
                 shape = db,
                 colour = db,
                 # size = change_size, 
                 # stroke = change_stroke
             ),
             size = 0.5,
             alpha = 0.25
             # shape = 3
             ) +#size = sizeS,
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5)) +
  labs(y = '', #expression(paste('Change in species richness', sep = '')),
       x = expression(paste('Change in rarefied richness [log(',S[n],']', sep = '')),
       tag = 'b') +
  scale_alpha(guide = FALSE) +
  scale_colour_manual(values = db_colour, guide = F) +
  scale_shape_manual(name = '', values = db_shape, guide = F) +
  theme_minimal() +
  theme(text = element_text(size = 6))

S_ENSPIE_uni_combine <-
  ggplot() +
  geom_point(data = SS_LU_univariate %>% 
               filter(LU!='Primary vegetation' & Sn > -2),
             aes(x = S_PIE, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             # alpha = 0.5
             size = 0.5
             ) +
  geom_point(data = fwater_study_LU_uni %>% 
               filter(LU!='Reference stream'),
             aes(x = S_PIE, y = S, 
                 shape = db,
                 # size = change_size,
                 colour = db),
             size = 0.5,
             # alpha = 0.75
  ) +
  geom_point(data = Sn_study_corr,
             aes(x = deltaENSPIE, deltaS, 
                 shape = db,
                 colour = db,
                 # size = change_size, stroke = change_stroke
                 ),
             alpha = 1,
             size = 0.5) +
  geom_point(data = fSAD_uni_coefs,
             aes(x = -S_PIE, -S, 
                 shape = db,
                 colour = db,
                 # size = change_size, 
                 # stroke = change_stroke
             ),
             size = 0.5,
             alpha = 0.25,
             # shape = 3
             ) +#size = sizeS,
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = '',# expression(paste('Change in species richness', sep = '')),
       x = expression(paste('Change in evenness [log(',S[PIE],']', sep = '')),
       tag = 'c') +
  scale_alpha(guide = FALSE) +
  scale_colour_manual(values = db_colour, guide = F) +
  scale_shape_manual(name = '', values = db_shape, guide = F) +
  theme_minimal() +
  theme(text = element_text(size = 6))


lower_panels <- cowplot::plot_grid(S_N_uni_combine,
                                   S_Sn_uni_combine,
                                   S_ENSPIE_uni_combine,
                                   ncol = 3)

combined_uni_title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Component changes estimated independently",
    # fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

cowplot::plot_grid(combined_uni_title,
                   lower_panels,
                   rel_heights = c(0.1, 1),
                   nrow=2, axis = 'tlbr')


ggsave('~/Dropbox/1current/BioTime/multidimensionalChangeMS/Figs/combined_uni_deltaDelta4_noTitle.png',
       width = 240, height = 80, units = 'mm')
