# new Figure 2: simplify and present two sections: positive relationships and negative (or no) relationship
# conceptual illustration of diversity change
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')
# here are some simulations that illustrate the changes I want to show (rerunning the simulations can result in slight alterations to the figure)
load("/Users/sb25gaqy/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/conceptual_figure_dat.Rdata")

# label to add to first panel
three_row_label = c('Component changes between the grey and colored',
                    'curves are represented by the respective shapes',
                    'on panels (g)-(i) below.')

four_row_label = c('Component changes between',
                   'grey and colored curves',
                    'are represented by shapes',
                    'on panels (g)-(i) below.')

# positive panels
top_positive <- plot_grid(NULL,
                          plot_grid(NULL,
                                    mih_ibr +
                                      labs(subtitle = '(a) More individuals only') +
                                      theme(axis.title.y = element_blank(),
                                            axis.text = element_blank(),
                                            axis.ticks = element_blank(),
                                            plot.subtitle = element_text(size = 7, face = 'bold')) +
                                      draw_text(text = four_row_label, x = 8.5, y = c(3.5, 3, 2.5, 2), hjust = 0, size = 7),
                                    even_ibr +
                                      labs(subtitle = '(b) Altered SAD (e.g., increased evenness)') +
                                      theme(axis.title.y = element_blank(),
                                            axis.text = element_blank(),
                                            axis.ticks = element_blank(),
                                            plot.subtitle = element_text(size = 7, face = 'bold')),
                                    even2_ibr +
                                      labs(subtitle = '(c) More individuals, more even SAD') +
                                      theme(axis.title.y = element_blank(),
                                            axis.text = element_blank(),
                                            axis.ticks = element_blank(),
                                            plot.subtitle = element_text(size = 7, face = 'bold')),
                                    nrow = 1,
                                    rel_widths = c(0.1, 1,1,1)),
                          rel_heights = c(0.075, 1),
                          ncol = 1) +
  draw_label('Species', x = 0.01, y = 0.5, angle = 90, size = 10) #+
  # draw_label('Individuals', x = 0.5, y = 0.01, size = 10) +
  # draw_label('Abundance and evenness change in same direction', x = 0.175, y = 0.97, size = 11, fontface = 'bold')

top_negative <- plot_grid(NULL,
                          plot_grid(NULL,
                                    dom_ibr +
                                      labs(subtitle = '(d) More individuals, less even, no change richness') +
                                      theme(axis.title.y = element_blank(),
                                            axis.text = element_blank(),
                                            axis.ticks = element_blank(),
                                            plot.subtitle = element_text(size = 7, face = 'bold')),
                                    dom2_ibr +
                                      labs(subtitle = '(e) More individuals, less even, fewer species') +
                                      theme(axis.title.y = element_blank(),
                                            axis.text = element_blank(),
                                            axis.ticks = element_blank(),
                                            plot.subtitle = element_text(size = 7, face = 'bold')),
                                    dom3_ibr +
                                      labs(subtitle = '(f) More individuals, less even SAD, more species') +
                                      theme(axis.title.y = element_blank(),
                                            axis.text = element_blank(),
                                            axis.ticks = element_blank(),
                                            plot.subtitle = element_text(size = 7, face = 'bold')),
                                    nrow = 1,
                                    rel_widths = c(0.1, 1,1,1)),
                          rel_heights = c(0.075, 1),
                          ncol = 1) +
  draw_label('Species', x = 0.01, y = 0.5, angle = 90, size = 10) #+
  # draw_label('Individuals', x = 0.5, y = 0.01, size = 12) #+
  # draw_label('Opposing changes in abundance and evenness', x = 0.16, y = 0.975, size = 11, fontface = 'bold')

dS_dN <-
ggplot() +
  geom_point(data = ES %>% 
               filter(treatment %in% c('individuals', 'even', 'even2', 'dom', 'dom2', 'dom3')), 
             aes(x = N_ES, y = S_ES, colour = treatment, shape = treatment),
             size = 3, alpha = 1, stroke = 1.5) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S))),
           x = 0, y = Inf, parse = T,
           hjust = 1.25, vjust = 1) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(N))),
           x = Inf, y = 0, parse = T,
           hjust = 0.5,
           vjust = 1.25
  ) +
  scale_color_manual(guide = 'none',
                     values = trt_col) +
  scale_shape_manual(guide = F, values = delta_shape) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(name = '', breaks = c(0)) +
  scale_y_continuous(name = '', breaks = c(0)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.title = element_text(size = 8),
        plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))

dS_dSn <-
  ggplot() +
  geom_point(data = ES %>% 
               filter(treatment %in% c('individuals', 'even', 'even2', 'dom', 'dom2', 'dom3')), 
             aes(x = Sn_ES, y = S_ES, colour = treatment, shape = treatment),
             size = 3, alpha = 1, stroke = 1.5) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S))),
           x = 0, y = Inf, parse = T,
           hjust = 0, vjust = 1) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S)[n])),
           x = Inf, y = 0, parse = T,
           hjust = 0.5,
           vjust = 1.25
  ) +
  scale_color_manual(guide = 'none',
                     values = trt_col) +
  scale_shape_manual(guide = F, values = delta_shape) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(name = '', breaks = c(0)) +
  scale_y_continuous(name = '', breaks = c(0)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.title = element_text(size = 8),
        plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))

dS_dS_PIE <-
  ggplot() +
  geom_point(data = ES %>% 
               filter(treatment %in% c('individuals', 'even', 'even2', 'dom', 'dom2', 'dom3')), 
             aes(x = ENSPIE_ES, y = S_ES, colour = treatment, shape = treatment),
             size = 3, alpha = 1, stroke = 1.5) +
  scale_shape_manual(values = delta_shape, guide = F) +
  scale_color_manual(guide = 'none',
                     values = trt_col) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S))),
           x = 0, y = Inf, parse = T,
           hjust = 1.25, vjust = 1) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S)[PIE])),
           x = Inf, y = 0, parse = T,
           hjust = 1,
           vjust = 1.25
  ) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(name = '', breaks = c(0)) +
  scale_y_continuous(name = '', breaks = c(0)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.title = element_text(size = 8),
        plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm')) 


label1 = substitute(paste('(g) ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'individuals'))
label2 = substitute(paste('(h) ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'rarefied richness'))
label3 = substitute(paste('(i) ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'evenness'))

# bottom <- 
cowplot::plot_grid(NULL, # white space for mansucript central header
                   top_positive,
                   top_negative,
                   shape_legend2,
                   plot_grid(dS_dN +
                               draw_label(label = label1, fontface = 'bold', 
                                          x = -Inf, y = Inf,
                                          hjust = 0.2, vjust = -0.5, lineheight = 1, size = 8),
                             dS_dSn +
                               draw_label(label = label2, fontface = 'bold', 
                                          x = -Inf, y = Inf,
                                          hjust = 0.2, vjust = -0.5, lineheight = 1, size = 8),
                             dS_dS_PIE +
                               draw_label(label = label3, 
                                          x = -Inf, y = Inf,
                                          hjust = 0.3, vjust = -0.5, lineheight = 1, size = 8,
                                          fontface = 'bold' ),
                             nrow = 1),
                   nrow = 5,
                   rel_heights = c(0.05, 1,1,0.4,1))


ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/revision/Fig2.pdf', 
        width = 203.2, height = 152.4, units = 'mm')
