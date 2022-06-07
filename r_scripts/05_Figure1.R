# conceptual illustration of diversity change metrics and rarefaction
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

# first want to show two rarefaction curves and illustrate deltaN, deltaS, deltaSn, deltaPIE
# reuse simulations from Chase et al 2018 Figure 5c
load(paste0(path2wd, 'multiComponentChange/data/conceptual_communities.Rdata'))

fig1_df <- bind_rows(tibble(species = attributes(comm0)$names) %>%
                       mutate(concept = 'A',
                              community = 'A',
                              N = as.numeric(comm0)),
                     tibble(species = attributes(comm01)$names) %>%
                       mutate(concept = 'A',
                              community = 'B',
                              N = as.numeric(comm01)),
                     tibble(species = attributes(comm02)$names) %>%
                       mutate(concept = 'A',
                              community = 'C',
                              N = as.numeric(comm02)),
                     # now the communities with crossing rarefactions
                     tibble(species = attributes(comm1)$names) %>%
                       mutate(concept = 'B',
                              community = 'A',
                              N = as.numeric(comm1)),
                     tibble(species = attributes(comm2)$names) %>%
                       mutate(concept = 'B',
                              community = 'B',
                              N = as.numeric(comm2)),
                     tibble(species = attributes(comm3)$names) %>%
                       mutate(concept = 'B',
                              community = 'C',
                              N = as.numeric(comm3))) %>%
  group_by(concept, community) %>%
  nest(data = c(species, N)) %>%
  # now calculate rarefaction curve each community
  mutate(Sn = map(data, ~rarefaction(.x$N, method = 'IBR', effort = 1:sum(.x$N)))) %>%
  ungroup() %>%
  # unnest for plotting
  unnest(Sn) %>%
  group_by(concept, community) %>%
  mutate(N = 1:length(Sn)) %>%
  ungroup()

metric_intro <-
ggplot() +
  # facet_wrap(community~concept, scales = 'free') +
  geom_line(data = fig1_df %>% filter(concept=='A' & community=='A'),
            aes(x = N, y = Sn),
            size = 1.25) +
  # geom_point(data = fig1_df %>% filter(concept=='A' & community=='A'),
  #           aes(x = max(N), y = max(Sn)),
  #           size = 3.25, stroke = 1.5,
  #           shape = 4) +
  geom_line(data = fig1_df %>% filter(concept=='B' & community=='A') %>% slice(1:70),
            aes(x = N, y = Sn),
            size = 1.25, colour = '#bdbdbd') +
  # arrows to depict slope at base
  geom_segment(data = fig1_df %>% filter(concept=='A' & community=='A'),
               aes(x = 1, xend = 5, y = 1, yend = Sn[which(N==5)]),
               arrow = arrow(length = unit(3, 'mm')),
               size = 1.5) +
  geom_segment(data = fig1_df %>% filter(concept=='B' & community=='A') %>% slice(1:70),
               aes(x = 1, xend = 5, y = 1, yend = Sn[which(N==5)]),
               arrow = arrow(length = unit(3, 'mm')),
               size = 1.5, colour = '#bdbdbd') +
  # vertical line for deltaSn
  geom_segment(data = fig1_df %>% filter(concept=='B' & community=='A') %>% slice(70) %>% rename(Smin = Sn) %>% 
                 mutate(Smax = 31.2),
               aes(x = N, xend = N, y = Smin, yend = Smax),
               linetype = 2,
               arrow = arrow(length = unit(3, 'mm'), ends = 'both', type = 'closed')) +
  # demarking line for deltaS & deltaN
  geom_segment(data = NULL,
               aes(x = 70, xend = 200, y = 20.6, yend = 20.6),
               linetype = 2,
               arrow = arrow(length = unit(3, 'mm'), ends = 'both', type = 'closed')) +
  geom_segment(data = fig1_df %>% filter(concept=='A' & community=='A') %>% slice(200) %>% rename(Smax = Sn) %>% 
                 mutate(Smin = 20.6),
               aes(x = N, xend = N, y = Smin, yend = Smax),
               linetype = 2,
               arrow = arrow(length = unit(3, 'mm'), ends = 'both', type = 'closed')) +
  annotate('text', x = 20, y = 3,
           label = 'Delta*PIE', parse = T, size = 5) +
  annotate('text', x = 77, y = 26,
           label = 'Delta*S[n]', parse = T, size = 5) +
  annotate('text', x = 205, y = 30,
           label = 'Delta*S', parse = T, size = 5) +
  annotate('text', x = 135, y = 22,
           label = 'Delta*N', parse = T, size = 5) +
  labs(x = 'Abundance [number of individuals]',
       y = 'Expected number of species',
       # tag = '(a)'
       ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(3, 3, 2, 3), units = 'cm'), # manuscript central adds line numbers to figures?!
        legend.position = 'none')

# calculate deltas
ES <- bind_cols(fig1_df %>% 
  filter(concept=='A' & community=='A' & (N==200 | N==70)) %>% 
  mutate(S_PIE = map(data, ~vegan::diversity(.x$N, index = 'invsimpson'))) %>% 
  unnest(S_PIE) %>% 
  slice(2) %>% 
  rename(S = Sn) %>%   
  mutate(Sn = 31.2) %>% 
  select(-data, -concept, -community),
  fig1_df %>% 
    filter(concept=='B' & community=='A' & N==70) %>% 
    mutate(S_PIE_ref = map(data, ~vegan::diversity(.x$N, index = 'invsimpson'))) %>% 
    unnest(S_PIE_ref) %>% 
    rename(S_ref = Sn,
           N_ref = N) %>% 
    mutate(Sn_ref = S_ref) %>% 
    select(-data, -concept, -community)) %>% 
  mutate(N_ES = log(N / N_ref),
         S_ES = log(S / S_ref),
         ENSPIE_ES = log(S_PIE / S_PIE_ref),
         Sn_ES = log(Sn / Sn_ref))


dS_dN <-
  ggplot() +
  geom_point(data = ES, 
             aes(x = N_ES, y = S_ES),
             size = 2, stroke =1.5, shape = 4) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S))),
           x = 0, y = Inf, parse = T,
           hjust = 1.25, vjust = 1) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(N))),
           x = Inf, y = 0, parse = T,
           hjust = 1.25,
           vjust = 1.25
  ) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(name = '', breaks = c(0)) +
  scale_y_continuous(name = '', breaks = c(0)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))

dS_dSn <-
  ggplot() +
  geom_point(data = ES, 
             aes(x = Sn_ES, y = S_ES),
             size = 2, stroke =1.5, shape = 4) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S))),
           x = 0, y = Inf, parse = T,
           hjust = 1.25, vjust = 1) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S)[n])),
           x = Inf, y = 0, parse = T,
           hjust = 1.25,
           vjust = 1.25
  ) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(name = '', breaks = c(0)) +
  scale_y_continuous(name = '', breaks = c(0)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))

dS_dS_PIE <-
  ggplot() +
  geom_point(data = ES, 
             aes(x = N_ES, y = ENSPIE_ES),
             size = 2, stroke =1.5, shape = 4) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S))),
           x = 0, y = Inf, parse = T,
           hjust = 1.25, vjust = 1) +
  annotate(geom = 'text', 
           label = expression(paste(Delta, italic(S)[PIE])),
           x = Inf, y = 0, parse = T,
           hjust = 1.25,
           vjust = 1.25
  ) +
  geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
  geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
  # geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
  scale_x_continuous(name = '', breaks = c(0)) +
  scale_y_continuous(name = '', breaks = c(0)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(axis.title = element_text(size = 10),
        plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm')) 


label1 = substitute(paste('(b) ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'individuals'))
label2 = substitute(paste('(c) ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'rarefied richness'))
label3 = substitute(paste('(d) ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'evenness'))

plot_grid(metric_intro,
          plot_grid(dS_dN +
                      draw_label(label = label1, fontface = 'bold', 
                                 x = -Inf, y = Inf,
                                 hjust = 0.2, vjust = -0.5, lineheight = 1, size = 12),
                    dS_dSn +
                      draw_label(label = label2, fontface = 'bold', 
                                 x = -Inf, y = Inf,
                                 hjust = 0.2, vjust = -0.5, lineheight = 1, size = 12),
                    dS_dS_PIE +
                      draw_label(label = label3, 
                                 x = -Inf, y = Inf,
                                 hjust = 0.3, vjust = -0.5, lineheight = 1, size = 12,
                                 fontface = 'bold' ),
                    ncol = 3),
          ncol = 1, rel_heights = c(1, 0.33))

ggsave(paste0(path2wd, 'Figs/revision/Fig1.pdf'),
       width = 250, height = 250, units = 'mm')
       
