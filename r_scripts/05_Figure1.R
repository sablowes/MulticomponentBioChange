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
  mutate(Sn = map(data, ~rarefaction(.x$N, method = 'indiv', effort = 1:sum(.x$N)))) %>%
  ungroup() %>%
  # unnest for plotting
  unnest(Sn) %>%
  group_by(concept, community) %>%
  mutate(N = 1:length(Sn)) %>%
  ungroup()

# metric_intro <-
ggplot() +
  # facet_wrap(community~concept, scales = 'free') +
  geom_line(data = fig1_df %>% filter(concept=='A' & community=='A'),
            aes(x = N, y = Sn),
            size = 1.25) +
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
  xlab('Abundance [number of individuals]') +
  ylab('Expected number of species') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = 'none')

ggsave(paste0(path2wd, 'Figs/submission/Fig1.pdf'),
       width = 150, height = 150, units = 'mm')
       