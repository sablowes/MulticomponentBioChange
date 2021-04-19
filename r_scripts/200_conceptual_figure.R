# conceptual illustration of diversity change
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')

s = 1793
# s = rnbinom(1, 1e-1, mu = 20000) + rpois(1, 10)
set.seed(s)
# 1: more individuals
# 2: more rare species
# 3: more common and rare species
# 4: increased N, decreased evenness, no change S
# 5: increased N, decreased evenness, decreased S
# 6: increased N, decreased evenness, increased S

# 12 spp colour map
spp_col = c('species1' = '#a6cee3', 'species2' = '#1f78b4', 'species3' = '#b2df8a',
            'species4' = '#33a02c', 'species5' = '#fb9a99','species6' = '#e31a1c', 
            'species7' = '#fdbf6f', 'species8' = '#ff7f00', 'species9' = '#cab2d6',
            'species10' = '#6a3d9a', 'species11' = '#ffff99', 'species12' = '#b15928')

# change shapes
delta_shape = c('individuals' = 2, 'rare' = 3, 'even' = 4,
                'dom' = 5, 'dom2' = 8, 'dom3' = 17)
# -------reference assemblage-------------
sd_lnorm1 <- 1
S_initial = 6
N_initial = 16

reference1 <- bind_cols(assemblage = 'Reference',
                sd_lnorm = sd_lnorm1,
                S = S_initial,
                N = N_initial) %>% 
  nest(data = c(sd_lnorm, S, N)) %>% 
  mutate(assemblage_map = map(data, ~ sim_thomas_community(.x$S, .x$N, sad_type = 'lnorm', fix_s_sim = F,
                                                                  sad_coef = list('meanlog'=.x$N/.x$S,
                                                                                   'sdlog' = .x$sd_lnorm#,
                                                                                   # 'cv_abund' = .x$sd_lnorm
                                                                                  ),
                                                                  sigma = 0.7))) %>% 
  # sample 20 quadrats from each community
  mutate(sample = map(assemblage_map, ~sample_quadrats(comm = .x, n_quadrats = 20,
                                                            method = 'random', avoid_overlap = T,
                                                            plot = F))) %>% 
  # calculate diversity
  mutate(N_alpha = map(sample, ~rowSums(.x$spec_dat) %>% as.numeric),
         S_alpha = map(sample, ~vegan::specnumber(.x$spec_dat)),
         ENSPIE_alpha = map(sample, ~vegan::diversity(.x$spec_dat, index = 'invsimpson')),
         N_gamma = map(assemblage_map, ~nrow(.x$census)),
         S_gamma = map(assemblage_map, ~ div_rect(x0 = 0, y0 = 0, 
                                                         xsize = 1, ysize = 1, 
                                                         comm = .x)[['n_species']]),
         ENSPIE_gamma = map(assemblage_map, ~div_rect(x0 = 0, y0 = 0, 
                                                              xsize = 1, ysize = 1, 
                                                              comm = .x)[['ens_simpson']]),
         # rarefaction curves (gamma-scale)
         ibr = map(assemblage_map, ~spec_sample_curve(.x, method = c("rare"))))

ref_map <-
reference1$assemblage_map[[1]]$census %>% 
  mutate(assemblage = 'Reference') %>% 
  ggplot() +
  facet_wrap(~assemblage) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  # scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  coord_fixed(xlim = c(0,1),
              ylim = c(0,1)) +
  theme(legend.position = 'none',
        strip.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank())

#------ 1: more individuals (same SAD)---------
MIH <- bind_cols(assemblage = 'MIH',
                 sd_lnorm = sd_lnorm1,
                 S = S_initial,
                 N_initial = N_initial, # for reference SAD
                 N = N_initial +  1.5 * N_initial
                 ) %>% 
  nest(data = c(sd_lnorm, S, N_initial, N)) %>% 
  mutate(assemblage_map = map(data, ~ sim_thomas_community(.x$S, .x$N, sad_type = 'lnorm', fix_s_sim = F,
                                                           sad_coef = list('meanlog'=.x$N/.x$S,
                                                                           'sdlog' = .x$sd_lnorm#,
                                                                           # 'cv_abund' = .x$sd_lnorm
                                                           ),
                                                           sigma = 0.7))) %>% 
  # sample 20 quadrats from each community
  mutate(sample = map(assemblage_map, ~sample_quadrats(comm = .x, n_quadrats = 20,
                                                       method = 'random', avoid_overlap = T,
                                                       plot = F))) %>% 
  # calculate diversity
  mutate(N_alpha = map(sample, ~rowSums(.x$spec_dat) %>% as.numeric),
         S_alpha = map(sample, ~vegan::specnumber(.x$spec_dat)),
         ENSPIE_alpha = map(sample, ~vegan::diversity(.x$spec_dat, index = 'invsimpson')),
         N_gamma = map(assemblage_map, ~nrow(.x$census)),
         S_gamma = map(assemblage_map, ~ div_rect(x0 = 0, y0 = 0, 
                                                  xsize = 1, ysize = 1, 
                                                  comm = .x)[['n_species']]),
         ENSPIE_gamma = map(assemblage_map, ~div_rect(x0 = 0, y0 = 0, 
                                                      xsize = 1, ysize = 1, 
                                                      comm = .x)[['ens_simpson']]),
         # rarefaction curves (gamma-scale)
         ibr = map(assemblage_map, ~spec_sample_curve(.x, method = c("rare"))))

MIH_map <-
MIH$assemblage_map[[1]]$census %>% 
  mutate(assemblage = 'More individuals') %>% 
  ggplot() +
  facet_wrap(~assemblage) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  # scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  coord_fixed(xlim = c(0,1),
              ylim = c(0,1)) +
  theme(legend.position = 'none',
        strip.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 2))

mih_ibr <-
bind_rows(reference1 %>%
            unnest(ibr) %>% 
            select(assemblage, n, spec_rarefied),
          MIH %>% 
            unnest(ibr) %>% 
            select(assemblage, n, spec_rarefied) %>% 
            mutate(assemblage = 'More individuals')) %>% 
  ggplot() +
  geom_line(aes(x = n, y = spec_rarefied, 
                colour = assemblage, 
                group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More individuals')]), shape = 'individuals', colour = 'More individuals'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
  scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                 'More individuals' = '#004b6e'),
                      name = '') +
  # scale_y_continuous(breaks = c(1,3,5,7,9)) +
  # scale_x_continuous(breaks = round(seq(1,30,length.out = 5))) +
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1), 
        # panel.background = element_blank(),
        plot.margin = margin(t = 2),
        legend.text = element_text(size = 8))

#----- 2: more rare species (chop off the MIH sample at the reference N)--------
rare_map <-
MIH$assemblage_map[[1]]$census %>% 
  mutate(assemblage = 'More rare species') %>% 
  # want same N as reference assemblage
  sample_n(N_initial) %>% 
  ggplot() +
  facet_wrap(~assemblage) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  # scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  coord_fixed(xlim = c(0,1),
              ylim = c(0,1)) +
  theme(legend.position = 'none',
        strip.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 2))

rare_ibr <-
bind_rows(reference1 %>%
            unnest(ibr) %>% 
            select(assemblage, n, spec_rarefied),
          MIH %>% 
            unnest(ibr) %>% 
            select(assemblage, n, spec_rarefied) %>% 
            mutate(assemblage = 'More rare species')) %>% 
  filter(n < N_initial) %>% 
  ggplot() +
  geom_line(aes(x = n, y = spec_rarefied, 
                colour = assemblage, 
                group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More rare species')]), shape = 'rare', colour = 'More rare species'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
  scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                 'More rare species' = '#4f5794'),
                      name = '') +
  # scale_y_continuous(breaks = c(1,3,5,7,9)) +
  # scale_x_continuous(breaks = round(seq(1,30,length.out = 5))) +
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 8),
        plot.margin = margin(t = 2))

#------3: more even community----------
# getting the greatest (piece of) PIE is tricky...must be done by hand
even_comm <- community(x = unlist(reference1$assemblage_map[[1]]$census['x']),
                       y = unlist(reference1$assemblage_map[[1]]$census['y']),
                       spec_id = c(rep('species1', times = 2),
                                   rep('species2', times = 2),
                                   rep('species3', times = 2),
                                   rep('species4', times = 2),
                                   rep('species5', times = 2),
                                   rep('species6', times = 2),
                                   rep('species7', times = 2),
                                   rep('species8', times = 2)))
# more_even_ass <- bind_cols(assemblage = 'More even assemblage',
#                  sd_lnorm = sd_lnorm1/2,
#                  S = S_initial + 0.25*S_initial,
#                  N = N_initial 
# ) %>% 
#   nest(data = c(sd_lnorm, S, N)) %>% 
#   mutate(assemblage_map = map(data, ~ sim_thomas_community(.x$S, .x$N, sad_type = 'lnorm', fix_s_sim = T,
#                                                            sad_coef = list('meanlog'=.x$N/.x$S,
#                                                                            'sdlog' = .x$sd_lnorm#,
#                                                                            # 'cv_abund' = .x$sd_lnorm
#                                                            ),
#                                                            sigma = 0.7))) %>% 
#   # sample 20 quadrats from each community
#   mutate(sample = map(assemblage_map, ~sample_quadrats(comm = .x, n_quadrats = 20,
#                                                        method = 'random', avoid_overlap = T,
#                                                        plot = F))) %>% 
#   # calculate diversity
#   mutate(N_alpha = map(sample, ~rowSums(.x$spec_dat) %>% as.numeric),
#          S_alpha = map(sample, ~vegan::specnumber(.x$spec_dat)),
#          ENSPIE_alpha = map(sample, ~vegan::diversity(.x$spec_dat, index = 'invsimpson')),
#          N_gamma = map(assemblage_map, ~nrow(.x$census)),
#          S_gamma = map(assemblage_map, ~ div_rect(x0 = 0, y0 = 0, 
#                                                   xsize = 1, ysize = 1, 
#                                                   comm = .x)[['n_species']]),
#          ENSPIE_gamma = map(assemblage_map, ~div_rect(x0 = 0, y0 = 0, 
#                                                       xsize = 1, ysize = 1, 
#                                                       comm = .x)[['ens_simpson']]),
#          # rarefaction curves (gamma-scale)
#          ibr = map(assemblage_map, ~spec_sample_curve(.x, method = c("rare"))))

even_map <-
  even_comm[[1]] %>% 
  mutate(assemblage = 'More even assemblage') %>% 
  ggplot() +
  facet_wrap(~assemblage) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  # scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  coord_fixed(xlim = c(0,1),
              ylim = c(0,1)) +
  theme(legend.position = 'none',
        strip.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 2))

even_ibr <-
  bind_rows(reference1 %>%
                        unnest(ibr) %>% 
                        select(assemblage, n, spec_rarefied),
                      spec_sample_curve(even_comm, method = 'rarefaction') %>% 
                        select(n, spec_rarefied) %>% 
                        mutate(assemblage = 'More even assemblage')) %>% 
  ggplot() +
  geom_line(aes(x = n, y = spec_rarefied, 
                colour = assemblage, 
                group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More even assemblage')]),
                 shape = 'even', colour = 'More even assemblage'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
  scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                 'More even assemblage' = '#a0579f'),
                      name = '') +
  # scale_y_continuous(breaks = c(1,3,5,7,9)) +
  # scale_x_continuous(breaks = round(seq(1,30,length.out = 5))) +
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 8),
        plot.margin = margin(t = 2))

#-------- 4: increased N, decreased evenness, no change S--------
# increased dominance: add 10 individuals of the two most common species
dom_spp <- tibble(x = runif(20),
                  y = runif(20),
                  species = rep(c('1'),
                                each = 20),
                  assemblage = 'Increased abundance, decreased evenness, no change in richness')

dom_map =
  bind_rows(reference1$assemblage_map[[1]]$census %>% 
            mutate(assemblage = 'Increased abundance, decreased evenness, no change in richness'),
            dom_spp %>% 
              mutate(species = paste0('species', species))) %>% 
  ggplot() +
  facet_wrap(~assemblage) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  # scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  coord_fixed() +
  theme(legend.position = 'none',
        strip.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 2))
  
dominance_ass = community(x = as.numeric(c(unlist(reference1$assemblage_map[[1]]$census['x']), 
                                unlist(dom_spp[,'x']))),
                          y = as.numeric(c(unlist(reference1$assemblage_map[[1]]$census['y']), 
                                           unlist(dom_spp[,'y']))),
                          spec_id = as.character(c(unlist(reference1$assemblage_map[[1]]$census['species']), 
                                      unlist(dom_spp[,'species']))))

dominance_ibr = spec_sample_curve(dominance_ass, method = 'rarefaction')  

dom_ibr =
  bind_rows(reference1 %>%
              unnest(ibr) %>% 
              select(assemblage, n, spec_rarefied),
            dominance_ibr %>% 
              mutate(assemblage = 'More individuals, less even SAD, no change in richness')) %>% 
  ggplot() +
    geom_line(aes(x = n, y = spec_rarefied, 
                  colour = assemblage, 
                  group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More individuals, less even SAD, no change in richness')]), 
                 shape = 'dom',
                 colour = 'More individuals, less even SAD, no change in richness'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
    scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                   'More individuals, less even SAD, no change in richness' = '#e65586'),
                        name = '') +
    labs(x = 'Individuals',
         y = 'Species') +
  coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = '#000000'),
          axis.ticks = element_line(colour = '#000000'),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.position = 'none', #c(0,1),
          legend.justification = c(0,1),
          legend.text = element_text(size = 8),
          plot.margin = margin(t = 2))
  
#----- 5: increased N, decreased evenness, decreased S------
dom2_ass <- bind_cols(assemblage = 'Dominance2',
                   sd_lnorm = sd_lnorm1+0.75,
                   S =  S_initial/2,
                   N = N_initial +  1.25 * N_initial
  ) %>% 
    nest(data = c(sd_lnorm, S, N)) %>% 
    mutate(assemblage_map = map(data, ~ sim_thomas_community(.x$S, .x$N, sad_type = 'lnorm', 
                                                             sad_coef = list('meanlog'=.x$N/.x$S,
                                                                             'sdlog' = .x$sd_lnorm#,
                                                                             # 'cv_abund' = .x$sd_lnorm
                                                             ),
                                                             sigma = 0.7))) %>% 
    # sample 20 quadrats from each community
    mutate(sample = map(assemblage_map, ~sample_quadrats(comm = .x, n_quadrats = 20,
                                                         method = 'random', avoid_overlap = T,
                                                         plot = F))) %>% 
    # calculate diversity
    mutate(N_alpha = map(sample, ~rowSums(.x$spec_dat) %>% as.numeric),
           S_alpha = map(sample, ~vegan::specnumber(.x$spec_dat)),
           ENSPIE_alpha = map(sample, ~vegan::diversity(.x$spec_dat, index = 'invsimpson')),
           N_gamma = map(assemblage_map, ~nrow(.x$census)),
           S_gamma = map(assemblage_map, ~ div_rect(x0 = 0, y0 = 0, 
                                                    xsize = 1, ysize = 1, 
                                                    comm = .x)[['n_species']]),
           ENSPIE_gamma = map(assemblage_map, ~div_rect(x0 = 0, y0 = 0, 
                                                        xsize = 1, ysize = 1, 
                                                        comm = .x)[['ens_simpson']]),
           # rarefaction curves (gamma-scale)
           ibr = map(assemblage_map, ~spec_sample_curve(.x, method = c("rare"))))
  
dom2_map <-
    dom2_ass$assemblage_map[[1]]$census %>% 
    mutate(assemblage = 'More individuals, less even, fewer species') %>% 
    ggplot() +
    facet_wrap(~assemblage) + 
    geom_point(aes(x=x, y = y, colour = species),
               size = 2.5, alpha = 0.8) +
    scale_color_manual(values = spp_col) +
    # scale_color_viridis_d(option = "inferno") +
    theme_minimal() +
    coord_fixed(xlim = c(0,1),
                ylim = c(0,1)) +
    theme(legend.position = 'none',
          strip.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(colour = '#000000', fill = NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(t = 2))
  
dom2_ibr <-
    bind_rows(reference1 %>%
                unnest(ibr) %>% 
                select(assemblage, n, spec_rarefied),
              dom2_ass %>% 
                unnest(ibr) %>% 
                select(assemblage, n, spec_rarefied) %>% 
                mutate(assemblage = 'More individuals, less even, fewer species')) %>% 
    ggplot() +
    geom_line(aes(x = n, y = spec_rarefied, 
                  colour = assemblage, 
                  group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More individuals, less even, fewer species')]), shape = 'dom2',
                 colour = 'More individuals, less even, fewer species'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
    scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                   'More individuals, less even, fewer species' = '#ff7055'),
                        name = '') +
    # scale_y_continuous(breaks = c(1,3,5,7,9)) +
    # scale_x_continuous(breaks = round(seq(1,30,length.out = 5))) +
    labs(x = 'Individuals',
         y = 'Species') +
    theme_minimal() +
  coord_cartesian(clip = 'off') +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = '#000000'),
          axis.ticks = element_line(colour = '#000000'),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.position = 'none', #c(0,1),
          legend.justification = c(0,1),
          legend.text = element_text(size = 8),
          plot.margin = margin(t = 2))
  
  
# --------6: increased N, decreased evenness, increased S  -------------
dom3_ass <- bind_cols(assemblage = 'Dominance3',
                      sd_lnorm = sd_lnorm1*3,
                      S =   S_initial + 4,
                      N = N_initial +  1.5 * N_initial
) %>% 
  nest(data = c(sd_lnorm, S, N)) %>% 
  mutate(assemblage_map = map(data, ~ sim_thomas_community(.x$S, .x$N, sad_type = 'lnorm', fix_s_sim = T,
                                                           sad_coef = list('meanlog'=.x$N/.x$S,
                                                                           'sdlog' = .x$sd_lnorm#,
                                                                           # 'cv_abund' = .x$sd_lnorm
                                                           ),
                                                           sigma = 0.7))) %>% 
  # sample 20 quadrats from each community
  mutate(sample = map(assemblage_map, ~sample_quadrats(comm = .x, n_quadrats = 20,
                                                       method = 'random', avoid_overlap = T,
                                                       plot = F))) %>% 
  # calculate diversity
  mutate(N_alpha = map(sample, ~rowSums(.x$spec_dat) %>% as.numeric),
         S_alpha = map(sample, ~vegan::specnumber(.x$spec_dat)),
         ENSPIE_alpha = map(sample, ~vegan::diversity(.x$spec_dat, index = 'invsimpson')),
         N_gamma = map(assemblage_map, ~nrow(.x$census)),
         S_gamma = map(assemblage_map, ~ div_rect(x0 = 0, y0 = 0, 
                                                  xsize = 1, ysize = 1, 
                                                  comm = .x)[['n_species']]),
         ENSPIE_gamma = map(assemblage_map, ~div_rect(x0 = 0, y0 = 0, 
                                                      xsize = 1, ysize = 1, 
                                                      comm = .x)[['ens_simpson']]),
         # rarefaction curves (gamma-scale)
         ibr = map(assemblage_map, ~spec_sample_curve(.x, method = c("rare"))))

dom3_map <-
  dom3_ass$assemblage_map[[1]]$census %>% 
  mutate(assemblage = 'More individuals, less even SAD, more species') %>% 
  ggplot() +
  facet_wrap(~assemblage, labeller = label_wrap_gen()) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  # scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  coord_fixed(xlim = c(0,1),
              ylim = c(0,1)) +
  theme(legend.position = 'none',
        strip.text = element_blank(),#element_text(face = 'bold',hjust = 0, size = 8, 
                                  # margin = margin(t = 0, r = 20, b = 1, l = 0, unit = 'mm')),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        plot.margin = margin(t = 2),
        axis.text = element_blank(),
        axis.title = element_blank())

dom3_ibr <-
  bind_rows(reference1 %>%
              unnest(ibr) %>% 
              select(assemblage, n, spec_rarefied),
            dom3_ass %>% 
              unnest(ibr) %>% 
              select(assemblage, n, spec_rarefied) %>% 
              mutate(assemblage = 'More individuals, less even SAD, more species')) %>% 
  ggplot() +
  geom_line(aes(x = n, y = spec_rarefied, 
                colour = assemblage, 
                group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More individuals, less even SAD, more species')]), 
                 shape = 'dom3',
                 colour = 'More individuals, less even SAD, more species'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
  scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                 'More individuals, less even SAD, more species' = '#ffa600'),
                      name = '') +
  labs(x = 'Individuals',
       y = 'Species') +
  theme_minimal() +
  coord_cartesian(clip = 'off') +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title = element_text(size = 8),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 8),
        plot.margin = margin(t = 2))
  

#-----combine ------
ref_label <- ggdraw() + 
  cowplot::draw_label('i. Reference', fontface = 'plain', size = 10, hjust = 0, x = 0.1)
mih_label <- ggdraw() + 
  cowplot::draw_label('ii. More individuals', fontface = 'plain', size = 10, hjust = 0, x = 0)
rare_label <- ggdraw() + 
  cowplot::draw_label('iii. More rare species', fontface = 'plain', size = 10, hjust = 0, x = 0)
even_label <- ggdraw() + 
  cowplot::draw_label('iv. More even SAD', fontface = 'plain', size = 10, hjust = 0, x = 0)
d1_label <- ggdraw() + 
  cowplot::draw_label('v. More individuals, less even SAD, same richness', fontface = 'plain', size = 10, hjust = 0, x = 0)
d2_label <- ggdraw() + 
  cowplot::draw_label('vi. More individuals, less even SAD, fewer species', fontface = 'plain', size = 10, hjust = 0, x = 0)
d3_label <- ggdraw() + 
  cowplot::draw_label('vii. More individuals, less even SAD, more species', fontface = 'plain', size = 10, hjust = 0, x = 0)

ref_panel = plot_grid(ref_label,
                      ref_map, rel_heights = c(0.1,1), ncol = 1, align = 'v', axis = 'l')
r1 = plot_grid(mih_label,
          plot_grid(MIH_map, mih_ibr, ncol = 2, rel_widths = c(1,1)),
          rel_heights = c(0.1,1), ncol = 1, align = 'v', axis = 'l')
r2 = plot_grid(rare_label,
               plot_grid(rare_map, rare_ibr, ncol = 2, rel_widths = c(1,1)), 
               rel_heights = c(0.1,1), ncol = 1, align = 'v', axis = 'l')
r3 = plot_grid(even_label,
               plot_grid(even_map, even_ibr, ncol = 2, rel_widths = c(1,1)), 
               rel_heights = c(0.1,1), ncol = 1, align = 'v', axis = 'l')
r4 = plot_grid(d1_label,
               plot_grid(dom_map, dom_ibr, ncol = 2, rel_widths = c(1,1)), 
               rel_heights = c(0.1,1), ncol = 1, align = 'v', axis = 'l')
r5 = plot_grid(d2_label,
               plot_grid(dom2_map, dom2_ibr, ncol = 2, rel_widths = c(1,1)), 
               rel_heights = c(0.1,1), ncol = 1, align = 'v', axis = 'l')
r6 = plot_grid(d3_label,
               plot_grid(dom3_map, dom3_ibr, ncol = 2, rel_widths = c(1,1)), 
               rel_heights = c(0.1,1), ncol = 1, align = 'v', axis = 'l')


middle1 = cowplot::plot_grid(NULL, r1, NULL, r2, NULL, r3, NULL, 
                             nrow = 1,
                             rel_widths = c(0.05,1,0.05,1,0.05,1,0.05))
                             
middle2 = cowplot::plot_grid(NULL, r4, NULL, r5, NULL, r6, NULL, 
                             nrow = 1,
                             rel_widths = c(0.05,1,0.05,1,0.05,1,0.05))


top = cowplot::plot_grid(NULL, ref_panel, NULL,
                          nrow = 1)

# cowplot::plot_grid(left, right_combo,
#                    ncol = 2, 
#                    rel_widths = c(0.6,1), 
#                    align = 'hv')
# 
# ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/conceptual2.png',
#        width = 150, height = 200, units = 'mm')


# what about effect sizes plot too?
metrics <- tibble(
  treatment = c('reference', 
                'individuals',
                'rare',
                'even',
                'dom',
                'dom2',
                'dom3'),
  N = c(as.numeric(reference1$N_gamma),
        as.numeric(MIH$N_gamma),
        N_initial,
        nrow(even_comm$census),
        nrow(dominance_ass$census),
        as.numeric(dom2_ass$N_gamma),
        as.numeric(dom3_ass$N_gamma)),
  S = c(as.numeric(reference1$S_gamma),
        as.numeric(MIH$S_gamma),
        mih_ibr$data$spec_rarefied[N_initial*2],
        n_distinct(even_comm$census$species),
        n_distinct(dominance_ass$census$species),
        as.numeric(dom2_ass$S_gamma),
        as.numeric(dom3_ass$S_gamma)),
  ENSPIE = c(as.numeric(reference1$ENSPIE_gamma),
             as.numeric(MIH$ENSPIE_gamma),
             as.numeric(MIH$ENSPIE_gamma),
             vegan::diversity(as.numeric(community_to_sad(even_comm)), index = 'invsimpson'),
             vegan::diversity(as.numeric(community_to_sad(dominance_ass)), index = 'invsimpson'),
             as.numeric(dom2_ass$ENSPIE_gamma),
             as.numeric(dom3_ass$ENSPIE_gamma)),
  S_ref_n = c(as.numeric(reference1$S_gamma),
              MIH$ibr[[1]][N_initial,'spec_rarefied'],
              MIH$ibr[[1]][N_initial,'spec_rarefied'],
              as.numeric(vegan::rarefy(as.numeric(community_to_sad(even_comm)), sample = N_initial)),
              as.numeric(vegan::rarefy(as.numeric(community_to_sad(dominance_ass)), sample = N_initial)),
              dom2_ass$ibr[[1]][N_initial, 'spec_rarefied'],
              dom3_ass$ibr[[1]][N_initial, 'spec_rarefied'])
  )

ES <- metrics %>% 
  mutate(N_ES = log(N / N[1]),
         S_ES = log(S / S[1]),
         ENSPIE_ES = log(ENSPIE / ENSPIE[1]),
         Sn_ES = log(S_ref_n / S_ref_n[1]))

trt_col = c(
  'individuals' = '#004b6e',
  'rare' = '#4f5794',
  'even' = '#a0579f',
  'dom' = '#e65586',
  'dom2' = '#ff7055',
  'dom3' = '#ffa600')

S_N_ES_plot <-
  ggplot() +
  geom_point(data = ES %>% 
               filter(treatment!='reference'), 
             aes(x = N_ES, y = S_ES, colour = treatment, shape = treatment),
             size = 3, alpha = 1, stroke = 1.5,
             position = position_jitter(width = 0.0025, height = 0.0025)) +
    geom_point(data = ES %>% 
                 filter(treatment!='reference'), 
               aes(x = -1 * N_ES, y = -1 * S_ES, colour = treatment, shape = treatment),
               size = 3, alpha = 1, stroke = 1.5,
               position = position_jitter(width = 0.0025, height = 0.0025)) +
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
    scale_color_manual(guide = FALSE,
                       values = trt_col) +
    scale_shape_manual(guide = F, values = delta_shape) +
    geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
    geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
    geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
    scale_x_continuous(name = '', breaks = c(0)) +
    scale_y_continuous(name = '', breaks = c(0)) +
    # labs(x = 'Change in total abundance',
    #      y = 'Change in species richness') +
  coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(axis.title = element_text(size = 8),
          plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))
  
  S_Sn_ES_plot <-
  ggplot() +
    geom_point(data = ES %>% 
                 filter(treatment!='reference'), 
               aes(x = Sn_ES, y = S_ES, colour = treatment, shape = treatment),
               size = 3, alpha = 1, stroke = 1.5,
               position = position_jitter(width = 0.0025, height = 0.0025)) +
    geom_point(data = ES %>% 
                 filter(treatment!='reference'), 
               aes(x = -1 * Sn_ES, y = -1 * S_ES, colour = treatment, shape = treatment),
               size = 3, alpha = 1, stroke = 1.5,
               position = position_jitter(width = 0.0025, height = 0.0025)) +
    annotate(geom = 'text', 
             label = expression(paste(Delta, italic(S))),
             x = 0, y = Inf, parse = T,
             hjust = 1.25, vjust = 1) +
    annotate(geom = 'text', 
             label = expression(paste(Delta, italic(S)[n])),
             x = Inf, y = 0, parse = T,
             hjust = 1,
             vjust = 1.25
    ) +
    scale_color_manual(guide = FALSE,
                       values = trt_col) +
    scale_shape_manual(guide = F, values = delta_shape) +
    geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
    geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
    geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
    scale_x_continuous(name = '', breaks = c(0)) +
    scale_y_continuous(name = '', breaks = c(0)) +
    # labs(x = 'Change in rarefied richness',
    #      y = 'Change in species richness') +
    coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(axis.title = element_text(size = 8),
          plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))
  
S_S_PIE_ES_plot <-
  ggplot() +
    geom_point(data = ES %>% 
                 filter(treatment!='reference'), 
               aes(x = ENSPIE_ES, y = S_ES, colour = treatment, shape = treatment),
               size = 3, alpha = 1, stroke = 1.5,
               position = position_jitter(width = 0.0025, height = 0.0025)) +
    geom_point(data = ES %>% 
                 filter(treatment!='reference'), 
               aes(x = -1 * ENSPIE_ES, y = -1 * S_ES, colour = treatment, shape = treatment),
               size = 3, alpha = 1, stroke = 1.5,
               position = position_jitter(width = 0.0025, height = 0.0025)) +
    scale_shape_manual(values = delta_shape, guide = F) +
    scale_color_manual(guide = FALSE,
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
    geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
    scale_x_continuous(name = '', breaks = c(0)) +
    scale_y_continuous(name = '', breaks = c(0)) +
    # labs(x = 'Change in evenness',
         # y = 'Change in species richness') +
  coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(axis.title = element_text(size = 8),
          plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm')) 


label1 = substitute(paste('viii. ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'individuals'))
label2 = substitute(paste('ix. ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'rarefied richness'))
label3 = substitute(paste('x. ', Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'evenness'))

bottom <- cowplot::plot_grid(NULL, S_N_ES_plot +
                                  draw_label(label = label1, fontface = 'bold', 
                                             x = -Inf, y = Inf,
                                             hjust = 0.25, vjust = -0.5, lineheight = 1, size = 10),
                                  S_Sn_ES_plot +
                                  draw_label(label = label2, fontface = 'bold', 
                                             x = -Inf, y = Inf,
                                             hjust = 0.250, vjust = -0.5, lineheight = 1, size = 10),
                                  S_S_PIE_ES_plot +
                                    draw_label(label = label3, 
                                               x = -Inf, y = Inf,
                                               hjust = 0.3, vjust = -0.5, lineheight = 1, size = 10,
                                               fontface = 'bold' ), NULL,
                                  nrow = 1, rel_widths = c(0.3,1,1,1,0.3))

cowplot::plot_grid(top, NULL, middle1, NULL, middle2, NULL, bottom,
                     nrow = 7, 
                     rel_heights = c(0.67,0.1, 0.6,0.1, 0.6, 0.1, 1.2),
                     align = 'hv')

ggsave(paste0('~/Dropbox/1current/multidimensionalChangeMS/Figs/conceptual_seed', s, '_h.png'),
         width = 270, height = 200, units = 'mm')
  