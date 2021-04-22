# conceptual illustration of diversity change
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')
# here are some simulations that illustrate the changes I want to show (rerunning the simulations can result in slight alterations to the figure)
load("/Users/sb25gaqy/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/conceptual_figure_dat.Rdata")

# s = 1793
# set.seed(s)

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


# -------parameters for the reference assemblage-------------
sd_lnorm1 <- 1
S_initial = 6
N_initial = 16

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
        strip.text = element_text(hjust = 0, size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 2))

# modify the MIH map to get the reference needed
mod_MIH <- MIH$assemblage_map[[1]]$census %>%
  # remove 1 species
  filter(species!='species6') %>%
  # subsample to have fewer individuals of other species
  slice_sample(n = 11)

ref_MIH_map <-
  mod_MIH %>%
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
        strip.text = element_text(hjust = 0, size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        # plot.margin = unit(c(0, 0, 0, 2.5), units = 'cm'), # manuscript central adds line numbers to figures?
        axis.text = element_blank(),
        axis.title = element_blank())


mih_ibr <-
bind_rows(MIH %>% 
            unnest(ibr) %>% 
            select(assemblage, n, spec_rarefied) %>% 
            mutate(assemblage = 'More individuals'),
          MIH %>% 
            unnest(ibr) %>% 
            select(assemblage, n, spec_rarefied) %>% 
            slice(1:11) %>% 
            mutate(assemblage = 'Reference')) %>% 
  ggplot() +
  geom_line(aes(x = n, y = spec_rarefied, 
                colour = assemblage, 
                group = assemblage, alpha = assemblage), size = 1.5) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More individuals')]), shape = 'individuals', colour = 'More individuals'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
  scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                 'More individuals' = '#004b6e'),
                      name = '') +
  scale_alpha_manual(values = c('Reference' = 0.9,
                                'More individuals' = 0.66),
                     guide = F) +
  # scale_y_continuous(breaks = c(1,3,5,7,9)) +
  # scale_x_continuous(breaks = round(seq(1,30,length.out = 5))) +
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1), 
        # panel.background = element_blank(),
        plot.margin = margin(t = 2, r = 2),
        legend.text = element_text(size = 10))

# reference assemblage for other scenarios
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
        strip.text = element_text(hjust = 0, size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank())


#----- 2: more rare species (change a couple of species labels in the reference community)--------
rare_ass <- reference1
rare_ass$assemblage_map[[1]]$census$species <- as.character(rare_ass$assemblage_map[[1]]$census$species)
n_spp = length(unique(rare_ass$assemblage_map[[1]]$census$species))
extra1 = paste0('species', n_spp+1)
extra2= paste0('species', n_spp+2)
rare_ass$assemblage_map[[1]]$census[N_initial,]$species = extra1
rare_ass$assemblage_map[[1]]$census[N_initial-1,]$species = extra2

# need to update metrics and the metrics
rare_ass <- rare_ass %>% 
  # calculate diversity
  mutate(N_gamma = map(assemblage_map, ~nrow(.x$census)),
         S_gamma = map(assemblage_map, ~ div_rect(x0 = 0, y0 = 0, 
                                                  xsize = 1, ysize = 1, 
                                                  comm = .x)[['n_species']]),
         ENSPIE_gamma = map(assemblage_map, ~div_rect(x0 = 0, y0 = 0, 
                                                      xsize = 1, ysize = 1, 
                                                      comm = .x)[['ens_simpson']]),
         # rarefaction curves (gamma-scale)
         ibr = map(assemblage_map, ~spec_sample_curve(.x, method = c("rare"))))

rare_map <-
rare_ass$assemblage_map[[1]]$census %>% 
  mutate(assemblage = 'More rare species') %>% 
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
        strip.text = element_text(hjust=0, size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(t = 2))

rare_ibr <-
bind_rows(reference1 %>%
            unnest(ibr) %>% 
            select(assemblage, n, spec_rarefied),
          rare_ass %>% 
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
  labs(x = 'Abundance [individuals]',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2))

#------3: more even community----------
# make perferctly even assemblage by hand
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
even_map <-
  even_comm[[1]] %>% 
  mutate(assemblage = 'More even') %>% 
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
        strip.text = element_text(hjust = 0, size = 10),
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
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2, r = 2))

#-------- 4: increased N, decreased evenness, no change S--------
# increased dominance: add 10 individuals of the two most common species
dom_spp <- tibble(x = runif(20),
                  y = runif(20),
                  species = rep(c('1'),
                                each = 20),
                  assemblage = 'More individuals, less even, no change in richness')

dom_map =
  bind_rows(reference1$assemblage_map[[1]]$census %>% 
            mutate(assemblage = 'More individuals, less even, no change in richness'),
            dom_spp %>% 
              mutate(species = paste0('species', species))) %>% 
  ggplot() +
  facet_wrap(~assemblage, labeller = label_wrap_gen()) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  # scale_color_viridis_d(option = "inferno") +
  theme_minimal() +
  coord_fixed() +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0, size = 8),
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
              mutate(assemblage = 'More individuals, less even, no change in richness')) %>% 
  ggplot() +
    geom_line(aes(x = n, y = spec_rarefied, 
                  colour = assemblage, 
                  group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More individuals, less even, no change in richness')]), 
                 shape = 'dom',
                 colour = 'More individuals, less even, no change in richness'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
    scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                   'More individuals, less even, no change in richness' = '#e65586'),
                        name = '') +
    labs(x = 'Individuals',
         y = 'Species') +
  coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = '#000000'),
          axis.ticks = element_line(colour = '#000000'),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10),
          legend.position = 'none', #c(0,1),
          legend.justification = c(0,1),
          legend.text = element_text(size = 10),
          plot.margin = margin(t = 2, r = 2))
  
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
    facet_wrap(~assemblage, labeller = label_wrap_gen()) + 
    geom_point(aes(x=x, y = y, colour = species),
               size = 2.5, alpha = 0.8) +
    scale_color_manual(values = spp_col) +
    theme_minimal() +
    coord_fixed(xlim = c(0,1),
                ylim = c(0,1)) +
    theme(legend.position = 'none',
          strip.text = element_text(hjust = 0, size = 10),
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
    labs(x = 'Abundance [individuals]',
         y = 'Species') +
    theme_minimal() +
  coord_cartesian(clip = 'off') +
    theme(panel.grid = element_blank(),
          axis.line = element_line(colour = '#000000'),
          axis.ticks = element_line(colour = '#000000'),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          legend.position = 'none', #c(0,1),
          legend.justification = c(0,1),
          legend.text = element_text(size = 10),
          plot.margin = margin(t = 2, r = 2))
  
  
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
  theme_minimal() +
  coord_fixed(xlim = c(0,1),
              ylim = c(0,1)) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0, size = 8),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        plot.margin = margin(t = 2),
        axis.text = element_blank(),
        axis.title = element_blank())

# modify reference map to show 3 species and 10 individuals
ref_dom3_map <-
  reference1$assemblage_map[[1]]$census %>%
  slice(7:16) %>% 
  mutate(species = as.character(species),
         species = ifelse(species == 'species4', 'species1', species)) %>% 
mutate(assemblage = 'Reference') %>% 
  ggplot() +
  facet_wrap(~assemblage, labeller = label_wrap_gen()) + 
  geom_point(aes(x=x, y = y, colour = species),
             size = 2.5, alpha = 0.8) +
  scale_color_manual(values = spp_col) +
  theme_minimal() +
  coord_fixed(xlim = c(0,1),
              ylim = c(0,1)) +
  theme(legend.position = 'none',
        strip.text = element_text(hjust = 0, size = 10),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = '#000000', fill = NA),
        plot.margin = margin(t = 2),
        axis.text = element_blank(),
        axis.title = element_blank())

dom3_ibr <-
  bind_rows(reference1 %>%
              unnest(ibr) %>% 
              slice(1:10) %>% 
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
  scale_y_continuous(breaks = c(2,4,6,8,10)) +
  labs(x = 'Abundance [individuals]',
       y = 'Species') +
  theme_minimal() +
  coord_cartesian(clip = 'off') +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title = element_text(size = 10),
        legend.position = 'none', 
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2, r = 2))
  

#-----combine ------

r1 <- plot_grid(ref_MIH_map, MIH_map, 
          mih_ibr, 
          ncol = 3, rel_widths = c(1,1,1.2),
          align = 'v', axis = 't')

r2 <- plot_grid(ref_map, rare_map, rare_ibr,
                ncol = 3, rel_widths = c(1,1,1.2),
                align = 'v', axis = 't')

r3 <- plot_grid(ref_map, even_map, even_ibr,
                ncol = 3, rel_widths = c(1,1,1.2),
                align = 'v', axis = 't')

r4 <- plot_grid(ref_map, dom_map, dom_ibr,
                ncol = 3, rel_widths = c(1,1,1.2),
                align = 'v', axis = 't')

r5 <- plot_grid(ref_map, dom2_map, dom2_ibr,
                ncol = 3, rel_widths = c(1,1,1.2),
                align = 'v', axis = 't')

r6 <- plot_grid(ref_dom3_map, dom3_map, dom3_ibr,
                ncol = 3, rel_widths = c(1,1,1.2),
                align = 'v', axis = 't')


# calculate effect sizes to plot 
metrics <- tibble(
  # treatment metrics
  treatment = c('individuals',
                'rare',
                'even',
                'dom',
                'dom2',
                'dom3'),
  N = c(as.numeric(MIH$N_gamma),
        N_initial,
        nrow(even_comm$census),
        nrow(dominance_ass$census),
        as.numeric(dom2_ass$N_gamma),
        as.numeric(dom3_ass$N_gamma)),
  S = c(as.numeric(MIH$S_gamma),
        as.numeric(rare_ass$S_gamma),
        n_distinct(even_comm$census$species),
        n_distinct(dominance_ass$census$species),
        as.numeric(dom2_ass$S_gamma),
        as.numeric(dom3_ass$S_gamma)),
  ENSPIE = c(as.numeric(MIH$ENSPIE_gamma),
             as.numeric(rare_ass$ENSPIE_gamma),
             vegan::diversity(as.numeric(community_to_sad(even_comm)), index = 'invsimpson'),
             vegan::diversity(as.numeric(community_to_sad(dominance_ass)), index = 'invsimpson'),
             as.numeric(dom2_ass$ENSPIE_gamma),
             as.numeric(dom3_ass$ENSPIE_gamma)),
  S_ref_n = c(MIH$ibr[[1]][11,'spec_rarefied'],
              rare_ass$ibr[[1]][N_initial,'spec_rarefied'],
              as.numeric(vegan::rarefy(as.numeric(community_to_sad(even_comm)), sample = N_initial)),
              as.numeric(vegan::rarefy(as.numeric(community_to_sad(dominance_ass)), sample = N_initial)),
              dom2_ass$ibr[[1]][N_initial, 'spec_rarefied'],
              dom3_ass$ibr[[1]][10, 'spec_rarefied']),
  # column for each of the metrics in the reference
  N_ref = c(11, as.numeric(reference1$N_gamma), as.numeric(reference1$N_gamma), as.numeric(reference1$N_gamma),
            as.numeric(reference1$N_gamma), 10),
  S_ref = c(5, as.numeric(reference1$S_gamma), as.numeric(reference1$S_gamma), as.numeric(reference1$S_gamma),
            as.numeric(reference1$S_gamma), 3),
  ENSPIE_ref = c(as.numeric(MIH$ENSPIE_gamma), as.numeric(reference1$ENSPIE_gamma), as.numeric(reference1$ENSPIE_gamma),
                 as.numeric(reference1$ENSPIE_gamma), as.numeric(reference1$ENSPIE_gamma),as.numeric(reference1$ENSPIE_gamma)),
  Sn_ref = c(MIH$ibr[[1]][11,'spec_rarefied'], reference1$ibr[[1]][N_initial,'spec_rarefied'], reference1$ibr[[1]][N_initial,'spec_rarefied'],
             reference1$ibr[[1]][N_initial,'spec_rarefied'], reference1$ibr[[1]][N_initial,'spec_rarefied'], reference1$ibr[[1]][10,'spec_rarefied'])
  )

ES <- metrics %>% 
  mutate(N_ES = log(N / N_ref),
         S_ES = log(S / S_ref),
         ENSPIE_ES = log(ENSPIE / ENSPIE_ref),
         Sn_ES = log(S_ref_n / Sn_ref))

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
    scale_color_manual(guide = FALSE,
                       values = trt_col) +
    scale_shape_manual(guide = F, values = delta_shape) +
    geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
    geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
    geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
    scale_x_continuous(name = '', breaks = c(0)) +
    scale_y_continuous(name = '', breaks = c(0)) +
  coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(axis.title = element_text(size = 10),
          plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))
  
  S_Sn_ES_plot <-
  ggplot() +
    geom_point(data = ES %>% 
                 filter(treatment!='reference'), 
               aes(x = Sn_ES, y = S_ES, colour = treatment, shape = treatment),
               size = 3, alpha = 1, stroke = 1.5) +
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
    coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(axis.title = element_text(size = 10),
          plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm'))
  
S_S_PIE_ES_plot <-
  ggplot() +
    geom_point(data = ES %>% 
                 filter(treatment!='reference'), 
               aes(x = ENSPIE_ES, y = S_ES, colour = treatment, shape = treatment),
               size = 3, alpha = 1, stroke = 1.5) +
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
  coord_cartesian(clip = 'off') +
    theme_minimal() +
    theme(axis.title = element_text(size = 10),
          plot.margin = margin(t = 8, r = 3, b = 3, l = 8, unit = 'mm')) 


label1 = substitute(paste(Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'individuals'))
label2 = substitute(paste(Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'rarefied richness'))
label3 = substitute(paste(Delta, 'richness', ' ~ ', #italic(f), 
                          Delta, 'evenness'))

bottom <- cowplot::plot_grid(NULL, S_N_ES_plot +
                                  draw_label(label = label1, fontface = 'bold', 
                                             x = -Inf, y = Inf,
                                             hjust = 0.2, vjust = -0.5, lineheight = 1, size = 10),
                                  S_Sn_ES_plot +
                                  draw_label(label = label2, fontface = 'bold', 
                                             x = -Inf, y = Inf,
                                             hjust = 0.1, vjust = -0.5, lineheight = 1, size = 10),
                                  S_S_PIE_ES_plot +
                                    draw_label(label = label3, 
                                               x = -Inf, y = Inf,
                                               hjust = 0.3, vjust = -0.5, lineheight = 1, size = 10,
                                               fontface = 'bold' ), NULL,
                                  nrow = 1, rel_widths = c(0.3,1,1,1,0.3),
                             labels = c('', 'a', 'b', 'c', ''))
top = plot_grid(r1, r2, r3, 
                r4, r5, r6,
                nrow = 3, ncol = 2,
                labels = c('d', 'e', 'f', 'g',
                           'h', 'i'),
                align = 'hv')
plot_grid(NULL, # NULL panels to add white space for manuscript central snafu
          plot_grid(NULL, bottom,
          top, NULL, nrow = 4, rel_heights = c(0.05, 0.55, 1, 0.05)),
          NULL, nrow = 1, rel_widths = c(0.05, 1, 0.02))

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/submission/Fig2.pdf',
         width = 350, height = 230, units = 'mm')

  