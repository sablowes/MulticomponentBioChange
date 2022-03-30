# conceptual illustration of diversity change
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')
# here are some simulations that illustrate the changes I want to show (rerunning the simulations can result in slight alterations to the figure)
load("/Users/sb25gaqy/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/conceptual_figure_dat.Rdata")
# load("/Users/jc155893/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/conceptual_figure_dat.Rdata")

# change shapes
delta_shape = c('individuals' = 2, 'rare' = 3, 'even' = 4, 'even2' = 6,
                'dom' = 5, 'dom2' = 8, 'dom3' = 17)


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
                                 'More individuals' = '#91bfdb'),
                      name = '') +
  scale_alpha_manual(values = c('Reference' = 0.95,
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
        axis.title.y = element_text(size = 12),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1), 
        # panel.background = element_blank(),
        plot.margin = margin(t = 2, r = 2, b = 6, l = 6),
        legend.text = element_text(size = 10))

#----- 2: more rare species (change a couple of species labels in the reference community)--------
rare_ass <- reference1
rare_ass$assemblage_map[[1]]$census$species <- as.character(rare_ass$assemblage_map[[1]]$census$species)
n_spp = length(unique(rare_ass$assemblage_map[[1]]$census$species))
extra1 = paste0('species', n_spp+1)
extra2= paste0('species', n_spp+2)
rare_ass$assemblage_map[[1]]$census[N_initial,]$species = extra1
rare_ass$assemblage_map[[1]]$census[N_initial-1,]$species = extra2

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
                                 'More even assemblage' = '#91bfdb'),
                      name = '') +
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2, r = 2, b = 6))

# one more rarefaction with positive change in N, evenness and S
even_comm2 <- community(x = runif(27),
                        y = runif(27),
                        spec_id = c(rep('species1', times = 3),
                                    rep('species2', times = 3),
                                    rep('species3', times = 3),
                                    rep('species4', times = 3),
                                    rep('species5', times = 3),
                                    rep('species6', times = 3),
                                    rep('species7', times = 3),
                                    rep('species8', times = 3),
                                    rep('species9', times = 3)))

even2_ibr <-
  bind_rows(reference1 %>%
              unnest(ibr) %>% 
              select(assemblage, n, spec_rarefied),
            spec_sample_curve(even_comm2, method = 'rarefaction') %>% 
              select(n, spec_rarefied) %>% 
              mutate(assemblage = 'More indiviudals, more even,  more species')) %>% 
  ggplot() +
  geom_line(aes(x = n, y = spec_rarefied, 
                colour = assemblage, 
                group = assemblage), size = 1.5, alpha = 0.66) +
  geom_point(aes(x = max(n), y = max(spec_rarefied[which(assemblage=='More indiviudals, more even,  more species')]),
                 shape = 'even2', colour = 'More indiviudals, more even,  more species'),
             size = 3, stroke = 1.5) +
  scale_shape_manual(guide = F,
                     values = delta_shape) +
  scale_colour_manual(values = c('Reference' = '#bdbdbd',
                                 'More indiviudals, more even,  more species' = '#91bfdb'),
                      name = '') +
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2, r = 2, b = 6))


#-------- 4: increased N, decreased evenness, no change S--------
# increased dominance: add 10 individuals of the two most common species
dom_spp <- tibble(x = runif(20),
                  y = runif(20),
                  species = rep(c('1'),
                                each = 20),
                  assemblage = 'More individuals, less even, no change in richness')


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
                                 'More individuals, less even, no change in richness' = '#fc8d59'),
                      name = '') +
  labs(x = 'Individuals',
       y = 'Species') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2, r = 2, b = 6, l = 6))

#----- 5: increased N, decreased evenness, decreased S------
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
                                 'More individuals, less even, fewer species' = '#fc8d59'),
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
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'none', #c(0,1),
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2, r = 2, b = 6))


# --------6: increased N, decreased evenness, increased S  -------------

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
                                 'More individuals, less even SAD, more species' = '#fc8d59'),
                      name = '') +
  scale_y_continuous(breaks = c(2,4,6,8,10)) +
  labs(x = 'Individuals',
       y = 'Species') +
  theme_minimal() +
  coord_cartesian(clip = 'off') +
  theme(panel.grid = element_blank(),
        axis.line = element_line(colour = '#000000'),
        axis.ticks = element_line(colour = '#000000'),
        axis.title.y = element_blank(),
        legend.position = 'none', 
        legend.justification = c(0,1),
        legend.text = element_text(size = 10),
        plot.margin = margin(t = 2, r = 2, b = 6))


# calculate effect sizes to plot 
metrics <- tibble(
  # treatment metrics
  treatment = c('individuals',
                'rare',
                'even',
                'even2',
                'dom',
                'dom2',
                'dom3'),
  N = c(as.numeric(MIH$N_gamma),
        N_initial,
        nrow(even_comm$census),
        nrow(even_comm2$census),
        nrow(dominance_ass$census),
        as.numeric(dom2_ass$N_gamma),
        as.numeric(dom3_ass$N_gamma)),
  S = c(as.numeric(MIH$S_gamma),
        as.numeric(rare_ass$S_gamma),
        n_distinct(even_comm$census$species),
        n_distinct(even_comm2$census$species),
        n_distinct(dominance_ass$census$species),
        as.numeric(dom2_ass$S_gamma),
        as.numeric(dom3_ass$S_gamma)),
  ENSPIE = c(as.numeric(MIH$ENSPIE_gamma),
             as.numeric(rare_ass$ENSPIE_gamma),
             vegan::diversity(as.numeric(community_to_sad(even_comm)), index = 'invsimpson'),
             vegan::diversity(as.numeric(community_to_sad(even_comm2)), index = 'invsimpson'),
             vegan::diversity(as.numeric(community_to_sad(dominance_ass)), index = 'invsimpson'),
             as.numeric(dom2_ass$ENSPIE_gamma),
             as.numeric(dom3_ass$ENSPIE_gamma)),
  S_ref_n = c(MIH$ibr[[1]][11,'spec_rarefied'],
              rare_ass$ibr[[1]][N_initial,'spec_rarefied'],
              as.numeric(vegan::rarefy(as.numeric(community_to_sad(even_comm)), sample = N_initial)),
              as.numeric(vegan::rarefy(as.numeric(community_to_sad(even_comm2)), sample = N_initial)),
              as.numeric(vegan::rarefy(as.numeric(community_to_sad(dominance_ass)), sample = N_initial)),
              dom2_ass$ibr[[1]][N_initial, 'spec_rarefied'],
              dom3_ass$ibr[[1]][10, 'spec_rarefied']),
  # column for each of the metrics in the reference
  N_ref = c(11, as.numeric(reference1$N_gamma), as.numeric(reference1$N_gamma), as.numeric(reference1$N_gamma), as.numeric(reference1$N_gamma),
            as.numeric(reference1$N_gamma), 10),
  S_ref = c(5, as.numeric(reference1$S_gamma), as.numeric(reference1$S_gamma), as.numeric(reference1$S_gamma), as.numeric(reference1$S_gamma),
            as.numeric(reference1$S_gamma), 3),
  ENSPIE_ref = c(as.numeric(MIH$ENSPIE_gamma), as.numeric(reference1$ENSPIE_gamma), as.numeric(reference1$ENSPIE_gamma), as.numeric(reference1$ENSPIE_gamma),
                 as.numeric(reference1$ENSPIE_gamma), as.numeric(reference1$ENSPIE_gamma),as.numeric(reference1$ENSPIE_gamma)),
  Sn_ref = c(MIH$ibr[[1]][11,'spec_rarefied'], reference1$ibr[[1]][N_initial,'spec_rarefied'], reference1$ibr[[1]][N_initial,'spec_rarefied'],
             reference1$ibr[[1]][N_initial,'spec_rarefied'], reference1$ibr[[1]][N_initial,'spec_rarefied'], reference1$ibr[[1]][N_initial,'spec_rarefied'], 
             reference1$ibr[[1]][10,'spec_rarefied'])
)

ES <- metrics %>% 
  mutate(N_ES = log(N / N_ref),
         S_ES = log(S / S_ref),
         ENSPIE_ES = log(ENSPIE / ENSPIE_ref),
         Sn_ES = log(S_ref_n / Sn_ref))

trt_col = c(
  'individuals' = '#91bfdb',
  'rare' = '#91bfdb',
  'even' = '#91bfdb',
  'even2' = '#91bfdb',
  'dom' = '#fc8d59',
  'dom2' = '#fc8d59',
  'dom3' = '#fc8d59')


shape_df <- tibble(x = 1:(length(delta_shape)-1), y = 1:(length(delta_shape)-1),
                   trt = c('individuals', 'even', 'even2', 'dom', 'dom2', 'dom3'))

shape_legend <- ggplot() +
  geom_point(data = shape_df, 
             aes(x = x, y = y, shape = trt, colour = trt)) +
  scale_shape_manual(name = '',
                     values = c('individuals' = 2, 'even' = 4, 'even2' = 6,
                                              'dom' = 5, 'dom2' = 8, 'dom3' = 17),
                     labels = c('More individuals', 'Altered SAD', 'More individuals, more even SAD',
                                'More individuals, less even, no change richness', 'More individuals, less even, fewer species',
                                'More individuals, less even, more species')) +
  scale_color_manual(name = '',
                     values = trt_col[-2],
                     labels = c('More individuals', 'Altered SAD', 'More individuals, more even SAD',
                                'More individuals, less even, no change richness', 'More individuals, less even, fewer species',
                                'More individuals, less even, more species')) +
  theme(legend.direction = 'horizontal',
        legend.position = 'top') +
  guides(shape = guide_legend(byrow = TRUE), color = guide_legend(byrow = TRUE))

# function to extract legend from ggplot
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/200_gg_legend.R')
shape_legend2 <- gg_legend(shape_legend)
