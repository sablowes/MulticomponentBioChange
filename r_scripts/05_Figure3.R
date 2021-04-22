# summary of component changes (with covariation)
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/00_init_dirs_load_packages.R')


load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/predicts_multi4_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/btx_multi4_global_results.Rdata')
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/spatial_multi4_results.Rdata')

# calculate proportions of assemblages for each combination of component change
prop_change <- bind_rows(
  bt_study_summary %>% 
    group_by(componentChange) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(total_n = sum(n),
           per = (n / sum(n)) * 100,
           db = 'BioTIME'),
  btx_study_summary %>% 
    group_by(componentChange) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(total_n = sum(n),
           per = (n / sum(n)) * 100,
           db = 'Experimental (time series)'),
  spat_coefs_multi4_filtered %>% 
    group_by(componentChange) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(total_n = sum(n),
           per = (n / sum(n)) * 100,
           db = 'Spatial gradients'),
  SS_LU_multi_filtered %>% 
    filter(LU!='Primary vegetation') %>% 
    group_by(componentChange) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    mutate(total_n = sum(n),
           per = (n / sum(n)) * 100,
           db = 'PREDICTS')
  )

ggplot() +
  facet_wrap(~factor(db, levels = c('BioTIME', 'Experimental (time series)',
                                    'Spatial gradients', 'PREDICTS'),
                     labels = c('a. Temporal change: natural variation',
                                'b. Temporal change: perturbed environments',
                                'c. Spatial change: natural variation',
                                'd. Spatial change: land use change')),
             ncol = 2)+
  geom_bar(data = prop_change %>% filter(componentChange!='No change'),
           aes(x = componentChange,
               y = per#, 
               # fill = sign, colour = componentChange
               ),
           stat = 'identity') +
  geom_text(data = prop_change %>% filter(componentChange!='No change'),
            aes(y = per + 1, x = componentChange, 
                label=n),
            parse = T) +
  geom_label(data = prop_change %>% filter(componentChange=='No change'),
            aes(y = 12.5, x = 3, 
                label=paste('No change in\nany component:\n', n, ' / ', total_n)), 
            hjust = 0
            ) +
  labs(x = 'Component(s) with high probability (>95%) of change',
     y = 'Percentage (%)') +
  scale_x_discrete(breaks = c(#'No change',
                              'N only', 'ENSPIE only', 'Sn only', 'S only',
                              'S & Sn', 'S & ENSPIE', 'S & N',
                              'Sn & ENSPIE', 'Sn & N',
                              'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                              'S, Sn, N & ENSPIE'),
                   labels = c(#'None',
                              expression(italic(N)), expression(paste(italic(S)[PIE])), expression(paste(italic(S)[n])), 'S',
                              expression(paste(italic(S), ' & ', italic(S)[n])), expression(paste(italic(S), ' & ', italic(S)[PIE])), 
                              expression(paste(italic(S), ' & ', italic(N))),
                              expression(paste(italic(S)[n], ' & ', italic(S)[PIE])), expression(paste(italic(S)[n], ' & ', italic(N))),
                              expression(paste(italic(N), ' & ', italic(S)[PIE])), expression(paste(italic(S)[n], ', ',italic(N), ' & ', italic(S)[PIE])), 
                              expression(paste(italic(S), ', ', italic(N), ' & ', italic(S)[PIE])),
                              expression(paste(italic(S), ', ', italic(S)[n], ', ', italic(N), ' & ', italic(S)[PIE])))
  ) +
  # scale_colour_manual(values = componentChange_col, guide = F) +
  # scale_fill_manual(values = componentChange_col, guide = F) +
  scale_color_grey(start = 0.5, end = 0.5, guide = F) +
  scale_fill_grey(na.value = 0, name = 'Signs of components\nchanging') +
  coord_flip() +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, hjust = 0),
        strip.background = element_blank(),
        legend.position = c(1,0.2),
        legend.justification = c(1,0),
        axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        # panel.grid = element_blank(),
        plot.margin = margin(30,30,30,70)
        )

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/submission/Fig3.pdf',
       width = 290, height = 200, units = 'mm')


prop_change %>% 
  filter(componentChange=='No change')
