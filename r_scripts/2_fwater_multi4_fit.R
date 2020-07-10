# load the Dani's data

rm(list=ls())

library(tidyverse)
library(brms)

# load Dani's freshwater stream land use data
Sys.setlocale('LC_ALL','C') # for the portuguese (I think) funkiness!

alphaDat <- read.table('~/Dropbox/mob-streams-idiv/analises/resultados/mob_data_alpha.txt') %>% 
  as_tibble()

alphaDat$Treatment <- factor(alphaDat$Treatment, 
                             levels = c('Reference', 'Agriculture', 'Forestry', 'Urbanization'))
alphaDat <- alphaDat %>% 
  unite(ss, c(Study, site.id), remove = F) %>% 
  unite(ssb, c(Study, site.id, Block), remove = F)

alphaDat %>% 
  summarise(
    min(S), min(Spie), min(N), min(Sn)
  )




S_model <- bf(S ~ Treatment + (Treatment | p | Study ) + (1| site.id/Block),
              family = 'poisson')
S_model2 <- bf(S ~ Treatment + (Treatment | p | Study ) + (Treatment| ss) +
                 (Treatment | ssb),
              family = 'poisson')

N_model <- bf(N ~ Treatment + (Treatment | p | Study ) + (1| site.id/Block),
              family = lognormal())
N_model2 <- bf(N ~ Treatment + (Treatment | p | Study ) + (Treatment| ss) +
                (Treatment | ssb),
              family = lognormal())

Sn_model <- bf(Sn ~ Treatment + (Treatment | p | Study ) + (1| site.id/Block),
               family = lognormal())
Sn_model2 <- bf(Sn ~ Treatment + (Treatment | p | Study ) + (Treatment| ss) +
                 (Treatment | ssb),
               family = lognormal())

S_PIE_model <- bf(Spie ~ Treatment + (Treatment | p | Study ) + (1| site.id/Block),
                  family = lognormal())
S_PIE_model2 <- bf(Spie ~ Treatment + (Treatment | p | Study ) + (Treatment| ss) +
                    (Treatment | ssb),
                  family = lognormal())

fwater_multi4_fit <- brm(S_model + N_model + S_PIE_model + Sn_model +
                            set_rescor(FALSE),
                          data= alphaDat,
                          # prior=hier_prior,
                          # inits = '0',
                          #init_r = 0.001,
                          # control = list(adapt_delta=0.95),
                          cores = 4,
                          chains = 4)

save(fwater_multi4_fit, file = '~/Dropbox/1current/BioTime/multidimensionalChangeMS/data_model_fits/fwater_multi4_fit.Rdata')

plot(fwater_multi4_fit)

fwater_S_pp <- pp_check(fwater_multi4_fit, resp = 'S') +
  scale_x_continuous(name = 'Species richness',
                     trans = 'log') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

fwater_N_pp <- pp_check(fwater_multi4_fit, resp = 'N') +
  scale_x_continuous(name = 'Total abundance',
                     trans = 'log') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

fwater_S_PIE_pp <- pp_check(fwater_multi4_fit, resp = 'Spie') +
  scale_x_continuous(name = 'Evenness (S_PIE)',
                     trans = 'log') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

fwater_Sn_pp <- pp_check(fwater_multi4_fit, resp = 'Sn') +
  scale_x_continuous(name = 'Rarefied richness (Sn)',
                     trans = 'log') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

combined_title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Posterior predictive checks: multivariate model",
    # fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )


bottom <- cowplot::plot_grid(fwater_S_pp,
                   fwater_Sn_pp,
                   fwater_S_PIE_pp,
                   fwater_N_pp,
                   nrow = 2, align = 'hv')

cowplot::plot_grid(combined_title, 
                   bottom, 
                   ncol = 1)
ggsave('~/Dropbox/1current/BioTime/multidimensionalChangeMS/Figs/fwater_multi4_pp.png',
       width = 200, height = 200, units = 'mm')



fwater_multi4_fit2 <- brm(S_model2 + N_model2 + S_PIE_model2 + Sn_model2 +
                           set_rescor(FALSE),
                         data= alphaDat,
                         cores = 4,
                         chains = 4)
