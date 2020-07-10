# wrangle BioTIME (Blowes et al rarefyIDs) to look at spatial patterns of
# biodiversity change

library(tidyverse)
set.seed(888)
# reproducible 'random' sample of metrics calculated from BioTIME at 96km2 grid-scale


load('~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/rarefied_medians_count.Rdata')

multiLoc <- rarefied_medians_count %>% 
  # remove observations with low minN (for rarefied richness) and low community N
  filter(minN > 6 & N > 20) %>% 
  group_by(STUDY_ID, YEAR) %>% 
  summarise(n_loc = n_distinct(rarefyID)) %>% 
  ungroup() %>% 
  filter(n_loc > 9)

multiLoc %>% distinct(STUDY_ID) # 84 studies
multiLoc %>% distinct(STUDY_ID, YEAR) # ~1000 study - year combinations

# want some examples from all REALMS...
meta <- read_csv('~/Dropbox/BioTIMELatest/bioTIMEmetadataSept18.csv')

multiLoc <- left_join(multiLoc,
                      meta %>% 
                        mutate(STUDY_ID = as.character(STUDY_ID)) %>% 
                        select(STUDY_ID, REALM, TAXA)) 
# get 20 study-year combinations from the marine and terrestrial realms
r20 <- multiLoc %>% 
  group_by(REALM) %>% 
  sample_n(20) %>% 
  ungroup() %>% 
  unite(row_id, c(STUDY_ID, YEAR))

bt_spatDat <- rarefied_medians_count %>% 
  filter(minN > 6 & N > 20) %>% 
  unite(row_id, c(STUDY_ID, YEAR), remove = FALSE) %>% 
  filter(row_id %in% r20$row_id) %>% 
  # get 10 locations (study-year) combinations from each study
  group_by(STUDY_ID) %>% 
  sample_n(10) %>% 
  ungroup()


bt_spatDat %>% 
  ggplot() +
  geom_density(aes(x = N)) +
  geom_density(aes(x = Sn)) +
  geom_density(aes(x = S)) +
  geom_density(aes(x = ENSPIE)) +
  scale_x_continuous(trans = 'log2') 

# simplify data, and set up reference location for each study-year combination
bt_space <- bt_spatDat %>% 
  select(STUDY_ID, rarefyID, YEAR, N, S, Sn, ENSPIE) %>% 
  group_by(STUDY_ID, YEAR) %>% 
  mutate(loc = paste0('location_', 1:n())) %>% 
  ungroup() %>% 
  unite(cell_yr, c(rarefyID, YEAR))

library(brms)

hier_prior <- c(prior(lognormal(log(1), log(2)), class='Intercept',resp = 'S'), 	# global intercept
                      prior(lognormal(log(1), log(2)), class='Intercept',resp = 'N'),
                prior(lognormal(log(1), log(2)), class='Intercept',resp = 'ENSPIE'),
                prior(lognormal(log(1), log(2)), class='Intercept',resp = 'Sn'),
                set_prior(prior = 'lkj(1)', class='cor')
)

S_formula <- bf(S ~ 1 + (loc | p | cell_yr) + (1 | STUDY_ID),
                family = brmsfamily('poisson'))

N_formula <- bf(N ~ 1 + (loc | p | cell_yr) + (1 | STUDY_ID)) +
  lognormal()

ENSPIE_formula <- bf(ENSPIE ~ 1 + (loc | p | cell_yr) + (1 | STUDY_ID)) +
  lognormal()

Sn_formula <- bf(Sn ~ 1 + (loc | p | cell_yr) + (1 | STUDY_ID)) +
  lognormal()

bt_space_multi4_fit_0 <- brm(S_formula + N_formula + ENSPIE_formula + Sn_formula +
                    set_rescor(FALSE),
                  data = bt_space,
                  # prior=hier_prior,
                  # inits = '0',
                  init_r = 0.001,
                  control = list(adapt_delta=0.95),
                  cores = 4,
                  chains = 4)

n_pp <- pp_check(bt_space_multi4_fit_0,
                 resp = 'N') +
  scale_x_continuous(trans = 'log',
                     name = '# individuals') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

s_pp <- pp_check(bt_space_multi4_fit_0,
                 resp = 'S') +
  scale_x_continuous(trans = 'log',
                     name = 'species richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

sn_pp <- pp_check(bt_space_multi4_fit_0,
                  resp = 'Sn') +
  scale_x_continuous(trans = 'log',
                     name = 'rarefied richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

enspie_pp <- pp_check(bt_space_multi4_fit_0,
                      resp = 'ENSPIE') +
  scale_x_continuous(trans = 'log',
                     name = 'evenness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

cowplot::plot_grid(n_pp, s_pp, 
          sn_pp, enspie_pp,
          nrow = 2)

ggsave(filename = '~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/bt_space_multi4_pp.png',
       width = 200, height = 200, units = 'mm')

save(bt_space_multi4_fit, bt_space,
     file = '~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/bt_space_multi4_fit.Rdata')

