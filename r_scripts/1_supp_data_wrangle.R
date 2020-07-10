# set working directory and load packages
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/0_init_dirs_load_packages.R')
theme_set(theme_minimal())
# code to load and prepare Supp & Ernest data for multicomponent change analysis

# community data; problems flagged here are studies missing the initial_year
comm <- read_csv(paste0(path2wd, 'data/community_analysis_data.csv'))

# matched comparisons (experiment, control) coded by siteID
comparisons <- read_csv(paste0(path2wd, 'data/orderedcomparisons.csv'), 
                        col_names = c('referenceID', 'ctrlID', 'trtID')) %>% 
  mutate(ctrl_filter = paste(referenceID, ctrlID, sep = '_'),
         trt_filter = paste(referenceID, trtID, sep = '_'),
         # grouping variable for matched comparisons
         group = paste(referenceID, ctrlID, trtID, sep = '_'))

# metadata for experiments
exp <- read_csv(paste0(path2wd, 'data/experiments_analysis_data.csv'))

# metadata for sites
sites <- read_csv(paste0(path2wd, 'data/sites_analysis_data.csv'))

# put treatment information into community df
temp <- comm %>% 
  mutate(ID = paste(referenceID, siteID, sep = '_')) %>% 
  mutate(trt = ifelse(ID %in% comparisons$ctrl_filter, 'control', 'treatment'))
  
# put the grouping variable
group_col <- tibble(group = NULL)
for(row in 1:nrow(temp)){
  print(paste(row, 'in ', nrow(temp)))
  referenceID = temp$referenceID[row]
  ID = temp$ID[row]
  control = filter(comparisons, ctrl_filter==ID)
  trt = filter(comparisons, trt_filter==ID)
  pair = ifelse(nrow(control)==0,  paste(trt$ctrlID, trt$trtID, sep = '_'),
                paste(control$ctrlID, control$trtID, sep = '_'))
  group_col = bind_rows(group_col, 
                        tibble(group = paste(referenceID, pair, sep = '_')))
}

dat <- bind_cols(temp, group_col)

dat_summary <- dat %>% 
  filter(abundance > 0) %>% 
  unite(species2, c(genus, species), remove = FALSE) %>% 
  group_by(referenceID, siteID, trt, group) %>% 
  summarise(S = n_distinct(species2),
            N = sum(abundance),
            ENSPIE = vegan::diversity(abundance, index = 'invsimpson')) %>% 
  ungroup()

# some treatments are not matched with controls
problems <- dat_summary %>% 
  separate(group, into = c('ref', 'x1', 'x2'), remove = F) %>% 
  filter(is.na(x2))

dat_summary <- dat_summary %>% 
  filter(!group %in% problems$group)

minN <- dat_summary %>% 
  group_by(group) %>% 
  summarise(minN = min(N))

Sn <- left_join(dat %>% 
                  filter(!group %in% problems$group) %>% 
                  unite(species2, c(genus, species), remove = FALSE),
                minN) %>% 
  filter(minN > 5) %>% 
  select(group, trt, species2, abundance, minN) %>% 
  group_by(group, trt) %>% 
  nest(data = c(species2, abundance, minN)) %>% 
  mutate(Sn = purrr::map(data, possibly(~mobr::rarefaction(.x$abundance, method = 'indiv', 
                                                           effort = unique(.x$minN)), otherwise = NULL))) %>% 
  ungroup()

Sn %>% 
  unnest(Sn)

supp_alpha <- left_join(dat_summary,
                         Sn %>% 
                           unnest(Sn) %>% 
                           select(group, trt, Sn))

save(supp_alpha,
     file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/intermediate_results/supp_alpha.Rdata')

supp_alphadat_summary %>% 
  ggplot() +
  facet_wrap(~referenceID) +
  geom_point(aes(x = trt, y = Sn, colour = group),
             position = position_dodge(width = 0.1)) +
  scale_y_continuous(trans = 'log2') +
  theme(legend.position = 'none')

# combine with other metadata for inspection
exp_dat <- left_join(supp_alpha,
                         exp) # spelling errors: experiment_type
# # maybe sigma ~ habitat or experiment type (fix errors), or simplified taxa?
# ggplot() +
#   facet_wrap(~biome) +
#   geom_point(data = exp_dat,
#              aes(x = trt, y = N, colour = group),
#              position = position_dodge(width = 0.1)) +
#   scale_y_continuous(trans = 'log2') +
#   theme(legend.position = 'none')
# 
site_dat <- left_join(supp_alpha,
                     sites)
# 
# ggplot() +
#   facet_wrap(~country) +
#   geom_point(data = site_dat,
#              aes(x = trt, y = N, colour = group),
#              position = position_dodge(width = 0.1)) +
#   scale_y_continuous(trans = 'log2') +
#   theme(legend.position = 'none')

# dat_summary %>% 
#   ggplot() +
#   facet_wrap(~referenceID) +
#   geom_point(aes(x = N, y = S, colour = trt)) +
#   # stat_ellipse(aes(x = N, y = S), 
#   #              geom  = "line", type = "norm",
#   #              size  = 0, alpha = .33,
#   #              level = 0.95) +
#   scale_x_continuous(trans = 'log2') +
#   scale_y_continuous(trans = 'log2') +
#   theme(legend.position = 'none')


S_formula <- bf(S ~ 1 + (1 |referenceID) + (trt | p | group) + (1 | siteID)) +
  poisson()
                  
N_formula <- bf(N ~ 1 + (1 | referenceID) + (trt | p | group)) +
  lognormal()

ENSPIE_formula <- bf(ENSPIE ~ 1 + (1 | referenceID) + (trt | p | group)) +
  lognormal()

Sn_formula <- bf(Sn ~ 1 + (1 | referenceID) + (trt | p | group)) +
  lognormal()

supp_multi4c_fit <- brm(S_formula + N_formula + ENSPIE_formula + Sn_formula +
                             set_rescor(FALSE),
                       data= supp_alpha,
                       prior = c(prior('normal(0,1)', class = 'sigma', resp = 'N'),
                                 prior('normal(0,1)', class = 'sigma', resp = 'ENSPIE'),
                                 prior('normal(0,1)', class = 'sigma', resp = 'Sn')),
                       control = list(adapt_delta=0.99),
                       iter = 8000,
                       # warmup = 1000,
                       thin = 4,
                       # inits = '0',
                       cores = 4,
                       chains = 4)

n_pp <- pp_check(supp_multi4d_fit,
                      resp = 'N') +
  scale_x_continuous(trans = 'log',
                     name = '# individuals') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

s_pp <- pp_check(supp_multi4d_fit,
                      resp = 'S') +
  scale_x_continuous(trans = 'log',
                     name = 'species richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

sn_pp <- pp_check(supp_multi4d_fit,
                      resp = 'Sn') +
  scale_x_continuous(trans = 'log',
                     name = 'rarefied richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

enspie_pp <- pp_check(supp_multi4d_fit,
         resp = 'ENSPIE') +
  scale_x_continuous(trans = 'log',
                     name = 'evenness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

plot_grid(n_pp, s_pp, 
          sn_pp, enspie_pp,
          nrow = 2)

ggsave(filename = '~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/supp_multi4_pp.png',
       width = 200, height = 200, units = 'mm')

save(supp_multi4c_fit, 
     file = '~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/supp_multi4_fit_prelim.Rdata')#file=Sys.getenv('OFILE')



ENSPIE_resid <- resid(supp_multi4d_fit, resp = 'ENSPIE',
                      type = 'pearson', method = 'predict') %>% 
  as_tibble() %>% 
  rename(resid = Estimate) %>% 
  bind_cols(supp_multi4d_fit$data)

ENSPIE_resid <- left_join(ENSPIE_resid,
                          exp_dat) %>% 
  left_join(site_dat) %>% 
  mutate(resp = 'ENSPIE')

Sn_resid <- resid(supp_multi4d_fit, resp = 'Sn',
                      type = 'pearson', method = 'predict') %>% 
  as_tibble() %>% 
  rename(resid = Estimate) %>% 
  bind_cols(supp_multi4d_fit$data)

Sn_resid <- left_join(Sn_resid,
                          exp_dat) %>% 
  left_join(site_dat) %>% 
  mutate(resp = 'Sn')

S_resid <- resid(supp_multi4d_fit, resp = 'S',
                      type = 'pearson', method = 'predict') %>% 
  as_tibble() %>% 
  rename(resid = Estimate) %>% 
  bind_cols(supp_multi4d_fit$data)

S_resid <- left_join(S_resid,
                          exp_dat) %>% 
  left_join(site_dat) %>% 
  mutate(resp = 'S')

N_resid <- resid(supp_multi4d_fit, resp = 'N',
                      type = 'pearson', method = 'predict') %>% 
  as_tibble() %>% 
  rename(resid = Estimate) %>% 
  bind_cols(supp_multi4d_fit$data)

N_resid <- left_join(N_resid,
                          exp_dat) %>% 
  left_join(site_dat) %>% 
  mutate(resp = 'N')

bind_rows(S_resid,
          N_resid,
          ENSPIE_resid,
          Sn_resid) %>% 
  ggplot() +
  facet_wrap(~resp) +
  geom_boxplot(aes(x = resid, y = trt)) +
  geom_vline(xintercept = 0, lty = 2)

bind_rows(S_resid,
          N_resid,
          ENSPIE_resid,
          Sn_resid) %>% 
  ggplot() +
  facet_wrap(~resp) +
  geom_boxplot(aes(x = resid, y = habitat)) +
  geom_vline(xintercept = 0, lty = 2)
