# inspection of Alban's efforts :-)

library(tidyverse)
library(brms)

dat <- read_csv('~/Dropbox/BioTimeX/long_table.csv') 

good_dat <- dat %>% 
  unite(study_trt, c(dataset_id, treatment), remove = F) %>% 
  mutate(cYear = year - mean(year)) 

# specify models for each response, linking the slopes and intercepts for linear models of time
S_model_s <- bf(S ~ 0 + (1 | dataset_id) + (cYear | p | study_trt) + (1| site),
                family = lognormal())

N_model_s <- bf(N ~ 0 + (1 | dataset_id) +  (cYear | p | study_trt) + (1| site),
                family = lognormal())

Sn_model_s <- bf(Sn ~ 0 + (1 | dataset_id) + (cYear | p | study_trt) + (1| site),
                 family = lognormal())

S_PIE_model_s <- bf(ENSPIE ~ 0 + (1 | dataset_id) + (cYear | p | study_trt) + (1| site),
                    family = lognormal())



btx_multi4_fit <- brm(S_model_s + N_model_s + S_PIE_model_s + Sn_model_s + 
                        set_rescor(FALSE),
                      data= good_dat,
                      control = list(adapt_delta=0.95),
                      cores = 4,
                      chains = 4)

S_btx_pp <- pp_check(btx_multi4_fit, resp = 'S') +
  scale_x_continuous(name = 'S', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

Sn_btx_pp <- pp_check(btx_multi4_fit, resp = 'Sn') +
  scale_x_continuous(name = 'Sn', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1)) 

N_btx_pp <- pp_check(btx_multi4_fit, resp = 'N') +
  scale_x_continuous(name = 'N', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

ENSPIE_btx_pp <- pp_check(btx_multi4_fit, resp = 'ENSPIE') +
  scale_x_continuous(name = 'ENSPIE', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

plot_grid(S_btx_pp,
          Sn_btx_pp,
          N_btx_pp,
          ENSPIE_btx_pp)

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/btx_multi4_pp.png',
       width = 200, height = 200, units = 'mm')

save(btx_multi4_fit, file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/btx_multi4_fit.Rdata')
