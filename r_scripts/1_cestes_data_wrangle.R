library(tidyverse)
library(brms)
load('~/Dropbox/1current/data/rCESTES/CESTES.RData')


# data are in a list: process and calculate metrics 
cestes <- tibble()

for(i in 1:length(LSmatred)){
  print(paste('dataset', i, 'in ', length(LSmatred)))
  
  name = names(LSmatred)[i]
  
  w_comm = LSmatred[name][[1]]$comm
  loc = LSmatred[name][[1]]$coord
  
  # metrics
  N = as.numeric(rowSums(w_comm))
  S = as.numeric(vegan::specnumber(w_comm))
  ENSPIE = as.numeric(vegan::diversity(w_comm, index = 'invsimpson'))
  # switch for N < 5
  # smallN = which(N < 6)
  # minN = min(N[N > 5])
  minN = min(N)
  # throw out sites with N < 5 before calculating Sn
  # if(length(smallN) > 1) w_comm = w_comm[-smallN,]
  Sn = as.numeric(vegan::rarefy(ceiling(w_comm), sample = minN))
  
  # need to fix up other metrics and metadata too
  # if(length(smallN) > 1) N = N[-smallN]
  # if(length(smallN) > 1) S = S[-smallN]
  # if(length(smallN) > 1) ENSPIE = ENSPIE[-smallN]
  # 
  # if(length(smallN) > 1) loc = loc[-smallN,]
  
  # combine
  cestes = bind_rows(cestes,
                     tibble(dataset_id = name,
                            x_utm = loc[,1],
                            y_utm = loc[,2],
                            fID = 1:nrow(loc),
                            N = N, 
                            S = S, 
                            ENSPIE = ENSPIE,
                            Sn = Sn,
                            minN = minN))
  
}

cestes %>% 
  filter(minN > 5) %>% 
  ggplot() +
  geom_density(aes(x = S)) +
  geom_density(aes(x = N), colour = 'red') +
  geom_density(aes(x = Sn), colour = 'blue') +
  geom_density(aes(x = ENSPIE), colour = 'green') +
  scale_x_continuous(trans = 'log2')

cestes <- cestes %>% 
  mutate(fID = as.factor(fID))

meta <- read_csv('~/Dropbox/1current/data/rCESTES/Metadat.csv')

cestes <- left_join(cestes, 
                    meta %>% 
                      mutate(dataset_id = dat) %>% 
                      select(-dat, -X1))

# reduce to six sites per study (model will estimate departures from focal site)
cestes_six <- cestes %>% 
  filter(minN > 5) %>% 
  group_by(dataset_id) %>% 
  sample_n(size = 6) %>% 
  group_by(dataset_id) %>% 
  mutate(newID = as.factor(1:n())) %>% 
  select(-fID) %>% 
  ungroup()


S_model = bf(S ~ 1 + (newID | p | dataset_id),
             family = 'poisson')
N_model = bf(N ~ 1 + (newID | p | dataset_id), 
             family = lognormal())
Sn_model = bf(Sn ~ 1 + (newID | p | dataset_id), 
              family = lognormal())
ENSPIE_model = bf(ENSPIE ~ 1 + (newID | p | dataset_id), 
                  family = lognormal())

m1_6sites <- brm(S_model + N_model + Sn_model + ENSPIE_model +
            set_rescor(FALSE),
          data = cestes_six,
          cores = 4,
          inits = '0')

S_cestes6_pp <- pp_check(m1_6sites, resp = 'S') +
  scale_x_continuous(name = 'S', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

Sn_cestes6_pp <- pp_check(m1_6sites, resp = 'Sn') +
  scale_x_continuous(name = 'Sn', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1)) 

N_cestes6_pp <- pp_check(m1_6sites, resp = 'N') +
  scale_x_continuous(name = 'N', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

ENSPIE_cestes6_pp <- pp_check(m1_6sites, resp = 'ENSPIE') +
  scale_x_continuous(name = 'ENSPIE', trans = 'log2') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

cowplot::plot_grid(S_cestes6_pp,
          Sn_cestes6_pp,
          N_cestes6_pp,
          ENSPIE_cestes6_pp)

ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/cestes6_multi4_pp.png',
       width = 200, height = 200, units = 'mm')

save(m1_6sites, file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/cestes6_multi4_fit.Rdata')

