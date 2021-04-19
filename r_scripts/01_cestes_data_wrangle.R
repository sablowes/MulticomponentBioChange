library(tidyverse)
library(brms)
# these data are available at: 
#https://doi.org/10.25829/idiv.286-21-2695
#https://figshare.com/articles/dataset/CESTES_-_a_global_database_for_metaCommunity_Ecology_Species_Traits_Environment_and_Space/9436586

load('~/Dropbox/1current/data/rCESTES/CESTES.RData')

set.seed(123)
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
  # coverage
  coverage = tibble(
    N = N,
    singletons = rowSums(w_comm==1),
    doubletons = rowSums(w_comm==2)) %>% 
    mutate(Chat_corrected = ifelse(doubletons > 0,
                          1 - (singletons/N) * (((N-1)*singletons)/((N-1)*singletons + 2*doubletons)), 
                          1 - (singletons/N) * (((N-1)*singletons)/((N-1)*singletons + 2))))
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
                            Chat_corrected = coverage$Chat_corrected,
                            Sn = Sn,
                            minN = minN))
  
}

cestes <- cestes %>% 
  mutate(fID = as.factor(fID))

# get metadata to remove studies associated anthropogenic environments
meta <- read_csv('~/Dropbox/1current/data/rCESTES/Metadat.csv')

cestes <- left_join(cestes, 
                    meta %>% 
                      mutate(dataset_id = dat) %>% 
                      select(-dat, -X1))

cestes %>% 
  filter(minN > 5 & (Hemeroby_4!='Mixed' & Hemeroby_4!='Anthropic')) %>% 
  # additional filter for coverage
  # filter(Chat_corrected > 0.85) %>% 
  group_by(dataset_id) %>% 
  summarise(n_sites = n_distinct(fID)) %>% 
  arrange(-desc(n_sites))

# reduce to ten sites per study (model will estimate departures from focal site)
cestes_10 <- cestes %>% 
  filter(minN > 5 & (Hemeroby_4!='Mixed' & Hemeroby_4!='Anthropic')) %>% 
  group_by(dataset_id) %>% 
  sample_n(size = 10) %>% 
  group_by(dataset_id) %>% 
  mutate(newID = as.factor(1:n())) %>% 
  select(-fID) %>% 
  ungroup() %>% 
  # remove one study with presence data
  filter(dataset_id!='Purschke2012a' &
           dataset_id!='Purschke2012b' &
           dataset_id!='Purschke2012c' &
           dataset_id!='Purschke2012d' &
           dataset_id!='Purschke2012e')

save(cestes_10, file = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/cestes10.Rdata')