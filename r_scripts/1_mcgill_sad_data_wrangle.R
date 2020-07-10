# -wrangle data from Chase et al (2018) Ecol Lett for reuse in multicomponent change paper
source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/0_init_dirs_load_packages.R')

## NB: some of these are not 'natural' gradients:

##--------------------------------------------------
##	read in data (only the 37 sads BM identified earlier)
sad1 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Arntz&Rumohr1982a_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm1')
sad2 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Arntz&Rumohr1982b_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm2')
sad3 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Austin1980c_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm3')
sad4 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Brock1979_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm4')
sad5 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Crighton&Fisher1982_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm5')
sad6 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Dunn1949a_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm6')
sad7 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Dunn1949b_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm7')
sad8 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Easton1947_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm8')
sad9 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Eggeling1947_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm9')
sad10 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Fleming1972_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm10')
sad11 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Gage1972_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm11')
# - Godfrey 1978: response to water pollution
# sad12 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Godfrey1978_SAD.csv', col_types = cols(year = col_character())) %>%
#   mutate(dataset_id = 'm12')
sad13 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Haila1983_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm13')
sad14 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Harrod1964a_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm14')
sad15 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Harrod1964b_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm15')
sad16 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Harrod1964c_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm16')
sad17 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Harrod1964d_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm17')
# - Heck 1976: pollution in seagrass meadows and adjacent habitats
# sad18 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Heck1976_SAD.csv', col_types = cols(year = col_character())) %>%
#   mutate(dataset_id = 'm18')															
sad19 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Heck1977_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm19')
sad20 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Inglesias1981_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm20')
sad21 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Josefson1981_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm21')
# - Lambshead 1986: sewage and industrial waste
# sad22 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Lambshead1986_SAD.csv', col_types = cols(year = col_character())) %>%
#   mutate(dataset_id = 'm22')
sad23 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/McCloskey1970_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm23')
sad24 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Menhinick1964_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm24')
sad25 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/O_Dea&Whittaker2007_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm25')
sad26 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Pearson1970_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm26')
sad27 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Pearson1975_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm27')
sad28 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Pollard1971_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm28')
# - Saloman & Naughton 1977: hurricane Eloise
# sad29 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Saloman&Naughton1977_SAD.csv', col_types = cols(year = col_character())) %>%
#   mutate(dataset_id = 'm29')
sad30 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Sanders1960_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm30')
sad31 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Warwick&Buchanan1970_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm31')
sad32 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Warwick&Gee1984_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm32')
sad33 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Warwick&Price1979_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm33')
sad34 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Wiegert1974_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm34')
sad35 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/Young&Rhoads1971_SAD.csv', col_types = cols(year = col_character())) %>%
  mutate(dataset_id = 'm35')
##	bbs and bci data are spp X site matrices
sad36 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/bbs20.csv', col_names=F) %>%
  # wide to long
  gather() %>%
  # add identifier for data set, and change column names
  mutate(sample_no = as.numeric(unlist(strsplit(key, 'X'))[which(unlist(strsplit(key, 'X'))!='')]),
         dataset_id = 'm36',
         site = key,
         count = value, 
         taxa = 'Terrestrial_birds') %>%
  select(-key, -value) %>%
  # remove 0 counts
  filter(count>0)

sad37 <- read_csv('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/the37/bci20.csv', col_names=F) %>%
  gather() %>%
  mutate(sample_no = as.numeric(unlist(strsplit(key, 'X'))[which(unlist(strsplit(key, 'X'))!='')]),
         dataset_id = 'm37',
         site = key,
         count = value, 
         taxa = 'Terrestrial_trees') %>%
  select(-key, -value) %>%
  # remove 0 counts
  filter(count>0)

##	put them together
all_sads <- bind_rows(sad1, sad2, sad3, sad4, sad5, sad6, sad7, 
                      sad8, sad9, sad10, sad11, sad13,#sad12
                      sad14, sad15, sad16, sad17, sad19, #sad18,
                      sad20, sad21, sad23, sad24, sad25, #sad22,
                      sad26, sad27, sad28, sad30, sad31, #sad29
                      sad32, sad33, sad34, sad35, sad36, sad37)

##	calculate number of sads per data set (I'm going to look at max of 10 sites only)
n_sads <- all_sads %>%
  group_by(dataset_id) %>%
  summarise(n_sads = n_distinct(sample_no)) %>%
  ungroup()

all_sads <- inner_join(n_sads, all_sads, by='dataset_id')

##	these are the sads that need sub-sampling
ss_reqd <- n_sads %>% 
  filter(n_sads>10)

##	get a random subset of 20 sample_no's
ss_sads <- inner_join(ss_reqd, all_sads, by='dataset_id') %>%
  distinct(dataset_id, sample_no) %>%
  group_by(dataset_id) %>%
  sample_n(10) %>%
  ungroup()	

##	the sads with <= 10 samples
ss_1 <- all_sads %>% filter(n_sads <= 10)
##	the sub-samples from sads with > 20 samples	
ss_2 <- inner_join(ss_sads, all_sads, by=c('dataset_id', 'sample_no'))
##	join
clean_sads <- bind_rows(ss_1, ss_2)

#-------------tidy and fix column names for .csv save for submission--------
SADs_for_metaAnal <- clean_sads %>%
  mutate(reference = reference,
         data_ID = dataset_id,
         taxa = taxa,
         N_sads = n_sads,
         sad_ID = sample_no,
         count = count) %>%
  select(reference, data_ID, taxa, N_sads, sad_ID, count)
# setwd('~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/')
# write_csv(SADs_for_metaAnal, path = '~/Dropbox/1current/conceptual/Chase_et_al_conceptsMS/data/SADs_for_metaAnalysis')
##------------calculate metrics--------------------------------------
summary_stats <- clean_sads %>%
  group_by(dataset_id, sample_no) %>%
  summarise(N = sum(count),
            ENSPIE = diversity(count, index='invsimpson'),
            S = length(count!=0)) %>%
  ungroup() %>%
  group_by(dataset_id) %>%
  mutate(n_sads = n_distinct(sample_no)) %>%
  ungroup()

minN <- summary_stats %>% 
  group_by(dataset_id) %>% 
  summarise(minN = min(N)) %>% 
  ungroup()

Sn <- left_join(clean_sads,
                minN) %>% 
  filter(minN > 5) %>% 
  select(dataset_id, sample_no, count, minN) %>% 
  group_by(dataset_id, sample_no) %>% 
  nest(data = c(count, minN)) %>% 
  mutate(Sn = purrr::map(data, possibly(~mobr::rarefaction(.x$count, method = 'indiv', 
                                                           effort = unique(.x$minN)), otherwise = NULL))) %>% 
  ungroup()

mcgill_sad <- left_join(summary_stats,
                        Sn %>% 
                          select(-data) %>% 
                          unnest(cols = Sn)) %>% 
  # fix sample_no for ease of wrangling later
  group_by(dataset_id) %>% 
  mutate(newID = 1:n(),
         newID = paste0('s_', newID))

# set 'first' site as reference for factor covariate
mcgill_sad$newID <- factor(mcgill_sad$newID,
                              levels = c('s_1', 's_2', 's_3', 's_4',
                                         's_5', 's_6', 's_7', 's_8',
                                         's_9', 's_10'))

S_formula <- bf(S ~ 1 + (newID | p | dataset_id)) +
  poisson()

N_formula <- bf(N ~ 1 + (newID | p | dataset_id)) +
  lognormal()

ENSPIE_formula <- bf(ENSPIE ~ 1 + (newID | p | dataset_id)) +
  lognormal()

Sn_formula <- bf(Sn ~ 1 + (newID | p | dataset_id)) +
  lognormal()

mcgill_multi4_fit <- brm(S_formula + N_formula + ENSPIE_formula + Sn_formula +
                          set_rescor(FALSE),
                        data= mcgill_sad,
                        control = list(adapt_delta=0.99),
                        inits = '0',
                        cores = 4,
                        chains = 4)

n_pp <- pp_check(mcgill_multi4_fit,
                 resp = 'N') +
  scale_x_continuous(trans = 'log',
                     name = '# individuals') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

s_pp <- pp_check(mcgill_multi4_fit,
                 resp = 'S') +
  scale_x_continuous(trans = 'log',
                     name = 'species richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

sn_pp <- pp_check(mcgill_multi4_fit,
                  resp = 'Sn') +
  scale_x_continuous(trans = 'log',
                     name = 'rarefied richness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

enspie_pp <- pp_check(mcgill_multi4_fit,
                      resp = 'ENSPIE') +
  scale_x_continuous(trans = 'log',
                     name = 'evenness') +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

plot_grid(n_pp, s_pp, 
          sn_pp, enspie_pp,
          nrow = 2)

ggsave(filename = '~/Dropbox/1current/multidimensionalChangeMS/Figs/diagnostic/mcgill_multi4_pp.png',
       width = 200, height = 200, units = 'mm')

mcgill_sad <- left_join(mcgill_sad,
                        clean_sads %>% 
                          distinct(dataset_id, sample_no, reference, treatment, habitat, longitude, latitude, taxa, year))
save(mcgill_multi4_fit, mcgill_sad,
     file = '~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/mcgill_multi4_fit.Rdata')
