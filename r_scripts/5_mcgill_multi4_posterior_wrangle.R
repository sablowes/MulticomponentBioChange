# wrangle mcgill db results

load('~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/mcgill_multi4_fit.Rdata')


mcgill_coefs <- coef(mcgill_multi4_fit, robust = TRUE, probs = c(0.05, 0.95))

mcgill_coefs_multi4 <- bind_rows(
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_2',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_2'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_2'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_2'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_2'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_2'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_2'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_2'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_2'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_2'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_2'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_2'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_2'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_3',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_3'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_3'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_3'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_3'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_3'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_3'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_3'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_3'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_3'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_3'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_3'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_3'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_4',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_4'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_4'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_4'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_4'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_4'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_4'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_4'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_4'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_4'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_4'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_4'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_4'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_5',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_5'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_5'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_5'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_5'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_5'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_5'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'S_newIDs_5'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'S_newIDs_5'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'S_newIDs_5'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_5'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_5'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_5'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_6',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_6'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_6'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_6'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_6'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_6'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_6'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_6'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_6'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_6'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_6'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_6'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_6'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_7',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_7'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_7'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_7'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_7'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_7'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_7'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_7'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_7'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_7'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_7'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_7'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_7'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_8',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_8'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_8'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_8'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_8'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_8'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_8'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_8'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_8'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_8'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_8'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_8'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_8'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_9',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_9'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_9'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_9'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_9'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_9'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_9'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_9'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_9'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_9'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_9'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_9'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_9'][,'Q5']
  ),
  tibble(
    study = rownames(mcgill_coefs[[1]]),
    newID = 's_10',
    # N
    N = mcgill_coefs[[1]][,,'N_newIDs_10'][,'Estimate'],
    N_upper = mcgill_coefs[[1]][,,'N_newIDs_10'][,'Q95'],
    N_lower = mcgill_coefs[[1]][,,'N_newIDs_10'][,'Q5'],
    # S
    S = mcgill_coefs[[1]][,,'S_newIDs_10'][,'Estimate'],
    S_upper = mcgill_coefs[[1]][,,'S_newIDs_10'][,'Q95'],
    S_lower = mcgill_coefs[[1]][,,'S_newIDs_10'][,'Q5'],
    # Sn
    Sn = mcgill_coefs[[1]][,,'Sn_newIDs_10'][,'Estimate'],
    Sn_upper = mcgill_coefs[[1]][,,'Sn_newIDs_10'][,'Q95'],
    Sn_lower = mcgill_coefs[[1]][,,'Sn_newIDs_10'][,'Q5'],
    # ENSPIE
    ENSPIE = mcgill_coefs[[1]][,,'ENSPIE_newIDs_10'][,'Estimate'],
    ENSPIE_upper = mcgill_coefs[[1]][,,'ENSPIE_newIDs_10'][,'Q95'],
    ENSPIE_lower = mcgill_coefs[[1]][,,'ENSPIE_newIDs_10'][,'Q5']
  )
) %>% 
  mutate(db = 'McGill SADs')


levels <- mcgill_multi4_fit$data %>% 
  as_tibble() %>% 
  distinct(dataset_id, newID) %>% 
  mutate(level = dataset_id,
         site = ifelse(newID=='s_1', 'Intercept', paste0('newID', newID))) %>% 
  group_by(dataset_id) %>% 
  nest(data = c(level, site)) 

mcgill_multi4_posterior <- levels %>%
  mutate(N_study = purrr::map(data, ~posterior_samples(mcgill_multi4_fit, 
                                                       pars = paste('r_dataset_id__N[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         Sn_study = purrr::map(data, ~posterior_samples(mcgill_multi4_fit, 
                                                        pars = paste('r_dataset_id__Sn[', as.character(.x$level), ',', 
                                                                     as.character(.x$site),']', sep=''),
                                                        fixed = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()),
         S_study = purrr::map(data, ~posterior_samples(mcgill_multi4_fit, 
                                                       pars = paste('r_dataset_id__S[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         ENSPIE_study = purrr::map(data, ~posterior_samples(mcgill_multi4_fit, 
                                                            pars = paste('r_dataset_id__ENSPIE[', as.character(.x$level), ',', 
                                                                         as.character(.x$site),']', sep=''),
                                                            fixed = TRUE,
                                                            # want 1000 samples
                                                            subset = seq(1, 4000, by = 4))
                                   %>% unlist() %>% as.numeric()))


mcgill_multi4_posterior <- mcgill_multi4_posterior %>% 
  # add indicator for database
  mutate(db = 'McGill SADs') %>% 
  select(-data) %>% 
  unnest(cols = c(N_study, S_study, ENSPIE_study, Sn_study)) %>% 
  ungroup()

mcgill_cor <- as.mcmc(mcgill_multi4_fit, combine_chains = TRUE, 
                      pars = "^cor") %>% 
  as_tibble()

mcgill_cor_long <- bind_rows(
  mcgill_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_2__N_newIDs_2,
           corS_S_PIE = cor_dataset_id__S_newIDs_2__ENSPIE_newIDs_2,
           corS_Sn = cor_dataset_id__S_newIDs_2__Sn_newIDs_2,
           corN_S_PIE = cor_dataset_id__N_newIDs_2__ENSPIE_newIDs_2,
           corSn_N = cor_dataset_id__N_newIDs_2__Sn_newIDs_2,
           corS_PIE_Sn = cor_dataset_id__ENSPIE_newIDs_2__Sn_newIDs_2) %>% 
    mutate(newID = '2'),
  mcgill_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_3__N_newIDs_3,
           corS_S_PIE = cor_dataset_id__S_newIDs_3__ENSPIE_newIDs_3,
           corS_Sn = cor_dataset_id__S_newIDs_3__Sn_newIDs_3,
           corN_S_PIE = cor_dataset_id__N_newIDs_3__ENSPIE_newIDs_3,
           corSn_N = cor_dataset_id__N_newIDs_3__Sn_newIDs_3,
           corS_PIE_Sn = cor_dataset_id__ENSPIE_newIDs_3__Sn_newIDs_3) %>% 
    mutate(newID = '3'),
  mcgill_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_4__N_newIDs_4,
           corS_S_PIE = cor_dataset_id__S_newIDs_4__ENSPIE_newIDs_4,
           corS_Sn = cor_dataset_id__S_newIDs_4__Sn_newIDs_4,
           corN_S_PIE = cor_dataset_id__N_newIDs_4__ENSPIE_newIDs_4,
           corSn_N = cor_dataset_id__N_newIDs_4__Sn_newIDs_4,
           corS_PIE_Sn = cor_dataset_id__ENSPIE_newIDs_4__Sn_newIDs_4) %>% 
    mutate(newID = '4'),
  mcgill_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_5__N_newIDs_5,
           corS_S_PIE = cor_dataset_id__S_newIDs_5__ENSPIE_newIDs_5,
           corS_Sn = cor_dataset_id__S_newIDs_5__Sn_newIDs_5,
           corN_S_PIE = cor_dataset_id__N_newIDs_5__ENSPIE_newIDs_5,
           corSn_N = cor_dataset_id__N_newIDs_5__Sn_newIDs_5,
           corS_PIE_Sn = cor_dataset_id__ENSPIE_newIDs_5__Sn_newIDs_5) %>% 
    mutate(newID = '5'),
  mcgill_cor %>% 
    select(corS_N = cor_dataset_id__S_newIDs_6__N_newIDs_6,
           corS_S_PIE = cor_dataset_id__S_newIDs_6__ENSPIE_newIDs_6,
           corS_Sn = cor_dataset_id__S_newIDs_6__Sn_newIDs_6,
           corN_S_PIE = cor_dataset_id__N_newIDs_6__ENSPIE_newIDs_6,
           corSn_N = cor_dataset_id__N_newIDs_6__Sn_newIDs_6,
           corS_PIE_Sn = cor_dataset_id__ENSPIE_newIDs_6__Sn_newIDs_6) %>% 
    mutate(newID = '6'))


save(mcgill_coefs_multi4, mcgill_multi4_posterior, mcgill_cor_long,
     file = paste0(path2wd, 'multiComponentChange/results/mcgill_multi4_results.Rdata'))


mcgill_S_N <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = mcgill_multi4_posterior %>%
                   filter(newID!='s_1'),
                 aes(x = N_study,
                     y = S_study,
                     fill = 'S_N'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = N, y = S#,
                 # colour = Hemeroby_4
             )) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  labs(y = expression(paste('Change in species richness [log(S)]', sep = '')),
       x = expression(paste('Change in total abundance [log(N)]', sep = ''))#,
       # tag = 'a'
  ) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        strip.text = element_text(hjust = 0))

mcgill_S_Sn <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = mcgill_multi4_posterior %>%
                   filter(newID!='s_1'),
                 aes(x = Sn_study,
                     y = S_study,
                     fill = 'S_Sn'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = Sn, y = S#,
                 # colour = Hemeroby_4
             )) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  labs(y = '',#expression(paste('Change in species richness [log(S)]', sep = '')),
       x = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = ''))#,
       # tag = 'a'
  ) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        strip.text = element_text(hjust = 0))

mcgill_S_S_PIE <- ggplot() +
  mapply(function(level) {
    stat_ellipse(data = mcgill_multi4_posterior %>%
                   filter(newID!='s_1'),
                 aes(x = ENSPIE_study,
                     y = S_study,
                     fill = 'S_S_PIE'),
                 geom  = "polygon", type = "norm",
                 size  = 0, alpha = .33,
                 level = level)
  },
  # Enter the levels here
  level = seq(from = 0.05, to = 0.95, by = 0.1)) +
  geom_point(data = mcgill_coefs_multi4,
             aes(x = ENSPIE, y = S#,
                 # colour = Hemeroby_4
             )) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  scale_fill_manual(values = reln_colours,
                    guide = FALSE) +
  labs(y = '',#expression(paste('Change in species richness [log(S)]', sep = '')),
       x = expression(paste('Change in evenness [log(',S[PIE],')]', sep = ''))#,
       # tag = 'a'
  ) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        strip.text = element_text(hjust = 0))

plot_grid(mcgill_S_N,
          mcgill_S_Sn,
          mcgill_S_S_PIE,
          nrow = 1)
# 
# ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/mcgill_multi4_6sites.png',
       # width = 240, height = 80, units = 'mm')
