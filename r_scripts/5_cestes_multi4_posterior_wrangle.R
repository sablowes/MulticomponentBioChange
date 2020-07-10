# plot results from cestes db

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/0_init_dirs_load_packages.R')

load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/cestes6_multi4_fit.Rdata')
meta <- read_csv('~/Dropbox/1current/data/rCESTES/Metadat.csv')

cestes_coefs <- coef(m1_6sites, robust = TRUE, probs = c(0.05, 0.95))

cestes_coefs_multi4 <- bind_rows(
  tibble(
    study = rownames(cestes_coefs[[1]]),
    newID = '2',
    # N
    N = cestes_coefs[[1]][,,'N_newID2'][,'Estimate'],
    N_upper = cestes_coefs[[1]][,,'N_newID2'][,'Q95'],
    N_lower = cestes_coefs[[1]][,,'N_newID2'][,'Q5'],
    # S
    S = cestes_coefs[[1]][,,'S_newID2'][,'Estimate'],
    S_upper = cestes_coefs[[1]][,,'S_newID2'][,'Q95'],
    S_lower = cestes_coefs[[1]][,,'S_newID2'][,'Q5'],
    # Sn
    Sn = cestes_coefs[[1]][,,'Sn_newID2'][,'Estimate'],
    Sn_upper = cestes_coefs[[1]][,,'Sn_newID2'][,'Q95'],
    Sn_lower = cestes_coefs[[1]][,,'Sn_newID2'][,'Q5'],
    # ENSPIE
    ENSPIE = cestes_coefs[[1]][,,'ENSPIE_newID2'][,'Estimate'],
    ENSPIE_upper = cestes_coefs[[1]][,,'ENSPIE_newID2'][,'Q95'],
    ENSPIE_lower = cestes_coefs[[1]][,,'ENSPIE_newID2'][,'Q5']
  ),
  tibble(
    study = rownames(cestes_coefs[[1]]),
    newID = '3',
    # N
    N = cestes_coefs[[1]][,,'N_newID3'][,'Estimate'],
    N_upper = cestes_coefs[[1]][,,'N_newID3'][,'Q95'],
    N_lower = cestes_coefs[[1]][,,'N_newID3'][,'Q5'],
    # S
    S = cestes_coefs[[1]][,,'S_newID3'][,'Estimate'],
    S_upper = cestes_coefs[[1]][,,'S_newID3'][,'Q95'],
    S_lower = cestes_coefs[[1]][,,'S_newID3'][,'Q5'],
    # Sn
    Sn = cestes_coefs[[1]][,,'Sn_newID3'][,'Estimate'],
    Sn_upper = cestes_coefs[[1]][,,'Sn_newID3'][,'Q95'],
    Sn_lower = cestes_coefs[[1]][,,'Sn_newID3'][,'Q5'],
    # ENSPIE
    ENSPIE = cestes_coefs[[1]][,,'ENSPIE_newID3'][,'Estimate'],
    ENSPIE_upper = cestes_coefs[[1]][,,'ENSPIE_newID3'][,'Q95'],
    ENSPIE_lower = cestes_coefs[[1]][,,'ENSPIE_newID3'][,'Q5']
  ),
  tibble(
    study = rownames(cestes_coefs[[1]]),
    newID = '4',
    # N
    N = cestes_coefs[[1]][,,'N_newID4'][,'Estimate'],
    N_upper = cestes_coefs[[1]][,,'N_newID4'][,'Q95'],
    N_lower = cestes_coefs[[1]][,,'N_newID4'][,'Q5'],
    # S
    S = cestes_coefs[[1]][,,'S_newID4'][,'Estimate'],
    S_upper = cestes_coefs[[1]][,,'S_newID4'][,'Q95'],
    S_lower = cestes_coefs[[1]][,,'S_newID4'][,'Q5'],
    # Sn
    Sn = cestes_coefs[[1]][,,'Sn_newID4'][,'Estimate'],
    Sn_upper = cestes_coefs[[1]][,,'Sn_newID4'][,'Q95'],
    Sn_lower = cestes_coefs[[1]][,,'Sn_newID4'][,'Q5'],
    # ENSPIE
    ENSPIE = cestes_coefs[[1]][,,'ENSPIE_newID4'][,'Estimate'],
    ENSPIE_upper = cestes_coefs[[1]][,,'ENSPIE_newID4'][,'Q95'],
    ENSPIE_lower = cestes_coefs[[1]][,,'ENSPIE_newID4'][,'Q5']
  ),
  tibble(
    study = rownames(cestes_coefs[[1]]),
    newID = '5',
    # N
    N = cestes_coefs[[1]][,,'N_newID5'][,'Estimate'],
    N_upper = cestes_coefs[[1]][,,'N_newID5'][,'Q95'],
    N_lower = cestes_coefs[[1]][,,'N_newID5'][,'Q5'],
    # S
    S = cestes_coefs[[1]][,,'S_newID5'][,'Estimate'],
    S_upper = cestes_coefs[[1]][,,'S_newID5'][,'Q95'],
    S_lower = cestes_coefs[[1]][,,'S_newID5'][,'Q5'],
    # Sn
    Sn = cestes_coefs[[1]][,,'S_newID5'][,'Estimate'],
    Sn_upper = cestes_coefs[[1]][,,'S_newID5'][,'Q95'],
    Sn_lower = cestes_coefs[[1]][,,'S_newID5'][,'Q5'],
    # ENSPIE
    ENSPIE = cestes_coefs[[1]][,,'ENSPIE_newID5'][,'Estimate'],
    ENSPIE_upper = cestes_coefs[[1]][,,'ENSPIE_newID5'][,'Q95'],
    ENSPIE_lower = cestes_coefs[[1]][,,'ENSPIE_newID5'][,'Q5']
  ),
  tibble(
    study = rownames(cestes_coefs[[1]]),
    newID = '6',
    # N
    N = cestes_coefs[[1]][,,'N_newID6'][,'Estimate'],
    N_upper = cestes_coefs[[1]][,,'N_newID6'][,'Q95'],
    N_lower = cestes_coefs[[1]][,,'N_newID6'][,'Q5'],
    # S
    S = cestes_coefs[[1]][,,'S_newID6'][,'Estimate'],
    S_upper = cestes_coefs[[1]][,,'S_newID6'][,'Q95'],
    S_lower = cestes_coefs[[1]][,,'S_newID6'][,'Q5'],
    # Sn
    Sn = cestes_coefs[[1]][,,'Sn_newID6'][,'Estimate'],
    Sn_upper = cestes_coefs[[1]][,,'Sn_newID6'][,'Q95'],
    Sn_lower = cestes_coefs[[1]][,,'Sn_newID6'][,'Q5'],
    # ENSPIE
    ENSPIE = cestes_coefs[[1]][,,'ENSPIE_newID6'][,'Estimate'],
    ENSPIE_upper = cestes_coefs[[1]][,,'ENSPIE_newID6'][,'Q95'],
    ENSPIE_lower = cestes_coefs[[1]][,,'ENSPIE_newID6'][,'Q5']
  )
)


cestes_coefs_multi4 <- left_join(cestes_coefs_multi4,
                                 meta %>% 
                                   mutate(study = dat) %>% 
                                   select(-dat, -X1)) %>% 
  mutate(db = 'CESTES')


levels <- m1_6sites$data %>% 
  as_tibble() %>% 
  distinct(dataset_id, newID) %>% 
  mutate(level = dataset_id,
         site = ifelse(newID==1, 'Intercept', paste0('newID', newID))) %>% 
  group_by(dataset_id) %>% 
  nest(data = c(level, site)) 

cestes_multi4_posterior <- levels %>%
  mutate(N_study = purrr::map(data, ~posterior_samples(m1_6sites, 
                                                       pars = paste('r_dataset_id__N[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         Sn_study = purrr::map(data, ~posterior_samples(m1_6sites, 
                                                        pars = paste('r_dataset_id__Sn[', as.character(.x$level), ',', 
                                                                     as.character(.x$site),']', sep=''),
                                                        fixed = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()),
         S_study = purrr::map(data, ~posterior_samples(m1_6sites, 
                                                       pars = paste('r_dataset_id__S[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       fixed = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         ENSPIE_study = purrr::map(data, ~posterior_samples(m1_6sites, 
                                                            pars = paste('r_dataset_id__ENSPIE[', as.character(.x$level), ',', 
                                                                         as.character(.x$site),']', sep=''),
                                                            fixed = TRUE,
                                                            # want 1000 samples
                                                            subset = seq(1, 4000, by = 4))
                                   %>% unlist() %>% as.numeric()))


cestes_multi4_posterior <- cestes_multi4_posterior %>% 
  # add indicator for database
  mutate(db = 'CESTES') %>% 
  select(-data) %>% 
  unnest(cols = c(N_study, S_study, ENSPIE_study, Sn_study))
# 
# 
# cestes_S_N <- ggplot() +
#   mapply(function(level) {
#     stat_ellipse(data = cestes_multi4_posterior %>% 
#                    filter(newID!='1'),
#                  aes(x = N_study,
#                      y = S_study,
#                      fill = 'S_N'),
#                  geom  = "polygon", type = "norm",
#                  size  = 0, alpha = .33,
#                  level = level)
#   }, 
#   # Enter the levels here
#   level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
#   geom_point(data = cestes_coefs_multi4,
#              aes(x = N, y = S#,
#                  # colour = Hemeroby_4
#                  )) +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_vline(xintercept = 0, lty = 2) +
#   scale_fill_manual(values = reln_colours,
#                     guide = FALSE) +
#   labs(y = expression(paste('Change in species richness [log(S)]', sep = '')),
#        x = expression(paste('Change in total abundance [log(N)]', sep = ''))#,
#        # tag = 'a'
#   ) +
#   theme(legend.position = c(1,0), 
#         legend.justification = c(1,0),
#         strip.text = element_text(hjust = 0))
# 
# cestes_S_Sn <- ggplot() +
#   mapply(function(level) {
#     stat_ellipse(data = cestes_multi4_posterior %>% 
#                    filter(newID!='1'),
#                  aes(x = Sn_study,
#                      y = S_study,
#                      fill = 'S_Sn'),
#                  geom  = "polygon", type = "norm",
#                  size  = 0, alpha = .33,
#                  level = level)
#   }, 
#   # Enter the levels here
#   level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
#   geom_point(data = cestes_coefs_multi4,
#              aes(x = Sn, y = S#,
#                  # colour = Hemeroby_4
#                  )) +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_vline(xintercept = 0, lty = 2) +
#   scale_fill_manual(values = reln_colours,
#                     guide = FALSE) +
#   labs(y = '',#expression(paste('Change in species richness [log(S)]', sep = '')),
#        x = expression(paste('Change in rarefied richness [log(',S[n],')]', sep = ''))#,
#        # tag = 'a'
#   ) +
#   theme(legend.position = c(1,0), 
#         legend.justification = c(1,0),
#         strip.text = element_text(hjust = 0))
# 
# cestes_S_S_PIE <- ggplot() +
#   mapply(function(level) {
#     stat_ellipse(data = cestes_multi4_posterior %>% 
#                    filter(newID!='1'),
#                  aes(x = ENSPIE_study,
#                      y = S_study,
#                      fill = 'S_S_PIE'),
#                  geom  = "polygon", type = "norm",
#                  size  = 0, alpha = .33,
#                  level = level)
#   }, 
#   # Enter the levels here
#   level = seq(from = 0.05, to = 0.95, by = 0.1)) +  
#   geom_point(data = cestes_coefs_multi4,
#              aes(x = ENSPIE, y = S#,
#                  # colour = Hemeroby_4
#                  )) +
#   geom_hline(yintercept = 0, lty = 2) +
#   geom_vline(xintercept = 0, lty = 2) +
#   scale_fill_manual(values = reln_colours,
#                     guide = FALSE) +
#   labs(y = '',#expression(paste('Change in species richness [log(S)]', sep = '')),
#        x = expression(paste('Change in evenness [log(',S[PIE],')]', sep = ''))#,
#        # tag = 'a'
#   ) +
#   theme(legend.position = c(1,0), 
#         legend.justification = c(1,0),
#         strip.text = element_text(hjust = 0))
# 
# plot_grid(cestes_S_N,
#           cestes_S_Sn,
#           cestes_S_S_PIE,
#           nrow = 1)
# ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/cestes_multi4_6sites.png',
#        width = 240, height = 80, units = 'mm')
