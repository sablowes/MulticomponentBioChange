# wrangle bt_space model
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_space_multi4_fit.Rdata')

bt_space_coefs <- coef(bt_space_multi4_fit, robust = TRUE, probs = c(0.05, 0.95))

bt_space_coefs_multi4 <- bind_rows(
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_2',
    # N
    N = bt_space_coefs['cell_yr'][[1]][,,'N_loclocation_2'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_2'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_2'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_2'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_2'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_2'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_2'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_2'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_2'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_2'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_2'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_2'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_3',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_3'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_3'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_3'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_3'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_3'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_3'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_3'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_3'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_3'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_3'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_3'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_3'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_4',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_4'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_4'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_4'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_4'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_4'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_4'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_4'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_4'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_4'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_4'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_4'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_4'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_5',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_5'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_5'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_5'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_5'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_5'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_5'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'S_loclocation_5'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'S_loclocation_5'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'S_loclocation_5'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_5'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_5'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_5'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_6',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_6'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_6'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_6'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_6'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_6'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_6'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_6'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_6'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_6'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_6'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_6'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_6'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_7',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_7'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_7'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_7'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_7'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_7'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_7'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_7'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_7'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_7'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_7'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_7'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_7'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_8',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_8'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_8'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_8'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_8'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_8'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_8'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_8'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_8'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_8'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_8'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_8'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_8'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_9',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_9'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_9'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_9'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_9'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_9'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_9'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_9'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_9'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_9'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_9'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_9'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_9'][,'Q5']
  ),
  tibble(
    cell_year = rownames(bt_space_coefs[[1]]),
    location = 's_10',
    # N
    N = bt_space_coefs[[1]][,,'N_loclocation_10'][,'Estimate'],
    N_upper = bt_space_coefs[[1]][,,'N_loclocation_10'][,'Q95'],
    N_lower = bt_space_coefs[[1]][,,'N_loclocation_10'][,'Q5'],
    # S
    S = bt_space_coefs[[1]][,,'S_loclocation_10'][,'Estimate'],
    S_upper = bt_space_coefs[[1]][,,'S_loclocation_10'][,'Q95'],
    S_lower = bt_space_coefs[[1]][,,'S_loclocation_10'][,'Q5'],
    # Sn
    Sn = bt_space_coefs[[1]][,,'Sn_loclocation_10'][,'Estimate'],
    Sn_upper = bt_space_coefs[[1]][,,'Sn_loclocation_10'][,'Q95'],
    Sn_lower = bt_space_coefs[[1]][,,'Sn_loclocation_10'][,'Q5'],
    # ENSPIE
    ENSPIE = bt_space_coefs[[1]][,,'ENSPIE_loclocation_10'][,'Estimate'],
    ENSPIE_upper = bt_space_coefs[[1]][,,'ENSPIE_loclocation_10'][,'Q95'],
    ENSPIE_lower = bt_space_coefs[[1]][,,'ENSPIE_loclocation_10'][,'Q5']
  )
) %>% 
  mutate(db = 'BioTIME (space)')


levels <- bt_space_multi4_fit$data %>% 
  as_tibble() %>% 
  distinct(cell_yr, loc) %>% 
  mutate(level = cell_yr,
         site = ifelse(loc=='location_1', 'Intercept', paste0('loc', loc))) %>% 
  group_by(cell_yr) %>% 
  nest(data = c(level, site)) 

bt_space_multi4_posterior <- levels %>%
  mutate(N_cell_year = purrr::map(data, ~posterior_samples(bt_space_multi4_fit, 
                                                       pars = paste('r_cell_yr__N[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       exact = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         Sn_cell_year = purrr::map(data, ~posterior_samples(bt_space_multi4_fit, 
                                                        pars = paste('r_cell_yr__Sn[', as.character(.x$level), ',', 
                                                                     as.character(.x$site),']', sep=''),
                                                        exact = TRUE,
                                                        # want 1000 samples
                                                        subset = seq(1, 4000, by = 4))
                               %>% unlist() %>% as.numeric()),
         S_cell_year = purrr::map(data, ~posterior_samples(bt_space_multi4_fit, 
                                                       pars = paste('r_cell_yr__S[', as.character(.x$level), ',', 
                                                                    as.character(.x$site),']', sep=''),
                                                       exact = TRUE,
                                                       # want 1000 samples
                                                       subset = seq(1, 4000, by = 4))
                              %>% unlist() %>% as.numeric()),
         ENSPIE_cell_year = purrr::map(data, ~posterior_samples(bt_space_multi4_fit, 
                                                            pars = paste('r_cell_yr__ENSPIE[', as.character(.x$level), ',', 
                                                                         as.character(.x$site),']', sep=''),
                                                            exact = TRUE,
                                                            # want 1000 samples
                                                            subset = seq(1, 4000, by = 4))
                                   %>% unlist() %>% as.numeric()))


bt_space_multi4_posterior <- bt_space_multi4_posterior %>% 
  # add indicator for database
  mutate(db = 'BioTIME (space)') %>%  
  select(-data) %>% 
  unnest(cols = c(N_cell_year, S_cell_year, ENSPIE_cell_year, Sn_cell_year))

# bt_space_S_N <- ggplot() +
#   mapply(function(level) {
#     stat_ellipse(data = bt_space_multi4_posterior %>%
#                    filter(loc!='location_1'),
#                  aes(x = N_cell_year,
#                      y = S_cell_year,
#                      fill = 'S_N'),
#                  geom  = "polygon", type = "norm",
#                  size  = 0, alpha = .33,
#                  level = level)
#   },
#   # Enter the levels here
#   level = seq(from = 0.05, to = 0.95, by = 0.1)) +
#   geom_point(data = bt_space_coefs_multi4,
#              aes(x = N, y = S#,
#                  # colour = Hemeroby_4
#              )) +
#   geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
#   geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
#   geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
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
# bt_space_S_Sn <- ggplot() +
#   mapply(function(level) {
#     stat_ellipse(data = bt_space_multi4_posterior %>%
#                    filter(loc!='location_1'),
#                  aes(x = Sn_cell_year,
#                      y = S_cell_year,
#                      fill = 'S_Sn'),
#                  geom  = "polygon", type = "norm",
#                  size  = 0, alpha = .33,
#                  level = level)
#   },
#   # Enter the levels here
#   level = seq(from = 0.05, to = 0.95, by = 0.1)) +
#   geom_point(data = bt_space_coefs_multi4,
#              aes(x = Sn, y = S#,
#                  # colour = Hemeroby_4
#              )) +
#   geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
#   geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
#   geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
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
# bt_space_S_S_PIE <- ggplot() +
#   mapply(function(level) {
#     stat_ellipse(data = bt_space_multi4_posterior %>%
#                    filter(loc!='location_1'),
#                  aes(x = ENSPIE_cell_year,
#                      y = S_cell_year,
#                      fill = 'S_S_PIE'),
#                  geom  = "polygon", type = "norm",
#                  size  = 0, alpha = .33,
#                  level = level)
#   },
#   # Enter the levels here
#   level = seq(from = 0.05, to = 0.95, by = 0.1)) +
#   geom_point(data = bt_space_coefs_multi4,
#              aes(x = ENSPIE, y = S#,
#                  # colour = Hemeroby_4
#              )) +
#   geom_vline(xintercept = 0, lty = 2, colour = '#bdbdbd') +
#   geom_hline(yintercept = 0, lty = 2, colour = '#bdbdbd') +
#   geom_abline(intercept = 0, slope = 1, lty = 2, colour = '#bdbdbd') +
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
# cowplot::plot_grid(bt_space_S_N,
#           bt_space_S_Sn,
#           bt_space_S_S_PIE,
#           nrow = 1)
# 
# ggsave('~/Dropbox/1current/multidimensionalChangeMS/Figs/bt_space_multi4_10sites.png',
# width = 240, height = 80, units = 'mm')
# 
# check <- bt_space_coefs_multi4 %>% 
#   separate(cell_year, c('STUDY_ID', 'cell', 'YEAR'), remove = F) %>% 
#   filter(abs(N) > 1) %>% 
#   distinct(cell_year, STUDY_ID, YEAR)
# 
# bt_space_coefs_multi4 %>% 
#   separate(cell_year, c('STUDY_ID', 'cell', 'YEAR'), remove = F) %>% 
#   filter(cell_year %in% check$cell_year) 
# 
# meta %>% 
#   filter(STUDY_ID %in% check$STUDY_ID) %>% 
#   select(TITLE, TAXA)
