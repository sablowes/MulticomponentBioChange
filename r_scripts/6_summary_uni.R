# combined plots (biotime and predicts) for changes estimated
# with univariate models

rm(list=ls())

source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/bt_uni_coef_wrangle.R')
source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/PREDICTS_uni_coef_wrangle.R')
source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/fwater_uni_wrangle.R')
source('~/Dropbox/1current/BioTime/multidimensionalChangeMS/code/fragSAD_coef_wrangle.R')

# colour scheme for 4 databases
db_colour <- c('PREDICTS' = '#1b9e77',
               'BioTIME' = '#d95f02',
               'fragSAD' = '#7570b3',
               'Freshwater LU' = '#e7298a')

db_shape <- c('PREDICTS' = 20,
              'BioTIME' = 15,
              'fragSAD' = 3,
              'Freshwater LU' = 19)

ggplot() +
  facet_wrap(~factor(db, levels = c('PREDICTS', 'BioTIME', 'Freshwater LU', 'fragSAD')), ncol = 2)+#, scales = 'free_x') +
  geom_bar(data = Sn_study_corr , 
           aes(x = componentChange, 
               y =(..count..)/sum(..count..),
               fill = componentChange, colour = componentChange),
           # stat = stat('prop')
  ) +
  geom_text(data = Sn_study_corr %>% 
              summarise(n_study = n_distinct(STUDY_ID)) %>% 
              mutate(db = 'BioTIME'),
            aes(y = 0.6, x = 13, 
                label=paste('n[study] == ', n_study)),
            parse = T) +
  geom_bar(data = SS_LU_univariate , 
           aes(x = componentChange, 
               y = (..count..)/sum(..count..),
               fill = componentChange, colour = componentChange),
           # stat = 'count'
  ) +
  geom_text(data = SS_LU_univariate %>% 
              summarise(n_study = n_distinct(SS)) %>% 
              mutate(db = 'PREDICTS'),
            aes(y = 0.6, x = 13, 
                label=paste('n[study] == ', n_study)),
            parse = T)  +
  geom_bar(data = fSAD_uni_coefs , 
           aes(x = componentChange, 
               y = (..count..)/sum(..count..),
               fill = componentChange, colour = componentChange),
           # stat = 'count'
  ) +
  geom_text(data = fSAD_uni_coefs %>% 
                summarise(n_study = n_distinct(Study)) %>% 
                mutate(db = 'fragSAD'),
              aes(y = 0.6, x = 13, 
                  label=paste('n[study] == ', n_study)),
              parse = T) +
  geom_bar(data = fwater_study_LU_uni, 
           aes(x = componentChange, 
               y = (..count..)/sum(..count..),
               fill = componentChange, colour = componentChange),
           # stat = 'count'
  ) +
  geom_text(data = fwater_study_LU_uni %>% 
                summarise(n_study = n_distinct(Study)) %>% 
                mutate(db = 'Freshwater LU'),
              aes(y = 0.6, x = 13, 
                  label=paste('n[study] == ', n_study)),
              parse = T) +  
  labs(x = 'Component(s) with high (>95%) probability of change',
       y = 'Percentage of studies') +
  scale_x_discrete(breaks = c('No change',
                              'N only', 'ENSPIE only', 'Sn only', 'S only',
                              'S & Sn', 'S & ENSPIE', 'S & N',
                              'Sn & ENSPIE', 'Sn & N',
                              'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                              'S, Sn, N & ENSPIE'),
                   labels = c('None',
                              'N', expression(paste(S[PIE])), expression(paste(S[n])), 'S',
                              expression(paste('S & ', S[n])), expression(paste('S & ', S[PIE])), 'S & N',
                              expression(paste(S[n], ' & ', S[PIE])), expression(paste(S[n], ' & N')),
                              expression(paste('N & ', S[PIE])), expression(paste(S[n], ', N & ', S[PIE])), expression(paste('S, N & ', S[PIE])),
                              expression(paste('S, ', S[n], ', N & ', S[PIE])))
  ) +
  scale_colour_manual(values = componentChange_col, guide = F) +
  scale_fill_manual(values = componentChange_col, guide = F) +
  coord_flip() +
  theme_minimal() +
  theme(strip.text = element_text(size = 16, face = 'bold', hjust = 0),
        strip.background = element_blank(),
        axis.text = element_text(size = 16), axis.title = element_text(size = 16),
        # panel.grid = element_blank(),
        plot.margin = margin(10,12,1,1))

ggsave('~/Dropbox/1current/BioTime/multidimensionalChangeMS/Figs/summary_uni_change.png',
       width = 290, height = 200, units = 'mm')

