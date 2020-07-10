# summary of component changes (with covariation)

# combined plot of models fit to BioTIME and PREDICTS
load('~/Dropbox/1current/BioTime/multidimensionalChangeMS/data_model_fits/bt_multi4_pois3_lnorm_results.Rdata')
source('~/Dropbox/1current/BioTIME/multidimensionalChangeMS/code/PREDICTS_multi_coef_wrangle.R')
source('~/Dropbox/1current/BioTIME/multidimensionalChangeMS/code/fragSAD_multi4_posterior_wrangle.R')
source('~/Dropbox/1current/BioTIME/multidimensionalChangeMS/code/fwater_multi4_wrangle.R')

# colour scheme for PREDICTS and BIOTIME
db_colour <- c('PREDICTS' = '#1b9e77',
               'BioTIME' = '#d95f02',
               'fragSAD' = '#7570b3',
               'Freshwater LU' = '#e7298a')

db_shape <- c('PREDICTS' = 20,
              'BioTIME' = 15,
              'fragSAD' = 3,
              'Freshwater LU' = 19)


ggplot() +
  facet_wrap(~factor(db, levels = c('PREDICTS', 'BioTIME', 'Freshwater LU', 'fragSAD')),
             ncol = 2)+
  geom_bar(data = SS_LU_multi,
           aes(x = componentChange,
               y = (..count..)/sum(..count..),
               fill = componentChange, colour = componentChange)) +
  geom_bar(data = bt_study_summary ,
                 aes(x = componentChange,
                     y =(..count..)/sum(..count..),
                     fill = componentChange, colour = componentChange)
                 ) +
  
  geom_bar(data = fragSAD_study_summary , 
                 aes(x = componentChange, 
                     y = (..count..)/sum(..count..),
                     fill = componentChange, colour = componentChange)) +
  geom_bar(data = fwater_study_LU_multi, 
                 aes(x = componentChange, 
                     y = (..count..)/sum(..count..),
                     fill = componentChange, colour = componentChange)) +
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


ggsave('~/Dropbox/1current/BioTime/multidimensionalChangeMS/Figs/summary_multi_change.png',
       width = 290, height = 200, units = 'mm')

