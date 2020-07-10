##======================================================================
##  Multivariate response hierarchical models fit to count data (N, PIE, etc)
##  this forces study-level correlation in the response variables
##======================================================================
rm(list=ls())

##	
#withr::with_libpaths(new = '/home/blowes/R/x86_64-pc-linux-gnu-library/3.3/', library(brms))
library(brms)
library(dplyr)
library(tidyr)

## the data
load('~/Dropbox/1current/multidimensionalChangeMS/data_model_fits/PREDICTS_alpha.Rdata')
# load('/data/idiv_chase/sablowes/multiC_alpha/resamps/PREDICTS_alpha.Rdata')
##======================================================================		
##	use default weakly regularising priors...
# set up formulas for each response
S_formula <- bf(S ~ LU + (LU | p | SS) + (1| SSB / SSBS),
                family = 'poisson')

N_formula <- bf(N_corrected ~ LU + (LU | p | SS) + (1| SSB / SSBS)) +
  lognormal()

ENSPIE_formula <- bf(ENSPIE ~ LU + (LU | p | SS) + (1| SSB / SSBS)) +
  lognormal()

Sn_formula <- bf(Sn_5 ~ LU + (LU | p | SS) + (1| SSB / SSBS)) +
  lognormal()

S_N_PREDICTS_multi2 <- brm(S_formula + N_formula +
                    set_rescor(FALSE),
	data= alpha %>% 
	  filter(!is.na(Sn_5)),
	cores = 4,
	chains = 4)

PREDICTS_S_N_fit <- S_N_PREDICTS_multi2
rm(S_N_PREDICTS_multi2)

PREDICTS_S_S_PIE_fit <- brm(S_formula + ENSPIE_formula +
                             set_rescor(FALSE),
                           data= alpha %>% 
                             filter(!is.na(Sn_5)),
                           cores = 4,
                           chains = 4)

PREDICTS_S_Sn_fit <- brm(S_formula + Sn_formula +
                             set_rescor(FALSE),
                           data= alpha %>% 
                             filter(!is.na(Sn_5)),
                           cores = 4,
                           chains = 4)

PREDICTS_S_PIE_Sn_fit <- brm(ENSPIE_formula + Sn_formula +
                             set_rescor(FALSE),
                           data= alpha %>% 
                             filter(!is.na(Sn_5)),
                           cores = 4,
                           chains = 4)

PREDICTS_N_S_PIE_fit <- brm(N_formula + ENSPIE_formula +
                             set_rescor(FALSE),
                           data= alpha %>% 
                             filter(!is.na(Sn_5)),
                           # prior=hier_prior,
                           # inits = '0',
                           #init_r = 0.001,
                           # control = list(adapt_delta=0.95),
                           cores = 4,
                           chains = 4)

PREDICTS_N_Sn_fit <- brm(N_formula + Sn_formula +
                             set_rescor(FALSE),
                           data= alpha %>% 
                             filter(!is.na(Sn_5)),
                           # prior=hier_prior,
                           # inits = '0',
                           #init_r = 0.001,
                           # control = list(adapt_delta=0.95),
                           cores = 4,
                           chains = 4)

save(S_N_PREDICTS_multi2, 
     file = '~/Dropbox/1current/BioTime/multidimensionalChangeMS/data_model_fits/PREDICTS_S_N_multi2.Rdata')#file=Sys.getenv('OFILE')
