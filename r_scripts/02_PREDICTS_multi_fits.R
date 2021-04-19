##======================================================================
##  Multivariate response hierarchical models fit to count data (N, PIE, etc)
##  this allows for study-level correlation in the response variables
##======================================================================
rm(list=ls())

##	
#withr::with_libpaths(new = '/home/blowes/R/x86_64-pc-linux-gnu-library/3.3/', library(brms))
library(brms)
library(dplyr)
library(tidyr)

## the data
load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/data/PREDICTS_alpha.Rdata')
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

PREDICTS_multi4 <- brm(S_formula + N_formula +
                         ENSPIE_formula + Sn_formula +
                         set_rescor(FALSE),
	data = alpha %>% 
	  filter(!is.na(Sn_5)),
	cores = 4,
	chains = 4)

save(PREDICTS_multi4, 
     file = '~/Dropbox/1current/BioTime/multidimensionalChangeMS/data_model_fits/PREDICTS_multi4.Rdata')#file=Sys.getenv('OFILE')
