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
# load('~/Dropbox/1current/BioTime/multidimensionalChangeMS/data_model_fits/rarefied_medians_count.Rdata')
load('/data/idiv_chase/sablowes/multiC_alpha/resamps/rarefied_medians_count.Rdata')
rarefied_medians_count <- rarefied_medians_count %>% 
  filter(STUDY_ID!=70)
##======================================================================		
##	set some weakly regularising priors...
hier_prior <- c(set_prior(prior = 'normal(0,0.2)', class='b', coef='cYEAR',resp = 'S'), 	# global slope
                set_prior(prior = 'normal(0,0.2)', class='b', coef='cYEAR',resp = 'N'),
                set_prior(prior = 'normal(0,0.2)', class='b', coef='cYEAR',resp = 'ENSPIE'),
                set_prior(prior = 'normal(0,0.2)', class='b', coef='cYEAR',resp = 'Sn'),
                set_prior(prior = 'normal(0,1)', class='Intercept', coef='', resp = 'S'),
                set_prior(prior = 'normal(0,1)', class='Intercept', coef='', resp = 'N'),
                set_prior(prior = 'normal(0,1)', class='Intercept', coef='', resp = 'ENSPIE'),
                set_prior(prior = 'normal(0,1)', class='Intercept', coef='', resp = 'Sn'),
                set_prior(prior = 'lkj(1)', class='cor')
)

# set up formulas for each response
S_formula <- bf(S ~ cYEAR + (cYEAR | p | STUDY_ID) + (cYEAR | q | rarefyID),
                family = brmsfamily('poisson'))

N_formula <- bf(N ~ cYEAR + (cYEAR | p | STUDY_ID) + (cYEAR | q | rarefyID)) +
  lognormal()
  
ENSPIE_formula <- bf(ENSPIE ~ cYEAR + (cYEAR | p | STUDY_ID) + (cYEAR | q | rarefyID),
			family = brmsfamily('poisson'))

Sn_formula <- bf(Sn ~ cYEAR + (cYEAR | p | STUDY_ID) + (cYEAR | q | rarefyID),
	family = brmsfamily('poisson'))

multi4_fit <- brm(S_formula + N_formula + ENSPIE_formula + Sn_formula +
                    set_rescor(FALSE),
	data= rarefied_medians_count %>% 
	  filter(minN > 5) %>%
	  mutate(ENSPIE = round(ENSPIE),
		 Sn = round(Sn)),
	prior=hier_prior,
	# inits = '0',
	init_r = 0.01,
	control = list(adapt_delta=0.95),
	cores = 4,
	chains = 4)

save(multi4_fit, file=Sys.getenv('OFILE'))
