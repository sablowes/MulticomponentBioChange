Characterising multicomponent biodiversity change
================
Shane Blowes

This repository includes R code to reproduce the analyses shown in the article:

**Local biodiversity change reflects interactions among changing abundance, evenness and richness**

*by Shane A. Blowes, Gergana N. Daskalova, Maria Dornelas, Thore Engel, Nicholas J. Gotelli, Anne Magurran, Inês S. Martins, Brian McGill, Daniel J. McGlinn, Alban Sagouis, Hideyasu Shimadzu, Sarah R. Supp, and Jonathan M. Chase*

Here we give a brief overview on the code and data files in this repository.

## Data

Files in the data folder include:

**conceptual\_communities.Rdata**: simulated data to reproduce Figure 1.

**conceptual\_figure\_dat.Rdata**: simulated data to reproduce Figure 2.

**rarefied_medians_count.Rdata**: data for *Temporal changes: natural environmental variation*; this was prepared with code available here: https://github.com/sChange-workshop/BioGeo-BioDiv-Change.

**long\_table.csv**: data for *Temporal changes: experimental or natural perturbations*. This data was prepared for analysis by Alban Sagouis using code available in folder temporal_change_data_preparation: <https://github.com/sablowes/MulticomponentBioChange/temporal_change_data_preparation>

**PREDICTS\_alpha.Rdata**: data for *Spatial comparison: anthropogenic perturbations*; this is the output of 01\_PREDICTS\_data\_wrangle.R

**spatial\_natural.Rdata**: data for *Spatial comparison: natural environmental variation*; this is the output of 01a\_CESTES\_mcgill\_combine.R

## Code

Files in the r\_scripts folder include

00\_: script to load packages and set working directory.

01\_: scripts to prepare data for analyses. BioTIME data were prepared using code available here: <https://github.com/sChange-workshop/BioGeo-BioDiv-Change>. Perturbed time series data were compiled and standardised with code written by Alban Sagouis available here: <https://github.com/chase-lab/BioTIMEx>.

02\_: scripts to fit models to the data. Some were written to run on a scientific computing cluster.

03\_: script to do posterior predictive checks.

04\_: scripts to wrangle posteriors for plotting results.

05\_: scripts to present results: figures and supplemental table.

200\_: script to generate data for conceptual figure.  

Recent versions of packages `rstan` and `StanHeaders` might need to be installed:  
```
# run the next line if you already have rstan installed
# remove.packages(c("StanHeaders", "rstan"))

install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
remotes::install_github("stan-dev/rstan/StanHeaders@StanHeaders_2.26")

```

## Results

Files in the results folder include:

**bt\_multi4\_pois3\_lnorm\_results.Rdata**: output of 04\_bt\_multi4\_posterior\_wrangle.R

**btx\_multi4\_global\_results.Rdata**: output of 04\_btx\_global\_multi4\_posterior\_wrangle.R

**predicts\_multi4\_results.Rdata**: output of 04\_PREDICTS\_multi\_coef\_wrangle.R

**spatial\_multi4\_results.Rdata**: output of 04\_spat\_natural\_multi4\_posterior\_wrangle.R  
