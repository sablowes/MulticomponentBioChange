# ordination to look for patterns of covariation with change estimates

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/0_init_dirs_load_packages.R')

load('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/results/bt_multi4_pois3_lnorm_results.Rdata')

# for PCA with 'mixed' data: continuous and categorical covariates
# see vignette: https://cran.r-project.org/web/packages/PCAmixdata/vignettes/PCAmixdata.html
library(PCAmixdata)

# simplify posteriors for PCA
bt_pca_dat <- bt_study_sample_posterior %>% 
  mutate(N = N_global + N_study,
         S = S_global + S_study,
         ENSPIE = ENSPIE_global + ENSPIE_study,
         Sn = Sn_global + Sn_study) %>% 
  select(N, S, Sn, ENSPIE, REALM, taxa_mod2)
# alternatively, use the medians of the posteriors
bt_pca_dat2 <- bt_study_summary %>% 
  mutate(N = N_slope,
         S = S_slope,
         ENSPIE = ENSPIE_slope,
         Sn = Sn_slope) %>% 
  select(N, S, Sn, ENSPIE, REALM, taxa_mod2)

bt_split <- splitmix(bt_pca_dat2)
bt_x1 <- bt_split$X.quanti
bt_x2 <- bt_split$X.quali
bt_pca <- PCAmix(X.quanti = bt_x1,
                 X.quali = bt_x2,
                 rename.level = TRUE,
                 graph = FALSE)


plot(bt_pca,
     choice="ind",
     coloring.ind=bt_x2$taxa_mod2,
     label=FALSE,
     posleg="bottomright", main="Observations")

plot(bt_pca,
     choice="levels",
     label=TRUE,
      main="Levels")

plot(bt_pca,
     choice="cor",
     label=TRUE,
     main="Levels")

plot(bt_pca,
     choice="sqload",
     coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_PREDICTS_multi_coef_wrangle.R')
#use the medians of the posteriors
PREDICTS_pca_dat2 <- SS_LU_multi %>% 
  select(N, S, Sn, S_PIE, 
         LU)

PREDICTS_split <- splitmix(PREDICTS_pca_dat2)
PREDICTS_x1 <- PREDICTS_split$X.quanti
PREDICTS_x2 <- PREDICTS_split$X.quali
PREDICTS_pca <- PCAmix(X.quanti = PREDICTS_x1,
                 X.quali = PREDICTS_x2,
                 rename.level = TRUE,
                 graph = FALSE)


plot(PREDICTS_pca,
     choice="ind",
     coloring.ind=PREDICTS_x2$LU,
     label=FALSE,
     posleg="bottomright", main="Observations")

plot(PREDICTS_pca,
     choice="levels",
     label=TRUE,
     main="Levels")

plot(PREDICTS_pca,
     choice="cor",
     label=TRUE,
     main="Levels")

plot(PREDICTS_pca,
     choice="sqload",
     coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_fragSAD_multi4_posterior_wrangle.R')

fragSAD_meta <- read_delim('~/Dropbox/1current/fragmentation_synthesis/FragFrame_1/data/new_meta_2_merge.csv',  delim =';') %>% 
  dplyr::rename(dataset_label = dataset_id) %>% 
  select(dataset_label,
         climate, taxa, time.since.fragmentation, Matrix.category)

# study-level estimates
fSAD_pca_dat2 <- fragSAD_study_summary %>% 
  mutate(N = N_slope,
         S = S_slope,
         ENSPIE = ENSPIE_slope,
         Sn = Sn_slope) %>% 
  select(dataset_label, N, S, Sn, ENSPIE) %>% 
  left_join(fragSAD_meta,
            by = 'dataset_label') %>% 
  select(-dataset_label)

fSAD_split <- splitmix(fSAD_pca_dat2)
fSAD_x1 <- fSAD_split$X.quanti
fSAD_x2 <- fSAD_split$X.quali
fSAD_pca <- PCAmix(X.quanti = fSAD_x1,
                 X.quali = fSAD_x2,
                 rename.level = TRUE,
                 graph = FALSE)


plot(fSAD_pca,
     choice="ind",
     coloring.ind=fSAD_x2$Matrix.category,
     label=FALSE,
     posleg="bottomright", main="Observations")

plot(fSAD_pca,
     choice="levels",
     label=TRUE,
     main="Levels")

plot(fSAD_pca,
     choice="cor",
     label=TRUE,
     main="Levels")

plot(fSAD_pca,
     choice="sqload",
     coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_fwater_multi4_wrangle.R')
fwater_pca_dat2 <- fwater_study_LU_multi %>% 
  select(N, S, Sn, S_PIE, 
         LU)

fwater_split <- splitmix(fwater_pca_dat2)
fwater_x1 <- fwater_split$X.quanti
fwater_x2 <- fwater_split$X.quali
fwater_pca <- PCAmix(X.quanti = fwater_x1,
                       X.quali = fwater_x2,
                       rename.level = TRUE,
                       graph = FALSE)
fwater_pca$eig


plot(fwater_pca,
     choice="ind",
     coloring.ind=fwater_x2$LU,
     label=FALSE,
     posleg="bottomright", main="Observations")

plot(fwater_pca,
     choice="levels",
     label=TRUE,
     main="Levels")

plot(fwater_pca,
     choice="cor",
     label=TRUE,
     main="Levels")

plot(fwater_pca,
     choice="sqload",
     coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_supp_multi4_posterior_wrangle.R')
exp <- read_csv(paste0(path2wd, 'data/experiments_analysis_data.csv'))
supp_pca_dat2 <- supp_trt_summary %>% 
  mutate(N = N_slope,
         S = S_slope,
         ENSPIE = ENSPIE_slope,
         Sn = Sn_slope) %>% 
  select(group, N, S, Sn, ENSPIE) %>% 
  separate(group, into = c('referenceID', 'siteID', 'site2'), remove = FALSE) %>% 
  left_join(exp %>% 
              mutate(siteID = as.character(siteID)) %>% 
              select(referenceID, siteID, taxa, habitat),
            by = c('referenceID', 'siteID')) %>% 
  # drop things not required for pca
  select(-referenceID, -group, -siteID, -site2)
  

supp_split <- splitmix(supp_pca_dat2)
supp_x1 <- supp_split$X.quanti
supp_x2 <- supp_split$X.quali
supp_pca <- PCAmix(X.quanti = supp_x1,
                     X.quali = supp_x2,
                     rename.level = TRUE,
                     graph = FALSE)
supp_pca$eig


plot(supp_pca,
     choice="ind",
     coloring.ind=supp_x2$taxa,
     label=FALSE,
     posleg="bottomright", main="Observations")

plot(supp_pca,
     choice="levels",
     label=TRUE,
     main="Levels")

plot(supp_pca,
     choice="cor",
     label=TRUE,
     main="Levels")

plot(supp_pca,
     choice="sqload",
     coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")

source('~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/r_scripts/5_btx_multi4_posterior_wrangle.R')
btx_pca_dat <- btx_study_summary %>% 
  mutate(N = N_slope,
         S = S_slope,
         ENSPIE = ENSPIE_slope,
         Sn = Sn_slope) %>% 
  select(study_trt, N, S, Sn, ENSPIE) %>% 
  