# coef wrangle for univariate models fit to BioTIME

load(paste0(path2wd, 'results/bt_alpha_coefs_uni.Rdata'))
load(paste0(path2wd, 'results/bt_beta_coefs_uni.Rdata'))

##----- get raw data and process for metadata--------
load('~/Dropbox/BiogeoBioTIME/rarefied_medians.Rdata')  

cell_count <- rarefied_medians %>%
  group_by(STUDY_ID) %>% 
  dplyr::summarise(n_cells = n_distinct(rarefyID)) %>%
  ungroup() 

##	rejoin
rarefied_medians <- left_join(cell_count, rarefied_medians, by=c('STUDY_ID'))

##	filter to count data and biome/taxa combinations with >3 cells
# these are the data that the model was fit to
rarefied_medians <- rarefied_medians %>%
  filter(BROAD_TYPE=='count') %>%
  # simplify realm covariate for plotting
  mutate(Realm2 = ifelse(REALM!='Marine', 'Terrestrial/Freshwater', 'Marine'))

##-------wrangle global level estimates-----------
studyOnly_global <- alpha_global_studyOnly %>% 
  mutate(term = rep(c('Intercept', 'Slope'), times = 4))

studyOnly_global$model_clean <- studyOnly_global$model
studyOnly_global$model_clean <- factor(studyOnly_global$model_clean, 
                                       levels = c('S_pois', 'ENSPIE_lnorm', 'N_lnorm', 'Sn_5_lnorm'),
                                       labels = c('Species richness', 'S_PIE', 'Total abundance', 'Rarefied richness'))

##------------wrangle study-level coef-------------
# NB: 46 studies have multiple biomes
studyOnly <- alpha_study_studyOnly %>% 
  # now join with the realm meta data
  left_join(rarefied_medians %>% distinct(REALM, taxa_mod, STUDY_ID) %>% 
              mutate(STUDY_ID = as.character(STUDY_ID)),
            by = c('STUDY_ID')) 
# NB: horn is similarity (not dissimilarity)
beta_studyOnly <- beta_study_studyOnly %>%
  # now join with the realm meta data
  left_join(rarefied_medians %>% distinct(REALM, taxa_mod, STUDY_ID) %>%
              mutate(STUDY_ID = as.character(STUDY_ID)),
            by = c('STUDY_ID'))

studyOnly <- bind_rows(studyOnly, beta_studyOnly)

meta <- rarefied_medians %>% distinct(REALM, Realm2, taxa_mod, STUDY_ID)

##-want study-level estimates-

S_PIE_study <- studyOnly %>%
  # remove duplicate entries created when adding meta data
  distinct(model,  taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'ENSPIE_lnorm') %>%
  mutate(deltaENSPIE = Estimate.cYEAR,
         deltaENSPIE_lower = lower_slope,
         deltaENSPIE_upper = upper_slope) %>% 
  select(STUDY_ID, deltaENSPIE, deltaENSPIE_lower, deltaENSPIE_upper)

N_study <- studyOnly %>%
  # remove duplicate entries created when adding meta data
  distinct(model,  taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'N_lnorm') %>%
  mutate(deltaN = Estimate.cYEAR,
         deltaN_lower = lower_slope,
         deltaN_upper = upper_slope) %>%
  select(STUDY_ID, deltaN, deltaN_lower, deltaN_upper) 

S_study <- studyOnly %>%
  # remove duplicate entries created when adding meta data
  distinct(model,  taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'S_pois' & time_period=='ALL') %>%
  mutate(deltaS = Estimate.cYEAR,
         deltaS_lower = lower_slope,
         deltaS_upper = upper_slope) %>%
  select(STUDY_ID, deltaS, deltaS_lower, deltaS_upper) 

Sn_study <- studyOnly %>%
  # remove duplicate entries created when adding meta data
  distinct(model,  taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Sn_5_lnorm' & time_period=='ALL') %>%
  mutate(deltaSn = Estimate.cYEAR,
         deltaSn_lower = lower_slope,
         deltaSn_upper = upper_slope) %>%
  select(STUDY_ID, deltaSn, deltaSn_lower, deltaSn_upper) 

Jtu_study <- studyOnly %>%
  # remove duplicate entries created when adding meta data
  distinct(model,  taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Jtu_norm' & time_period=='ALL') %>%
  mutate(deltaJtu = Estimate.cYEAR,
         deltaJtu_lower = lower_slope,
         deltaJtu_upper = upper_slope) %>%
  select(STUDY_ID, deltaJtu, deltaJtu_lower, deltaJtu_upper) 

Horn_study <- studyOnly %>%
  # remove duplicate entries created when adding meta data
  distinct(model,  taxa_mod, Estimate.cYEAR, .keep_all = T) %>%
  filter(model == 'Horn_norm' & time_period=='ALL') %>%
  mutate(deltaHorn = Estimate.cYEAR,
         deltaHorn_lower = lower_slope,
         deltaHorn_upper = upper_slope) %>%
  select(STUDY_ID, deltaHorn, deltaHorn_lower, deltaHorn_upper) 

study_corr <- left_join(N_study,
                        S_PIE_study,
                        by = 'STUDY_ID') %>% 
  left_join(S_study,
            by = 'STUDY_ID') %>% 
  left_join(Jtu_study,
            by = 'STUDY_ID') %>% 
  left_join(Horn_study,
            by = 'STUDY_ID')

# Sn contains slightly different studies (e.g., years with N > 5)
Sn_study_corr <- inner_join(study_corr, Sn_study,
                            by = 'STUDY_ID') %>% 
  left_join(meta %>% mutate(STUDY_ID = as.character(STUDY_ID)),
            by = 'STUDY_ID')


# put the meta data in
study_corr <- left_join(study_corr, 
                        meta %>% mutate(STUDY_ID = as.character(STUDY_ID)),
                        by = 'STUDY_ID')

# add another indicator for linking change in different components 
study_corr <- study_corr %>%
  mutate(# conceptual model of change in alpha diversity: N
    N_quad = ifelse((deltaN < 0 & deltaS > 0), 'n1',
                    ifelse((deltaN > 0 & deltaS > 0), 'n2',
                           ifelse((deltaN > 0 & deltaS < 0), 'n3', 'n4'))),
    N_quad_sig = ifelse((N_quad=='n1' & deltaN_upper < 0 & deltaS_lower > 0), 'n1_sig',
                        ifelse((N_quad=='n2' & deltaN_lower > 0 & deltaS_lower > 0), 'n2_sig',
                               ifelse((N_quad=='n3' & deltaN_lower > 0 & deltaS_upper < 0), 'n3_sig',
                                      ifelse((N_quad=='n4' & deltaN_upper < 0 & deltaS_upper < 0), 'n4_sig', N_quad)))),
    # conceptual model of change in alpha diversity: ENSPIE
    ENSPIE_quad = ifelse((deltaENSPIE < 0 & deltaS > 0), 'e1',
                         ifelse((deltaENSPIE > 0 & deltaS > 0), 'e2',
                                ifelse((deltaENSPIE > 0 & deltaS < 0), 'e3', 'e4'))),
    ENSPIE_quad_sig = ifelse((ENSPIE_quad=='e1' & deltaENSPIE_upper < 0 & deltaS_lower > 0), 'e1_sig',
                             ifelse((ENSPIE_quad=='e2' & deltaENSPIE_lower > 0 & deltaS_lower > 0), 'e2_sig',
                                    ifelse((ENSPIE_quad=='e3' & deltaENSPIE_lower > 0 & deltaS_upper < 0), 'e3_sig',
                                           ifelse((ENSPIE_quad=='e4' & deltaENSPIE_upper < 0 & deltaS_upper < 0), 'e4_sig', ENSPIE_quad)))))



# create some covariates for plotting whether changes differ from zero
study_corr <- study_corr %>%
  mutate(alphaS = ifelse((deltaS_lower < 0 & deltaS_upper > 0), 0.95, 1),
         alphaN = ifelse((deltaN_lower < 0 & deltaN_upper > 0), 0.95, 1),
         alphaENSPIE = ifelse((deltaENSPIE_lower < 0 & deltaENSPIE_upper > 0), 0.95, 1))

Sn_study_corr <- Sn_study_corr %>%
  mutate(alphaS = ifelse((deltaS_lower < 0 & deltaS_upper > 0), 0.95, 1),
         alphaN = ifelse((deltaN_lower < 0 & deltaN_upper > 0), 0.95, 1),
         alphaENSPIE = ifelse((deltaENSPIE_lower < 0 & deltaENSPIE_upper > 0), 0.95, 1),
         alphaSn = ifelse((deltaSn_lower < 0 & deltaSn_upper > 0), 0.95, 1))

# want to combine the quadrants for N and ENSPIE
study_corr <- study_corr %>%
  unite(N_ENSPIE_quad, c(N_quad, ENSPIE_quad), sep = '_', remove = FALSE) %>%
  # add add indicator if changes differ from zero
  mutate(N_ENSPIE_quad_sig = ifelse((N_ENSPIE_quad=='n1_e1' & N_quad_sig=='n1_sig' & ENSPIE_quad_sig=='e1_sig'), 'n1_e1_sig',
                                    ifelse((N_ENSPIE_quad=='n1_e2' & N_quad_sig=='n1_sig' & ENSPIE_quad_sig=='e2_sig'), 'n1_e2_sig',
                                           ifelse((N_ENSPIE_quad=='n2_e1' & N_quad_sig=='n2_sig' & ENSPIE_quad_sig=='e1_sig'), 'n2_e1_sig',
                                                  ifelse((N_ENSPIE_quad=='n2_e2' & N_quad_sig=='n2_sig' & ENSPIE_quad_sig=='e2_sig'), 'n2_e2_sig',
                                                         ifelse((N_ENSPIE_quad=='n3_e3' & N_quad_sig=='n3_sig' & ENSPIE_quad_sig=='e3_sig'), 'n3_e3_sig',
                                                                ifelse((N_ENSPIE_quad=='n3_e4' & N_quad_sig=='n3_sig' & ENSPIE_quad_sig=='e4_sig'), 'n3_e4_sig',
                                                                       ifelse((N_ENSPIE_quad=='n4_e3' & N_quad_sig=='n4_sig' & ENSPIE_quad_sig=='e3_sig'), 'n4_e3_sig',
                                                                              ifelse((N_ENSPIE_quad=='n4_e4' & N_quad_sig=='n4_sig' & ENSPIE_quad_sig=='e4_sig'), 'n4_e4_sig', N_ENSPIE_quad)))))))))



# set covariates for plotting based on whether changes differ from zero
Sn_study_corr <- Sn_study_corr %>%
  mutate(strokeS = ifelse(alphaS==1, 1.1, 1),
         sizeS = ifelse(alphaS==1, 2, .5),
         strokeSn = ifelse(alphaSn==1, 1.1, 1),
         sizeSn = ifelse(alphaSn==1, 2, .5),
         sizeN = ifelse(alphaN==1, 2, .5),
         sizeENSPIE = ifelse(alphaENSPIE==1, 2, .5),
         sizeN_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE!=1 & alphaSn!=1), 2, .5),
         sizeENSPIE_only = ifelse((alphaS!=1 & alphaN!=1 & alphaSn!=1 & alphaENSPIE==1), 2, .5),
         sizeS_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn!=1), 2, .5),
         sizeSn_only = ifelse((alphaS!=1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeN_ENSPIE_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE==1 & alphaSn!=1), 2, .5),
         sizeS_N_only = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE!=1 & alphaSn!=1), 2, .5),
         sizeSn_N_only = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeS_N_ = ifelse((alphaS==1 & alphaN==1), 2, .5),
         sizeS_Sn_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE!=1 & alphaSn==1), 2, .5),
         sizeS_ENSPIE_only = ifelse((alphaS==1 & alphaN!=1 & alphaENSPIE==1 & alphaSn!=1), 2, .5),
         sizeSn_ENSPIE_only = ifelse((alphaS!=1 & alphaN!=1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
         sizeS_ENSPIE_ = ifelse((alphaS==1 & alphaENSPIE==1), 2, .5),
         sizeS_N_and_ENSPIE = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE==1 & alphaSn!=1), 2, .5),
         sizeSn_N_and_ENSPIE = ifelse((alphaS!=1 & alphaN==1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
         sizeS_Sn_N_and_ENSPIE = ifelse((alphaS==1 & alphaN==1 & alphaENSPIE==1 & alphaSn==1), 2, .5),
         # categorical variable for barplot
         componentChange = ifelse(sizeN_only==2, 'N only',
                                  ifelse(sizeENSPIE_only==2, 'ENSPIE only',
                                    ifelse(sizeS_only==2, 'S only',
                                      ifelse(sizeSn_only==2, 'Sn only',
                                        ifelse(sizeN_ENSPIE_only==2, 'N & ENSPIE',
                                          ifelse(sizeS_N_only==2, 'S & N',
                                            ifelse(sizeSn_N_only==2, 'Sn & N',
                                              ifelse(sizeS_Sn_only==2, 'S & Sn',
                                                ifelse(sizeS_ENSPIE_only==2, 'S & ENSPIE',
                                                  ifelse(sizeSn_ENSPIE_only==2, 'Sn & ENSPIE',
                                                    ifelse(sizeSn_N_and_ENSPIE==2, 'Sn, N & ENSPIE',
                                                      ifelse(sizeS_Sn_N_and_ENSPIE==2, 'S, Sn, N & ENSPIE',
                                                        ifelse(sizeS_N_and_ENSPIE==2, 'S, N & ENSPIE', 'No change')))))))))))))
  )

# shapes for taxa_mod
shapes = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
           'Plants' = 5, 'All' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8, 'Reptiles' = 4)

Sn_study_corr$taxa_mod2 <- factor(Sn_study_corr$taxa_mod, 
                               levels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plant", "Amphibians",
                                          "All", "Marine invertebrates/plants", "Mammals", "Reptiles"),
                               labels = c("Fish", "Benthos", "Birds", "Invertebrates", "Plants", "Amphibians",
                                          "Multiple taxa", "Marine invertebrates/plants", "Mammals", "Reptiles"))

# shapes for taxa_mod
shapes_mod2 = c('Invertebrates' = 0, 'Fish' = 1, 'Benthos' = 2, 'Birds' = 15, 'Mammals' = 17,
                'Plants' = 5, 'Multiple taxa' = 6, 'Marine invertebrates/plants' = 7, 'Amphibians' = 8, 'Reptiles' = 4)

# colours for taxa_mod
taxa_col = c('Multiple taxa' = '#a6cee3',
             'Amphibians' = '#b2df8a',
             'Benthos' = '#ff7f00',
             'Birds' = '#cab2d6',
             'Fish' = '#3288bd',
             'Invertebrates' = '#fb9a99',
             'Mammals' = '#e31a1c',
             'Marine invertebrates/plants' = '#fdbf6f',
             'Plants' = '#33a02c',
             'Reptiles' = '#6a3d9a')


# colour code for the componentChange covariate
componentChange_col = c('No change' = '#666666', 
                        'N only' = '#a6cee3', 
                        'ENSPIE only' = '#1f78b4', 
                        'S only' = '#b2df8a', 
                        'Sn only' = '#33a02c', 
                        'N & ENSPIE' = '#fb9a99', 
                        'S & N' = '#e31a1c',
                        'S & Sn' = '#fdbf6f',
                        'Sn & N' = '#ff7f00',
                        'S & ENSPIE' = '#cab2d6', 
                        'Sn & ENSPIE' = '#2d004b', 
                        'S, N & ENSPIE' = '#01665e',
                        'Sn, N & ENSPIE' = '#b2182b',
                        'S, Sn, N & ENSPIE' = '#543005')

# shapes for realm
shapes_realm <- c('Marine' = 15, 'Terrestrial' = 16, 'Freshwater' = 17)

Sn_study_corr <- Sn_study_corr %>%
  mutate(realm_label0 = ifelse(REALM=='Marine', 'b  Marine',
                               ifelse(REALM=='Freshwater', 'a  Freshwater', 'c  Terrestrial')),
         realm_label = ifelse(REALM=='Marine', 'a.  Marine', 'b.   Terrestrial and freshwater'),
         realm_label2 = ifelse(REALM=='Marine', 'c.  Marine', 'd.   Terrestrial and freshwater'),
         realm_label3 = ifelse(REALM=='Marine', 'e.  Marine', 'f.   Terrestrial and freshwater'))

Sn_study_corr$componentChange <- factor(Sn_study_corr$componentChange,
                                     levels = c('No change', 
                                                'N only', 'ENSPIE only', 'Sn only', 'S only', 
                                                'S & Sn', 'S & ENSPIE', 'S & N',
                                                'Sn & ENSPIE', 'Sn & N',  
                                                'N & ENSPIE', 'Sn, N & ENSPIE', 'S, N & ENSPIE',
                                                'S, Sn, N & ENSPIE'))

# want to make the 'no change' points smaller
Sn_study_corr <- Sn_study_corr %>% 
  mutate(change_size = ifelse(componentChange=='No change', 'small', 'big'),
         change_stroke = ifelse(componentChange=='No change', 0.5, 1.1))

Sn_study_corr <- Sn_study_corr %>% 
  mutate(db = 'BioTIME')
