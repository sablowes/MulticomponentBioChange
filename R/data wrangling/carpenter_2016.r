## carpenter_2016
library(data.table)

dataset_id <- 'carpenter_2016'
load(file='data/raw data/carpenter_2016/ddata')
setDT(ddata)

setnames(ddata, c('year4', 'lakename', 'taxon_name', 'abundance'),
         c('year', 'site', 'species', 'value'))

# Exclusion of lakes with unknown disturbance history
ddata <- ddata[!site %in% c('Ward Lake','Hummingbird Lake')]

effort <- ddata[, .(effort = length(unique(sampledate))), by = .(year, site)]
ddata <- merge(ddata, effort, by = c('year', 'site'))
ddata <- ddata[, .(value = sum(value / effort)), by = .(year, site, species)]


beforeafter <- ifelse(ddata$site %in% c("Paul Lake", 'Crampton Lake'), '',
                      ifelse(
                         (ddata$site %in% c('East Long Lake', 'West Long Lake') & ddata$year < 1991)  |
                            (ddata$site %in% c('Peter Lake','Tuesday Lake') & ddata$year < 1985), 'B', 'A')
)

ddata[, ':='(
   dataset_id = dataset_id,
   treatment = ifelse(site %in% c("Paul Lake", 'Crampton Lake'), 'control',
                      ifelse(site %in% c('East Long Lake', 'West Long Lake'), 'eutrophication',
                             ifelse(site %in% c('Peter Lake','Tuesday Lake'), 'community manipulation', NA)
                      )
   ),
   treatment_type = 'eutrophication + fish community manipulation',
   design = paste0(beforeafter, ifelse(site %in% c("Paul Lake", 'Crampton Lake'), 'C', 'I')),
   timepoints = paste0('T',seq_along(unique(year))[match(year, unique(year))]),
   time_since_disturbance = ifelse(site %in% c('East Long Lake', 'West Long Lake') & beforeafter == 'A',
                                   year - 1991,
                                   ifelse(site %in% c('Peter Lake','Tuesday Lake') & beforeafter == 'A',
                                          year - 1985,
                                          NA)
   ),
   realm = 'freshwater',
   taxon = 'zooplankton',
   metric = 'standardised count',
   unit = 'count',
   comment = 'Time since disturbance is the difference between sampledate and the FIRST disturbance. Most manipulations are reported in ./supporting litterature/Carpenter - Table 1 - Synthesis of a 33 year-series of whole lake experiments - lol2.10094.pdf. Zooplankton abundances are additionned and devided by the number of surveys in a lake in a year (3 to 19).'
)]




dir.create(paste0('data/wrangled data/', dataset_id), showWarnings = FALSE)
fwrite(ddata, paste0('data/wrangled data/', dataset_id, "/", dataset_id, '.csv'),
          row.names=FALSE)
