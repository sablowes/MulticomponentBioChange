## bateman_2018
library(data.table)


dataset_id <- 'bateman_2018'
load(file='data/raw data/bateman_2018/ddata')
setDT(ddata)

setnames(ddata, old = c('reach', 'site_code','common_name','bird_count','survey_date'),
         new = c('site', 'block', 'species','value','date'))
ddata[, ':='(year = as.integer(format(date, '%Y')),
             month = as.integer(format(date, '%m')))]

# Selecting surveys between January and May, excluding 2018 which has only January
ddata <- ddata[ year != 2018 & month <= 5]

ddata[is.na(value), value := 1]

# Community
ddata[, ':='(
         N = sum(value),
         S = length(unique(species)),
         ENSPIE = vegan::diversity(x = value, index = 'invsimpson')
      ),
      by = .(site, block, year, date)
]

ddata[, minN := min(N), by = .(site, block)] # 0% minN < 6

ddata[, Sn := vegan::rarefy(value, sample = minN), by = .(site, block, year, date)]

ddata <- ddata[,
               lapply(.SD, mean),
               by = .(site, block, year),
               .SDcols = c('N','S','Sn','ENSPIE')
               ]


ddata[, ':='(
   dataset_id = dataset_id,
   treatment = ifelse(site %in% c('Ave35','Ave67','Price','Priest'), 'Urban_notRestored',
                      ifelse(site %in% c('BM','Rio'), 'urban_restored',
                             'notUrban_notRestored')
                      ),
   treatment_type = 'riverbank vegetation restoration',
   timepoints = paste0('T',seq_along(unique(year))[match(year, unique(year))]),
   time_since_disturbance = ifelse(site == 'BM', year - 2012,
                                   ifelse(site == 'Rio', year - 2005, NA)
                                   ),
   realm = 'terrestrial',
   taxon = 'birds',

   comment = 'Some restored and some unrestored sites along the Salt river. Each station was surveyed several times a year (2 to 9). Abundances are summed per year and divided by the number of sampling events.'
)
][, design := paste0('A', ifelse(treatment == "urban_restored", 'I', 'C'))]

dir.create(paste0('data/wrangled data/', dataset_id), showWarnings = FALSE)
fwrite(ddata, paste0('data/wrangled data/', dataset_id, '/', dataset_id, '.csv',  row.names=FALSE))
