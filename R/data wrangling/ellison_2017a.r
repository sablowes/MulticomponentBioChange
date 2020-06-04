## ellison_2017a
library(data.table)

dataset_id <- 'ellison_2017a'
load(file='data/raw data/ellison_2017a/ddata')
setDT(ddata)

setnames(ddata, old = c('cham', 'subs', 'n'),
         new = c('block', 'plot', 'value'))

warming_table <- na.omit(unique(ddata[, c('year', 'site','block','warming','target.delta')]))

warming_table[, ':='(warming = gsub(warming, pattern = ' c| C', replacement = '_c'),
                     treatment = paste(warming, target.delta, sep = '_'))
              ][,
                ':='(warming = NULL, target.delta = NULL)]

ddata <- merge(ddata, warming_table, by = c('year', 'site', 'block'))

ddata[method != 'Winkler', ':='(species = paste(genus, species))]


effort <- ddata[,
                .(effort = length(unique(sampling.id))),
                by = .(site, year, block, plot, treat, treatment)]

ddata <- ddata[,
               .(value = sum(value)),   # sum of counts per year
               by = .(site, year, block, plot, treat, treatment, species)
               ]
ddata <- merge(ddata, effort, by = c('year', 'site','block', 'plot', 'treat','treatment'))


ddata[, ':='(dataset_id = dataset_id,

             treatment_type = 'warming',
             design = paste0(ifelse(treat == 'Pre-treat', 'B', "A"),
                             ifelse(grepl(treatment, pattern = 'control'), 'C', "I")),

             timepoints = paste0('T',seq_along(unique(year))[match(year, unique(year))]),
             time_since_disturbance = ifelse(treat == 'Pre-treat' | grepl(treatment, pattern = 'control'),
                                             NA,
                                             ifelse(site == 'HF',
                                                    year - 2009, year - 2010
                                             )
             ),
             realm = 'terrestrial',
             taxon = 'invertebrates',
             metric = 'count',
             value = value / effort,
             unit = 'ind per survey',
             comment = "Block design with treatments being, no chamber, a chamber without warming, a chamber and warming with different warming intensities. Winkler samples are excluded. Repeated samplings in a single year are pooled. Counts are added and divided by effort. Effort is defined as the number of pitfall surveys per year (1 to 13). Maybe the time difference between first and last sampling surveys would be more appropriate? It depends if the pitfalls stayed several months and were emptied some times (then dividing by the number of sampling day would be better) or if each survey set pitfalls for a limited number of days and the number of surveys is the best proxy of effort. What's up with block/chamber 6? Its temperature changes. In site HF, both pre and post treatment samples were made in 2009.",
             treat = NULL,
             effort =NULL)]


# dat <- dat[!is.na(value)]    # three rows have a NA value for value but there is a species name.

dir.create(paste0('data/wrangled data/', dataset_id), showWarnings = FALSE)
fwrite(ddata, paste0('data/wrangled data/', dataset_id, "/", dataset_id, '.csv'),
          row.names=FALSE)

