## joern_2020

# Package ID: knb-lter-knz.121.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: PBG07 Grasshopper species abundances in the Patch-Burn Grazing experiment at Konza Prairie.
# Data set creator:  Anthony Joern -
# Metadata Provider:  Konza LTER -
# Contact:  Konza LTER -    - knzlter@ksu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu




infile2 <- paste0(getwd(),'/data/raw data/joern_2020/PBG072.csv')
if(!dir.exists(paste0(getwd(),'/data/raw data/joern_2020/')) || !file.exists(infile2))   {
   dir.create(paste0(getwd(),'/data/raw data/joern_2020/'), showWarnings = FALSE)
   inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/121/5/67a04b9c87218fd3c377d99f4e6b834e"
   download.file(inUrl2,infile2,method="curl")
}



dt2 <-read.csv(infile2,header=F
               ,skip=1
               ,sep=","
               ,quot="'"
               , col.names=c(
                  "Datacode",
                  "Rectype",
                  "Recyear",
                  "Recmonth",
                  "Recday",
                  "Watershed",
                  "Repsite",
                  "Spcode",
                  "Genus",
                  "Species",
                  "S1",
                  "S2",
                  "S3",
                  "S4",
                  "S5",
                  "S6",
                  "S7",
                  "S8",
                  "S9",
                  "S10",
                  "Total",
                  "Comments"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Datacode)!="factor") dt2$Datacode<- as.factor(dt2$Datacode)
if (class(dt2$Rectype)=="factor") dt2$Rectype <-as.numeric(levels(dt2$Rectype))[as.integer(dt2$Rectype) ]
if (class(dt2$Rectype)=="character") dt2$Rectype <-as.numeric(dt2$Rectype)
if (class(dt2$Recyear)=="factor") dt2$Recyear <-as.numeric(levels(dt2$Recyear))[as.integer(dt2$Recyear) ]
if (class(dt2$Recyear)=="character") dt2$Recyear <-as.numeric(dt2$Recyear)
if (class(dt2$Recmonth)=="factor") dt2$Recmonth <-as.numeric(levels(dt2$Recmonth))[as.integer(dt2$Recmonth) ]
if (class(dt2$Recmonth)=="character") dt2$Recmonth <-as.numeric(dt2$Recmonth)
if (class(dt2$Recday)=="factor") dt2$Recday <-as.numeric(levels(dt2$Recday))[as.integer(dt2$Recday) ]
if (class(dt2$Recday)=="character") dt2$Recday <-as.numeric(dt2$Recday)
if (class(dt2$Watershed)!="factor") dt2$Watershed<- as.factor(dt2$Watershed)
if (class(dt2$Repsite)!="factor") dt2$Repsite<- as.factor(dt2$Repsite)
if (class(dt2$Spcode)=="factor") dt2$Spcode <-as.numeric(levels(dt2$Spcode))[as.integer(dt2$Spcode) ]
if (class(dt2$Spcode)=="character") dt2$Spcode <-as.numeric(dt2$Spcode)
if (class(dt2$Genus)!="factor") dt2$Genus<- as.factor(dt2$Genus)
if (class(dt2$Species)!="factor") dt2$Species<- as.factor(dt2$Species)
if (class(dt2$S1)=="factor") dt2$S1 <-as.numeric(levels(dt2$S1))[as.integer(dt2$S1) ]
if (class(dt2$S1)=="character") dt2$S1 <-as.numeric(dt2$S1)
if (class(dt2$S2)=="factor") dt2$S2 <-as.numeric(levels(dt2$S2))[as.integer(dt2$S2) ]
if (class(dt2$S2)=="character") dt2$S2 <-as.numeric(dt2$S2)
if (class(dt2$S3)=="factor") dt2$S3 <-as.numeric(levels(dt2$S3))[as.integer(dt2$S3) ]
if (class(dt2$S3)=="character") dt2$S3 <-as.numeric(dt2$S3)
if (class(dt2$S4)=="factor") dt2$S4 <-as.numeric(levels(dt2$S4))[as.integer(dt2$S4) ]
if (class(dt2$S4)=="character") dt2$S4 <-as.numeric(dt2$S4)
if (class(dt2$S5)=="factor") dt2$S5 <-as.numeric(levels(dt2$S5))[as.integer(dt2$S5) ]
if (class(dt2$S5)=="character") dt2$S5 <-as.numeric(dt2$S5)
if (class(dt2$S6)=="factor") dt2$S6 <-as.numeric(levels(dt2$S6))[as.integer(dt2$S6) ]
if (class(dt2$S6)=="character") dt2$S6 <-as.numeric(dt2$S6)
if (class(dt2$S7)=="factor") dt2$S7 <-as.numeric(levels(dt2$S7))[as.integer(dt2$S7) ]
if (class(dt2$S7)=="character") dt2$S7 <-as.numeric(dt2$S7)
if (class(dt2$S8)=="factor") dt2$S8 <-as.numeric(levels(dt2$S8))[as.integer(dt2$S8) ]
if (class(dt2$S8)=="character") dt2$S8 <-as.numeric(dt2$S8)
if (class(dt2$S9)=="factor") dt2$S9 <-as.numeric(levels(dt2$S9))[as.integer(dt2$S9) ]
if (class(dt2$S9)=="character") dt2$S9 <-as.numeric(dt2$S9)
if (class(dt2$S10)=="factor") dt2$S10 <-as.numeric(levels(dt2$S10))[as.integer(dt2$S10) ]
if (class(dt2$S10)=="character") dt2$S10 <-as.numeric(dt2$S10)
if (class(dt2$Total)=="factor") dt2$Total <-as.numeric(levels(dt2$Total))[as.integer(dt2$Total) ]
if (class(dt2$Total)=="character") dt2$Total <-as.numeric(dt2$Total)
if (class(dt2$Comments)!="factor") dt2$Comments<- as.factor(dt2$Comments)

# Convert Missing Values to NA for non-dates

dt2$Spcode <- ifelse((trimws(as.character(dt2$Spcode))==trimws("blank")),NA,dt2$Spcode)
suppressWarnings(dt2$Spcode <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$Spcode))==as.character(as.numeric("blank"))),NA,dt2$Spcode))
dt2$Spcode <- ifelse((trimws(as.character(dt2$Spcode))==trimws(".")),NA,dt2$Spcode)
suppressWarnings(dt2$Spcode <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$Spcode))==as.character(as.numeric("."))),NA,dt2$Spcode))
dt2$S1 <- ifelse((trimws(as.character(dt2$S1))==trimws("blank")),NA,dt2$S1)
suppressWarnings(dt2$S1 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S1))==as.character(as.numeric("blank"))),NA,dt2$S1))
dt2$S1 <- ifelse((trimws(as.character(dt2$S1))==trimws(".")),NA,dt2$S1)
suppressWarnings(dt2$S1 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S1))==as.character(as.numeric("."))),NA,dt2$S1))
dt2$S2 <- ifelse((trimws(as.character(dt2$S2))==trimws("blank")),NA,dt2$S2)
suppressWarnings(dt2$S2 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S2))==as.character(as.numeric("blank"))),NA,dt2$S2))
dt2$S2 <- ifelse((trimws(as.character(dt2$S2))==trimws(".")),NA,dt2$S2)
suppressWarnings(dt2$S2 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S2))==as.character(as.numeric("."))),NA,dt2$S2))
dt2$S3 <- ifelse((trimws(as.character(dt2$S3))==trimws("blank")),NA,dt2$S3)
suppressWarnings(dt2$S3 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S3))==as.character(as.numeric("blank"))),NA,dt2$S3))
dt2$S3 <- ifelse((trimws(as.character(dt2$S3))==trimws(".")),NA,dt2$S3)
suppressWarnings(dt2$S3 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S3))==as.character(as.numeric("."))),NA,dt2$S3))
dt2$S4 <- ifelse((trimws(as.character(dt2$S4))==trimws("blank")),NA,dt2$S4)
suppressWarnings(dt2$S4 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S4))==as.character(as.numeric("blank"))),NA,dt2$S4))
dt2$S4 <- ifelse((trimws(as.character(dt2$S4))==trimws(".")),NA,dt2$S4)
suppressWarnings(dt2$S4 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S4))==as.character(as.numeric("."))),NA,dt2$S4))
dt2$S5 <- ifelse((trimws(as.character(dt2$S5))==trimws("blank")),NA,dt2$S5)
suppressWarnings(dt2$S5 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S5))==as.character(as.numeric("blank"))),NA,dt2$S5))
dt2$S5 <- ifelse((trimws(as.character(dt2$S5))==trimws(".")),NA,dt2$S5)
suppressWarnings(dt2$S5 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S5))==as.character(as.numeric("."))),NA,dt2$S5))
dt2$S6 <- ifelse((trimws(as.character(dt2$S6))==trimws("blank")),NA,dt2$S6)
suppressWarnings(dt2$S6 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S6))==as.character(as.numeric("blank"))),NA,dt2$S6))
dt2$S6 <- ifelse((trimws(as.character(dt2$S6))==trimws(".")),NA,dt2$S6)
suppressWarnings(dt2$S6 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S6))==as.character(as.numeric("."))),NA,dt2$S6))
dt2$S7 <- ifelse((trimws(as.character(dt2$S7))==trimws("blank")),NA,dt2$S7)
suppressWarnings(dt2$S7 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S7))==as.character(as.numeric("blank"))),NA,dt2$S7))
dt2$S7 <- ifelse((trimws(as.character(dt2$S7))==trimws(".")),NA,dt2$S7)
suppressWarnings(dt2$S7 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S7))==as.character(as.numeric("."))),NA,dt2$S7))
dt2$S8 <- ifelse((trimws(as.character(dt2$S8))==trimws("blank")),NA,dt2$S8)
suppressWarnings(dt2$S8 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S8))==as.character(as.numeric("blank"))),NA,dt2$S8))
dt2$S8 <- ifelse((trimws(as.character(dt2$S8))==trimws(".")),NA,dt2$S8)
suppressWarnings(dt2$S8 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S8))==as.character(as.numeric("."))),NA,dt2$S8))
dt2$S9 <- ifelse((trimws(as.character(dt2$S9))==trimws("blank")),NA,dt2$S9)
suppressWarnings(dt2$S9 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S9))==as.character(as.numeric("blank"))),NA,dt2$S9))
dt2$S9 <- ifelse((trimws(as.character(dt2$S9))==trimws(".")),NA,dt2$S9)
suppressWarnings(dt2$S9 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S9))==as.character(as.numeric("."))),NA,dt2$S9))
dt2$S10 <- ifelse((trimws(as.character(dt2$S10))==trimws("blank")),NA,dt2$S10)
suppressWarnings(dt2$S10 <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$S10))==as.character(as.numeric("blank"))),NA,dt2$S10))
dt2$S10 <- ifelse((trimws(as.character(dt2$S10))==trimws(".")),NA,dt2$S10)
suppressWarnings(dt2$S10 <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$S10))==as.character(as.numeric("."))),NA,dt2$S10))
dt2$Total <- ifelse((trimws(as.character(dt2$Total))==trimws("blank")),NA,dt2$Total)
suppressWarnings(dt2$Total <- ifelse(!is.na(as.numeric("blank")) & (trimws(as.character(dt2$Total))==as.character(as.numeric("blank"))),NA,dt2$Total))
dt2$Total <- ifelse((trimws(as.character(dt2$Total))==trimws(".")),NA,dt2$Total)
suppressWarnings(dt2$Total <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt2$Total))==as.character(as.numeric("."))),NA,dt2$Total))

ddata <- dt2
save(ddata, file = paste0(getwd(),'/data/raw data/joern_2020/ddata'))

