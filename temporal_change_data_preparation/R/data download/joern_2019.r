## joern_2019

# Package ID: knb-lter-knz.29.14 Cataloging System:https://pasta.edirepository.org.
# Data set title: CGR02 Sweep Sampling of Grasshoppers on Konza Prairie LTER watersheds.
# Data set creator:  Anthony Joern -
# Metadata Provider:    - Konza LTER
# Contact:  Konza LTER -    - knzlter@ksu.edu
# Stylesheet v2.10 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu

dataset_id <- 'joern_2019'
infile1 <- paste0(getwd(),'/data/raw data/joern_2019/CGR021.csv')
if(!dir.exists(paste0(getwd(),'/data/raw data/joern_2019/')) || !file.exists(infile1))   {
   dir.create(paste0(getwd(),'/data/raw data/joern_2019/'),  showWarnings = FALSE)
   inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/29/14/aaa7b3b477019f6bb96f47a9a6ae8943"
   download.file(inUrl1, infile1, method="curl")
}

# sampling and environmental data

dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                  "DataCode",
                  "RecType",
                  "RecYear",
                  "RecMonth",
                  "RecDay",
                  "Watershed",
                  "Soiltype",
                  "Repsite",
                  "RecTime",
                  "Wind",
                  "AirTemp",
                  "Relhum",
                  "Cloudcov"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$DataCode)!="factor") dt1$DataCode<- as.factor(dt1$DataCode)
if (class(dt1$RecType)!="factor") dt1$RecType<- as.factor(dt1$RecType)
if (class(dt1$RecYear)=="factor") dt1$RecYear <-as.numeric(levels(dt1$RecYear))[as.integer(dt1$RecYear) ]
if (class(dt1$RecYear)=="character") dt1$RecYear <-as.numeric(dt1$RecYear)
if (class(dt1$Watershed)!="factor") dt1$Watershed<- as.factor(dt1$Watershed)
if (class(dt1$Soiltype)!="factor") dt1$Soiltype<- as.factor(dt1$Soiltype)
if (class(dt1$Repsite)!="factor") dt1$Repsite<- as.factor(dt1$Repsite)
if (class(dt1$Wind)=="factor") dt1$Wind <-as.numeric(levels(dt1$Wind))[as.integer(dt1$Wind) ]
if (class(dt1$Wind)=="character") dt1$Wind <-as.numeric(dt1$Wind)
if (class(dt1$AirTemp)=="factor") dt1$AirTemp <-as.numeric(levels(dt1$AirTemp))[as.integer(dt1$AirTemp) ]
if (class(dt1$AirTemp)=="character") dt1$AirTemp <-as.numeric(dt1$AirTemp)
if (class(dt1$Relhum)=="factor") dt1$Relhum <-as.numeric(levels(dt1$Relhum))[as.integer(dt1$Relhum) ]
if (class(dt1$Relhum)=="character") dt1$Relhum <-as.numeric(dt1$Relhum)
if (class(dt1$Cloudcov)=="factor") dt1$Cloudcov <-as.numeric(levels(dt1$Cloudcov))[as.integer(dt1$Cloudcov) ]
if (class(dt1$Cloudcov)=="character") dt1$Cloudcov <-as.numeric(dt1$Cloudcov)

# Convert Missing Values to NA for non-dates

dt1$Wind <- ifelse((trimws(as.character(dt1$Wind))==trimws(".")),NA,dt1$Wind)
dt1$Wind <- ifelse((trimws(as.character(dt1$Wind))==trimws("blank")),NA,dt1$Wind)
dt1$AirTemp <- ifelse((trimws(as.character(dt1$AirTemp))==trimws(".")),NA,dt1$AirTemp)
dt1$AirTemp <- ifelse((trimws(as.character(dt1$AirTemp))==trimws("Blank")),NA,dt1$AirTemp)
dt1$Relhum <- ifelse((trimws(as.character(dt1$Relhum))==trimws("blank")),NA,dt1$Relhum)
dt1$Cloudcov <- ifelse((trimws(as.character(dt1$Cloudcov))==trimws(".")),NA,dt1$Cloudcov)
dt1$Cloudcov <- ifelse((trimws(as.character(dt1$Cloudcov))==trimws("Blank")),NA,dt1$Cloudcov)

# Biological data

infile2 <- paste0(getwd(),'/data/raw data/joern_2019/CGR022.csv')
if(!dir.exists(paste0(getwd(),'/data/raw data/joern_2019/')) || !file.exists(infile2))   {
   dir.create(paste0(getwd(),'/data/raw data/joern_2019/'),  showWarnings = FALSE)
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/29/14/3fb352e2478f776517f7e880fe31b808"
   download.file(inUrl2, infile2, method="curl")
}


dt2 <-read.csv(infile2,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                  "DataCode",
                  "RecType",
                  "RecYear",
                  "RecMonth",
                  "RecDay",
                  "Watershed",
                  "Soiltype",
                  "Repsite",
                  "Spcode",
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

if (class(dt2$DataCode)!="factor") dt2$DataCode<- as.factor(dt2$DataCode)
if (class(dt2$RecType)!="factor") dt2$RecType<- as.factor(dt2$RecType)
if (class(dt2$RecYear)=="factor") dt2$RecYear <-as.numeric(levels(dt2$RecYear))[as.integer(dt2$RecYear) ]
if (class(dt2$RecYear)=="character") dt2$RecYear <-as.numeric(dt2$RecYear)
if (class(dt2$Watershed)!="factor") dt2$Watershed<- as.factor(dt2$Watershed)
if (class(dt2$Soiltype)!="factor") dt2$Soiltype<- as.factor(dt2$Soiltype)
if (class(dt2$Repsite)!="factor") dt2$Repsite<- as.factor(dt2$Repsite)
if (class(dt2$Spcode)!="factor") dt2$Spcode<- as.factor(dt2$Spcode)
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

dt2$Spcode <- as.factor(ifelse((trimws(as.character(dt2$Spcode))==trimws("blank")),NA,as.character(dt2$Spcode)))
dt2$S1 <- ifelse((trimws(as.character(dt2$S1))==trimws("Blank")),NA,dt2$S1)
dt2$S2 <- ifelse((trimws(as.character(dt2$S2))==trimws("Blank")),NA,dt2$S2)
dt2$S3 <- ifelse((trimws(as.character(dt2$S3))==trimws("Blank")),NA,dt2$S3)
dt2$S4 <- ifelse((trimws(as.character(dt2$S4))==trimws("Blank")),NA,dt2$S4)
dt2$S5 <- ifelse((trimws(as.character(dt2$S5))==trimws("1")),NA,dt2$S5)
dt2$S6 <- ifelse((trimws(as.character(dt2$S6))==trimws("Blank")),NA,dt2$S6)
dt2$S7 <- ifelse((trimws(as.character(dt2$S7))==trimws("Blank")),NA,dt2$S7)
dt2$S8 <- ifelse((trimws(as.character(dt2$S8))==trimws("blank")),NA,dt2$S8)
dt2$S9 <- ifelse((trimws(as.character(dt2$S9))==trimws("blank")),NA,dt2$S9)
dt2$S10 <- ifelse((trimws(as.character(dt2$S10))==trimws("Blank")),NA,dt2$S10)

# DataCode has to be cleaned
dt2[dt2$DataCode == 'cgr02', 'DataCode'] <- 'CGR02'
dt2 <- dt2[dt2$DataCode == 'CGR02',]

ddata <- dt2
save(ddata, file=paste0(getwd(),'/data/raw data/joern_2019/ddata'))


















### fire dates download

# Package ID: knb-lter-knz.42.9 Cataloging System:https://pasta.edirepository.org.
# Data set title: KFH01 Konza Prairie Fire History.
# Data set creator:  John Blair -
# Data set creator:  Patrick O'Neal -
# Metadata Provider:    - Konza LTER
# Contact:  Patrick O'Neal -    - poneal@ksu.edu
# Contact:    -  Konza LTER  -
# Stylesheet v2.10 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu


infile4 <- paste0(getwd(),'/data/raw data/joern_2019/kfh011.csv')
if(!dir.exists(paste0(getwd(),'/data/raw data/joern_2019/')) || !file.exists(infile4))   {
   dir.create(paste0(getwd(),'/data/raw data/joern_2019/'),  showWarnings = FALSE)
   inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/42/9/3565885edc7640f2276174ca1f00240b"
   download.file(inUrl4, infile4, method="curl")
}


dt4 <-read.csv(infile4,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                  "Watershed",
                  "Hname",
                  "Hectares",
                  "Acres",
                  "Date",
                  "Type",
                  "Year",
                  "Code",
                  "Comments"
               ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$Watershed)!="character") dt4$Watershed<- character(dt4$Watershed)# modified
if (class(dt4$Hname)!="factor") dt4$Hname<- as.factor(dt4$Hname)
if (class(dt4$Hectares)=="factor") dt4$Hectares <-as.numeric(levels(dt4$Hectares))[as.integer(dt4$Hectares) ]
if (class(dt4$Hectares)=="character") dt4$Hectares <-as.numeric(dt4$Hectares)
if (class(dt4$Acres)=="factor") dt4$Acres <-as.numeric(levels(dt4$Acres))[as.integer(dt4$Acres) ]
if (class(dt4$Acres)=="character") dt4$Acres <-as.numeric(dt4$Acres)
# attempting to convert dt4$Date dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%m/%d/%Y" # modified
tmp1Date<-as.Date(dt4$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt4$Date <- tmp1Date } else {print("Date conversion failed for dt4$Date. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1Date)
if (class(dt4$Type)!="factor") dt4$Type<- as.factor(dt4$Type)
if (class(dt4$Year)=="factor") dt4$Year <-as.numeric(levels(dt4$Year))[as.integer(dt4$Year) ]
if (class(dt4$Year)=="character") dt4$Year <-as.numeric(dt4$Year)
if (class(dt4$Code)!="factor") dt4$Code<- as.factor(dt4$Code)
if (class(dt4$Comments)!="factor") dt4$Comments<- as.factor(dt4$Comments)

# Convert Missing Values to NA for non-dates

dt4$Watershed <- ifelse((trimws(as.character(dt4$Watershed))==trimws(".")),NA,as.character(dt4$Watershed))

fd <- dt4
save(fd, file=paste0(getwd(),'/data/raw data/joern_2019/ddata_env'))
