## landis_2018

# Package ID: knb-lter-kbs.23.26 Cataloging System:https://pasta.edirepository.org.
# Data set title: Insect Population Dynamics on the Main Cropping System Experiment at the Kellogg Biological Station, Hickory Corners, MI  (1989 to 2017).
# Data set creator:  Douglas Landis - Michigan State University
# Contact:    - Data Manager Kellogg Biological Station  - lter.data.manager@kbs.msu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu



infile1 <- paste0(getwd(),'/data/raw data/landis_2018/Insect+Populations+via+Sticky+Traps')
if(!dir.exists(paste0(getwd(),'/data/raw data/landis_2018/')) || !file.exists(infile1))   {
   dir.create(paste0(getwd(),'/data/raw data/landis_2018/'), showWarnings = FALSE)
   inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-kbs/23/26/8d33fa9169147f266d20bdcd09a07820"
   download.file(inUrl1,infile1,method="curl")
}




dt1 <-read.csv(infile1,header=F
               ,skip=29
               ,sep=","
               ,quot='"'
               , col.names=c(
                  "Sample_Date",
                  "Treatment",
                  "Replicate",
                  "Station",
                  "Species",
                  "Family",
                  "Order",
                  "Adults",
                  "utm_easting",
                  "utm_northing",
                  "Year"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$Sample_Date dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1Sample_Date<-as.Date(dt1$Sample_Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Sample_Date) == length(tmp1Sample_Date[!is.na(tmp1Sample_Date)])){dt1$Sample_Date <- tmp1Sample_Date } else {print("Date conversion failed for dt1$Sample_Date. Please inspect the data and do the date conversion yourself.")}
rm(tmpDateFormat,tmp1Sample_Date)
if (class(dt1$Treatment)!="factor") dt1$Treatment<- as.factor(dt1$Treatment)
if (class(dt1$Replicate)!="factor") dt1$Replicate<- as.factor(dt1$Replicate)
if (class(dt1$Station)!="factor") dt1$Station<- as.factor(dt1$Station)
if (class(dt1$Species)!="factor") dt1$Species<- as.factor(dt1$Species)
if (class(dt1$Family)!="factor") dt1$Family<- as.factor(dt1$Family)
if (class(dt1$Order)!="factor") dt1$Order<- as.factor(dt1$Order)
if (class(dt1$Adults)=="factor") dt1$Adults <-as.numeric(levels(dt1$Adults))[as.integer(dt1$Adults) ]
if (class(dt1$Adults)=="character") dt1$Adults <-as.numeric(dt1$Adults)
if (class(dt1$utm_easting)=="factor") dt1$utm_easting <-as.numeric(levels(dt1$utm_easting))[as.integer(dt1$utm_easting) ]
if (class(dt1$utm_easting)=="character") dt1$utm_easting <-as.numeric(dt1$utm_easting)
if (class(dt1$utm_northing)=="factor") dt1$utm_northing <-as.numeric(levels(dt1$utm_northing))[as.integer(dt1$utm_northing) ]
if (class(dt1$utm_northing)=="character") dt1$utm_northing <-as.numeric(dt1$utm_northing)

# Convert Missing Values to NA for non-dates

ddata <- dt1
save(ddata, file = paste0(getwd(),'/data/raw data/landis_2018/ddata'))

