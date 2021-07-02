################################################################################
# Updated version of the R code for the analysis in:
#
#  "Scortichini M, Schneider Dos Santos R, De' Donato F, De Sario M,
#    Michelozzi P, Davoli M, Masselot P, Sera F, Gasparrini A.
#    Excess mortality during the COVID-19 outbreak in Italy: a two-stage
#    interrupted time-series analysis.
#    International Journal of Epidemiology. 2020. DOI: 10.1093/ije/dyaa169."
#  http://www.ag-myresearch.com/2020_scortichini_ije.html
#
# Update: 21 October 2020
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/ItalyCOVIDdeath
################################################################################

################################################################################
# DOWNLOAD THE DATA, RESHAPE, RENAME AND TRANSFORM 
################################################################################

# DOWNLOAD AND UNZIP THE DATA
# NB: ZIP IN FOLDER, UNCOMMENT TO DOWNLOAD AGAIN
source <- paste0("https://www.istat.it/it/files//2020/03/Dataset-decessi-",
  "comunali-giornalieri-e-tracciato-record-1.zip")
file <- paste0("data/", 
  "Dataset-decessi-comunali-giornalieri-e-tracciato-record-1.zip")
# curl_download(url=source, destfile=file, quiet=FALSE, mode="wb")
unzip(zipfile=file, exdir=paste(getwd(), "data", sep="/"), overwrite=F)

# READ THE DATA, THEN ERASE UNZIPPED (LARGE FILE)
dataorig <- fread("data/comuni_giornaliero_15maggio.csv",na.strings="n.d.")
file.remove("data/comuni_giornaliero_15maggio.csv")

# RESHAPE TO LONG
datafull <- dataorig[rep(seq(nrow(dataorig)), each=6), 1:9]
datafull$year <- rep(2015:2020, nrow(dataorig))
datafull$male <- c(t(dataorig[,grep("M_", names(dataorig), fixed=T), with=F]))
datafull$female <- c(t(dataorig[,grep("F_", names(dataorig), fixed=T), with=F]))
datafull$tot <- c(t(dataorig[,grep("T_", names(dataorig), fixed=T), with=F]))

# SELECT AND RENAME VARIABLES
datafull <- datafull %>% 
  rename(regcode=REG, regname=NOME_REGIONE, provcode=PROV,
    provname=NOME_PROVINCIA, municcode=COD_PROVCOM, municname=NOME_COMUNE,
  munictype=TIPO_COMUNE, agegrfull=CL_ETA)

# GENERATE DATE
datafull <- datafull %>%
  mutate(month=floor(GE/100), day=GE-month*100, date=make_date(year,month,day),
    GE=NULL)

# DEFINE THE DATE SERIES, THEN REMOVE LAST PERIOD AND ERRONEOUS LEAP DAYS
seqdate <- seq(from=dmy("01012015"), to=dmy("15052020"), by=1)
datafull <- subset(datafull, date%in%seqdate)

# ORDER BY REGION/PROVINCE/DATE, AND CREATE SEQUENCE AND LABELS (REDUCED)
datafull <- arrange(datafull, regcode, provcode, date)
