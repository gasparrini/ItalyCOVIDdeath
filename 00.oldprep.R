################################################################################
# PRELIMINARY ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

# LOAD THE PACKAGES
library(curl) ; library(data.table)
library(dplyr) ; library(lubridate)
library(dlnm) ; library(splines) ; library(mixmeta)
library(scales)
library(sp) ; library(tmap) ; library(GISTools) ; library(sf)

# DOWNLOAD AND UNZIP THE DATA
# NB: UNCOMMENT TO RUN IT
# source <- "https://www.istat.it/it/files//2020/03/Dataset-decessi-comunali-giornalieri-e-tracciato-record.zip"
# file <- "Dataset-decessi-comunali-giornalieri-e-tracciato-record.zip"
# curl_download(url=source, destfile=file, quiet=FALSE, mode="wb")
# unzip(zipfile=file, exdir=getwd())

# READ THE DATA
dataorig <- fread("comuni_giornaliero.csv",na.strings="n.d.")

# RESHAPE TO LONG
names(dataorig)
data <- dataorig[rep(seq(nrow(dataorig)),each=6),1:9]
data$year <- rep(2015:2020,nrow(dataorig))
data$male <- c(t(dataorig[,grep("M_",names(dataorig),fixed=T),with=F]))
data$female <- c(t(dataorig[,grep("F_",names(dataorig),fixed=T),with=F]))
data$tot <- c(t(dataorig[,grep("T_",names(dataorig),fixed=T),with=F]))

# GENERATE DATE
data[,':='(month=as.numeric(substr(GE,1,1)),day=as.numeric(substr(GE,2,3)))]
data[,date:=make_date(year,month,day)]

# SELECT TYPE OF MUNICIPALITY AND RELATED PERIOD OF YEAR
# NB: TYPE 1 UP TO 15/04/2020, TYPE 2 UP TO 31/03/2020
type <- 2
sub <- if(type==2) data$month%in%1:3 else if(type==1)
  data$month%in%1:3 | (data$month==4 & data$day<16) else stop("error")
data <- subset(data,TIPO_COMUNE==type & sub)

# DEFINE THE DATE SERIES
seqdate <- seq(from=dmy("01012015"),to=dmy("15042020"),by=1)
seqdate <- if(type==2) seqdate[month(seqdate) %in% 1:3] else
  seqdate[month(seqdate)%in%1:3 | (month(seqdate)==4 & day(seqdate)<16)]

# REMOVE ERRONEOUS LEAP DAYS
data <- subset(data, date%in%seqdate)

# SELECT AGE GROUPS AND SEX
data <- subset(data,CL_ETA %in% 0:21)
data$y <- data$tot

# COLLAPSE AT PROVINCE LEVEL
data <- data %>% 
  group_by(REG,NOME_REGIONE,PROV,NOME_PROVINCIA,date) %>%
  summarize(y=sum(y)) %>%
  rename(regcode=REG,region=NOME_REGIONE,provcode=PROV,province=NOME_PROVINCIA) %>%
  arrange(regcode,provcode,date)

# COMPLETE THE SERIES (FILL MISSING DAYS WITH 0'S)
comb <- unique(data[1:4])
expcomb <- cbind(comb[rep(seq(nrow(comb)),each=length(seqdate)),],
  date=rep(seqdate,nrow(comb)))
data <- merge(data,expcomb,all.y=T)
data[is.na(data)] <- 0

# DEFINE COVID STARTING DAY, THE SERIES, AND THE KNOTS FOR THE SPLINE
startdate <- dmy(01022020)
data$post <- pmax(data$date-startdate,0)
postknots <- equalknots(data$post,3)

# MERGE HERE WITH TEMPERATURE AND FLU DATA
