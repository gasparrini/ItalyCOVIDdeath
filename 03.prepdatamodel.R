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
# PREPARE THE AGGREGATED DATA FOR THE MODEL
################################################################################

# SELECT AGE GROUP, COLLAPSE DEATHS AT PROVINCE LEVEL
# NB: COMPUTE FOR munictype=1 ONLY (FULL PERIOD), AND TOTAL (UP TO 2019)
datamodel <- datafull %>% 
  filter(agegrfull %in% agegrsel) %>%
  group_by(regcode, regname, provcode, provname, date) %>%
  summarize(y = sum(ifelse(munictype==1, .data[[ysel]], 0)),
    totdeath = sum(.data[[ysel]])) 

# EXTRAPOLATE TOTAL MORTALITY IN 2020 FOR MUNICIPALITIES WITHOUT SUCH INFO
# NB: BASED ON PROPORTION IN PREVIOUS YEARS IN MUNICIPALITIES WITH FULL DATA
datamodel <- datamodel %>%
  group_by(provcode) %>%
  mutate(ind = year(date)<2020, prop = sum(totdeath[ind])/sum(y[ind])) %>%
  ungroup() %>%
  mutate(totdeath = ifelse(is.na(totdeath), y*prop, totdeath)) %>%
  dplyr::select(-ind, -prop)

# COMPLETE THE SERIES (FILL MISSING DAYS WITH 0'S) BY MERGING
comb <- unique(datamodel[1:4])
expcomb <- cbind(comb[rep(seq(nrow(comb)), each=length(seqdate)),],
  date=rep(seqdate, nrow(comb)))
datamodel <- merge(data.table(datamodel), expcomb, all.y=T, by=names(expcomb))
if(any(naind <- is.na(datamodel$y))) datamodel[naind,c("y","totdeath")] <- 0

# DEFINE POST-PERIOD SERIES, AND THE KNOTS FOR THE SPLINE
datamodel$tspost <- pmax(as.numeric(datamodel$date-startdate),0)

# MERGE WITH FLU DATA
# NB: AGE CONSIDERED AS > AND/OR <= 64 IF ANY GROUP BELONGING TO THESE RANGES
# NB: REGION 4 CODED AS 41-42 - NEED TO CHANGE FOR MERGING THEN TO CHANGE BACK
flu <- read.csv("data/flu_inc_2015-2020.csv", stringsAsFactors=F, colClasses=c(date="Date"))
fluind <- c(any(agegrsel<=13), any(agegrsel>13))
flu$flu <- if(all(fluind)) flu$inc else if(fluind[1]) flu$inc064 else flu$inc65
datamodel <- mutate(datamodel, regcode=replace(regcode,provcode==21,41),
  regcode=replace(regcode,provcode==22,42))
datamodel <- merge(datamodel, flu[,c("regcode","date","flu")],
  by=c("regcode","date"))
datamodel <- mutate(datamodel, regcode=replace(regcode,regcode%in%41:42,4))

# MERGE WITH TEMPERATURE DATA
temp <- read.csv("data/era5_mean2mt_2015-2020.csv", stringsAsFactors=F)
temp <- temp %>%
  rename(provcode=COD_PROV, tmean=X2m_tem_Kelvin) %>%
  mutate(date=as.Date(date), tmean=tmean-273.15)
datamodel <- merge(datamodel, temp, by=c("provcode","date"))

# RE-ORDER AND BACK TO DATA.FRAME
datamodel <- as.data.frame(arrange(datamodel, regcode, provcode, date))

# DEFINE PERIODS
seqperiod <- cut(unique(datamodel$tspost), cutdate-startdate, labels=labperiod2)

# SEQUENCES AND REPETITIONS
seqprov <- unique(datamodel$provcode)
seqreg <- unique(datamodel$regcode)
repprovreg <- with(datamodel, tapply(provcode, factor(regcode, levels=seqreg), 
  function(x) length(unique(x))))
seqregprov <- rep(seqreg, repprovreg)

# DEFINE AREAS (NORTH/CENTRAL/SOUTH/ISLAND)
areareg <- rep(c("North","Central","South","Islands"), c(8,4,6,2))
areaprov <- areareg[seqregprov]

# DEFINE LABELS
labprov <- sapply(strsplit(unique(datafull$provname), "/"), "[[", 1)
labreg <- sapply(strsplit(unique(datafull$regname), "/"), "[[", 1)
