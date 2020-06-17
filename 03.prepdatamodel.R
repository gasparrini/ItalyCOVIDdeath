################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
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
flu <- read.csv("data/flu.csv", stringsAsFactors=F)
flu$date <- as.Date(flu$date)
fluind <- c(any(agegrsel<=13), any(agegrsel>13))
flu$flu <- if(all(fluind)) flu$inc else if(fluind[1]) flu$inc064 else flu$inc65
#datamodel <- merge(datamodel, flu[,c("regcode","date","flu")],
#  by=c("regcode","date"))

# MERGE WITH TEMPERATURE DATA
temp <- read.csv("data/era5_mean2mt_2015-2020.csv", stringsAsFactors=F)
temp <- temp %>%
  rename(provcode=COD_PROV, tmean=X2m_tem_Kelvin) %>%
  mutate(date=as.Date(date), tmean=tmean-273.15)
datamodel <- merge(datamodel, temp, by=c("provcode","date"))

# RE-ORDER AND BACK TO DATA.FRAME
datamodel <- as.data.frame(arrange(datamodel, regcode, provcode, date))
