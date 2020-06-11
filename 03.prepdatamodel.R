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
  mutate(totdeath = ifelse(is.na(totdeath), round(y*prop), totdeath)) %>%
  dplyr::select(-ind, -prop)

# COMPLETE THE SERIES (FILL MISSING DAYS WITH 0'S) BY MERGING
# NB: RE-ORDER AFTER THE MERGE
comb <- unique(datamodel[1:4])
expcomb <- cbind(comb[rep(seq(nrow(comb)), each=length(seqdate)),],
  date=rep(seqdate, nrow(comb)))
datamodel <- merge(datamodel, expcomb, all.y=T)
datamodel[is.na(datamodel)] <- 0
datamodel <- arrange(datamodel, regcode, provcode, date)

# DEFINE POST-PERIOD SERIES, AND THE KNOTS FOR THE SPLINE
datamodel$tspost <- pmax(as.numeric(datamodel$date-startdate),0)

# MERGE HERE WITH TEMPERATURE AND FLU DATA
