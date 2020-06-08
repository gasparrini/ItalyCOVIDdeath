################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# MODELLING: TWO-STAGE, PLUS COMPUTE THE EXCESS MORTALITY 
################################################################################

################################################################################
# FIRST STAGE

# LIST TO STORE COEF/VCOV AND CONVERGENCE INDICATOR
stage1list <- vector("list", length(seqprov))
names(stage1list) <- labprov

# LOOP ACROSS PROVINCES

for(i in seq(seqprov)) {
  
  # PRINT
  cat(labprov[i],"")
  
  # EXTRACT THE DATA AND COMPLETE IT
  dd <- subset(datamodel, provcode==seqprov[i])
  
  # DEFINE BASIS FUNCTIONS FOR COVID EXCESS, SEASONALITY, TEMPERATURE, FLU
  # NB: USE onebasis TO SIMPLIFY PREDICTIONS AND PLOTTING
  kcovid <- equalknots(dd$covidts, nkcovid)
  bcovid <- onebasis(dd$covidts, fun="bs", degree=2, knots=kcovid)
  kseas <- equalknots(yday(dd$date), dfseas)
  bseas <- onebasis(yday(dd$date), fun="pbs", knots=kseas)
  # ADD TEMPERATURE AND FLU

  # RUN THE MODEL
  mod <- glm(y ~ bcovid + date + bseas, data=dd, family=quasipoisson)

  # SAVE THE RESULTS: COEF/VCOV OF VARIOUS TERMS PLUS RESIDUALS
  indcovid <- grep("bcovid", names(coef(mod)))
  indseas <- grep("bseas", names(coef(mod)))
  stage1list[[i]] <- list(
    covid = list(coef=coef(mod)[indcovid], vcov=vcov(mod)[indcovid,indcovid]),
    seas = list(coef=coef(mod)[indseas], vcov=vcov(mod)[indseas,indseas]),
    residuals = residuals(mod, type="deviance")
  )
  # ADD TEMPERATURE (OVERALL CUMULATIVE) AND FLU
}

################################################################################
# SECOND-STAGE META-ANALYSES

# COVID EXCESS
coefcov <- t(sapply(stage1list, function(x) x$covid$coef))
Scov <- lapply(stage1list, function(x) x$covid$vcov)
metacovid <- mixmeta(coefcov, Scov)
blupcovid <- blup(metacovid, vcov=T)

################################################################################
# COMPUTE EXCESS MORTALITY

# REDEFINE BASIS 
kcovid <- equalknots(datamodel$covidts, nkcovid)
bcovid <- onebasis(unique(datamodel$covidts), fun="bs", degree=2, knots=kcovid)

# DEFINE PERIODS
labcovperiod <- sapply(2:length(cutdate), function(i) 
  paste(paste(day(cutdate[c(i-1,i)]+1:0), collapse="-"), 
    month(cutdate[i], lab=T)))
seqcovperiod <- cut(unique(datamodel$covidts), cutdate-startdate,
  labels=labcovperiod, include.lowest=T)

# DEFINE ARRAY TO STORE THE EXCESS DEATHS BY PROVINCE, PERIOD, SIMULATION
excarray <- array(NA, dim=c(length(seqprov), length(labcovperiod), nsim+1),
  dimnames=list(labprov, labcovperiod, c("est",paste0("sim",seq(nsim)))))

# LOOP ACROSS PROVINCES
for(i in seq(seqprov)) {
  
  # PRINT
  cat(labprov[i],"")
  
  # RETRIEVE COEF/VCOV AND TOTAL DEATHS
  coef <- blupcovid[[i]]$blup
  vcov <- blupcovid[[i]]$vcov
  death <- subset(datamodel, provcode==seqprov[i] & date>=startdate)$totdeath
  
  # COMPUTE ATTRIBUTABLE NUMBER (EXCESS), AND STORE THE SUM BY PERIOD
  an <- (1-exp(-bcovid%*%coef))*death
  excarray[i,,"est"] <- tapply(an, seqcovperiod, sum)
  
  # SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
  set.seed(13041975)
  coefsim <- mvrnorm(nsim, coef, vcov)
  
  # LOOP ACROSS ITERATIONS AND DO AS ABOVE WITH RESAMPLES COEF
  for(s in seq(nsim)) {
    an <- (1-exp(-bcovid%*%coefsim[s,]))*death
    excarray[i,,s+1] <- tapply(an, seqcovperiod, sum)
  }
}
