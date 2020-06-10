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
  #cat(labprov[i],"")
  
  # EXTRACT THE DATA AND COMPLETE IT
  dd <- subset(datamodel, provcode==seqprov[i])
  
  # DEFINE BASIS FUNCTIONS FOR POST-PERIOD, SEASONALITY, TEMPERATURE, FLU
  # NB: USE onebasis TO SIMPLIFY PREDICTIONS AND PLOTTING
  kpost <- equalknots(dd$tspost, nkpost)
  bpost <- onebasis(dd$tspost, fun="bs", degree=2, knots=kpost)
  kseas <- equalknots(yday(dd$date), dfseas)
  bseas <- onebasis(yday(dd$date), fun="pbs", knots=kseas)
  # ADD TEMPERATURE AND FLU

  # RUN THE MODEL
  mod <- glm(y ~ bpost + date + bseas, data=dd, family=quasipoisson)

  # SAVE THE RESULTS: COEF/VCOV OF VARIOUS TERMS PLUS RESIDUALS
  indpost <- grep("bpost", names(coef(mod)))
  indseas <- grep("bseas", names(coef(mod)))
  stage1list[[i]] <- list(
    post = list(coef=coef(mod)[indpost], vcov=vcov(mod)[indpost,indpost]),
    seas = list(coef=coef(mod)[indseas], vcov=vcov(mod)[indseas,indseas]),
    residuals = residuals(mod, type="deviance")
  )
  # ADD TEMPERATURE (OVERALL CUMULATIVE) AND FLU
}

################################################################################
# SECOND-STAGE META-ANALYSES

# POST-PERIOD EXCESS
coefpost <- t(sapply(stage1list, function(x) x$post$coef))
Scov <- lapply(stage1list, function(x) x$post$vcov)
metapost <- mixmeta(coefpost, Scov)
bluppost <- blup(metapost, vcov=T)

################################################################################
# COMPUTE EXCESS MORTALITY

# REDEFINE BASIS 
kpost <- equalknots(datamodel$tspost, nkpost)
bpost <- onebasis(unique(datamodel$tspost), fun="bs", degree=2, knots=kpost)

# DEFINE PERIODS
seqperiod <- cut(unique(datamodel$tspost), cutdate-startdate,
  labels=labperiod, include.lowest=T)

# DEFINE ARRAY TO STORE THE EXCESS DEATHS BY PROVINCE, PERIOD, RESAMPLING
excprovsim <- array(NA, dim=c(length(seqprov), length(labperiod), nsim+1),
  dimnames=list(labprov, labperiod, c("est",paste0("sim",seq(nsim)))))

# LOOP ACROSS PROVINCES
for(i in seq(seqprov)) {
  
  # PRINT
  #cat(labprov[i],"")
  
  # RETRIEVE COEF/VCOV AND TOTAL DEATHS
  coef <- bluppost[[i]]$blup
  vcov <- bluppost[[i]]$vcov
  death <- subset(datamodel, provcode==seqprov[i] & date>=startdate)$totdeath
  
  # COMPUTE ATTRIBUTABLE NUMBER (EXCESS), AND STORE THE SUM BY PERIOD
  an <- (1-exp(-bpost%*%coef))*death
  excprovsim[i,,"est"] <- tapply(an, seqperiod, sum)
  
  # SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
  set.seed(13041975)
  coefsim <- mvrnorm(nsim, coef, vcov)
  
  # LOOP ACROSS ITERATIONS AND DO AS ABOVE WITH RESAMPLES COEF
  for(s in seq(nsim)) {
    an <- (1-exp(-bpost%*%coefsim[s,]))*death
    excprovsim[i,,s+1] <- tapply(an, seqperiod, sum)
  }
}

# SEQUENCE OF REGIONS BY PROVINCE
provrep <- tapply(datamodel$provcode, factor(datamodel$regcode,levels=seqreg),
  function(x) length(unique(x)))
regprovcode <- rep(seqreg, provrep)

# COLLAPSE BY REGION AND THEN FULL COUNTRY
excregsim <- apply(excprovsim, 2:3, tapply, regprovcode, sum)
excitalysim <- apply(excregsim, 2:3, sum)


