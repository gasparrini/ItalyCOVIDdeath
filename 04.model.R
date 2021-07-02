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
  cbtmean <- crossbasis(dd$tmean, lag=lagtmean,
    argvar=list(fun="bs", degree=2, knots=quantile(dd$tmean, kpertmean/100)),
    arglag=list(knots=logknots(lagtmean, nklagtmean)))
  flu013 <- runMean(dd$flu, 0:13)
  
  # RUN THE MODEL
  # NB: PRESERVE MISSING TO COMPUTE RESIDUALS LATER
  mod <- glm(mformula, data=dd, family=quasipoisson, na.action="na.exclude")

  # SAVE THE RESULTS: COEF/VCOV, RESIDUALS, OVERDISPERSION
  loglik <- sum(dpois(mod$y,mod$fitted.values,log=TRUE))
  disp <- sum(residuals(mod,type="pearson")^2, na.rm=T)/mod$df.res
  stage1list[[i]] <- list(coef=coef(mod), vcov=vcov(mod), dispersion=disp,
    residuals=residuals(mod, type="deviance"))
}

################################################################################
# SECOND-STAGE META-ANALYSES

# MULTIVARIATE META-ANALYSIS OF COEFFICIENTS OF POST-PERIOD EXCESS
indpost <- grep("bpost", names(stage1list[[1]]$coef))
coefpost <- t(sapply(stage1list, function(x) x$coef[indpost]))
Scov <- lapply(stage1list, function(x) x$vcov[indpost,indpost])
metapost <- mixmeta(coefpost, Scov)
bluppost <- blup(metapost, vcov=T)

################################################################################
# COMPUTE EXCESS MORTALITY

# REDEFINE BASIS 
kpost <- equalknots(datamodel$tspost, nkpost)
bpost <- onebasis(unique(datamodel$tspost), fun="bs", degree=2, knots=kpost)

# DEFINE ARRAY TO STORE THE EXCESS DEATHS BY PROVINCE, PERIOD, RESAMPLING
excprovsim <- array(NA, dim=c(length(seqprov), length(labperiod1)+1, nsim+1),
  dimnames=list(labprov, c("15Feb-15May",labperiod1),
    c("est",paste0("sim",seq(nsim)))))

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
  indcovid <- seqpost>=coviddate
  excprovsim[i,1,"est"] <- sum(an[indcovid])
  excprovsim[i,-1,"est"] <- tapply(an, seqperiod, sum)
  
  # SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
  set.seed(13041975)
  coefsim <- mvrnorm(nsim, coef, vcov)
  
  # LOOP ACROSS ITERATIONS AND DO AS ABOVE WITH RESAMPLES COEF
  for(s in seq(nsim)) {
    an <- (1-exp(-bpost%*%coefsim[s,]))*death
    excprovsim[i,1,s+1] <- sum(an[indcovid])
    excprovsim[i,-1,s+1] <- tapply(an, seqperiod, sum)
  }
}

# COLLAPSE BY REGION AND THEN FULL COUNTRY
excregsim <- apply(excprovsim, 2:3, tapply, seqregprov, sum)
excitalysim <- apply(excregsim, 2:3, sum)


