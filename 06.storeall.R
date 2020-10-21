################################################################################
# Updated version of the R code for the analysis in:
#
#  "Scortichini M, Schneider Dos Santos R, De' Donato F, De Sario M,
#    Michelozzi P, Davoli M, Masselot P, Sera F, Gasparrini A.
#    Excess mortality during the COVID-19 outbreak in Italy: a two-stage
#    interrupted time-series analysis.
#    International Journal of Epidemiology. 2020. DOI: 10.1093/ije/dyaa169."
#  http://www.ag-myresearch.com/italyCOVIDdeath.html
#
# Update: 21 October 2020
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/ItalyCOVIDdeath
################################################################################

################################################################################
# PERFORM ALL THE ANALYSES AND STORE ALL THE RESULTS
################################################################################

################################################################################
# CREATE OBJECTS TO STORE RESULTS

# EXCESS AND TOTAL MORTALITY BY PROVINCE
excprov <- array(NA, dim=c(length(seqprov), length(labperiod1)+1, 3,
  length(agegrlist),3), dimnames=list(labprov, dimnames(excprovsim)[[2]],
    sexlab, agegrlab, c("Est","eCIlow","eCIhigh")))
totprov <- excprov[,,,,1]

# EXCESS AND TOTAL MORTALITY BY REGION
excreg <- array(NA, dim=c(length(seqreg), dim(excprov)[-1]),
  dimnames=c(list(labreg),dimnames(excprov)[-1]))
totreg <- excreg[,,,,1]

# EXCESS AND TOTAL MORTALITY BY COUNTRY
excitaly <- array(NA, dim=dim(excprov)[-1], dimnames=dimnames(excprov)[-1])
totitaly <- excitaly[,,,1]

# DEFINE SEX/AGE COMBINATIONS (LABELS CONSISTENT WITH PARAMETERS)
matcomb <- expand.grid(sex=dimnames(excprov)[[3]], 
  agegr=dimnames(excprov)[[4]], stringsAsFactors=F)

# COEF/VCOV OF META-ANALYSIS
metalist <- vector("list", nrow(matcomb))
names(metalist) <- paste(matcomb[,1], matcomb[,2], sep="/")

################################################################################
# DEFINE A LOOP TO PREPARE DATA AND RUN MODELS BY SEX AND AGE

# LOOP ACROSS COMBINATIONS
for(k in seq(nrow(matcomb))) {
  
  # PRINT
  cat("Sex:",matcomb[k,1],"-","Age:",matcomb[k,2],"\n")
  
  # RE-DEFINE PARAMETERS
  source("02.param.R")
  ysel <- sexlist[[matcomb[k,1]]]
  agegrsel <- agegrlist[[matcomb[k,2]]]
  
  # AGGREGATE DATA AND RUN THE MODELS
  source("03.prepdatamodel.R")
  source("04.model.R")
  
  # STORE COEF/VCOV OF META-ANALYSIS
  metalist[[k]] <- list(coef=coef(metapost), vcov=vcov(metapost))

  # STORE EXCESS MORTALITY - BY PROVINCE AND PERIOD (REQUIRES PERMUTATION)
  excprov[,, matcomb[k,1], matcomb[k,2], "Est"] <- excprovsim[,,1]
  excprov[,, matcomb[k,1], matcomb[k,2], -1] <- 
    aperm(apply(excprovsim[,,-1], 1:2, quantile, c(2.5,97.5)/100), c(2,3,1))

  # STORE EXCESS MORTALITY - BY REGION AND PERIOD (REQUIRES PERMUTATION)
  excreg[,, matcomb[k,1], matcomb[k,2], "Est"] <- excregsim[,,1]
  excreg[,, matcomb[k,1], matcomb[k,2], -1] <- 
    aperm(apply(excregsim[,,-1], 1:2, quantile, c(2.5,97.5)/100), c(2,3,1))

  # STORE EXCESS MORTALITY - FULL COUNTRY BY PERIOD 
  excitaly[, matcomb[k,1], matcomb[k,2], "Est"] <- excitalysim[,1]
  excitaly[, matcomb[k,1], matcomb[k,2], -1] <- 
    t(apply(excitalysim[,-1], 1, quantile, c(2.5,97.5)/100))
  
  # REDUCE DATA TO COVID AND DEFINE CATEGORIES (BE AWARE OF ORDER USING LEVELS)
  datadeath <- datamodel %>%
    filter(date>=startdate) %>%
    mutate(provcode = factor(rep(seqprov, each=length(seqperiod)), levels=seqprov),
      period = rep(seqperiod, length(seqprov)))
  
  # STORE TOTAL MORTALITY BY PROVINCE
  totprov[,1, matcomb[k,1], matcomb[k,2]] <- 
    with(subset(datadeath, date>=coviddate), tapply(totdeath,provcode,sum))
  totprov[,-1, matcomb[k,1], matcomb[k,2]] <-
    with(datadeath, tapply(totdeath, list(provcode,period), sum))
}

# AGGREGATE TOTAL MORTALITY BY REGION AND FULL COUNTRY
totreg <- apply(totprov, 2:4, tapply, seqregprov, sum)
totitaly <- apply(totreg, 2:4, sum)
