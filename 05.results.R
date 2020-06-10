################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# SUMMARISE RESULTS
################################################################################

################################################################################
# CREATE ARRAYS TO STORE EXCESS AND TOTAL MORTALITY

# DEFINE PARAMETERS
source("02.param.R")

# BY PROVINCE
excprov <- array(NA, dim=c(length(seqprov), length(labperiod)+1, 3,
  length(agegrlist),3), dimnames=list(labprov, c("Mar-Apr",labperiod),
    c("tot","male","female"), agegrlab, c("Est","eCIlow","eCIhigh")))
totprov <- excprov[,,,,1]

# BY REGION
excreg <- array(NA, dim=c(length(seqreg), dim(excprov)[-1]),
  dimnames=c(list(labreg),dimnames(excprov)[-1]))
totreg <- excreg[,,,,1]

# BY COUNTRY
excitaly <- array(NA, dim=dim(excprov)[-1], dimnames=dimnames(excprov)[-1])
totitaly <- excitaly[,,,1]

################################################################################
# DEFINE A LOOP TO PREPARE DATA AND RUN MODELS BY SEX AND AGE

# DEFINE SEX/AGE COMBINATIONS (LABELS CONSISTENT WITH PARAMETERS)
matcomb <- expand.grid(sex=dimnames(excprov)[[3]], 
  agegr=dimnames(excprov)[[4]], stringsAsFactors=F)

# LOOP ACROSS COMBINATIONS
for(k in seq(nrow(matcomb))) {
  
  # PRINT
  cat("Sex:",matcomb[k,1],"-","Age:",matcomb[k,2],"\n")
  
  # RE-DEFINE PARAMETERS
  source("02.param.R")
  ysel <- matcomb[k,1]
  agegrsel <- agegrlist[[matcomb[k,2]]]
  
  # AGGREGATE DATA AND RUN THE MODELS
  source("03.prepdatamodel.R")
  source("04.model.R")
  
  # STORE EXCESS MORTALITY - BY PROVINCE IN MAR-APR
  excprov[,"Mar-Apr", matcomb[k,1], matcomb[k,2], "Est"] <-  
    rowSums(excprovsim[,-c(1:3),1])
  excprov[,"Mar-Apr", matcomb[k,1], matcomb[k,2], -1] <- 
    t(apply(apply(excprovsim[,-c(1:3),-1],c(1,3),sum), 1, quantile,
      c(2.5,97.5)/100))
  
  # STORE EXCESS MORTALITY - BY PROVINCE AND PERIOD (REQUIRES PERMUTATION)
  excprov[,-1, matcomb[k,1], matcomb[k,2], "Est"] <- excprovsim[,,1]
  excprov[,-1, matcomb[k,1], matcomb[k,2], -1] <- 
    aperm(apply(excprovsim[,,-1], 1:2, quantile, c(2.5,97.5)/100), c(2,3,1))
  
  # STORE EXCESS MORTALITY - BY REGION IN MAR-APR
  excreg[,"Mar-Apr", matcomb[k,1], matcomb[k,2], "Est"] <-
    rowSums(excregsim[,-c(1:3),1])
  excreg[,"Mar-Apr", matcomb[k,1], matcomb[k,2], -1] <- 
    t(apply(apply(excregsim[,-c(1:3),-1],c(1,3),sum), 1, quantile,
      c(2.5,97.5)/100))
  
  # STORE EXCESS MORTALITY - BY REGION AND PERIOD (REQUIRES PERMUTATION)
  excreg[,-1, matcomb[k,1], matcomb[k,2], "Est"] <- excregsim[,,1]
  excreg[,-1, matcomb[k,1], matcomb[k,2], -1] <- 
    aperm(apply(excregsim[,,-1], 1:2, quantile, c(2.5,97.5)/100), c(2,3,1))
  
  # STORE EXCESS MORTALITY - FULL COUNTRY IN MAR-APR
  excitaly["Mar-Apr", matcomb[k,1], matcomb[k,2], "Est"] <-
    sum(excitalysim[-c(1:3),1])
  excitaly["Mar-Apr", matcomb[k,1], matcomb[k,2], -1] <- 
    quantile(apply(excitalysim[-c(1:3),-1],2,sum), c(2.5,97.5)/100)
  
  # STORE EXCESS MORTALITY - FULL COUNTRY BY PERIOD 
  excitaly[-1, matcomb[k,1], matcomb[k,2], "Est"] <- excitalysim[,1]
  excitaly[-1, matcomb[k,1], matcomb[k,2], -1] <- 
    t(apply(excitalysim[,-1], 1, quantile, c(2.5,97.5)/100))
  
  # REDUCE DATA TO COVID AND DEFINE CATEGORIES (BE AWARE OF ORDER USING LEVELS)
  datadeath <- datamodel %>%
    filter(date>=startdate) %>%
    mutate(provcode = factor(rep(seqprov, each=length(seqperiod)), levels=seqprov),
      period = rep(seqperiod, length(seqprov)))
  
  # STORE TOTAL MORTALITY BY PROVINCE
  provperdeath <- with(datadeath, tapply(totdeath, list(provcode,period), sum))
  totprov[,-1, matcomb[k,1], matcomb[k,2]] <- provperdeath
  totprov[,"Mar-Apr", matcomb[k,1], matcomb[k,2]] <-
    rowSums(provperdeath[,-c(1:3)])
}

# AGGREGATE TOTAL MORTALITY BY REGION AND FULL COUNTRY
totreg <- apply(totprov, 2:4, tapply, regprovcode, sum)
totitaly <- apply(totreg, 2:4, sum)
