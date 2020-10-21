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
# OUTPUT
################################################################################

################################################################################
# BY PROVINCE

# CREATE LIST OF DATASET

outprovlist <- lapply(seq(dim(excprov)[2]), function(i) {
  
  # COMPUTE TOTAL AND EXCESS (N AND %)
  exc <- excprov[,i,,,]
  tot <- totprov[,i,,]
  excper <- exc/(-exc+c(tot))*100
  excper[is.na(excper)] <- 0
  
  # DEFINE TABLE
  dim <- dim(excprov)[-c(2,5)]
  dimnames <- dimnames(excprov)[-c(2,5)]
  tab <- data.frame(Province=rep(dimnames[[1]], prod(dim[-1])),
    Region=rep(labreg[seqregprov], prod(dim[-1])),
    Area=rep(areareg[seqregprov], prod(dim[-1])),
    Sex=rep(rep(dimnames[[2]], dim[3]), each=dim[1]),
    Age=rep(dimnames[[3]], each=prod(dim[-3])))
  
  # ADD ESTIMATES (NB: EXPLOIT ORDER OF ARRAYS)
  tab$Deaths <- round(c(tot))
  tabest <- matrix(c(round(c(exc)), round(c(excper),1)),nrow=nrow(tab))
  colnames(tabest) <- c(t(outer(c("Excess","ExcessPer"), 
    c("","95eCIlow","95eCIhigh"),paste0)))
  tab <- cbind(tab,tabest)
  
  # ORDER
  for(i in c(1,4,5)) tab[,i] <- factor(tab[,i], levels=unique(tab[,i]))
  tab <- arrange(tab, Province, Sex, Age)

  # RETURN
  tab
})

# NAMES AND WRITE
names(outprovlist) <- dimnames(excprov)[[2]]
write.xlsx(outprovlist, file = "output/byprovince.xlsx")

################################################################################
# BY REGION

# CREATE LIST OF DATASET

outreglist <- lapply(seq(dim(excreg)[2]), function(i) {
  
  # COMPUTE TOTAL AND EXCESS (N AND %)
  exc <- excreg[,i,,,]
  tot <- totreg[,i,,]
  excper <- exc/(-exc+c(tot))*100
  excper[is.na(excper)] <- 0
  
  # DEFINE TABLE
  dim <- dim(excreg)[-c(2,5)]
  dimnames <- dimnames(excreg)[-c(2,5)]
  tab <- data.frame(Region=rep(dimnames[[1]], prod(dim[-1])),
    Area=rep(areareg, prod(dim[-1])),
    Sex=rep(rep(dimnames[[2]], dim[3]), each=dim[1]),
    Age=rep(dimnames[[3]], each=prod(dim[-3])))
  
  # ADD ESTIMATES (NB: EXPLOIT ORDER OF ARRAYS)
  tab$Deaths <- round(c(tot))
  tabest <- matrix(c(round(c(exc)), round(c(excper), 1)), nrow=nrow(tab))
  colnames(tabest) <- c(t(outer(c("Excess","ExcessPer"), 
    c("","95eCIlow","95eCIhigh"), paste0)))
  tab <- cbind(tab,tabest)
  
  # ORDER
  for(i in c(1,3,4)) tab[,i] <- factor(tab[,i], levels=unique(tab[,i]))
  tab <- arrange(tab, Region, Sex, Age)

  # RETURN
  tab
})

# NAMES AND WRITE
names(outreglist) <- dimnames(excreg)[[2]]
write.xlsx(outreglist, file = "output/byregion.xlsx")

################################################################################
# ITALY

# CREATE LIST OF DATASET

outitalylist <- lapply(seq(dim(excitaly)[1]), function(i) {
  
  # COMPUTE TOTAL AND EXCESS (N AND %)
  exc <- excitaly[i,,,]
  tot <- totitaly[i,,]
  excper <- exc/(-exc+c(tot))*100
  excper[is.na(excper)] <- 0
  
  # DEFINE TABLE
  dim <- dim(excitaly)[-c(1,4)]
  dimnames <- dimnames(excitaly)[-c(1,4)]
  tab <- data.frame(Sex=rep(dimnames[[1]], dim[2]),
    Age=rep(dimnames[[2]], each=dim[1]))
  
  # ADD ESTIMATES (NB: EXPLOIT ORDER OF ARRAYS)
  tab$Deaths <- round(c(tot))
  tabest <- matrix(c(round(c(exc)), round(c(excper), 1)), nrow=nrow(tab))
  colnames(tabest) <- c(t(outer(c("Excess","ExcessPer"), 
    c("","95eCIlow","95eCIhigh"), paste0)))
  tab <- cbind(tab,tabest)
  
  # ORDER
  for(i in 1:3) tab[,i] <- factor(tab[,i], levels=unique(tab[,i]))
  tab <- arrange(tab, Sex, Age)

  # RETURN
  tab
})

# NAMES AND WRITE
names(outitalylist) <- dimnames(excitaly)[[1]]
write.xlsx(outitalylist, file = "output/italy.xlsx")
