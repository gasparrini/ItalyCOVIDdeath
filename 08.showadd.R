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
# ADDITIONAL RESULTS
################################################################################

################################################################################
# ESTIMATES IN PEOPLE LESS THAN 50 YEARS OLD

# RE-RUN FOR THIS AGE GROUP
source("02.param.R")
agegrsel <- 1:10
source("03.prepdatamodel.R")
source("04.model.R")

# EXCESS MORTALITY IN 15FEB-15MAY 2020
excitalysim["15Feb-15May", "est"]
quantile(excitalysim[1,], c(2.5,97.5)/100)

# EXCESS MORTALITY BY WEEK
excitalysim[-1, "est"]

################################################################################
# ADJUSTING FOR FLU

# RE-RUN WITH DIFFERENT FORMULA
source("02.param.R")
mformula <- y ~ bpost + date + bseas + factor(wday(date)) + cbtmean + flu013
source("03.prepdatamodel.R")
source("04.model.R")

# EXCESS MORTALITY IN 15FEB-15MAY 2020
excitalysim["15Feb-15May", "est"]
quantile(excitalysim[1,], c(2.5,97.5)/100)
