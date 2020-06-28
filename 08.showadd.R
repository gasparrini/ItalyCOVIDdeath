################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
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

# EXCESS MORTALITY IN 01MAR-15MAY 2020
excitalysim["01Mar-15May", "est"]
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

# EXCESS MORTALITY IN 01MAR-15MAY 2020
excitalysim["01Mar-15May", "est"]
quantile(excitalysim[1,], c(2.5,97.5)/100)
