################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# SET THE PARAMETERS FOR THE ANALYSIS (TO BE CHANGED FOR SUB-ANALYSES)
################################################################################

# SELECT TOTAL OR SEX-SPECIFIC DEATHS
ysel <- "tot"
sexlab <- c("Total", "Males", "Females")
sexlist <- c("tot","male","female")
names(sexlist) <- sexlab

# SELECT AGE, THEN DEFINE AGE AGGREGATION GROUPS
agegrsel <- 0:21
agegrlab <- c("All ages","Less than 60","60-69","70-79","80-89","90 and older")
agegrlist <- list(0:21,0:12,12:14,15:16,17:18,19:21)
names(agegrlist) <- agegrlab

# DEFINE STARTING DAY FOR POST-PERIOD
startdate <- dmy(01022020)

# DEFINE BREAKS FOR FEB-APR (LEFT-OPEN)
cutdate <- c(dmy(paste0(c(outer(c("01","11","21"), c("02","03","04"), paste0)),
  "2020"))-1, dmy("30042020"))
labperiod <- sapply(2:length(cutdate), function(i) 
  paste(paste(day(cutdate[c(i-1,i)]+1:0), collapse="-"), 
    month(cutdate[i], lab=T)))

# DEFINE THE KNOTS FOR THE SPLINES FOR MODELLING EXCESS IN POST-PERIOD
nkpost <- 4

# DEFINE THE DF FOR THE CYCLIC SPLINE FOR SEASONALITY
dfseas <- 5

# DEFINE PARAMETERS OF CROSS-BASIS FOR TEMPERATURE
lagtmean <- 21
kpertmean <- c(10,75,90)
nklagtmean <- 3

# DEFINE LAG FOR FLU
lagflu <- 14

# MODEL FORMULA
mformula <- y ~ bpost + date + bseas + factor(wday(date)) + cbtmean

# NUMBER OF RESAMPLING ITERATIONS FOR EMPIRICAL CI
nsim <- 1000
