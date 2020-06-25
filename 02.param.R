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

# DEFINE START AND END DAY FOR POST-PERIOD
startdate <- dmy(01022020)
enddate <- dmy(30042020)

# DEFINE WEEK PERIODS FOR FEB-APR
seqpost <- seq(startdate, enddate, 1)
cutdate <- unique(c(startdate-1,seqpost[seqpost %in% tapply(seqpost, week(seqpost), last)]))
labperiod1 <- sapply(seq(length(cutdate)-1), function(i) 
  paste(paste0(day(cutdate[i]+1), month(cutdate[i]+1,lab=T)),
    paste0(day(cutdate[i+1]), month(cutdate[i+1],lab=T)), sep="-"))
labperiod2 <- paste("Week", unique(week(seqpost)))

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
