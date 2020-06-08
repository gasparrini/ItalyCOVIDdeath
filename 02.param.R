################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# SET THE PARAMETERS FOR THE ANALYSIS (TO BE CHANGED FOR SUB-ANALYSES)
################################################################################

# SELECT TOTAL OR SEX-SPECIFIC DEATHS
ysel <- "tot"

# SELECT AGE GROUPS
agegrsel <- 0:21

# DEFINE COVID STARTING DAY
startdate <- dmy(01022020)

# DEFINE BREAKS FOR COVID PERIOD (LEFT-OPEN)
cutdate <- c(dmy(paste0(c(outer(c("01","11","21"), c("02","03","04"), paste0)),
  "2020"))-1, dmy("30042020"))

# DEFINE THE KNOTS FOR THE SPLINES FOR MODELLING THE EXCESS
nkcovid <- 4

# DEFINE THE DF FOR THE CYCLIC SPLINE FOR SEASONALITY
dfseas <- 5

# NUMBER OF RESAMPLING ITERATIONS FOR EMPIRICAL CI
nsim <- 1000
