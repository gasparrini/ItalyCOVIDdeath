################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# SOME EXAMPLES OF RESULTS
################################################################################

# TOTAL NUMBER OF DEATHS IN ITALY AND A SELECTED PROVINCE IN MAR-APR 2020
(tot <- sum(subset(datamodel, date>=dmy("01032020"))$totdeath))
(totfl <- sum(subset(datamodel, date>=dmy("01032020") &
    provname=="Firenze")$totdeath))

# EXCESS MORTALITY IN THE SAME AREAS AND PERIOD
excitalysim["01Mar-30Apr", "est"]
excprovsim["Firenze", "01Mar-30Apr", "est"]

# EMPIRICAL CONFIDENCE INTERVALS
quantile(excitalysim["01Mar-30Apr",-1], c(2.5,97.5)/100)

# IN A PERCENTAGE SCALE
excitalysim["01Mar-30Apr","est"] / (tot-excitalysim["01Mar-30Apr","est"]) *100
excprovsim["Firenze","01Mar-30Apr","est"] /
  (totfl-excprovsim["Firenze","01Mar-30Apr","est"]) * 100

# EXCESS MORTALITY BY WEEK (FIRST AND LAST ONLY PARTIAL)
excitalysim[-1,"est"]

# THE SAME FIGURES CAN BE COMPUTED BY SEX AND AGE SUB-GROUPS
