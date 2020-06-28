################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# SOME EXAMPLES OF RESULTS
################################################################################

# TOTAL NUMBER OF DEATHS IN ITALY AND A SELECTED PROVINCE IN 15Feb-15MAY 2020
(tot <- sum(subset(datamodel, date>=dmy("15022020"))$totdeath))
(totfl <- sum(subset(datamodel, date>=dmy("15022020") &
  provname=="Firenze")$totdeath))

# EXCESS MORTALITY IN THE SAME AREAS AND PERIOD
excitalysim["15Feb-15May", "est"]
excprovsim["Firenze", "15Feb-15May", "est"]

# EMPIRICAL CONFIDENCE INTERVALS
quantile(excitalysim["15Feb-15May",-1], c(2.5,97.5)/100)

# IN A PERCENTAGE SCALE
excitalysim["15Feb-15May","est"] / (tot-excitalysim["15Feb-15May","est"]) *100
excprovsim["Firenze","15Feb-15May","est"] /
  (totfl-excprovsim["Firenze","15Feb-15May","est"]) * 100

# EXCESS MORTALITY BY WEEK (FIRST AND LAST ONLY PARTIAL)
excitalysim[-1,"est"]

# THE SAME FIGURES CAN BE COMPUTED BY SEX AND AGE SUB-GROUPS
