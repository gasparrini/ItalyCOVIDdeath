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
# MAIN RESULTS
################################################################################

################################################################################
# ESTIMATES

# TOTAL NUMBER OF DEATHS IN ITALY IN 15FEB-15MAY 2020
(tot <- totitaly["15Feb-15May", "Total", "All ages"])

# ESTIMATED EXCESS IN THE SAME AREA AND PERIOD, WITH 95%eCI
(exc <- excitaly["15Feb-15May", "Total", "All ages",])

# IN PERCENTAGE, WITH 95%eCI
(exc/(tot-exc))*100

# EXCESS IN THE REGION OF LOMBARDY IN THE SAME PERIOD
excreg["Lombardia", "15Feb-15May", "Total", "All ages", "Est"]

# PERCENTAGE OF THE TOTAL EXCESS IN LOMBARDY, VENETO, AND EMILIA-ROMAGNA
sum(excreg[c("Lombardia","Veneto","Emilia-Romagna"), "15Feb-15May", "Total", 
  "All ages","Est"]) / exc["Est"] *100

# EXCESS DEATHS IN MEN AND WOMEN, WITH 95%eCI
(excsex <- excitaly["15Feb-15May", -1, "All ages", ])

# IN PERCENTAGE, WITH 95%eCI (NB: VECTORIZED OPERATION)
totsex <- totitaly["15Feb-15May", -1, "All ages"]
(excsex/(totsex-excsex))*100

# PERCENTAGE EXCESS MORTALITY IN PEOPLE LESS THAN 60 YEARS OLD, WITH 95%eCI
excless60 <- excitaly["15Feb-15May", "Total", "Less than 60",]
totless60 <- totitaly["15Feb-15May", "Total", "Less than 60"]
(excless60/(totless60-excless60))*100

# PERCENTAGE EXCESS MORTALITY IN ITALY IN THE WEEK 18-24 MARCH 2020, WITH 95%eCI
excweek <- excitaly["18Mar-24Mar", "Total", "All ages", ] 
totweek <- totitaly["18Mar-24Mar", "Total", "All ages"]
(excweek/(totweek-excweek))*100

# PERCENTAGE EXCESS MORTALITY ACROSS PROVINCES IN THE SAME WEEK
excweek <- excprov[, "18Mar-24Mar", "Total", "All ages", ] 
totweek <- totprov[, "18Mar-24Mar", "Total", "All ages"]
(excweek/(totweek-excweek))*100
