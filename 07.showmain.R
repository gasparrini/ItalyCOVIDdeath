################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# MAIN RESULTS
################################################################################

################################################################################
# ESTIMATES

# TOTAL NUMBER OF DEATHS IN ITALY IN 01MAR-15MAY 2020
(tot <- totitaly["01Mar-15May", "Total", "All ages"])

# ESTIMATED EXCESS IN THE SAME AREA AND PERIOD, WITH 95%eCI
(exc <- excitaly["01Mar-15May", "Total", "All ages",])

# IN PERCENTAGE, WITH 95%eCI
(exc/(tot-exc))*100

# EXCESS IN THE REGION OF LOMBARDY IN THE SAME PERIOD
excreg["Lombardia", "01Mar-15May", "Total", "All ages", "Est"]

# PERCENTAGE OF THE TOTAL EXCESS IN LOMBARDY, VENETO, AND EMILIA-ROMAGNA
sum(excreg[c("Lombardia","Veneto","Emilia-Romagna"), "01Mar-15May", "Total", 
  "All ages","Est"]) / exc["Est"] *100

# EXCESS DEATHS IN MEN AND WOMEN, WITH 95%eCI
(excsex <- excitaly["01Mar-15May", -1, "All ages", ])

# IN PERCENTAGE, WITH 95%eCI (NB: VECTORIZED OPERATION)
totsex <- totitaly["01Mar-15May", -1, "All ages"]
(excsex/(totsex-excsex))*100

# PERCENTAGE EXCESS MORTALITY IN PEOPLE LESS THAN 60 YEARS OLD, WITH 95%eCI
excless60 <- excitaly["01Mar-15May", "Total", "Less than 60",]
totless60 <- totitaly["01Mar-15May", "Total", "Less than 60"]
(excless60/(totless60-excless60))*100

# PERCENTAGE EXCESS MORTALITY IN ITALY IN THE WEEK 18-24 MARCH 2020, WITH 95%eCI
excweek <- excitaly["18Mar-24Mar", "Total", "All ages", ] 
totweek <- totitaly["18Mar-24Mar", "Total", "All ages"]
(excweek/(totweek-excweek))*100

# PERCENTAGE EXCESS MORTALITY ACROSS PROVINCES IN THE SAME WEEK
excweek <- excprov[, "18Mar-24Mar", "Total", "All ages", ] 
totweek <- totprov[, "18Mar-24Mar", "Total", "All ages"]
(excweek/(totweek-excweek))*100

################################################################################
# GRAPHS

