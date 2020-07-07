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

################################################################################
# MAP

# READ THE SHAPEFILE OF THE PROVINCES SIMPLIFY (SOLVE SOME ISSUES), REORDER
spprov <- st_as_sf(readRDS("data/province weighted centroids.Rds"))
spprov <- st_simplify(spprov, dTolerance=1000)
spprov <- spprov[match(seqprov, spprov$COD_PROV),]

# BREAKS AND PALETTE
breaks <- c(-Inf, 0, 20, 50, 100, 150, 200, 300, 400, 500, Inf)
col <- colorRampPalette(c("yellow","red","purple3"))(10)

# EXTRACT ESTIMATES AND COMPUTE THE EXCESS IN PERCENTAGE
exc <- excprovsim[,1,1]
tot <- with(subset(datamodel, date>=coviddate),
  tapply(totdeath, factor(provcode, levels=unique(provcode)), sum))
spprov$excess <- exc/(tot-exc)*100

# MAP FOR BOTH SEXES AND ALL AGES
tm_shape(spprov) + 
  tm_polygons("excess", palette=col, breaks=breaks, midpoint=NA) + 
  tm_layout(frame=F, title="Both sexes - All ages",
    title.position=c("center","bottom"))

