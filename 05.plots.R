################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# PLOTS
################################################################################

par(las=1)

################################################################################
# PLOT OF EXCESS RISK

cpcovid <- crosspred(bcovid, coef=coef(metacovid), vcov=vcov(metacovid),
  model.link="log", cen=0, by=1)
plot(cpcovid, type="n", ci="n", xaxt="n", yaxt="n", ylab="Excess mortality (%)", 
  xlab="Date", ylim=c(0,10), main="Pooled and blups")
axis(1,at=0:10*10, labels=format(startdate+0:10*10,"%d %b"), cex.axis=0.7)
axis(2,at=1:10, labels=(1:10-1)*100, cex.axis=0.8)
for(i in seq(stage1list))
  lines(cpcovid$predvar, exp(c(bcovid %*% blupcovid[[i]]$blup)),col=grey(0.8))
lines(cpcovid, col=2, ci="area", ci.arg=list(col=alpha("red",0.3)))
abline(h=1)

################################################################################
# MAP OF ATTRIBUTABLE FRACTION OF DEATHS AFTER THE 1ST OF MARCH

# READ THE SHAPEFILE OF THE PROVINCES SIMPLIFY (SOLVE SOME ISSUES), REORDER
spprov <- st_as_sf(readRDS("province weighted centroids.Rds"))
spprov <- st_simplify(spprov, dTolerance=1000)
spprov <- spprov[match(seqprov, spprov$COD_PROV),]

# COMPUTE THE % EXCESS 
apply(excarray[,,1],1,sum)

TO BE FINISHED

# MAP
spprov$excess <- excperiod$excess
spprov$af <- excperiod$af

tm_shape(spprov) + 
  tm_polygons("excess", palette="Reds", n=10, title="Excess (%)") + 
  tm_layout(frame=F)
tm_shape(spprov) + 
  tm_polygons("af", palette="Reds", n=10, title="AF (%)") + 
  tm_layout(frame=F)

################################################################################
# ANALYSIS OF RESIDUALS

# DERIVE RESIDUALS
datamodel$res <- unlist(lapply(stage1list,"[[","residuals"))

# PLOT IN 2020
datamodel %>%
  filter(year(date)==2020) %>%
  ggplot(aes(date, res)) + 
  geom_point(col=grey(0.9)) + 
  geom_smooth() + 
  theme_light()

################################################################################
# SEASONALITY

coef <- t(sapply(stage1list, function(x) x$seas$coef))
S <- lapply(stage1list, function(x) x$seas$vcov)
metaseas <- mixmeta(coef, S)
blupseas <- blup(metaseas, vcov=T)

# TO BE FINISHED
