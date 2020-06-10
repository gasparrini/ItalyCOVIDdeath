################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# PLOTS
################################################################################

par(las=1)

################################################################################
# PLOT OF EXCESS RISK

# RE-RUN FOR WHOLE SEX-AGE
source("02.param.R")
source("03.prepdatamodel.R")
source("04.model.R")

cppost <- crosspred(bpost, coef=coef(metapost), vcov=vcov(metapost),
  model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", yaxt="n", ylab="Excess mortality (%)", 
  xlab="Date", ylim=c(0,10), main="Excess mortality in the COVID period")
axis(1,at=0:10*10, labels=format(startdate+0:10*10,"%d %b"), cex.axis=0.7)
axis(2,at=1:10, labels=(1:10-1)*100, cex.axis=0.8)
for(i in seq(stage1list))
  lines(cppost$predvar, exp(c(bpost %*% bluppost[[i]]$blup)),col=grey(0.8))
lines(cppost, col=2, ci="area", ci.arg=list(col=alpha("red",0.3)))
abline(h=1)
mtext("Pooled and by province")

################################################################################
# MAP OF EXCESS MORTALITY (%) AFTER THE 1ST OF MARCH

# READ THE SHAPEFILE OF THE PROVINCES SIMPLIFY (SOLVE SOME ISSUES), REORDER
spprov <- st_as_sf(readRDS("province weighted centroids.Rds"))
spprov <- st_simplify(spprov, dTolerance=1000)
spprov <- spprov[match(seqprov, spprov$COD_PROV),]

# DEFINE CATEGORIES FOR MAPS
mapcomb <- data.frame(sex=c("tot","male","female")[c(1,2,3,1,1,1)],
  agegr=rep(agegrlab, c(3,1,1,1)), stringsAsFactors=F)
maplab <- data.frame(sex=c("Both sexes","Males","Females")[c(1,2,3,1,1,1)],
  agegr=c(rep("All ages", 3), paste(agegrlab[-1], "years old")),
  stringsAsFactors=F)

# BREAKS AND PALETTE
breaks <- c(-Inf, 0, 20, 50, 100, 150, 200, 300, 400, 500, Inf)
col <- colorRampPalette(c("yellow","red","purple3"))(10)

# RUN THE LOOP
maplist <- lapply(seq(nrow(mapcomb)), function(i) {
  
  # EXTRACT ESTIMATES AND COMPUTE THE EXCESS IN PERCENTAGE, LINK IT
  exc <- excprov[,"Mar-Apr",mapcomb[i,1],mapcomb[i,2],"Est"]
  tot <- totprov[,"Mar-Apr",mapcomb[i,1],mapcomb[i,2]]
  spprov$excess <- exc/(tot-exc)*100
  
  # MAP
  tm_shape(spprov) + 
    tm_polygons("excess", palette=col, breaks=breaks, title="Excess (%)", midpoint=NA) + 
    tm_layout(frame=F, title=paste(maplab[i,1], maplab[i,2], sep=" - "),
      title.position=c("center","bottom"), legend.height=0.35, title.size=0.8)
})

# PUT TOGETHER
pushViewport(viewport(layout=grid.layout(2, 3)))
for(i in seq(maplist))
  print(maplist[[i]], vp=viewport(layout.pos.row=rep(1:2, each=3)[i],
    layout.pos.col=rep(1:3, 2)[i]))

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

# RUN META-ANALYSIS
coefseas <- t(sapply(stage1list, function(x) x$seas$coef))
Sseas <- lapply(stage1list, function(x) x$seas$vcov)
metaseas <- mixmeta(coefseas, Sseas)
blupseas <- blup(metaseas, vcov=T)

# PLOT
bseas <- onebasis(1:366, fun="pbs", knots=kseas)
cpseas <- crosspred(bseas, coef=coef(metaseas), vcov=vcov(metaseas),
  model.link="log", cen=180, by=1)
plot(cpseas, type="n", ci="n", ylab="RR", 
  xlab="Day of the year", ylim=c(0.8,1.6), main="Seasonality")
for(i in seq(stage1list)) {
  cpseasi <- crosspred(bseas, coef=blupseas[[i]]$blup, vcov=blupseas[[i]]$vcov,
  model.link="log", cen=180, by=1)
  lines(cpseasi,col=grey(0.8))
}
lines(cpseas, col=2, ci="area", ci.arg=list(col=alpha("red",0.3)))
abline(h=1)
mtext("Pooled and by province")

################################################################################
# PERIOD-SPECIFIC EXCESS

# COMPUTE PERCENTAGE INCREASE AT COUNTRY LEVEL
excitaly/(-(excitaly-c(totitaly)))*100
