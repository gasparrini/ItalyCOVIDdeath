################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# PLOTS
################################################################################

################################################################################
# MAP OF EXCESS MORTALITY (%) AFTER THE 1ST OF MARCH

# READ THE SHAPEFILE OF THE PROVINCES SIMPLIFY (SOLVE SOME ISSUES), REORDER
spprov <- st_as_sf(readRDS("province weighted centroids.Rds"))
spprov <- st_simplify(spprov, dTolerance=1000)
spprov <- spprov[match(seqprov, spprov$COD_PROV),]

# DEFINE CATEGORIES FOR MAPS
mapcomb <- data.frame(sex=c("tot","male","female")[c(1,2,3,rep(1,5))],
  agegr=rep(agegrlab, c(3,rep(1,5))), stringsAsFactors=F)
maplab <- data.frame(sex=c("Both sexes","Males","Females")[c(1,2,3,rep(1,5))],
  agegr=c(rep("All ages", 3), agegrlab[-1]),
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
    tm_polygons("excess", palette=col, breaks=breaks, midpoint=NA,
      legend.show=F) + 
    tm_layout(frame=F, title=paste(maplab[i,1], maplab[i,2], sep=" - "),
      title.position=c("center","bottom"), scale=0.8)
})

# ADD THE LEGEND
legend <- tm_shape(spprov) + 
    tm_polygons("excess", palette=col, breaks=breaks, title="Excess (%)",
      midpoint=NA, legend.height=0.8) + 
    tm_layout(frame=F, legend.only=T, legend.position=c("center","center"), scale=0.7)
maplist[9] <- list(legend)

# PUT TOGETHER
grid.newpage()
pushViewport(viewport(layout=grid.layout(3, 3)))
for(i in seq(maplist))
  print(maplist[[i]], vp=viewport(layout.pos.row=rep(1:3, each=3)[i],
    layout.pos.col=rep(1:3, 3)[i]))

################################################################################
# PERIOD-SPECIFIC EXCESS

# DEFINE AREAS (NORTH/CENTRAL/SOUTH/ISLAND) BY PROVINCE
area <- rep(rep(c("Northern","Central","Southern","Islands"), c(8,4,6,2)), 
  provrep)

# RE-RUN FOR ALL SEX-AGE AND RE-RUN META-ANALYSIS BY AREA, THEN PREDICT
source("02.param.R")
source("03.prepdatamodel.R")
source("04.model.R")
metapostarea <- update(metapost, . ~ 0 + area)
predpostarea <- predict(metapostarea, newdata=data.frame(area=unique(area)),
  vcov=T)

# SET LAYOUT AND PLOT PARAMETERS
layout(matrix(1:4),heights=c(1,1,1,0.3))
par(mar=c(0.2,4,0.5,1), las=1, mgp=c(2.5,1,0))

# DERIVE PREDICTIONS (USING DLNM FUNCTIONS) AND THE PLOT FOR ALL
cppost <- crosspred(bpost, coef=metalist[["tot/All"]]$coef,
  vcov=metalist[["tot/All"]]$vcov, model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", yaxt="n", ylab="Excess mortality (%)", 
  xlab="", ylim=c(0.8,2), frame.plot=F)
axis(2,at=4:10/10*2, labels=(4:10/10*2-1)*100, cex.axis=0.8)
lines(cppost, col=1, ci="area", lwd=1.5, ci.arg=list(col=alpha(grey(0.5),0.1)))

# THEN BY SEX
cppost <- crosspred(bpost, coef=metalist[["male/All"]]$coef,
  vcov=metalist[["male/All"]]$vcov, model.link="log", cen=0, by=1)
lines(cppost, col=4, ci="n", lwd=1.5, lty=2, ci.arg=list(col=alpha("blue",0.1)))
cppost <- crosspred(bpost, coef=metalist[["female/All"]]$coef,
  vcov=metalist[["female/All"]]$vcov, model.link="log", cen=0, by=1)
lines(cppost, col="orchid", ci="n", lwd=1.5, lty=4, ci.arg=list(col=alpha("orchid",0.1)))
legend("topleft", c("All","Males","Females"), col=c(1,4,"orchid"), lwd=1.5,
  lty=c(1,2,4), bty="n", inset=0.05, title="By sex")

# PLOT FOR ALL
cppost <- crosspred(bpost, coef=metalist[["tot/All"]]$coef,
  vcov=metalist[["tot/All"]]$vcov, model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", yaxt="n", ylab="Excess mortality (%)", 
  xlab="", ylim=c(0.8,2), frame.plot=F)
axis(2,at=4:10/10*2, labels=(4:10/10*2-1)*100, cex.axis=0.8)
lines(cppost, col=1, ci="area", lwd=1.5, ci.arg=list(col=alpha(grey(0.5),0.1)))

# THEN BY AGE GROUP
col <- colorRampPalette(c("green","red"))(length(agegrlab)-1)
for(i in seq(agegrlab[-1])) {
  ind <- paste("tot", agegrlab[i+1], sep="/")
  cppost <- crosspred(bpost, coef=metalist[[ind]]$coef,
    vcov=metalist[[ind]]$vcov, model.link="log", cen=0, by=1)
  lines(cppost, col=col[i], ci="n", lwd=1.5, lty=i+1)
}
legend("topleft", agegrlab, col=c(1,col), lwd=1.5, lty=seq(agegrlab), bty="n",
  inset=0.05, title="By age")

# PLOT FOR ALL
cppost <- crosspred(bpost, coef=metalist[["tot/All"]]$coef,
  vcov=metalist[["tot/All"]]$vcov, model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", yaxt="n", ylab="Excess mortality (%)", 
  xlab="", ylim=c(0.8,2), frame.plot=F)
axis(2,at=4:10/10*2, labels=(4:10/10*2-1)*100, cex.axis=0.8)
lines(cppost, col=1, ci="area", lwd=1.5, ci.arg=list(col=alpha(grey(0.5),0.1)))

# THEN BY AREA
col <- c("blue","red","orange","brown")
for(i in 1:4) {
  cppost <- crosspred(bpost, coef=predpostarea[[i]]$fit,
    vcov=predpostarea[[i]]$vcov, model.link="log", cen=0, by=1)
  lines(cppost, col=col[i], ci="n", lwd=1.5, lty=i+1)
}
legend("topleft", c("All",unique(area)), col=c(1,col), lwd=1.5, lty=1:5, bty="n",
  inset=0.05, title="By area")

# X AXIS
par(mar=c(4,4,0,1),mgp=c(2.5,1,0))
plot(cppost$predvar, rep(0,length(cppost$predvar)), type="n", axes=F, ylab="",
    xlab="Date", frame.plot=F)
axis(1,at=0:10*10, labels=format(startdate+0:10*10,"%d %b"), cex.axis=0.7)
