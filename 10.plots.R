################################################################################
# Updated version of the R code for the analysis in:
#
#  "Scortichini M, Schneider Dos Santos R, De' Donato F, De Sario M,
#    Michelozzi P, Davoli M, Masselot P, Sera F, Gasparrini A.
#    Excess mortality during the COVID-19 outbreak in Italy: a two-stage
#    interrupted time-series analysis.
#    International Journal of Epidemiology. 2020. DOI: 10.1093/ije/dyaa169."
#  http://www.ag-myresearch.com/2020_scortichini_ije.html
#
# Update: 21 October 2020
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/ItalyCOVIDdeath
################################################################################

################################################################################
# PLOTS
################################################################################

################################################################################
# EXCESS BY SEX AND AGE GROUP

# EXTRACT ESTIMATES AND COMPUTE THE EXCESS IN PERCENTAGE
# NB: TRICK FOR EXPLOITING THE ORDER AND DIMENSIONS OF ARRAYS
exc <- excitaly[1,,,]
tot <- totitaly[1,,]
excper <- exc/(-exc+c(tot))*100

pdf("graphs/excessagesex.pdf", height=4, width=6)
parold <- par(no.readonly=TRUE)
par(mar=c(2,4,1,1), las=1, mgp=c(2.5,1,0))

plot(seq(dim(excper)[2]), type="n", xaxt="n", cex.axis=0.8, lab=c(5,7,7), xlab="",
  ylab="Excess mortality (%)", xlim=c(0,dim(excper)[2])+0.5, 
  ylim=range(excper)+diff(range(excper))/8*c(-1,1), frame.plot=F)
axis(1, at=seq(dim(excper)[2]), labels=agegrlab, lwd=0, line=-2, cex.axis=0.8)
title(xlab="Age group", mgp=c(0.5,1,0))
xval <- rep(seq(dim(excper)[2]), each=3) + -1:1*0.2
arrows(xval, c(excper[,,2]), xval, c(excper[,,3]), length=0.02, angle=90, code=3)
points(xval, c(excper[,,1]), pch=21:23, bg=c(grey(0.3),"blue","orchid"), cex=1.3)
abline(h=0)
legend("top", c("Both","Males","Females"), ncol=3, bty="n", pt.cex=1.3,
  pch=21:23, pt.bg=c(grey(0.3),"blue","orchid"))

# RESET AND SAVE
par(parold)
dev.off()

################################################################################
# MAP OF EXCESS MORTALITY (%) AFTER THE 1ST OF MARCH

# DEFINE CATEGORIES FOR MAPS
mapcomb <- data.frame(sex=sexlab[c(1,2,3,rep(1,5))],
  agegr=rep(agegrlab, c(3,rep(1,5))), stringsAsFactors=F)
maplab <- data.frame(sex=sexlab[c(1,2,3,rep(1,5))],
  agegr=agegrlab[c(rep(1,3), 2:length(agegrlab))], stringsAsFactors=F)

# RUN THE LOOP
maplist <- lapply(seq(nrow(mapcomb)), function(i) {
  
  # EXTRACT ESTIMATES AND COMPUTE THE EXCESS IN PERCENTAGE, LINK IT
  exc <- excprov[,1,mapcomb[i,1],mapcomb[i,2],"Est"]
  tot <- totprov[,1,mapcomb[i,1],mapcomb[i,2]]
  spprov$excess <- exc/(tot-exc)*100
  
  # MAP
  tm_shape(spprov) + 
    tm_polygons("excess", palette=col, breaks=breaks, midpoint=NA,
      legend.show=F) + 
    tm_layout(frame=F, title=paste(maplab[i,1], maplab[i,2], sep=" - "),
      title.position=c("center","bottom"), scale=0.7)
})

# ADD THE LEGEND
legend <- tm_shape(spprov) + 
    tm_polygons("excess", palette=col, breaks=breaks, title="Excess (%)",
      midpoint=NA, legend.height=0.8) + 
    tm_layout(frame=F, legend.only=T, legend.position=c("center","center"), scale=0.9)
maplist[9] <- list(legend)

# PUT TOGETHER
pdf("graphs/map.pdf", height=10, width=7)
grid.newpage()
pushViewport(viewport(layout=grid.layout(3, 3)))
for(i in seq(maplist))
  print(maplist[[i]], vp=viewport(layout.pos.row=rep(1:3, each=3)[i],
    layout.pos.col=rep(1:3, 3)[i]))
dev.off()

################################################################################
# PERIOD-SPECIFIC EXCESS

# RE-RUN FOR ALL SEX-AGE AND RE-RUN META-ANALYSIS BY AREA, THEN PREDICT
source("02.param.R")
source("03.prepdatamodel.R")
source("04.model.R")
metapostarea <- update(metapost, . ~ 0 + areaprov)
predpostarea <- predict(metapostarea, newdata=data.frame(areaprov=unique(areaprov)),
  vcov=T)

# SET LAYOUT AND PLOT PARAMETERS
pdf("graphs/excesstrend.pdf", height=7, width=5)
parold <- par(no.readonly=TRUE)
layout(matrix(1:4), heights=c(1,1,1,0.3))
par(mar=c(0.2,4,0.5,1), las=1, mgp=c(2.5,1,0))

# DERIVE PREDICTIONS (USING DLNM FUNCTIONS) AND THE PLOT FOR ALL
cppost <- crosspred(bpost, coef=metalist[[1]]$coef, vcov=metalist[[1]]$vcov,
  model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", ylab="Relative risk", 
  xlab="", ylim=c(0.8,2.18), frame.plot=F, cex.axis=0.9)
lines(cppost, col=1, ci="area", lwd=1.5, ci.arg=list(col=alpha(grey(0.5),0.1)))
abline(v=dmy("11032020")-startdate, lty=2)

# THEN BY SEX
ind <- which(matcomb[,1]=="Males" & matcomb[,2]=="All ages")
cppost <- crosspred(bpost, coef=metalist[[ind]]$coef, vcov=metalist[[ind]]$vcov,
  model.link="log", cen=0, by=1)
lines(cppost, col=4, ci="n", lwd=1.5, lty=2, ci.arg=list(col=alpha("blue",0.1)))
ind <- which(matcomb[,1]=="Females" & matcomb[,2]=="All ages")
cppost <- crosspred(bpost, coef=metalist[[ind]]$coef, vcov=metalist[[ind]]$vcov,
  model.link="log", cen=0, by=1)
lines(cppost, col="orchid", ci="n", lwd=1.5, lty=4, ci.arg=list(col=alpha("orchid",0.1)))
legend("topleft", sexlab, col=c(1,4,"orchid"), lwd=1.5,
  lty=c(1,2,4), bty="n", inset=0.05, title="By sex")

# PLOT FOR ALL
cppost <- crosspred(bpost, coef=metalist[[1]]$coef, vcov=metalist[[1]]$vcov,
  model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", ylab="Relative risk", 
  xlab="", ylim=c(0.8,2.18), frame.plot=F, cex.axis=0.9)
lines(cppost, col=1, ci="area", lwd=1.5, ci.arg=list(col=alpha(grey(0.5),0.1)))
abline(v=dmy("11032020")-startdate, lty=2)

# THEN BY AGE GROUP
col <- colorRampPalette(c("green","red"))(length(agegrlab)-1)
for(i in seq(agegrlab[-1])) {
  ind <- paste("Total", agegrlab[i+1], sep="/")
  cppost <- crosspred(bpost, coef=metalist[[ind]]$coef,
    vcov=metalist[[ind]]$vcov, model.link="log", cen=0, by=1)
  lines(cppost, col=col[i], ci="n", lwd=1.5, lty=i+1)
}
legend("topleft", agegrlab, col=c(1,col), lwd=1.5, lty=seq(agegrlab), bty="n",
  inset=0.05, title="By age")

# PLOT FOR ALL
cppost <- crosspred(bpost, coef=metalist[[1]]$coef, vcov=metalist[[1]]$vcov,
  model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", ylab="Relative risk", 
  xlab="", ylim=c(0.8,2.18), frame.plot=F, cex.axis=0.9)
lines(cppost, col=1, ci="area", lwd=1.5, ci.arg=list(col=alpha(grey(0.5),0.1)))
abline(v=dmy("11032020")-startdate, lty=2)

# THEN BY AREA
col <- c("blue","red","orange","brown")
for(i in 1:4) {
  cppost <- crosspred(bpost, coef=predpostarea[[i]]$fit,
    vcov=predpostarea[[i]]$vcov, model.link="log", cen=0, by=1)
  lines(cppost, col=col[i], ci="n", lwd=1.5, lty=i+1)
}
legend("topleft", c("Italy",unique(areareg)), col=c(1,col), lwd=1.5, lty=1:5,
  bty="n", inset=0.05, title="By area")

# X AXIS
par(mar=c(4,4,0,1),mgp=c(2.5,1,0))
plot(cppost$predvar, rep(0,length(cppost$predvar)), type="n", axes=F, ylab="",
    xlab="Date", frame.plot=F)
axis(1,at=0:11*10, labels=format(startdate+0:11*10,"%d %b"), cex.axis=0.9)

# RESET AND SAVE
layout(1)
par(parold)
dev.off()
