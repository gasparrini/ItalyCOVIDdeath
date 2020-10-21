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
# OTHER PLOTS
################################################################################

################################################################################
# PLOT OF EXCESS RISK BY PROVINCE (WITH BLUPS)

# RE-RUN FOR ALL SEX-AGE
source("02.param.R")
source("03.prepdatamodel.R")
source("04.model.R")

# DERIVE PREDICTIONS (USING DLNM FUNCTIONS)
cppost <- crosspred(bpost, coef=coef(metapost), vcov=vcov(metapost),
  model.link="log", cen=0, by=1)

# PLOT PARAMETERS
pdf("graphs/excesstrendprov.pdf", height=5, width=8)
parold <- par(no.readonly=TRUE)
par(mar=c(4,4,1,1), las=1, mgp=c(2.5,1,0))

# PLOT THE LAYOUT
plot(cppost, type="n", ci="n", xaxt="n", yaxt="n", ylab="Excess mortality (%)", 
  xlab="Date", ylim=c(0,10))
axis(1,at=0:10*10, labels=format(startdate+0:10*10,"%d %b"), cex.axis=0.7)
axis(2,at=1:10, labels=(1:10-1)*100, cex.axis=0.8)

# ADD PROVINCE-SPECIFIC AND THEN POOLED
for(i in seq(stage1list))
  lines(cppost$predvar, exp(c(bpost %*% bluppost[[i]]$blup)),col=grey(0.8))
lines(cppost, col=2, ci="area", lwd=1.5, ci.arg=list(col=alpha("red",0.3)))
abline(h=1)
legend("topleft", c("Country-pooled","Province BLUPs"), col=c(2,grey(0.8)),
  lty=1, bty="n", inset=0.05)

# RESET AND SAVE
par(parold)
dev.off()

################################################################################
# SEASONALITY

# RUN META-ANALYSIS
coefseas <- t(sapply(stage1list, function(x) x$seas$coef))
Sseas <- lapply(stage1list, function(x) x$seas$vcov)
metaseas <- mixmeta(coefseas, Sseas)
blupseas <- blup(metaseas, vcov=T)

# DERIVE PREDICTIONS (USING DLNM FUNCTIONS)
bseas <- onebasis(1:366, fun="pbs", knots=kseas)
cpseas <- crosspred(bseas, coef=coef(metaseas), vcov=vcov(metaseas),
  model.link="log", cen=180, by=1)

# PLOT PARAMETERS
pdf("graphs/seasonality.pdf", height=5, width=8)
parold <- par(no.readonly=TRUE)
par(mar=c(4,4,1,1), las=1, mgp=c(2.5,1,0))

# PLOT THE LAYOUT
plot(cpseas, type="n", ci="n", ylab="RR", xlab="Day of the year",
  ylim=c(0.95,1.3))

# ADD PROVINCE-SPECIFIC AND THEN POOLED
for(i in seq(stage1list)) {
  cpseasi <- crosspred(bseas, coef=blupseas[[i]]$blup, vcov=blupseas[[i]]$vcov,
  model.link="log", cen=180, by=1)
  lines(cpseasi,col=grey(0.8))
}
lines(cpseas, col=2, ci="area", ci.arg=list(col=alpha("red",0.3)))
abline(h=1)
legend("top", c("Country-pooled","Province BLUPs"), col=c(2,grey(0.8)),
  lty=1, bty="n", inset=0.05)

# RESET AND SAVE
par(parold)
dev.off()

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
