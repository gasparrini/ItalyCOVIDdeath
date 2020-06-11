################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
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

# PLOT PARAMETERS
layout(1)
par(mar=c(5,4,5,1)+0.1, las=1, mgp=c(2.5,1,0))

# DERIVE PREDICTIONS (USING DLNM FUNCTIONS) AND THE PLOT LAYOUT
cppost <- crosspred(bpost, coef=coef(metapost), vcov=vcov(metapost),
  model.link="log", cen=0, by=1)
plot(cppost, type="n", ci="n", xaxt="n", yaxt="n", ylab="Excess mortality (%)", 
  xlab="Date", ylim=c(0,10), main="Excess mortality in the COVID period")
axis(1,at=0:10*10, labels=format(startdate+0:10*10,"%d %b"), cex.axis=0.7)
axis(2,at=1:10, labels=(1:10-1)*100, cex.axis=0.8)

# ADD PROVINCE-SPECIFIC AND THEN POOLED
for(i in seq(stage1list))
  lines(cppost$predvar, exp(c(bpost %*% bluppost[[i]]$blup)),col=grey(0.8))
lines(cppost, col=2, ci="area", ci.arg=list(col=alpha("red",0.3)))
abline(h=1)
mtext("Pooled and by province")

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
