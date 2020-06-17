################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# TABLES
################################################################################

################################################################################
# TABLE 1: DESCRIPTIVE BY REGION

# CREATE THE MAIN TABLE
datatab1 <- datafull %>%
  group_by(regcode, regname) %>%
  summarize(prov=length(unique(provcode)), 
    munic=length(unique(municcode)), 
    tot=sum(tot, na.rm=T), 
    prop=length(unique(municcode[munictype==1]))/munic*100) %>%
  ungroup() %>%
  dplyr::select(-regcode)

# ADD AREA
datatab1 <- cbind(datatab1[,1],Area=rep(c("North","Central","South","Islands"),
  c(8,4,6,2)),datatab1[,-1])


# ADD THE TOTAL FOR COUNTRY
datatab1[nrow(datatab1)+1,] <- data.frame(regname="Italy", "",
  prov=sum(datatab1$prov), munic=sum(datatab1$munic), tot=sum(datatab1$tot), 
  prop=sum(datatab1$munic*datatab1$prop)/sum(datatab1$munic))

# FORMAT AND NAMES
for(i in 3:5) datatab1[,i] <- formatC(datatab1[,i,drop=T], format="f", digits=0,
  big.mark=",")
datatab1[-nrow(datatab1),1] <- labreg
datatab1[,6] <- paste0(formatC(datatab1[,6,drop=T], format="f", digits=1), "%")
names(datatab1) <- c("Region","Area", "Provinces", "Municipalities", "Deaths",
  "Prop")

# SAVE
write.csv(datatab1, file="tables/tab1.csv", row.names=F)

################################################################################
# TABLE 2: EXCESS BY REGION

tab2list <- lapply(c("tot","male","female"), function(x) {
  
  # COMPUTE EXCESS AND TOTAL
  exc <- excreg[,"Mar-Apr",x,"All",]
  tot <- totreg[,"Mar-Apr",x,"All"]
  
  # ADD TOTAL FOR COUNTRY
  exc <- rbind(exc, round(excitaly["Mar-Apr",x,"All",]))
  tot <- c(tot, totitaly["Mar-Apr",x,"All"])
  rownames(exc)[nrow(exc)] <- "Italy"

  # FORMAT
  tot <- formatC(tot, format="f", digits=0, big.mark=",")
  exc <- formatC(exc, format="f", digits=0, big.mark=",", zero.print="0")

  # TABLE
  tab <- cbind(tot, paste0(exc[,1]," (",exc[,2]," to ", exc[,3],")"))
  dimnames(tab) <- list(rownames(exc), paste(c("Death", "Excess"),x))
  tab
})

datatab2 <- do.call(cbind, tab2list)

# SAVE
write.csv(datatab2, file="tables/tab2.csv", row.names=T)

