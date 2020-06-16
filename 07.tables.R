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
  summarize(tot=sum(tot, na.rm=T), 
    prov=length(unique(provcode)), 
    munic=length(unique(municcode)), 
    prop=length(unique(municcode[munictype==1]))/munic*100) %>%
  ungroup() %>%
  dplyr::select(-regcode)

# ADD THE TOTAL FOR COUNTRY
datatab1[nrow(datatab1)+1,] <- data.frame(regname="Italy", 
  tot=sum(datatab1$tot), prov=sum(datatab1$prov), munic=sum(datatab1$munic), 
  prop=sum(datatab1$munic*datatab1$prop)/sum(datatab1$munic))

# FORMAT AND NAMES
for(i in 2:4) datatab1[,i] <- formatC(datatab1[,i,drop=T], format="f", digits=0,
  big.mark=",")
datatab1[,5] <- paste0(formatC(datatab1[,5,drop=T], format="f", digits=1), "%")
names(datatab1) <- c("Region", "Deaths", "Provinces", "Municipalities", "Prop")

# SAVE
write.csv(datatab1, file="tables/tab1.csv", row.names=F)


################################################################################
# TABLE 2: EXCESS BY REGION

tab2list <- lapply(c("tot","male","female"), function(x) {
  
  # COMPUTE EXCESS AND TOTAL
  exc <- round(excreg[,"Mar-Apr",x,"All",])
  tot <- totreg[,"Mar-Apr",x,"All"]
  
  # ADD TOTAL FOR COUNTRY
  exc <- rbind(exc, round(excitaly["Mar-Apr",x,"All",]))
  tot <- c(tot, totitaly["Mar-Apr",x,"All"])
  rownames(exc)[nrow(exc)] <- "Italy"
  
  # PERCENTAGE EXCESS
  excper <- exc/tot*100
  
  # FORMAT
  tot <- formatC(tot, format="f", digits=0, big.mark=",")
  exc <- formatC(exc, format="f", digits=0, big.mark=",", zero.print="0")
  excper <- formatC(excper, format="f", digits=1, big.mark=",")
  
  # TABLE
  tab <- cbind(tot, paste0(exc[,1]," (",exc[,2]," to ", exc[,3],")"),
    paste0(excper[,1],"% (",excper[,2]," to ", excper[,3],")"))
  dimnames(tab) <- list(rownames(exc), paste(c("Death", "Excess N", "Excess %"),x))
  tab
})

datatab2 <- do.call(cbind, tab2list)

# SAVE
write.csv(datatab2, file="tables/tab2.csv", row.names=T)

