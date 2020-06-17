################################################################################
# ANALYSIS ON THE EXCESS MORTALITY IN ITALY DURING COVID-19
################################################################################

################################################################################
# OUTPUT
################################################################################

################################################################################
# BY PROVINCE

# CREATE LIST OF DATASET

outprovlist <- lapply(seq(dim(excprov)[2]), function(i) {
  
  # COMPUTE TOTAL AND EXCESS (N AND %)
  exc <- excprov[,i,,,]
  tot <- totprov[,i,,]
  excper <- exc/(-exc+c(tot))*100
  
  # DEFINE TABLE
  dim <- dim(excprov)[-c(2,5)]
  dimnames <- dimnames(excprov)[-c(2,5)]
  tab <- data.frame(Province=rep(dimnames[[1]], each=prod(dim[-1])),
    Sex=rep(rep(dimnames[[2]], each=dim[3]), dim[1]),
    Age=rep(dimnames[[3]], prod(dim[-3])),
    Deaths=round(c(tot)),
    Excess=round(c(exc[,,,1])),
    Excess95eCIlow=round(c(exc[,,,2])),
    Excess95eCIhigh=round(c(exc[,,,3])),
    ExcessPer=round(c(excper[,,,1]), 1),
    ExcessPer95eCIlow=round(c(excper[,,,2]), 1),
    ExcessPer95eCIhigh=round(c(excper[,,,3]), 1)
  )
  
  # RETURN
  tab
})

# NAMES
names(outprovlist) <- dimnames(excprov)[[2]]

test <- list(tab1=outprovlist[[1]][1:3,],tab2=outprovlist[[2]][1:3,])

library(openxlsx)
write.xlsx(test, file = "output/test.xlsx")
write.xlsx(outprovlist, file = "output/test.xlsx")

library(writexl)
write_xlsx(test, "output/test2.xlsx")
write_xlsx(outprovlist, "output/test2.xlsx")


https://stackoverflow.com/questions/27713310/easy-way-to-export-multiple-data-frame-to-multiple-excel-worksheets

library(xlsx)
write.xlsx(dataframe1, file="filename.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(dataframe2, file="filename.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

require(openxlsx)
list_of_datasets <- list("Name of DataSheet1" = dataframe1, "Name of Datasheet2" = dataframe2)
write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")

library(writexl)
sheets <- list("sheet1Name" = sheet1, "sheet2Name" = sheet2) #assume sheet1 and sheet2 are data frames
write_xlsx(sheets, "path/to/location"




