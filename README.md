## ItalyCOVIDdeath
Analysis of excess mortality in Italy during the COVID-19 pandemic

--------------------------------------------------------------------------------

This repository stores the R code, data, and full results presented in the article:

Scortichini M, Schneider dos Santos R, De' Donato F, De Sario M, Michelozzi P, Davoli M, Masselot P, Sera F, Gasparrini A. Excess mortality during the COVID-19 outbreak in Italy: a two-stage interrupted time series analysis. *International Journal of Epidemiology*. 2020;49(6):1909.1917. DOI: 10.1093/ije/dyaa169 [[freely available here](http://www.ag-myresearch.com/2020_scortichini_ije.html)]

The work was supported by the Medical Research Council-UK (Grant ID: MR/M022625/1) and the European Union’s Horizon 2020 Project Exhaustion (Grant ID: 820655).

### Full results by geographical area, sex, age groups, and period

The full set of results, including number and fraction of excess deaths (with 95\%eCI) by geographical aggregation (provinces, region, and full country), sex, age groups, and period (15th of February - 15th of May 2020, and then by week starting from the 1st of February) is provided in the [output folder](https://github.com/gasparrini/ItalyCOVIDdeath/tree/master/output) of this repository.

### Shiny app

A Shiny app (still under development) to visualize the results in maps and other graphs is available [here](https://mscortichini.shinyapps.io/app20200703/).

### R code and data for fully replicable analysis

The R scripts in this repository can be used to fully replicate the analysis and results illustrated in the article. The code can be used to download the original data from this [webpage](https://www.istat.it/it/archivio/240401) in the ISTAT website (update of 18 June), and then to perform all the steps of the analysis. The scripts are expected to be run in their order. Specifically:

  * *00.pkg.R*  loads the R packages
  * *01.prepdatafull.R* downloads and unzip the data in the folder `data`, and prepares the dataset by reshaping it, creating time variables, and ordering
  * *02.param.R* defines the sub-groups and the modelling parameters for the analysis
  * *03.prepdatamodel.R* creates the final dataset by selecting the data and aggregating province-specific series
  * *04.model.R* runs the two-stage model
  * *05.showexample.R* illustrates how to extract the main results and produce a map
  * *06.storeall.R* performs the analysis for all the combinations of geographical area, sex, and age group, and stores them in arrays
  * *07.showmain.R* shows the estimates reported in the paper
  * *08.showadd.R* shows additional results
  * *09.tables.R* produces the tables of the article and store them in the folder `tables`
  * *10.plots.R* produces the graphs included in the article and store them in the folder `graphs`
  * *11.output.R* produces the final output of the analysis as excel files with all the estimates and store them in the folder `output`
  * *12.otherplot.R* produces other graphs





