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

# LOAD THE PACKAGES
library(curl) ; library(data.table)
library(dplyr) ; library(lubridate)
library(dlnm) ; library(mixmeta) ; library(tsModel)
library(splines) ; library(pbs) 
library(scales)
library(sp) ; library(tmap) ; library(GISTools) ; library(sf)
library(ggplot2) ; library(grid)
library(openxlsx)
