
## ======================================================================
## R code to create the data product time series used in the article
##
## P. F. Craigmile and P. Guttorp,
## A combined estimate of global temperature
##
## Requires the ncdf4 R library to be installed.
##
## Contact: pfc@stat.osu.edu
## ======================================================================

library(ncdf4)

source("functions/subset_and_anom.R")
source("functions/extract_nc_date.R")


## ======================================================================
## Berkeley
##
##  "Uncertainties represent the 95% confidence interval for
##  statistical and spatial undersampling effects as well as ocean
##  biases."
##
## We recommended dividing the uncertainty values by 1.96 to obtain
## the standard error for the global mean temperatures.
## ======================================================================

## Variables:
##       Land + Ocean anomaly using air temperature above sea ice        Land + Ocean using water temperature below sea ice
## Year, Annual Anomaly, Annual Unc., Five-year Anomaly, Five-year Unc., Annual Anomaly, Annual Unc., Five-year Anomaly, Five-year Unc.

Berkeley.df <- read.table("Berkeley/Land_and_Ocean_summary.txt",
                 skip=48)

Berkeley <- subset.and.anom(years    = Berkeley.df[,1],
                            the.mean = Berkeley.df[,2],
                            the.vars = (Berkeley.df[,3]/1.96)^2)


## ======================================================================
## HadCRUT 5.0.1.0
## ======================================================================

df <- nc_open("HadCRUT.5.0.1.0/HadCRUT.5.0.1.0.analysis.summary_series.global.annual.nc")

lower <- ncvar_get(df, "tas_lower")
upper <- ncvar_get(df, "tas_upper")

tas <- ncvar_get(df, "tas_mean")

years <- as.numeric(substr(extract.nc.date(df), 1, 4))

HadCRUT5.vars <- ((upper - lower) / (1.96*2))^2

HadCRUT5 <- subset.and.anom(years    = years,
                            the.mean = tas,
                            the.vars = HadCRUT5.vars)


## ======================================================================
## NOAA
##
## Annual data (aravg.ann.*) :
## 1st column = year
## 2nd column = anomaly of temperature (K)
## 3rd column = total error variance (K**2)
## 4th column = high-frequency error variance (K**2)
## 5th column = low-frequency error variance (K**2)
## 6th column = bias error variance (K**2)
##
## NOTE:
## 4+5+6 = 3, except for the last year
## ======================================================================

NOAA.df <- read.table("NOAA/aravg.ann.land_ocean.90S.90N.v5.0.0.202012.asc.txt")

NOAA <- subset.and.anom(years    = NOAA.df[,1],
                        the.mean = NOAA.df[,2],
                        ##the.vars = NOAA.df[,4]) ## high freq
                        ##the.vars = NOAA.df[,5]) ## low freq
                        ##the.vars = NOAA.df[,6]) ## bias
                        the.vars = NOAA.df[,4]+NOAA.df[,5]+NOAA.df[,6])


## ======================================================================
## GISS
##
## We calculate the variances of the annual values using a time series
## analysis of the monthly values.
## ======================================================================

GISS.df <- read.csv("GISS/totalCI_ERA.csv")

GISS <- subset.and.anom(years    = GISS.df[,1],
                        the.mean = GISS.df[,2],
                        the.vars = (GISS.df[,3] / (1.96*2))^2)




## ======================================================================
## Japan - JMA
## ======================================================================

JMA <- read.csv("JMA/year_wld.csv")

years <- JMA[,1]
range(years)

JMA.mean <- JMA[,2]

JMA.sds <- read.csv("JMA/JMA_SDs.csv")[,2]

JMA.initial <- subset.and.anom(years     = years,
                               the.mean = JMA.mean,
                               the.vars = JMA.sds^2)

## ======================================================================
## Now build the longer series for JMA (hard wired for JMA years - if
## the dataset changes, this code has to change!)
## ======================================================================

JMA <- list(years    = Berkeley$years,
            the.mean = c(rep(NA,11), JMA.initial$the.mean),
            the.vars = c(rep(NA,11), JMA.initial$the.vars))




## ======================================================================
## Now combine the datasets and save to a file
## ======================================================================

years <- Berkeley$years

global.anoms <- cbind(Berkeley$the.mean,
                      HadCRUT5$the.mean,
                      NOAA$the.mean,
                      GISS$the.mean,
                      JMA$the.mean)

global.vars <- cbind(Berkeley$the.vars,
                     HadCRUT5$the.vars,
                     NOAA$the.vars,
                     GISS$the.vars,
                     JMA$the.vars)

data.prods.labels <- c("Berkeley",
                       "HadCRUT5",
                       "NOAA",
                       "GISS",
                       "JMA")

data.prods <- list(years        = years,
                   global.anoms = global.anoms,
                   global.vars  = global.vars,
                   labels       = data.prods.labels)

save(data.prods,
     file="Derived_Data/data_products.RData")
