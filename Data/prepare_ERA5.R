
## ======================================================================
## R code to create the ERA5 global average temperature anomaly.
##
## P. F. Craigmile and P. Guttorp,
## A combined estimate of global temperature
##
## Requires the ncdf4 R library to be installed.
##
## Contact: pfc@stat.osu.edu
## ======================================================================


source("functions/calc_area_by_lat.R")
source("functions/area_weighted_average.R")
source("functions/create_yearly.R")

library(ncdf4)

df <- nc_open("ERA5/adaptor.mars.internal-1629201992.8076582-8552-12-99fa969f-eed3-49d0-b1c9-2b4ba840bd3a.nc")


## units: hours since 1900-01-01 00:00:00.0
days <- ncvar_get(df, "time")/24

the.dates <- as.Date("1900-01-01") + days

## 2020 and before
sel <- the.dates <= "2020-12-01"

long <- ncvar_get(df, "longitude")
lat  <- ncvar_get(df, "latitude")

expver <- ncvar_get(df, "expver")

N <- max((1:length(sel))[sel])

t2m <- array(NA, c(length(long), length(lat), N))

for (t in 1:N) {

    if (t%%50==0) cat(round(t/N*100), "% ")

    t2m[,,t] <- ncvar_get(df, "t2m", c(1,1,1,t),
                          c(length(long),length(lat),1,1)) - 273.15
}

nc <- list(long=long, lat=lat, date=the.dates[sel], tas=t2m)

areas <- calc.area.matrix(nc)


v <- area.weighted.average(t2m, areas, quiet=FALSE)

monthly.avg <- sapply(v, function (x) x[1])

year <- substr(nc$date, 1, 4)

tas.yearly <- as.numeric(tapply(monthly.avg, year, mean))

ERA.yearly <- tas.yearly-mean(tas.yearly)



ERA5 <- list(year=as.numeric(unique(year)),
             temp=ERA.yearly)

save(ERA5, file="Derived_Data/ERA5.RData")
