

create.yearly <- function (nc, areas, model.name="", run="", quiet=TRUE) {

    year <- substr(nc$date, 1, 4)

    monthly.avg <- area.weighted.average(nc$tas, areas, quiet=quiet)[1,]
        
    ##    monthly.avg <- sapply(1:dim(nc$tas)[[3]], function (j) mean(nc$tas[,,j]))
    
    tas.yearly <- as.numeric(tapply(monthly.avg, year, mean))

    list(model.name=model.name,
         run=run,
         year=as.numeric(unique(year)),
         tas.yearly=tas.yearly)
}
