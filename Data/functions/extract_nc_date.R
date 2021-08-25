
extract.nc.date <- function (nc) {

    info     <- capture.output(print(nc))
    line.num <- grep("days since", info)
    origin   <- as.Date(strsplit(info[[line.num]], " ")[[1]][16])
    
    ## Calculate the date
    days.since <- ncvar_get(nc, "time")
    as.Date(origin) + days.since
}

