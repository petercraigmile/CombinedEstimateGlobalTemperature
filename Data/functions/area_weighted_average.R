
area.weighted.average <- function (temp.array, areas, quiet=TRUE) {

    N <- dim(temp.array)[[3]]

    library(parallel)
    
    mclapply(1:N, function (l) {

        if (!quiet & (l%%20==0)) cat(round(l/N*100), "% ")

        y <- as.numeric(temp.array[,,l])
        
        sel <- !is.na(y)
        
        a <- as.numeric(areas)[sel]
        
        z <- y[sel]
        
        wt <- a/sum(a)
        
        wmean     <- sum(wt * z)
        wmean.var <- sum(wt * (z - wmean)^2) * sum(wt^2)
        
        c(wmean, wmean.var)
    }, mc.cores=6)
}

