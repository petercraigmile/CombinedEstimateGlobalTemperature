
## ======================================================================
## R code to fit the models used in the article
##
## P. F. Craigmile and P. Guttorp,
## A combined estimate of global temperature
##
## The truncatedNormals R library is available from
## https://github.com/petercraigmile/truncatedNormals
##
## Contact: pfc@stat.osu.edu
## ======================================================================

library(splines)
library(mvtnorm)
library(truncatedNormals)
library(excursions)

source("functions/pfcBayes.R")
source("functions/AR.R")
source("functions/cov_Bayes_util.R")

load("Derived_Data/data_products.RData")

source("obs_updates_corr.R")

## Change as appropriate (ranges from 1 to 5)
to.remove <- 1

model.name <- paste("AR4_CV", to.remove, sep="")

if (to.remove==5) {
    
    dch <- init.model(data.prods$years,
                      data.prods$global.anoms[,-to.remove],
                      data.prods$global.vars[,-to.remove],
                      X = cbind(1, bs(data.prods$years, 6)),
                      eta0  = rep(0.4, 4),
                      impute.JMA=0)  ## no imputation need
    
} else {
   
    dch <- init.model(data.prods$years,
                      data.prods$global.anoms[,-to.remove],
                      data.prods$global.vars[,-to.remove],
                      X = cbind(1, bs(data.prods$years, 6)),
                      eta0  = rep(0.4, 4),
                      impute.JMA=4) # JMA is now the 4th data product

}

    

to.update <- c("Y", "delta", "tau2", "R", "JMA", "beta", "eta.sigma2")


R.prop.sds <- rep(NA, length(dch$R.indexes))

for (k in 1:length(dch$R.indexes)) {

    aa <- dch$R.indexes[[k]]
    R.prop.sds[k] <- 0.03
}


print(system.time(run.MCMC(dch, to.update, 2000, every=500, burn.in=TRUE)))


for (k in 1:50) {
    
    run.MCMC(dch, to.update, 20000, every=2500, thin=20)
    
    cat("R jumps: ")
    print(round(rowMeans(simplify2array(dch$R.jumps.chain)), 2))
    cat("\n")
    
    pdf(file=paste("trace_plots/trace_plots_data_products_", model.name, ".pdf", sep=""),
        width=6.3, height=6.5)
    trace.plots(dch)
    dev.off()
    
    save(dch,
         file=paste("chains/data_products_corr_", model.name, ".RData", sep=""))    
}
