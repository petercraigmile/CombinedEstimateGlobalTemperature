

source("functions/plot_CI.R")
source("functions/figure_utils.R")

load("../Data/Derived_Data/data_products.RData")

load("chains/data_products_corr_AR4.RData")

library(excursions)

YY <- simplify2array(dch$Y.chain)
bb <- dch$X %*% simplify2array(dch$beta.chain)

exc.YY <- simconf.mc(YY, 0.05)


pdf(file="paper_figures/Fig2_BHM_data_products_AR4.pdf", width=6.4, height=2.2)
par(mfrow=c(1,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

plot(dch$years, rowMeans(YY),
     type="n", ylim=cylim, xlab="Year", ylab=anomaly.label)

plot.CI(dch$years, YY, 
        exc.YY$a, exc.YY$b,
        col="lightgray")

mtext("(a)", side=3, line=0, cex=0.7)

lines(dch$years, rowMeans(YY))

plot(dch$years, apply(YY, 1, sd), type="l", ylim=c(0.015,0.04),
     xlab="Year", ylab="Posterior SD")

mtext("(b)", side=3, line=0, cex=0.7)

dev.off()

