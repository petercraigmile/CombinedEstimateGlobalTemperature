

source("functions/figure_utils.R")
source("functions/AR.R")
source("functions/plot_CI.R")

load("../Data/Derived_Data/data_products.RData")

load("chains/data_products_corr_AR4.RData")

library(excursions)



pdf(file="paper_figures/Fig4_discrepancies_AR4.pdf", width=6.4, height=5)
par(mfrow=c(3,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

for (j in 1:dch$J) {
    
    delta.j <- sapply(dch$delta.chain, function (x) x[,j])

    exc.delta.j <- simconf.mc(delta.j, 0.05)

    plot(dch$years, rowMeans(delta.j), type="n", ylim=c(-0.15,0.15),
         xlab="Year", ylab=expression(paste("Discrepany (",degree,"C)")),
         yaxt="n", xlim=c(1880-2, 2020+2))

    axis(side=2, at=c(-0.10, 0, 0.10))

    mtext(data.prods$labels[j], line=0, side=3, cex=0.75)

    abline(h=0, lty=1)
    
    plot.CI(dch$years, delta.j,
            exc.delta.j$a, exc.delta.j$b,
            col="lightgray")
    
    lines(dch$years, rowMeans(delta.j))
}

dev.off()
