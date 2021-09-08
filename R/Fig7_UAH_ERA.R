
library(excursions)

source("functions/figure_utils.R")
source("functions/anomalies.R")
source("functions/plot_CI.R")

load("chains/data_products_corr_AR4.RData")

ALH=read.table("../Data/UAH/uahncdc_lt_6.0.txt",header=T)[-1,]
load("../Data/Derived_Data/ERA5.Rdata")

the.Y <- simplify2array(dch$Y.chain)

post.mean <- rowMeans(the.Y)


n <- length(post.mean)

BF <- qnorm(1-.05/n)

theyears <- 1880:2020

nn <- mean(post.mean[theyears>=1979 & theyears <=2020])


YY <- the.Y-nn

exc <- simconf.mc(YY, 0.05)


cylim <- c(-0.95, 1.2)

pdf(file="paper_figures/Fig7_UAH_ERA.pdf", width=4, height=2.2)
par(mfrow=c(1,1), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

plot(dch$years, rowMeans(YY), type="n", ylim=cylim,
     xlab="Year", ylab=anomaly.label, xlim=c(1975, 2020))

plot.CI(dch$years, YY,
        lower=exc$a, upper=exc$b,
        col="lightgray", alpha=0.05/n)

legend(1975, cylim[2], c("UAH", "ERA", "Posterior mean"),
       col=c("steelblue", "deeppink", "black"),
       lty=1, cex=1, bty="n")

lines(dch$years, rowMeans(YY))

uah=cbind(ALH$Year+ALH$Mo/12,ALH$Globe)
uah_man=monthly.anoms(uah[,1],uah[,2],1979,2020.95)$monthly.anoms
uah_yan=monthly.to.year.anoms(uah[,1],uah_man)

lines(1979:2020,uah_yan$yearly.anoms[-43]-mean(uah_yan$yearly.anoms[-43]), col="steelblue",lwd=1)
lines(ERA5$year, ERA5$temp, col="deeppink",lwd=1)

dev.off()



