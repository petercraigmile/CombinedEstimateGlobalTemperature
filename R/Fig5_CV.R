
source("functions/figure_utils.R")
source("functions/plot_CI.R")

load("../Data/Derived_Data/data_products.RData")

load("chains/data_products_corr_AR4.RData")

library(excursions)

YY <- simplify2array(dch$Y.chain)
bb <- dch$X %*% simplify2array(dch$beta.chain)

exc.YY <- simconf.mc(YY, 0.05)

post.mean <- rowMeans(YY)
post.SD   <- apply(YY, 1, sd)

CV_post_mean=matrix(NA,141,5)
CV_post_SD=matrix(NA,141,5)

for (i in 1:5) {
    load(paste("chains/data_products_corr_AR4_CV", i, ".RData", sep=""), envir=.GlobalEnv)
    tmpY=simplify2array(dch$Y.chain)
    CV_post_mean[,i]=rowMeans(tmpY)
    CV_post_SD[,i]=apply(tmpY,1,sd)
}
			
CV_diff=matrix(NA,141,5)
for(i in 1:5) {
    
    CV_diff[,i]=CV_post_mean[,i]-post.mean
}


pdf(file="paper_figures/Fig5_CV_plots.pdf", width=6.4, height=2.2)
par(mfrow=c(1,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

plot(range(dch$years), c(-.12,.12),type="n",
     xlab="Year", ylab=anomaly.label, yaxt="n")

axis(side=2, at=c(-0.10, 0, 0.10))

plot.CI(dch$years, YY-rowMeans(YY), 
        exc.YY$a-rowMeans(YY), exc.YY$b-rowMeans(YY),
        col="lightgray") 

matlines(dch$years,CV_diff,col=the.cols,lty=1)
lines(dch$years,rep(0,length(dch$years)))

legend(1965, -.01, c(data.prods$labels, "Posterior"),
       col=c(the.cols,"black"), lty=1, cex=0.9, bty="n")

mtext("(a)", side=3, line=0, cex=0.7)

plot(range(dch$years),range(CV_post_SD),type="n", xlab="Year",
     ylab=expression(paste("Posterior standard deviations (",degree,"C)")))

matlines(dch$years,CV_post_SD,col=the.cols,lty=1)
lines(dch$years,post.SD)

mtext("(b)", side=3, line=0, cex=0.7)

dev.off()


