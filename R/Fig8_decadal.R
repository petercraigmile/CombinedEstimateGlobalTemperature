
load("chains/data_products_corr_AR4.RData")

load("../Data/Derived_Data/data_products.RData")

the.cols <- c("blue", "red", "green", "purple", "cyan")

YY <- simplify2array(dch$Y.chain)

post.mean <- rowMeans(YY)
post.SD   <- apply(YY, 1, sd)

dec=se=matrix(NA,14,5)
ARse=ARdec=rep(NA,14)

for(i in 1:14) {
    for(j in 1:5){
        dec[i,j]=mean(data.prods$global.anom[((i-1)*10+2):(i*10+1),j])
        se[i,j]=sqrt(sum(data.prods$global.vars[((i-1)*10+2):(i*10+1),j],na.rm=T)/10)}
    ARdec[i]=mean(post.mean[((i-1)*10+2):(i*10+1)])
    ARse[i]=sqrt(sum(post.SD[((i-1)*10+2):(i*10+1)]^2)/10)
}



## With HadCrUT5 ses
## par(mfrow=c(2,1))	
## plot(range(data.prods$years),c(-.6,1),type="n",xlab="Year",ylab="Decadal average")
## for (i in 1:14) {
##    decadeL <- data.prods$years[((i-1)*10+2)]
##    decadeR <- data.prods$years[(i*10+1)]
##segments(x0=decadeL,x1=decadeR,y0=dec[i,2],col="red")
## segments(x0=decadeL,x1=decadeR,y0=dec[i,2]+2*se[i,2],col="red",lwd=0.5)
## segments(x0=decadeL,x1=decadeR,y0=dec[i,2]-2*se[i,2],col="red",lwd=0.5)
## segments(x0=decadeL,y0=dec[i,2]-2*se[i,2],y1=dec[i,2]+2*se[i,2],col="red",lwd=0.5)
## segments(x0=decadeR,y0=dec[i,2]-2*se[i,2],y1=dec[i,2]+2*se[i,2],col="red",lwd=0.5)
## segments(x0=decadeL,y0=dec[i,2]-2*se[i,2],y1=dec[i,2]+2*se[i,2],col="red",lwd=0.5)
## segments(x0=decadeL,x1=decadeR,y0=dec[i,1],col=the.cols[1])
## segments(x0=decadeL,x1=decadeR,y0=dec[i,3],col=the.cols[3])
## segments(x0=decadeL,x1=decadeR,y0=dec[i,4],col=the.cols[4])
## segments(x0=decadeL,x1=decadeR,y0=dec[i,5],col=the.cols[5])
## segments(x0=decadeL,x1=decadeR,y0=ARdec[i])
## }


pdf("paper_figures/Fig8_decades.pdf", width=4, height=2.2)
par(mfrow=c(1,1), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

## With our SEs
plot(range(data.prods$years),c(-.6,1),type="n",xlab="Year",
     ylab=expression(paste("Decadal average (",degree,"C)")))

for(i in 1:14) {

    decadeL <- data.prods$years[((i-1)*10+2)]
    decadeR <- data.prods$years[(i*10+1)]

    rect(decadeL, ARdec[i]-2*ARse[i],
         decadeR, ARdec[i]+2*ARse[i], col="gray70", border="gray70")

    for (j in 1:5) {
        segments(x0=decadeL, x1=decadeR, y0=dec[i,j], col=the.cols[j])
    }

    segments(x0=decadeL, x1=decadeR, y0=ARdec[i])
}

legend(1880, 1, c(data.prods$labels,"Posterior"),
       col=c(the.cols,"black"), lty=1, cex=1, bty="n")

dev.off()
