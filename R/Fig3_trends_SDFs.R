

source("functions/figure_utils.R")
source("functions/AR.R")
source("functions/plot_CI.R")

load("chains/data_products_corr_AR4.RData")

library(excursions)

dB <- function (x) {
    10 * log10(x)
}

ar.sdf <- function (freqs, phi, sigma2 = 1, delta.t = 1) 
{
    ws <- -2 * pi * delta.t * freqs
    js <- seq(length(phi))
    reals <- sapply(ws, function(w, js, phi) 1 - sum(phi * cos(js * 
        w)), js = js, phi = phi)
    imags <- sapply(ws, function(w, js, phi) sum(phi * sin(js * 
        w)), js = js, phi = phi)
    (sigma2 * delta.t)/(reals * reals + imags * imags)
}

fs <- seq(0, 1/2, length=256)

bb <- dch$X %*% simplify2array(dch$beta.chain)


sdfs <- sapply(1:length(dch$eta.chain), function (k) {
    dB(ar.sdf(fs, AR.pacf.to.ar(dch$eta.chain[[k]]),
              dch$sigma2.chain[[k]])) })

exc.bb   <- simconf.mc(bb,   0.05)

exc.sdfs <- simconf.mc(sdfs, 0.05)


pdf(file="paper_figures/Fig3_trends_sdf_AR4.pdf", width=6.4, height=2.2)
par(mfrow=c(1,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

plot(dch$years, rowMeans(bb), type="n", ylim=cylim,
     xlab="Year", ylab=expression(paste("Posterior Trend (",degree,"C)")))

plot.CI(dch$years, bb, 
        exc.bb$a, exc.bb$b,
        col="lightgray")

lines(dch$years, rowMeans(bb))

mtext("(a)", side=3, line=0, cex=0.7)


plot(fs, rowMeans(sdfs), type="n", ylim=c(-35, 25),
     xlab="Frequency", ylab="Posterior SDF (dB)")

plot.CI(fs, sdfs,
        exc.sdfs$a, exc.sdfs$b,
        col="lightgray")

lines(fs, rowMeans(sdfs))

mtext("(b)", side=3, line=0, cex=0.7)

dev.off()

