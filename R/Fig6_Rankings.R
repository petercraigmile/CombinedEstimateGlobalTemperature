
rank.sim = function (mean,sd, years, end.year=max(years),Niter=100000)
{
   n=length(years)
   ranks=matrix(nrow=Niter,ncol=n,byrow=TRUE)
   for(i in 1:Niter) {
     ranks[i,]=rank(rnorm(n, mean, sd))
   }
      return(invisible(n+1-ranks))
}

load("../Data/Derived_Data/data_products.RData")

load("chains/data_products_corr_AR4.RData")

YY <- simplify2array(dch$Y.chain)

post.mean <- rowMeans(YY)
post.SD   <- apply(YY, 1, sd)


###Berkeley Earth
berkrank=rank.sim(data.prods$global.anom[,1],sqrt(data.prods$global.vars[,1]),data.prods$years)
for(i in 121:141) print(c("B",data.prods$years[i]," ",sum(berkrank[,i]==1)))

###HadCRUT5
hadrank=rank.sim(data.prods$global.anom[,2],sqrt(data.prods$global.vars[,2]),data.prods$years)
for(i in 121:141) print(c("H",data.prods$years[i]," ",sum(hadrank[,i]==1)))

###NOAA
noaarank=rank.sim(data.prods$global.anom[,3],sqrt(data.prods$global.vars[,3]),data.prods$years)
for(i in 121:141) print(c("N",data.prods$years[i]," ",sum(noaarank[,i]==1)))

###GISTEMP
gissrank=rank.sim(data.prods$global.anom[,4],sqrt(data.prods$global.vars[,4]),data.prods$years)
for(i in 121:141) print(c("G",data.prods$years[i]," ",sum(gissrank[,i]==1)))

###JMA
sel <- which(!is.na(data.prods$global.anom[,5]))
jmarank=rank.sim(data.prods$global.anom[sel,5],sqrt(data.prods$global.vars[sel,5]),data.prods$years[sel])
for(i in 91:length(sel)) print(c("J",data.prods$years[sel][i]," ",sum(jmarank[,i]==1)))

###C&G posterior
# CGrank.AR2=rank.sim(AR2.post.mean,AR2.post.SD,data.prods$years)
# for(i in 121:141) print(c("AR2",data.prods$years[i]," ",sum(CGrank.AR2[,i]==141)))

CGrank.AR4=matrix(nrow=dim(YY)[2],ncol=141,byrow=TRUE)
for(i in 1:dim(YY)[2])CGrank.AR4[i,]=142-rank(YY[,i])
for(i in 121:141) print(c("AR4",data.prods$years[i]," ",sum(CGrank.AR4[,i]==1)))


pdf(file='paper_figures/Fig6_hist_rankings.pdf',width=6.4, height=2)
par(mfrow=c(1,6), cex=0.65, mar=c(2.8,2.6,1,0.2), mgp=c(1.6,0.4,0), bty="L") 

for(ii in 6:1){
hist(CGrank.AR4[,142-ii],freq=F,xlab="Ranks",breaks=0.5+c(0:7),ylim=c(0,1),main=2021-ii)}

dev.off()

#hist(142-CGrank.AR4[,141],freq=F,breaks=0.5+c(0:6),xlab="Ranks",main="2020")

