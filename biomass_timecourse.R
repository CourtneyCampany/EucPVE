source("functions and packages/startscripts.R")

### look at time course of biomass reductions from model

biomass_time <- read.csv("calculated data/biomass_time.csv")
biomass_time$volume <- as.factor(biomass_time$volume)

uniqueDate <- seq.Date(from=as.Date("2013/01/22"), to=as.Date("2013/05/21"), by="days")

biomass_time$Date <- rep(uniqueDate, 7)

plot(biomass~ Date, type='n', ylim=c(0, 200),data=biomass_time)
points(biomass~ Date, data=biomass_time, col=cols[volume], pch=pchs[volume])


##need to determine relative rate of decrease to free----------------------------------------------------------------------


biomass_ls <- split(biomass_time, biomass_time$volume)
free <- biomass_ls[[7]]$biomass

biomass_reductions <- lapply(biomass_ls, function(x) {x$reduction_scaled = x$biomass/free; return(x)})

###multiply biomass *.5 if C
windows(7,7)

par(mar=c(5,5,2,2), cex.axis=1, cex.lab = 1, las=1)
plot(reduction_scaled ~ Date, pch=16, data= biomass_reductions[[1]], type='n',
     xlab="",ylab="Daily Biomass Reduction Scaled to Free Seedlings")
for(i in 1:6){
  lines(biomass_reductions[[i]]$reduction_scaled~ biomass_reductions[[i]]$Date,  col=cols[i], lwd=2)
}
abline(1,0, lwd=2, lty=2, col=cols[7])
legend("bottomleft", leglab, lty= c(rep(1,6),2),text.font=1, inset=0.01, title=vollab, col=palette(), bty='n',cex=1.0, lwd=1.5)

dev.copy2pdf(file= "master_scripts/manuscript_figs/massreductions.pdf")  
dev.off() 