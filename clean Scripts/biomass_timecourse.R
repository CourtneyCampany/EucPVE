source("functions and packages/startscripts.R")

### look at time course of biomass reductions from model

biomass_time <- read.csv("calculated data/biomass_time.csv")
biomass_time$volume <- as.factor(biomass_time$volume)

uniqueDate <- seq.Date(from=as.Date("2013/01/22"), to=as.Date("2013/05/21"), by="days")

biomass_time$Date <- rep(uniqueDate, 7)


#leaf area interpolated--------------------------------------------------------------------------
leafarea_time <- read.csv("calculated data/LApred_volume.csv")
  leafarea_time <-datevol_func (leafarea_time)

##LA dataframe with initial date leaf area added back (model starts day 2).  use model start valuye
LAday1 <- mean(leafarea_time[leafarea_time$Date == "2013-01-21", 3])

LAmodel <- biomass_time[, c(2,5:6)]

startLA <- data.frame(Date = rep(as.Date("2013/01/21"),7), volume = as.factor(c(5,10,15,20,25,35,1000)), 
                        leafarea = rep(LAday1, 7))

LAmodel121 <- rbind(LAmodel, startLA)


###make 2-panel with leaf area and biomass-------------------------------------------------------------------------

##need to determine relative rate of decrease to free
biomass_ls <- split(biomass_time, biomass_time$volume)
free_mass <- biomass_ls[[7]]$biomass
free_la <- biomass_ls[[7]]$leafarea

biomass_reductions <- lapply(biomass_ls, function(x) {x$reduction_scaled = x$biomass/free_mass; return(x)})
leafarea_reductions <- lapply(biomass_ls, function(x) {x$reduction_scaled = x$leafarea/free_la; return(x)})

###multiply biomass *.5 if C
windows(7,10)

####multipanel plot of s1, s2
par(las=1,mgp=c(3,1,0),mfrow=c(2,1), las=1)

#mass
par(mar=c(2,5,1,1))
plot(reduction_scaled ~ Date, pch=16, data= biomass_reductions[[1]], type='n', ylim=c(0, 1.2),
     xlab="",ylab="Biomass Reduction Scaled")
for(i in 1:6){
  lines(biomass_reductions[[i]]$reduction_scaled~ biomass_reductions[[i]]$Date,  col=cols[i], lwd=2)
}
abline(1,0, lwd=2, lty=2, col=cols[7])
legend("bottomleft", leglab, lty= c(rep(1,6),2),text.font=1, inset=0.01, title=vollab, col=palette(), bty='n',cex=1.0, lwd=1.5)

#leafarea
par(mar=c(4,5,1,1))
plot(reduction_scaled ~ Date, pch=16, data= leafarea_reductions[[1]], type='n',ylim=c(0, 1.2),
     xlab="",ylab="Leaf Area Reduction Scaled")
for(i in 1:6){
  lines(leafarea_reductions[[i]]$reduction_scaled~ leafarea_reductions[[i]]$Date,  col=cols[i], lwd=2)
}
abline(1,0, lwd=2, lty=2, col=cols[7])


dev.copy2pdf(file= "master_scripts/manuscript_figs/modeltimecourse.pdf")  
dev.off() 



###leaf area plotted vs measured------------------------------------------------------------------------------------------

pchsopen <- c(rep(1, 6), 2)
#LA plot
windows(7,7)
par(mar=c(4,5,1,1), cex=1)
plot(leafarea~ Date, type='n', ylim=c(0, .55),data=LAmodel121, ylab="Leaf Area", xlab="")
points(leafarea~ Date, data=LAmodel121, col=cols[volume], pch=pchs[volume], cex=.8)
points(canopysqm_pred~ Date, data=leafarea_time, col=cols[volume], pch=pchsopen[volume], cex=.8)
legend("topleft", c("Observed", "Modelled"), pch=c(1,16),text.font=1, inset=0.01, bty='n',cex=1.0 )
dev.copy2pdf(file= "master_scripts/manuscript_figs/LA.pdf")  
dev.off() 
