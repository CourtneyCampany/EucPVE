#Spot measurements of Asat and Amax

source("functions and packages/load packages.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

#Read in spot A measurements and merge plot design
source("read data scripts/physiology read data.R")

PS <- merge(plotsumm, subset(gasexchange, select = c("campaign", "plot", "pot", "CO2", "CO2R",
                                                     "Photo", "Cond", "Ci", "Tleaf", "VpdL", "Area", "Trmmol")))
  
#run function to add campaign date
PS <- add_campaign_date(PS)

#write.csv(PS, "calculated data/Asat_obs.csv", row.names=FALSE)

#factor designations
#Ps$volume <- as.factor(Ps$volume)
PS$type <- factor(ifelse (PS$CO2 == "400", "Asat", "Amax"))

#pot volume means
PS_agg <- aggregate(cbind(Photo, Cond, Ci, Trmmol) ~ campaign + type + volume, data=PS, FUN=mean)

#subset Amax
PSmax <- subset(PS, type=="Amax")
PSmax <- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSmax)
PSmax$volume <- as.factor(PSmax$volume)

#write.csv(PSmax, "calculated data/Amax.csv", row.names=FALSE)

PSagg <- summaryBy(Photo ~ Date + volume, data=PSmax, 
                   FUN=c(mean,sd,length))
PSagg$SE <- with(PSagg, Photo.sd/sqrt(Photo.length))
#PSagg$Date <- as.Date(mdy(PSagg$Date))
PSagg$volume <- as.factor(PSagg$volume)

#subset Asat
PSsat <- subset(PS, type=="Asat")
PSsat <- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSsat)
PSsat$volume <- as.factor(PSsat$volume)

PSsat_agg <- summaryBy(Photo ~ Date + volume, data=PSsat, 
                   FUN=c(mean,sd,length))

PSsat_agg$SE <- with(PSsat_agg, Photo.sd/sqrt(Photo.length))
#PSsat_agg$Date <- as.Date(mdy(PSsat_agg$Date))
PSsat_agg$volume <- as.factor(PSsat_agg$volume)

#write.csv(PSsat, "calculated data/Asat.csv", row.names=FALSE)
#write.csv(PSsat_agg, "calculated data/Asat treatment means.csv", row.names=FALSE)


#----------------------------------------------------------------------------------------------------

#AMAX
windows(8,6)
par(oma = c(0, 1, 0, 0), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)

with(PSagg, plot(Date, Photo.mean, cex=1.6, pch=pchs, col=volume, 
                 ylab = maxlab,
                 ylim=c(0,40), xlab=""))
for(i in 1:7){
  
  dat <- subset(PSagg, volume==levels(volume)[i]) 
  with(dat, arrows(Date, Photo.mean, Date, Photo.mean+SE, angle=90, length=0.05,col=palette()[i], lwd=2))
  with(dat, arrows(Date, Photo.mean, Date, Photo.mean-SE, angle=90, length=0.05,col=palette()[i], lwd=2)) 
  with(dat,points(Date, Photo.mean, type='l', lwd=2, col=palette()[i],  pch=pchs[i]))           
}

legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.02, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

dev.copy2pdf(file= "output/Amax.pdf")
dev.off()


#ASAT
windows(8,6)
par(oma = c(0, 1, 0, 0), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)

with(PSsat_agg, plot(Date, Photo.mean, cex=1.6, pch=pchs, col=volume, 
                 ylab = satlab,
                 ylim=c(0,30), xlab=""))
for(i in 1:7){
  
  dat <- subset(PSsat_agg, volume==levels(volume)[i]) 
  with(dat, arrows(Date, Photo.mean, Date, Photo.mean+SE, angle=90, length=0.05,col=palette()[i], lwd=2))
  with(dat, arrows(Date, Photo.mean, Date, Photo.mean-SE, angle=90, length=0.05,col=palette()[i], lwd=2))
  with(dat,points(Date, Photo.mean, type='l', lwd=2, col=palette()[i],  pch=pchs[i])) 
}

#legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.0, 
      # title=expression(Pot~volume~(l)), col=palette(), bty='n')
dev.copy2pdf(file= "output/Asat.pdf")



#two panel graph of A

windows(10,10)
par(mfrow=c(2,1), omi=c(1,0,0.5,0.5),mar=c(0,5,0,0),mgp = c(2.5, 1, 0))
#Amax
plot(Photo.mean ~ Date, data=PSagg, axes=FALSE,xlab="", ylab="",
     type='n', ylim=c(0,40))
box()
axis(2, labels=TRUE)  
title(ylab=maxlab)
with(PSagg, arrows(Date, Photo.mean, Date, Photo.mean+SE, angle=90, col=palette(),length=0.05))
with(PSagg, arrows(Date, Photo.mean, Date, Photo.mean-SE, angle=90, col=palette(),length=0.05))
d_ply(PSagg, .(volume), function(x) points(x$Photo.mean ~ x$Date,  
                                                  col=x$volume, type="b", pch = pchs[x$volume],cex=1.6,))
legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')

#Asat
plot(Photo.mean ~ Date, data=PSsat_agg, xlab="", ylab=satlab,
     ylim=c(0,40))
  with(PSsat_agg, arrows(Date, Photo.mean, Date, Photo.mean+SE, angle=90, col=palette(),length=0.05))
  with(PSsat_agg, arrows(Date, Photo.mean, Date, Photo.mean-SE, angle=90, col=palette(),length=0.05))
  d_ply(PSsat_agg, .(volume), function(x) points(x$Photo.mean ~ x$Date,  
                                           col=x$volume, type="b", pch = pchs[x$volume], cex=1.6))



dev.copy2pdf(file= "output/stats_plots/photo.pdf")
dev.off()

#--------------------------------------------------------------------------------------------------------

#Conductance

#pot volume means
cond_agg <- aggregate(Cond~ campaign + type + volume, data=PS, FUN=mean)

cond_campaign <- summaryBy(Cond ~ Date + volume, data=PSmax, FUN=c(mean,se))


#cond
windows()
plot(Cond.mean ~ Date, data=cond_campaign, xlab="", ylab="", ylim=c(0,1))
title(ylab=condlab, mgp=ypos)
with(cond_campaign, arrows(Date, Cond.mean, Date, Cond.mean+Cond.se, angle=90, col=palette(),length=0.05))
with(cond_campaign, arrows(Date, Cond.mean, Date, Cond.mean-Cond.se, angle=90, col=palette(),length=0.05))
d_ply(cond_campaign, .(volume), function(x) points(x$Cond.mean ~ x$Date,  
                                col=x$volume, type="b", pch = pchs[x$volume], cex=1.6))
legend("topright", leglab, pch=pchs,text.font=3, inset=0.02, title=vollab, col=palette(), bty='n')

dev.copy2pdf(file= "output/stats_plots/Gs.pdf")
dev.off()












#stats
require(nlme)
lme1 <- lme(Photo ~ volume, random= ~1|ID, data=PSmax)
anova(lme1)

lme2 <- lme(Photo ~ volume, random= ~1|ID, data=PSmax, subset=volume != "1000")
anova(lme2)





#plots of Ps and Ci with treatments and campaigns
#windows(4,4)
leglab <- unique(PS$Date)

#ASAT plots of Photosynthesis, Conductance and Ci
with(subset(PS,type=="Asat"), bargraph.CI(as.factor(volume), Photo, as.factor(Date), 
      #legend=TRUE, leg.lab = leglab, args.legend = (x="topleft"),
      ylim = c(0,30), ylab = expression(italic(A)[sat]~~(mu*mol~m^-2~s^-1)), xlab = "Pot Volume (l)", 
      col=c("gray80", "gray70", "gray60", "gray50", "gray40", "gray30")))
box()
dev.copy2pdf(file= "ASAT.pdf")
dev.off()

with(subset(PS,type=="Asat"), bargraph.CI(as.factor(volume), Cond, as.factor(Date),
      #legend=TRUE, leg.lab = leglab, args.legend = (x="topleft"),
      ylim = c(0,1),ylab = expression(Conductance~~(mol~m^-2~s^-1)),
      xlab = "Pot Volume (l)", 
      col=c("gray80", "gray70", "gray60", "gray50", "gray40", "gray30")))
box()
dev.copy2pdf(file= "ASAT_cond.pdf")
dev.off()

with(subset(PS,type=="Asat"), bargraph.CI(as.factor(volume), Ci, as.factor(Date),
      ylim = c(0,350),ylab = "Ci", xlab = "Pot Volume (l)", 
      col=c("gray80", "gray70", "gray60", "gray50", "gray40", "gray30")))
box()
dev.copy2pdf(file= "ASAT_.Ci.pdf")
dev.off()

#AMAX plots of Photosynthesis  and Ci
with(subset(PS,type=="Amax"), bargraph.CI(as.factor(volume), Photo, as.factor(Date),
      #legend=TRUE, leg.lab = leglab, args.legend = (x="topleft"), 
      ylim = c(0,40),ylab = expression(italic(A)[max]~~(mu*mol~m^-2~s^-1)), xlab = "Pot Volume (l)", 
      col=c("gray80", "gray70", "gray60", "gray50", "gray40", "gray30"))) 
box()
dev.copy2pdf(file= "AMAX.pdf")
dev.off()

with(subset(PS,type=="Amax"), bargraph.CI(as.factor(volume), Ci, as.factor(Date),
      ylim = c(0,1750),ylab = "Ci", xlab = "Pot Volume (l)", 
      col=c("gray80", "gray70", "gray60", "gray50", "gray40", "gray30"))) 
box()
dev.copy2pdf(file= "AMAX_Ci.pdf")
dev.off()

#transpiration plots
with(subset(PS,type=="Asat"), bargraph.CI(as.factor(volume), Trmmol, as.factor(Date),
                                          ylim = c(0,7.5),ylab = "Transpiration (Asat)", xlab = "Pot Volume (l)", 
                                          col=c("gray80", "gray70", "gray60", "gray50", "gray40", "gray30"))) 
box()
dev.copy2pdf(file= "transpiration_Asat.pdf")
dev.off()


with(subset(PS,type=="Amax"), bargraph.CI(as.factor(volume), Trmmol, as.factor(Date),
                                          ylim = c(0,7.5),ylab = "Transpiration (Amax)", xlab = "Pot Volume (l)", 
                                          col=c("gray80", "gray70", "gray60", "gray50", "gray40", "gray30"))) 

box()
dev.copy2pdf(file= "transpiration_Amax.pdf")
dev.off()







