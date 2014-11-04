#source functions, packages, anbd plot objects
source("functions and packages/startscripts.R")

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


#Amax------------------------------------------------------------------------------
PSmax <- subset(PS, type=="Amax")
PSmax <- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSmax)
PSmax$volume <- as.factor(PSmax$volume)

#write.csv(PSmax, "calculated data/Amax.csv", row.names=FALSE)

PSagg <- summaryBy(Photo ~ Date + volume, data=PSmax, 
                   FUN=c(mean,sd,length))
PSagg$SE <- with(PSagg, Photo.sd/sqrt(Photo.length))
#PSagg$Date <- as.Date(mdy(PSagg$Date))
PSagg$volume <- as.factor(PSagg$volume)

#Asat----------------------------------------------------------------------------------------------
PSsat <- subset(PS, type=="Asat")

#mean of 5 logs per plant
PSsat_spot<- summaryBy(. ~ ID +Date, FUN=mean, keep.names=TRUE, data=PSsat)

#mean by plant over all dates, then treatment means
PSsat_ID <- summaryBy(. ~ ID, FUN=mean, keep.names=TRUE, data=PSsat_spot)
  PSsat_ID$volume <- as.factor(PSsat_ID$volume)

PSsat_id_agg <- summaryBy(Photo ~volume, data=PSsat_ID, FUN=c(mean,se))

#treatment mean with se across dates
PSsat_agg <- summaryBy(Photo ~ Date + volume, data=PSsat_spot, FUN=c(mean,se))
  PSsat_agg$volume <- as.factor(PSsat_agg$volume)

#write.csv(PSsat, "calculated data/Asat.csv", row.names=FALSE)
#write.csv(PSsat_agg, "calculated data/Asat treatment means.csv", row.names=FALSE)


#Plotting----------------------------------------------------------------------------------------------

#ASAT
windows(8,6)
par(oma = c(0, 1, 0, 0), mgp = c(2.5, 1, 0), cex.axis=1.0, cex.lab=1.3)

with(PSsat_agg, plot(Date, Photo.mean, cex=1.6, pch=pchs, col=volume, 
                     ylab = satlab,
                     ylim=c(0,30), xlab=""))
for(i in 1:7){
  
  dat <- subset(PSsat_agg, volume==levels(volume)[i]) 
  with(dat, arrows(Date, Photo.mean, Date, Photo.mean+Photo.se, angle=90, length=0.05,col=palette()[i], lwd=2))
  with(dat, arrows(Date, Photo.mean, Date, Photo.mean-Photo.se, angle=90, length=0.05,col=palette()[i], lwd=2))
  with(dat,points(Date, Photo.mean, type='l', lwd=2, col=palette()[i],  pch=pchs[i])) 
}

#legend("bottomright", leglab, pch=pchs,text.font=3, inset=0.0, 
# title=expression(Pot~volume~(l)), col=palette(), bty='n')
dev.copy2pdf(file= "output/Asat.pdf")

###barplot with asat values
bar(Photo, volume, PSsat_ID,half.errbar=FALSE, xlab="Soil Volume  (L)",ylab="", ylim=c(0,25), names.arg = leglab,
                col=palette(), legend=FALSE)
title(ylab=satlab, mgp=ypos)




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
require(visreg)
require(broom)

#amax
amax_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSmax)
amax2 <- anova(lme(Photo ~ volume, random=~1 | ID, method="ML", data=PSmax))
anova(amax_lm)
summary(amax_lm)
visreg(amax_lm)

amax_vol_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSmax, subset=volume != "1000")
amax2_vol <- anova(lme(Photo ~ volume, random=~1 | ID, method="ML", data=PSmax, subset=volume != "1000"))
anova(amax_vol_lm)
summary(amax_vol_lm)
visreg(amax_vol_lm)

#asat
asat_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSsat)
asat2 <- anova(lme(Photo ~ volume, random=~1 | ID, method="ML", data=PSsat))
anova(asat_lm)
summary(asat_lm)
visreg(asat_lm)

asat_vol_lm <- lme(Photo ~ volume, random= ~1|ID, data=PSsat, subset=volume != "1000")
asat2_vol <- anova(lme(Photo ~ volume, random=~1 | ID, method="ML", data=PSsat, subset=volume != "1000"))
anova(asat_vol_lm)
summary(asat_lm)
visreg(asat_lm)








