#source functions and packages
source("functions and packages/functions.R")
source("functions and packages/stats packages.R")
source("functions and packages/plot objects.R")
photo_chem <- read.csv("calculated data/Amax_chem.csv")

#re work labels for volume
photo_chem$volume <- gsub("free", "1000", photo_chem$volume)
photo_chem$volume <- gsub("^5", "05", photo_chem$volume)
photo_chem$volume <- as.factor(photo_chem$volume)

#--------------------------------------------------------------------------------------
#calculate overall means and means for each date

#means(all dates)
photo_chem_agg <- summaryBy(A_mass+starch+Nmass_notnc ~ volume, FUN=c(mean, se), 
                            data=photo_chem)

volumeorder <- order(photo_chem_agg$A_mass.mean, by=photo_chem_agg$volume)
photo_chem_agg<-photo_chem_agg[volumeorder,]

# #means(campaign)
# photo_chem_campaign <- summaryBy(A_mass+starch+Nmass_notnc ~ campaign + volume, FUN=c(mean, se), 
#                                   keep.names=TRUE, data=photo_chem)

#simple model mass based--------------------------------------------------------------------

#simple models first
#model3 (adds campaign as fixed effect)
Amass_simple <- lm(A_mass~ starch*Nmass_notnc, data=photo_chem)
anova(Amass_simple)
summary(Amass_simple)

AmassN <- lm(A_mass~ Nmass_notnc, data=photo_chem)
anova(AmassN)
summary(AmassN)

AmassS <- lm(A_mass~ starch, data=photo_chem)
anova(AmassS)
summary(AmassS)
#-----------------------------------------------------------------------------------------
#visualise effects in 3d
windows()
with(photo_chem, {
  scatterplot3d(Nmass_notnc,   # x axis
                starch,     # y axis
                A_mass,    # z axis
                scale.y=.65, #reduces yaxis by 25%
                type="h", lty.hplot=2, pch=pchs[volume], color=volume,
                main="Photosynthesis vs N and TNC",xlab=nfree, ylab=starchlab, zlab=Amasslab)
                legend("right", leglab, inset=.1,     
                       bty="n", text.font=2,pch=pchs, col=palette(), title=vollab)
             
})
dev.copy2pdf(file= "output/stats_plots/photo_chem3d.pdf")
dev.off()
#means plot
windows()
with(photo_chem_campaign, {
  s3d <- scatterplot3d(Nmass_notnc.mean, starch.mean, A_mass.mean,   
                type="h", lty.hplot=2, pch=pchs[volume], color=volume,
                main="Photosynthesis vs N and TNC",xlab=nfree, ylab=starchlab, zlab=Amasslab)
  s3d.coords <- s3d$xyz.convert(Nmass_notnc.mean, starch.mean,  A_mass.mean)
  text(s3d.coords$x, s3d.coords$y,labels=campaign,cex=.75, pos=4) 
  legend("right", leglab, inset=.1,     
         bty="n", text.font=2,pch=pchs, col=palette(), title=vollab) 
})

#full 3d plotting
photo5 <- subset(photo_chem, volume=="05")
photo5f <- subset(photo_chem, volume=="05"| volume=="1000")
photo5f$volume<- droplevels(photo5f$volume)
rb<- c("red", "blue")

with(photo_chem, plot3d(Nmass_notnc,starch,A_mass, col=volume, size=1, type='s'))
with(photo5, plot3d(Nmass_notnc,starch,A_mass, col="red", size=1, type='s'))
with(photo_chem3, plot3d(Nmass_notnc,starch,A_mass, col="blue", size=1, type='s'))
with(photo5f, plot3d(Nmass_notnc,starch,A_mass, col=rb[volume], size=1, type='s'))


library(Rcmdr)
scatter3d(A_mass~Nmass_notnc+starch, data=photo_chem,fit="smooth", point.col=volume)
scatter3d(A_mass~Nmass_notnc+starch, data=photo5f,fit="smooth", point.col=rb[volume])

scatter3d(A_mass~Nmass_notnc+starch, data=subset(photo_chem, volume=="1000",fit="smooth"))
scatter3d(A_mass~Nmass_notnc+starch, data=subset(photo_chem, volume=="05",fit="smooth"))
scatter3d(A_mass~Nmass_notnc+starch, data=subset(photo_chem, volume=="10",fit="smooth"))
scatter3d(A_mass~Nmass_notnc+starch, data=subset(photo_chem, volume=="15",fit="smooth"))
scatter3d(A_mass~Nmass_notnc+starch, data=subset(photo_chem, volume=="20",fit="smooth"))
scatter3d(A_mass~Nmass_notnc+starch, data=subset(photo_chem, volume=="25",fit="smooth"))
scatter3d(A_mass~Nmass_notnc+starch, data=subset(photo_chem, volume=="35",fit="smooth"))

with(photo_chem, scatter3d(Nmass_notnc, starch,A_mass, fit="linear"))

#-----------------------------------------------------------------------------------------
#plot predicted and observed in bins

#make bin levels and mid points for both starch and nitrogen
starchbin <- c(0, 0.04, 0.08, 0.12, 0.16, 100)
starchmid <- c(0.02, 0.06, 0.1, 0.14, 0.2)

nitrobin <- c(0, 0.0025, 0.005, 0.0075, 0.01, 100)
nitromid <- c(0.00125, 0.00375, 0.00625, 0.00875, 0.0125)

#create models and extract coefs
#use two fits so the equations and stay the same for pred, y=b+b1*x1+b2*x2
lmfit <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem)
p <- coef(lmfit)
lmfit2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem)
p2 <- coef(lmfit2)
#set bins in raw dfr
photo_chem$starchbin <- cut(photo_chem$starch, breaks = starchbin)
photo_chem$nitrobin <- cut(photo_chem$Nmass_notnc, breaks = nitrobin)

#plot bits
cols <- c("green2", "cyan4", "blue2", "darkviolet", "red")
coln <- c("red", "darkviolet", "blue2", "cyan4", "green2")
binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
pchbin <- c(1,2,3,4,5)
legpch <- c(5, 10, 15, 20, 25, 35, "free")

#plot and predict (2 graphs)

#Amass vs N
windows()
with(photo_chem, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin], ylim=c(0,800)))
for(i in 1:length(starchbin)){
  
  x <- seq(min(photo_chem$Nmass_notnc), max(photo_chem$Nmass_notnc), length=101)
  y <- p[[1]] + p[[2]]*x + p[[3]]*starchmid[i] + p[[4]]*starchmid[i]*x
  lines(x,y, col=cols[i], lwd=2)
}
legend("topleft", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')

#Amass vs starch
windows()
with(photo_chem, plot(starch, A_mass, pch=19, col=coln[nitrobin], ylim=c(0,800), xlim=c(0, .275)))
for(i in 1:length(nitrobin)){
  
  x <- seq(0, max(photo_chem$starch), length=101)
  y <- p2[[1]] + p2[[2]]*x + p2[[3]]*nitromid[i] + p2[[4]]*nitromid[i]*x
  lines(x,y, col=coln[i], lwd=2)
}
legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')

#--------------------------------------------------------------------
photo_chem2 <- subset(photo_chem, volume !="1000")
coln2 <- c("red", "darkviolet", "blue2", "cyan4")

Amean <- mean(photo_chem2$A_mass)
Nmean <- mean(photo_chem2$Nmass_notnc)
Smean <- mean(photo_chem2$starch)

#use two fits so the equations and stay the same for pred, y=b+b1*x1+b2*x2
lmpot <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem2)
potcf <- coef(lmpot)
lmpot2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem2)
potcf2 <- coef(lmpot2)
#set bins
photo_chem2$starchbin <- cut(photo_chem2$starch, breaks = starchbin)
photo_chem2$nitrobin <- cut(photo_chem2$Nmass_notnc, breaks = nitrobin)


#Amass vs N (no free)
windows()
with(photo_chem2, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin], ylim=c(0,800)))
for(i in 1:length(starchbin)){
  
  x <- seq(min(photo_chem2$Nmass_notnc), max(photo_chem2$Nmass_notnc), length=101)
  y <- potcf[[1]] + potcf[[2]]*x + potcf[[3]]*starchmid[i] + potcf[[4]]*starchmid[i]*x
  lines(x,y, col=cols[i], lwd=2)
}
legend("topleft", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')
title(main="Within Pots")

#Amass vs starch (no free)
windows()
with(photo_chem2, plot(starch, A_mass, pch=19, col=coln2[nitrobin], ylim=c(0,800), xlim=c(0, .275)))
for(i in 1:length(nitrobin)){
  
  x <- seq(min(photo_chem2$starch), max(photo_chem2$starch), length=101)
  y <- potcf2[[1]] + potcf2[[2]]*x + potcf2[[3]]*nitromid[i] + potcf2[[4]]*nitromid[i]*x
  lines(x,y, col=coln2[i], lwd=2)
}
legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')
title(main="Within Pots")
#only free-----------------------------------------------------------------------------------
# 
photo_chem3 <- subset(photo_chem, volume =="1000")
coln3 <- c("darkviolet", "blue2", "cyan4", "green2")

#look at similar effects with pots and non pots
#make bin levels for both starch and nitrogen
starchbin3 <- c(0, 0.04, 0.08, 0.12, 0.16)
nitrobin3 <- c(0.0025, 0.005, 0.0075, 0.01, 100)
#make midpoints for pred
starchmid3 <- c(0.02, 0.06, 0.1, 0.14)
nitromid3 <- c(0.00375, 0.00625, 0.00875, 0.0125)

#use two fits so the equations and stay the same for pred, y=b+b1*x1+b2*x2
lmfree <- lm(A_mass ~ Nmass_notnc * starch, data=photo_chem3)
Fcf <- coef(lmfree)
lmfree2 <- lm(A_mass ~ starch*Nmass_notnc, data=photo_chem3)
Fcf2 <- coef(lmfree2)
#set bins in raw dfr
photo_chem3$starchbin3 <- cut(photo_chem3$starch, breaks = starchbin3)
photo_chem3$nitrobin3 <- cut(photo_chem3$Nmass_notnc, breaks = nitrobin3)
 
#Amass vs N (free only)
windows()
with(photo_chem3, plot(Nmass_notnc, A_mass, pch=16, col=cols[starchbin3], ylim=c(0,800)))
for(i in 1:length(starchbin3)){
   
x <- seq(min(photo_chem3$Nmass_notnc), max(photo_chem3$Nmass_notnc), length=101)
y <- Fcf[[1]] + Fcf[[2]]*x + Fcf[[3]]*starchmid3[i] + Fcf[[4]]*starchmid3[i]*x
lines(x,y, col=cols[i], lwd=2)
}
legend("bottomright", binlab, pch=15, text.font=1, inset=0.02, col=cols,title=starchlab , bty='n')
title(main="Free")
 
#Amass vs starch (free only)
windows()
with(photo_chem3, plot(starch, A_mass, pch=19, col=coln3[nitrobin3], ylim=c(0,800), xlim=c(0, .20)))
for(i in 1:length(nitrobin3)){ 
   x <- seq(min(photo_chem3$starch), max(photo_chem3$starch), length=101)
   y <- Fcf2[[1]] + Fcf2[[2]]*x + Fcf2[[3]]*nitromid3[i] + Fcf2[[4]]*nitromid3[i]*x
   lines(x,y, col=coln3[i], lwd=2)
 }
 legend("topright", binlab, pch=15, text.font=1, inset=0.02, col=coln,title=nfree , bty='n')
 title(main="Free")



#complex model mass based--------------------------------------------------------------------
#if comparing models with different structure then use ML method

#model1 (volume as factor, ID is random effect, and all others are fixed effects)
Amass_model <- lme(A_mass~ starch*Nmass_notnc*volume, random=~1|ID, data=photo_chem, method="ML")
#model2 (adds campaign as random effect)
Amass_model2 <- lme(A_mass~ starch*Nmass_notnc*volume, random=~campaign|ID, data=photo_chem, method="ML")

#model3 (adds campaign as fixed effect)
Amass_model3 <- lme(A_mass~ campaign*starch*Nmass_notnc*volume, random=~1|ID, data=photo_chem,method="ML")

anova(Amass_model, Amass_model2, Amass_model3)

#USE simply model function to step and delete down to the best model (choosing model with time)
timemod2 <- simp_mod(Amass_model3)
timemod3 <- timemod2$model.ml
timemod4 <- update(timemod3,~. -campaign:starch:Nmass_notnc)
anova(timemod3, timemod4)

timemod5 <- simp_mod(timemod4)
timemod6 <- update(timemod5$model.ml,~. -starch:Nmass_notnc)
Anova(timemod6)

timemod7 <- simp_mod(timemod6)
finalmod <- timemod7$model.reml
Anova(finalmod)

#plot the two seperate interactions
plot(allEffects(finalmod), "campaign:starch:volume", x.var = "starch")
plot(allEffects(finalmod), "campaign:Nmass_notnc:volume", x.var = "Nmass_notnc")
xyplot(A_mass ~ starch| volume + factor(campaign), data = photo_chem, type = c("p", "r"))
xyplot(A_mass ~ Nmass_notnc| volume + factor(campaign), data = photo_chem, type = c("p", "r"))


# #orginal model setup
# Anova(Amass_model)
# Amm2 <- update(Amass_model, method = "ML")
# Amm3 <- stepAIC(Amm2)
# Anova(Amm3)#minimal adequate mod
# Amm4 <- update(Amm3, method = "REML")
# Anova(Amm4)
# #significant effect of Nmass(volume) and Starch (volume) on Amax but no interaction between them
# summary(Amm4)
# 
# plot(allEffects(Amm4), "starch:volume", x.var="starch")
# plot(allEffects(Amm4), "Nmass_notnc:volume", x.var="Nmass_notnc")



#---------------------------------------------------------------------------------------
#plot stuff
manX <- seq(0, .25, .05)
meanX <- seq(0, .20, .05)

nX <- seq(0, 015, .005)
nmeanX <- seq(0, .01, .005)
#---------------------------------------------------------------------------------------
#amax and N interaction

windows(16,6)
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)

#raw
plot(A_mass ~ Nmass_notnc, data = photo_chem, col=volume, xlim=c(0, .02),ylim=c(100, 800), pch=pchs[volume], 
     axes=FALSE, xlab="", ylab="")
box()
axis(1, at = nX, labels = nX)
axis(2, labels=TRUE)
mtext(Amasslab, side=2, line=2.5, cex=1.3)
title(main="Amax*Leaf N * volume, p < 0.0122", line=-1.5, cex.main=.75, font.main=1,adj=.05)

d_ply(photo_chem, .(volume), function(x) add_trend_line("Nmass_notnc", "A_mass", x, ))

#volume means
plot(A_mass.mean ~ Nmass_notnc.mean, data = photo_chem_agg, col=volume, xlim=c(0, .0125),ylim=c(100, 800),
     pch=pchs[volume], ylab="", axes=FALSE,cex=2)
box()
axis(1, at = nmeanX, labels = nmeanX)
mtext(nfree, side=1, line=2.5, cex=1.3)
title(main=volmean, line=-1.5, font.main=1, adj=.05, cex.main=.75)
legend("bottomright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n', cex=.75)

#means by date
plot(A_mass.mean ~ Nmass_notnc.mean, data = photo_chem_campaign, col=volume, xlim=c(0, .0125),
     ylim=c(100, 800),pch=pch2[volume], cex=2, ylab="", axes=FALSE)
box()
axis(1, at = nmeanX, labels = nmeanX)
title(main=datemean, line=-1.5, font.main=1, adj=.05, cex.main=.75)
d_ply(photo_chem_campaign, .(volume), function(x) c(add_trend_line("Nmass_notnc.mean", "A_mass.mean", x),
                                    text(x$A_mass.mean~x$Nmass_notnc.mean, labels=unique(x$campaign), 
                                         font=2,cex=.75)))
dev.copy2pdf(file="output/stats_plots/amaxnfree.pdf")
dev.off()
#---------------------------------------------------------------------------------------------------

#amax and starch interaction

windows(16,6)
par(mfrow=c(1,3),omi=c(1,1,0.2,0.2), mar=c(0,0,0,0), cex=1.3)

#raw
plot(A_mass ~ starch, data = photo_chem, col=volume, xlim=c(0, .3),ylim=c(100, 800), pch=pchs[volume], 
     axes=FALSE, xlab="", ylab="")
box()
axis(1, at = manX, labels = manX)
axis(2, labels=TRUE)
mtext(Amasslab, side=2, line=2.5, cex=1.3)
title(main="Amax*starch* volume, p < 0.0001", line=-1.5, font.main=1, adj=.1, cex.main=.75)

d_ply(photo_chem, .(volume), function(x) add_trend_line("starch", "A_mass", x, ))

#volume means
plot(A_mass.mean ~ starch.mean, data = photo_chem_agg, col=volume, xlim=c(0, .2),ylim=c(100, 800),
     pch=pchs[volume], ylab="", axes=FALSE,cex=2)
box()
axis(1, at = meanX, labels = meanX)
mtext(starchlab, side=1, line=2.5, cex=1.3)
title(main=volmean, line=-1.5, font.main=1, adj=.1, cex.main=.75)
legend("topright", leglab, pch=pchs,text.font=1, inset=0.02,title=vollab, col=palette(), bty='n', cex=.75)
#means by date
plot(A_mass.mean ~ starch.mean, data = photo_chem_campaign, col=volume, xlim=c(0, .2),
     ylim=c(100, 800),pch=pch2[volume], cex=2, ylab="", axes=FALSE)
box()
axis(1, at = meanX, labels = meanX)
title(main=datemean, line=-1.5, font.main=1, adj=.1, cex.main=.75)
d_ply(photo_chem_campaign, .(volume), function(x) c(add_trend_line("starch.mean", "A_mass.mean", x),
                                                    text(x$A_mass.mean~x$starch.mean, labels=unique(x$campaign), 
                                                         font=2,cex=.75)))
dev.copy2pdf(file="output/stats_plots/amaxstarch.pdf")
dev.off()




#model area based---------------------------------------------------------------------

#variables for area based model

# #Amax per unit leaf area
# photo_chem$A_area <- with(photo_chem, Photo/area)
# #tnc on area basis
# photo_chem$tncarea <- with(photo_chem, ((leafsugar+leafstarch)/area))
# #n on area bases
# photo_chem$narealeaf_notnc <- with(photo_chem, nmassleaf_notnc/area) 
# 
# #model
# Aarea_model <- lme(Photo~ tncarea*narealeaf_notnc*volume, random=~1|ID, data=photo_chem)
# Anova(Aarea_model)
# Aam2 <- update(Aarea_model, method = "ML")
# Aam3 <- stepAIC(Aam2)
# Anova(Aam3)
# Aam4 <- update(Aam3, method = "REML")
# Anova(Aam4)

# summary(Aam4)
# plot(allEffects(Aam4), x.var="narealeaf_notnc")
# summary(photo_chem)
# plot(Photo ~ narealeaf_notnc, data = photo_chem, col = volume, pch = 16)


