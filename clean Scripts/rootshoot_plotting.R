
source("functions and packages/startscripts.R")
seedlingmass<- read.csv("calculated data/seedling mass.csv")   

ratio <- subset(seedlingmass, select = c("ID", "volume", "fineroot", "leafmass", "root", "shoot"))
  ratio$volume <- as.factor(ratio$volume)
  ratio_nofree <- subset(ratio, volume !="1000")
  ratio$RS <- with(ratio, root/shoot)
  ratio$LR <- with(ratio, fineroot/leafmass)

#treatment means
ratio_agg <- summaryBy( .~ volume , data = ratio,  FUN=c(mean,se))

##root shoot for paper table-----------------------------------------------------------------------------
rs_agg <- summaryBy( RS+LR~ volume , data = ratio,  FUN=c(mean,se))
write.csv(rs_agg, "calculated data/rootshoot_mean.csv", row.names=FALSE)

require(nlme)
require(visreg)
library(multcomp)

#rs(not different)
rs_lm <- lme(RS~ volume, random= ~1|ID, data=ratio)
anova(rs_lm)
summary(rs_lm)

#lr(not different)
LR_lm <- lme(LR~ volume, random= ~1|ID, data=ratio)
anova(LR_lm)
summary(LR_lm)



rs_mean <- with(ratio, mean(RS))
rl_mean <- with(ratio, mean(LR))


#PLOT of Froot:leaf means with SE-------------------------------------------------------------------------
par(mar=c(5,5,1,1), cex.axis=1.0, cex.lab=LABcex)
with(ratio_agg, plot(log(fineroot.mean), log(leafmass.mean), ylim=c(0,5), xlim=c(0,5),
                     pch=pchs, col=palette(), cex=PTcex,
                     xlab = "ln(Fine Root Mass) (g)",
                     ylab = "ln(Leaf Mass) (g)"))

with(ratio_agg, arrows(x0=log(fineroot.mean), y0=log(leafmass.mean), x1=log(fineroot.mean+fineroot.se), angle=90, 
                        length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=log(fineroot.mean), y0=log(leafmass.mean), x1=log(fineroot.mean-fineroot.se), angle=90, 
                        length=0.05,col=palette(), lwd=2))

with(ratio_agg, arrows(x0=log(fineroot.mean), y0=log(leafmass.mean), y1=log(leafmass.mean+leafmass.se), angle=90, 
                        length=0.05,col=palette(), lwd=2))
with(ratio_agg, arrows(x0=log(fineroot.mean), y0=log(leafmass.mean), y1=log(leafmass.mean-leafmass.se), angle=90, 
                        length=0.05,col=palette(), lwd=2))

abline(0,1)
legend("topleft", leglab, pch=pchs,text.font=3, inset=0.01, 
       title=expression(Pot~volume~(l)), col=palette(), bty='n')

# #dev.copy2pdf(file= "output/Leaf_Froot.pdf")
# ------------------------------------------------------------------------------------------------------
#PLOT of Root:Shoot means with SE
#mar=c(5,5,1,1)
windows(8,6)
png(filename = "output/rootshoot.png", width = 8, height = 6, units = "in", res= 400)
par(cex.axis=1.25, cex.lab=1.25, mar=c(5,5,2,2))
with(ratio_agg, plot(root.mean, shoot.mean, ylim=c(0,100), xlim=c(0,100),
                     pch=pchs,col=palette(), cex=1.2,
                     xlab = "Root Mass (g)",
                     ylab = "Shoot Mass (g)"))

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean+root.se, angle=90, 
                        length=0.05,col=palette(), lwd=1.5))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, x1=root.mean-root.se, angle=90, 
                       length=0.05,col=palette(), lwd=1.5))

with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean+shoot.se, angle=90, 
                       length=0.05,col=palette(), lwd=1.5))
with(ratio_agg, arrows(x0=root.mean, y0=shoot.mean, y1=shoot.mean-shoot.se, angle=90, 
                       length=0.05,col=palette(), lwd=1.5))

abline(0,1, lty=2)
legend("topleft", leglab, pch=pchs, inset=0.02, title=expression(Pot~volume~(l)), col=palette(), bty='n',cex=1)
mtext(expression(paste(italic("Eucalpytus tereticornis"), " seedlings", sep=" ")), side=3,line=.5, cex=1.25)
box()
dev.off()
#dev.copy2pdf(file= "output/Shoot_Root.pdf")