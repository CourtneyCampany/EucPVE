#this script is for sourcing the Amax vs TNC and N plots into master
source("functions and packages/startscripts.R")

source("functions and packages/startscripts.R")
require(lme4)
require(lmerTest)
library(effects)


photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)

#plot predicted and observed in bins------------------------------------------------------

#make bin levels and mid points for both starch and nitrogen
starchbin <- c(0, 0.04, 0.08, 0.12, 0.16, 100)
starchmid <- c(0.02, 0.06, 0.1, 0.14, 0.2)

nitrobin <- c(0, 0.0025, 0.005, 0.0075, 0.01, 100)
nitromid <- c(0.00125, 0.00375, 0.00625, 0.00875, 0.0125)

#set bins in raw dfr
photo_chem$starchbin <- cut(photo_chem$starch, breaks = starchbin)
photo_chem$nitrobin <- cut(photo_chem$Nmass_notnc, breaks = nitrobin)


#model and stats
Afit_full <- lmer(A_mass ~ Nmass_notnc+starch+Nmass_notnc:starch + (1|ID), data=photo_chem)
#####model parameters for plotting
f <- fixef(Afit_full)


###same model with fixed effects in different order for ease of plotting preductions later
Afit2_full <- lmer(A_mass ~ starch+Nmass_notnc+starch:Nmass_notnc + (1|ID), data=photo_chem)
f2 <- fixef(Afit2_full)


#plot and predict (2 graphs)---------------------------------------------------------------------
cols <- c("green3", "cyan4", "#0081FFFF", "darkviolet", "red")
#cols <- c("forestgreen", "#80FE1AFF","#ECFD08FF","#FF7500FF","#FF3300FF")
#coln <- c("#FF3300FF", "#FF7500FF","#ECFD08FF",  "#80FE1AFF", "forestgreen" )
coln <- c("red", "darkviolet", "#0081FFFF", "cyan4", "green3")
binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
binlab2 <- c(" 0    -.001", ".001-.003", ".003-.006", ".006-.008", ".008-.012")

pchbin <- c(1,2,3,4,5)
legpch <- c(5, 10, 15, 20, 25, 35, "free")



###two panel of graph above, using predict------------------------------------------------------------
require(scales)

coln2 <- alpha(coln, .75)
cols2 <- alpha(cols, .75)

#using predict instead of eqautions
Npred <- expand.grid(starch = c(0.02, 0.06, 0.1, 0.14, 0.2),
                     Nmass_notnc = seq(0.0007432, 0.0175502, length=101))
Npred$Amass_pred <- predict(Afit_full, Npred, re.form=NA)

TNCpred <- expand.grid(Nmass_notnc = c(0.00125, 0.00375, 0.00625, 0.00875, 0.0125),
                       starch = seq(min(photo_chem$starch), max(photo_chem$starch), length=101))
TNCpred$Amass_pred <- predict(Afit2_full, TNCpred, re.form=NA)

##plot
alab <- expression(A[mass]~~(n*mol~g^-1~s^-1))


windows(10, 7)
par(cex.axis=.96, cex.lab=1.2,mfrow=c(1,2),  oma=c(0.1,0.1,0.1,0.1), las=1)

# First Panel
par(mar=c(5,5,2,0))
plot(A_mass ~ starch, data=photo_chem,type='n',ylim=c(0,800), xlim=c(0, 0.275),ylab=alab, axes=FALSE, xlab="")  
box()
axis(1, labels=TRUE) 
axis(2, labels=TRUE)  
#mtext(alab, outer=TRUE, line=2.5, side=2, cex=1.5)
points(A_mass ~ starch, pch=21, data=photo_chem, bg=coln2[nitrobin], ylim=c(0,800), ylab="",
       xlab="",xlim=c(0, .275),cex=1.2, ylab="")

for(i in 1:length(unique(TNCpred$Nmass_notnc))){
  m <- subset(TNCpred, Nmass_notnc == unique(TNCpred$Nmass_notnc)[i])
  with(m, lines(starch, Amass_pred, col=coln[i], lwd=2))
}
text(0, 805, "(a)", adj=-.1, cex=1.2)
mtext(starchlab, cex=1.2, line=3.5, side=1)
legend("topright", binlab2, pch=21, text.font=1.2, inset=0.02, pt.bg=cols2,title=nfree , bty='n', cex=1.2) 
 
# Second Panel
par(mar=c(5,0,2,2))
plot(A_mass ~ Nmass_notnc, data=photo_chem, type='n',ylab="", axes=FALSE, xlim=c(0, 0.02), ylim=c(0,800), xlab="")
box()
axis(1, labels=TRUE) 
axis(2, labels=FALSE, tcl=0.5)  

points(A_mass ~ Nmass_notnc, data=photo_chem, pch=21, bg=cols2[starchbin], 
       ylab="", xlab="",ylim=c(0,800),cex=1.2)

for(i in 1:length(unique(Npred$starch))){
  d <- subset(Npred, starch == unique(Npred$starch)[i])
  with(d, lines(Nmass_notnc, Amass_pred, col=cols[i], lwd=2))
}
text(0, 805, "(b)", adj=-.1, cex=1.2)
mtext(nmass_noTNC, side=1, cex=1.2, line=3.5)
legend("bottomright", binlab, pch=21, text.font=1.2, inset=0.02, pt.bg=coln2,title=starchlab , bty='n', cex=1.2)

# dev.copy2pdf(file= "master_scripts/manuscript_figs/A_leafchem.pdf")
# dev.off()
