#this script is for sourcing the Amax vs TNC and N plots into master
source("functions and packages/startscripts.R")
library(lme4)
library(lmerTest)
library(effects)


photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)
#get better units for plotting
photo_chem$starch_perc <- photo_chem$starch*100
photo_chem$nitro_mg <- photo_chem$Nmass_notnc *1000


photo_chem$starch_area <- with(photo_chem, massarea * (starch_perc/100))
photo_chem$Narea2 <- with(photo_chem, lma_notnc * (Npercentage/100))

legpch <- c(5, 10, 15, 20, 25, 35, "free")

windows(10, 7)
par(cex.axis=.96, cex.lab=1.2,mfrow=c(1,2),  oma=c(0.1,0.1,0.1,0.1), las=1)
plot(Photo ~ Narea2,data=photo_chem)  
plot(Photo ~ starch_area,data=photo_chem) 


###two panel of graph above, using predict------------------------------------------------------------


##plot
alab <- expression(A[mass]~~(n*mol~g^-1~s^-1))
nitro_noTNC <- expression(Leaf~Nitrogen[TNC[free]]~~(mg~g^-1))


windows(10, 7)

par(cex.axis=.96, cex.lab=1.2,mfrow=c(1,2),  oma=c(0.1,0.1,0.1,0.1), las=1)

# First Panel
par(mar=c(5,5,2,0))
plot(A_mass ~ starch_perc, data=photo_chem,type='n',ylim=c(0,800), xlim=c(0, 27.5),ylab=alab, axes=FALSE, xlab="")  
box()
axis(1, labels=TRUE) 
axis(2, labels=TRUE)  
#mtext(alab, outer=TRUE, line=2.5, side=2, cex=1.5)
points(A_mass ~ starch_perc, pch=21, data=photo_chem, bg=coln2[nitrobin], ylab="",
       xlab="",cex=1.2, ylab="")

for(i in 1:length(unique(TNCpred$nitro_mg))){
  m <- subset(TNCpred, nitro_mg == unique(TNCpred$nitro_mg)[i])
  with(m, lines(starch_perc, Amass_pred, col=coln[i], lwd=2))
}
text(0.5, 805, "(a)", cex=1.2)
mtext("Leaf Starch (%)", cex=1.2, line=3.5, side=1)
legend("topright", binlab2, pch=21, text.font=1, inset=0.02, pt.bg=coln2,title=nitro_noTNC , bty='n', cex=1) 

# Second Panel
par(mar=c(5,0,2,2))
plot(A_mass ~ nitro_mg, data=photo_chem, type='n',ylab="", axes=FALSE, xlim=c(0, 20), ylim=c(0,800), xlab="")
box()
axis(1, labels=TRUE) 
axis(2, labels=FALSE, tcl=0.5)  

points(A_mass ~ nitro_mg, data=photo_chem, pch=21, bg=cols2[starchbin], ylab="", xlab="",cex=1.2)

for(i in 1:length(unique(Npred$starch_perc))){
  d <- subset(Npred, starch_perc == unique(Npred$starch_perc)[i])
  with(d, lines(nitro_mg, Amass_pred, col=cols[i], lwd=2))
}
text(0.75, 805, "(b)", cex=1.2)
mtext(nitro_noTNC, side=1, cex=1.2, line=3.5)
legend("bottomright", binlab, pch=21, text.font=1, inset=0.02, pt.bg=coln2,title="Leaf Starch (%)" , bty='n', cex=1)

