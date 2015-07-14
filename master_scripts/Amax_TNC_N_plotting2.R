#this script is for sourcing the Amax vs TNC and N plots into master
source("functions and packages/startscripts.R")
require(lme4)
require(lmerTest)
library(effects)


photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)
  #get better units for plotting
  photo_chem$starch_perc <- photo_chem$starch*100
  photo_chem$nitro_mg <- photo_chem$Nmass_notnc *1000

#plot predicted and observed in bins------------------------------------------------------

#make bin levels and mid points for both starch and nitrogen
starchbin <- c(0, 4, 8, 12, 16, 100)
starchmid <- c(2, 6, 10, 14, 20)

nitrobin <- c(0, 2.5, 5, 7.5, 10, 100)
nitromid <- c(1.25, 3.75, 6.25, 8.75, 12.5)

#set bins in raw dfr
photo_chem$starchbin <- cut(photo_chem$starch_perc, breaks = starchbin)
photo_chem$nitrobin <- cut(photo_chem$nitro_mg, breaks = nitrobin)

#model and stats
Afit_full <- lmer(A_mass ~ nitro_mg+starch_perc+nitro_mg:starch_perc + (1|ID), data=photo_chem)
#####model parameters for plotting
f <- fixef(Afit_full)

###same model with fixed effects in different order for ease of plotting preductions later
Afit2_full <- lmer(A_mass ~ starch_perc+nitro_mg+starch_perc:nitro_mg + (1|ID), data=photo_chem)
f2 <- fixef(Afit2_full)

#plot and predict (2 graphs)---------------------------------------------------------------------
cols <- c("green3", "cyan4", "#0081FFFF", "darkviolet", "red")
#cols <- c("forestgreen", "#80FE1AFF","#ECFD08FF","#FF7500FF","#FF3300FF")
#coln <- c("#FF3300FF", "#FF7500FF","#ECFD08FF",  "#80FE1AFF", "forestgreen" )
coln <- c("red", "darkviolet", "#0081FFFF", "cyan4", "green3")
binlab <- c("0-4", "4-8", "8-12", "12-16", ">16")
binlab2 <- c("0-1", "1-3", "3-6", "6-8", ">8")

pchbin <- c(1,2,3,4,5)
legpch <- c(5, 10, 15, 20, 25, 35, "free")

coln2 <- alpha(coln, .75)
cols2 <- alpha(cols, .75)

###two panel of graph above, using predict------------------------------------------------------------

#using predict instead of eqautions
Npred <- expand.grid(starch_perc = c(2, 6, 10, 14, 20),
                     nitro_mg = seq(min(photo_chem$nitro_mg), max(photo_chem$nitro_mg), length=101))
Npred$Amass_pred <- predict(Afit_full, Npred, re.form=NA)

TNCpred <- expand.grid(nitro_mg = c(1.25, 3.75, 6.25, 8.75, 12.5),
                       starch_perc = seq(min(photo_chem$starch_perc), max(photo_chem$starch_perc), length=101))
TNCpred$Amass_pred <- predict(Afit2_full, TNCpred, re.form=NA)

##plot
alab <- expression(A[mass]~~(n*mol~g^-1~s^-1))
nitro_noTNC <- expression(Leaf~Nitrogen[TNC[free]]~~(mg~g^-1))


#windows(10, 7)
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
  text(0, 805, "(a)", cex=1.2)
  mtext("Leaf Starch (%)", cex=1.2, line=3.5, side=1)
  legend("topright", binlab2, pch=21, text.font=1.2, inset=0.02, pt.bg=coln2,title=nitro_noTNC , bty='n', cex=1.2) 
 
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
  text(0.5, 805, "(b)", cex=1.2)
  mtext(nitro_noTNC, side=1, cex=1.2, line=3.5)
  legend("bottomright", binlab, pch=21, text.font=1.2, inset=0.02, pt.bg=coln2,title="Leaf Starch (%)" , bty='n', cex=1.2)

# dev.copy2pdf(file= "master_scripts/manuscript_figs/A_leafchem.pdf")
# dev.off()




####png of starch xaxis
png(filename = "master_scripts/manuscript_figs/png/leafchem.png", width = 11, height = 8.5, units = "in", res= 400)
par(mar=c(5,5,2,2),cex.axis=1.5, cex.lab=1.75,  las=1)
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
mtext("Leaf Starch (%)", cex=1.25, line=3.5, side=1)
legend("topright", binlab2, pch=21, text.font=1.2, inset=0.02, pt.bg=coln2,title=nitro_noTNC , bty='n', cex=1.2) 
dev.off()