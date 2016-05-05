source("functions and packages/plot objects.R")
library(doBy)

### this replaces the model scenarios
### calulate total net daily carbon gain (modelled using gross, net and shading)
### compare to harvest mass, calculate CUE

##read data-----------------------------------------------------------------------------------------------------------------
finalmass <- read.csv("calculated data/harvest_mass_means.csv")

la_pred <- read.csv("Calculated data/LApred_volume.csv")
  names(la_pred)[3] <- "LA"
  
Cday_net <- read.csv("calculated data/Aleaf_model/cday_120_clean.csv")
Cday_gross <- read.csv("calculated data/Aleaf_model/cday_120_clean_gross.csv")

##Cday with leaf area needs self shading (use slope intercept, from 5-1000 vol)
sigma <- read.csv("calculated data/M_leafarea_model.csv")


#Calculate total net daily carbon day per tree with Cday and real leaf area------------------------------------------------
  dailyCnet <- merge(la_pred,Cday_net)
  ##this needs to include self shadeing (M as a linear function of leaf area)
  dailyCnet <- merge(dailyCnet, sigma[, c(2,3,5)], by="volume")
  dailyCnet$M <- with(dailyCnet, b*LA+intercept)
    #calculate total daily C gain with self shading
  dailyCnet$tdcg <- with(dailyCnet, LA * carbon_day * M)

#Calculate total seedling C gain over experiment (120d)
  plantCnet <- summaryBy(tdcg ~ volume, FUN=sum, data=dailyCnet)

#Compare estimated Daily C gain with final mass from harvest
  harvestC <- merge(finalmass, plantCnet)
  harvestC$massC <- harvestC$mass*.5
  
  
##repeat with gross Cday-----------------------------------------------------------------------------------------------------

dailyCgross <- merge(la_pred,Cday_gross)
  ##this needs to include self shadeing (M as a linear function of leaf area)
  dailyCgross <- merge(dailyCgross, sigma[, c(2,3,5)], by="volume")
  dailyCgross$M <- with(dailyCgross, b*LA+intercept)
  #calculate total daily C gain with self shading
  dailyCgross$tdcg <- with(dailyCgross, LA * carbon_day * M)
  
  #Calculate total seedling C gain over experiment (120d)
  plantC_gross <- summaryBy(tdcg ~ volume, FUN=sum, data=dailyCgross)
  
  #Compare estimated Daily C gain with final mass from harvest
  harvestC_gross <- merge(finalmass, plantC_gross)
  harvestC_gross$massC <- harvestC$mass*.5
  

#mean Cday (net and gross)----------------------------------------------------------------------------------------------------
  anet_vol <- summaryBy(. ~ volume, data=Cday_net, FUN=mean)
  agross_vol <- summaryBy(. ~ volume, data=Cday_gross, FUN=mean)
  
  #merge with tdcg dfr
  harvestC2 <- merge(harvestC, anet_vol)
  harvestC2_gross <- merge(harvestC_gross, agross_vol) 
  

  
###PLOT------------------------------------------------------------------------------------------------------------------------
#####manuscrupt figure (two panel of C in biomass to either Cday sum or Cdaygross sum)
pch3 <- c(rep(1,6), 6)
leglab3 <- c(5, 10, 15, 20, 25, 35, "Free","Gross", "Net")
legcol <- c("red", "#D4002A","#AA0055","#7F007F","#5500AA","#2A00D4","blue" ,"black", "black")

windows(7,7)

# Fraction of net Daily Carbon gain to C mass
with(harvestC2, plot(massC/tdcg.sum, massC,col=as.factor(volume), pch=pchs, cex=1.5,
                     xlim=c(0.25,0.45), ylim=c(0, 100),
                      ylab = "Seedling Carbon Mass (g)",
                      xlab = "Carbon Use Efficiency"))

with(harvestC2_gross, points(massC/tdcg.sum, massC,col=as.factor(volume), pch=pch3, cex=1.5))

legend("topleft", leglab3, pch=c(rep(16, 6), 17, 1,16), text.font=3, inset=0.01, title=vollab, 
       col=legcol, bty='n',cex=1)

dev.copy2pdf(file= "master_scripts/manuscript_figs/CUE.pdf")
dev.off()


#other plots---------------------------------------------------------------------------------------------------
  
  
#compare total carbon gain to the fraction that ends up in biomass (35% pots, 41% free)
with(harvestC, plot(tdcg.sum, massC/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
                      xlab = "Total Seedling Carbon Assimilation (g C)", 
                      ylab="CUE",xlim=c(0, 250), ylim=c(.25,.45)))
  
  
#Same with Cday (also make 1-CUE as leftover)
with(harvestC2, plot(carbon_day.mean, massC/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
                       xlim=c(5,8.5),  ylim=c(.25,.45), xlab = cdaylab))  
  
with(harvestC2, plot(carbon_day.mean, 1- massC/tdcg.sum,col=as.factor(volume), pch=c(rep(16, 6), 17), cex=1.5,
                       xlim=c(5,8.5),  ylim=c(.5,.8), xlab = cdaylab))  


windows(7,7)

#----------------------------------------------------------------------------------------------------------------------------
dat3 <- harvestC2
  # dat3$volume <- gsub(1000, "free", dat3$volume)
  # dat3$volume <- gsub("^5", "05", dat3$volume)
  # dat3$volume <- as.factor(dat3$volume)
  dat3$cue <- with(dat3,massC/tdcg.sum )

# Fraction of net Daily Carbon gain to C mass
plot(cue~as.factor(volume), data=dat3,col=as.factor(volume), pch=21, ylim=c(.25,.5))

plot(massC/tdcg.sum~ as.factor(volume),col=as.factor(volume), data=harvestC_gross,ylim=c(.25,.5))

plot(massC/tdcg.sum ~ as.factor(volume),col=as.factor(volume), pch=21,cex=1.5, data=harvestC2,ylim=c(.25,.5))



