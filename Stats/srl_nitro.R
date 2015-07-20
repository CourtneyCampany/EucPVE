library(doBy)
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")


#SRL------------------------------------------------------------------------------------------------------
srl <- read.csv("raw data/SRLmass.csv")
  srl$ID <- paste(srl$plot, srl$pot, sep = "-")
  srl <- merge(srl, plotsumm[3:4], all=TRUE)
  srl <- subset(srl, !is.na(volume))
  srl$SRL <- with(srl, (total_length_cm/100)/(srl_fw*(ss_dw/ss_fw)))
  srl$volume <- as.factor(srl$volume)
  row.names(srl) <- NULL
  
srl <- vollab_func(srl)

srl_clean <- srl[srl$ID != c("2-8", "1-6", "2-4"),]  
srl2 <- srl[srl$SRL <= 100,]


###leaf N

photo_chem <- read.csv("calculated data/Amax_chem.csv")
  #run volume format func
  photo_chem<- vollab_func(photo_chem)
  #simple leaf N %
  photo_chem$leafnperc <- with(photo_chem, Nperc*100)

N_harvest <- photo_chem[photo_chem$campaign ==6,]

###root N
rootN <- read.csv("calculated data/root_N_clean.csv")  
rootN$volume <- gsub("free", 1000, rootN$volume)  
frootN <- rootN[, c(1:2, 5, 8, 10, 12)]

##merge data

N_srl <- merge(N_harvest[, c("ID", "volume", "Nperc", "Nmass", "Narea")], 
               srl[,c("ID", "volume", "SRL")], all=TRUE)

N_srl2 <- merge(N_srl, frootN)


plot(Narea~SRL, data=N_srl, col=volume, pch=16, xlim=c(0, 150), ylim=c(0,1.1))

plot(N_perc~SRL, data=N_srl2, col=volume, pch=16, xlim=c(0, 150), ylim=c(0.5,1.1))


####leaf Narea--------------------------------------------------------------------------------------------------

##leafN
Nsrl_mod <- lme(Nperc ~ SRL ,random= ~1|ID, data=N_srl)
summary(Nsrl_mod)
anova(Nsrl_mod)
visreg(Nsrl_mod)

##rootN
Nsrl_mod <- lme(N_perc ~ SRL ,random= ~1|ID, data=N_srl2)
summary(Nsrl_mod)
anova(Nsrl_mod)
visreg(Nsrl_mod)


srl_mod <- lme(SRL ~ volume ,random= ~1|ID, data=N_srl)
summary(srl_mod)
anova(srl_mod)
visreg(Nsrl_mod)


