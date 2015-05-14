source("functions and packages/functions.R")
source("functions and packages/plot objects.R")
library(doBy)

c13 <- read.csv("raw data/harvest_c13.csv")


#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#merge with plot summary
c13 <- merge(c13[,c(2,4)], plotsumm[,3:4], by ="ID")
c13$volume <- as.factor(c13$volume)

c13_agg <- summaryBy(d13c ~ volume, data=c13, FUN =c(mean, se))
write.csv(c13_agg, "calculated data/c13_means.csv", row.names=FALSE)

windows(7,5)
par(mar=c(5,5,2,2), cex.axis=0.8, las=1)
bar(d13c, volume, c13,half.errbar=FALSE, ylim=c(-32,0), col="grey",, legend=FALSE, xlab=vollab, ylab="")
title(ylab=c13lab, mgp=ypos)

dev.copy2pdf(file= "master_scripts/manuscript_figs/leafc13.pdf")
dev.off()


##Stats for c13-------------------------------------------------------------------------------------
# require(nlme)
# require(visreg)
# library(multcomp)
# 
# c13$volume <-  relevel(c13$volume, ref="1000")
# 
# #srl (not different)
# c13_container <- lme(d13c ~ volume, random= ~1|ID, data=c13)
# anova(c13_container)
# summary(c13_container)
# visreg(c13_container)
# 
# tukey_c13<- glht(c13_container, linfct = mcp(volume = "Tukey"))
# c13_siglets <-cld(tukey_c13)
# c13_siglets2 <- c13_siglets$mcletters$Letters
# write.csv(c13_siglets2, "master_scripts/sigletters/sigletts_plant/sl_c13.csv", row.names=FALSE)  
