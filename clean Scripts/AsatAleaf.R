#apply photosynthesis model to generate PS over the experiment
#compare to actual measurements

source("functions and packages/startscripts.R")

#Read in spot A measurements and merge plot design
#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

#read in gas exchange master file, all dates
gasexchange <- read.csv("raw data/AsatAmax_master.csv")
  gasexchange$Date <- as.Date(gasexchange$Date)
  gasexchange$ID <- paste(gasexchange$plot, gasexchange$pot, sep = "-")

PS <- merge(plotsumm, subset(gasexchange, select = c("campaign", "ID", "CO2", "Photo", "RH_S")))
  PS$type <- factor(ifelse (PS$CO2 == "400", "Asat", "Amax"))

#run function to add campaign date
PS <- add_campaign_date(PS)

asat <- PS[PS$CO2 == "400",]
##get rh values from licor master
rh <- asat[, "RH_S"]

#READ DATA, calculated data for A parameters

#generated jmax and vcmax
jmax_vcmax <- read.csv("calculated data/jmax_vcmax_clean.csv")

#Rdark Q10 equations by volume
rdarkq10 <- read.csv("calculated data/rdarkq10.csv")
rd25_clean <- read.csv("calculated data/rdark_clean.csv")

#---------------------------------------------------------------------------------------------
#merge ps parameters to met data

A_model <- merge(jmax_vcmax, rdarkq10[,1:2], by="volume")
A_model <- merge(A_model, rd25_clean[,1:2], by="volume")

q25_drake <- 1.86

#rdark_eq$rd25_euct <- with(rdark_eq, rd12.3*(q25_drake^((abs(CTleaf-25))/10)))

#refit RD
A_model$Rd_pred2 <- with(A_model, rd12.3 * q25_drake^((25-12.3)/10))

#------------------------------------------------------------------------------------------------------
#input parameters from optimal conductance model (using nls)

g1 <- read.csv("calculated data/g1_pred.csv")
g1_agg <- summaryBy(g1_date ~ volume, data=g1, keep.names=TRUE)


A_model <- merge(A_model, g1_agg, by="volume")

#-------------------------------------------------------------------------------------------------
#now run the model (includes pred Rdark from crouseq10, and gs parameters modelled from spot measurements)

#convert RH to VPD
VPD_obs <- RHtoVPD(rh, 25, Pa=101)

#model, should return the aleaf for every 15 minutes. (will retrun Aleaf at 15min interval)
A_sat_pred <- Photosyn(VPD=1.06,Ca=400, PPFD=1800, Tleaf=25, 
                   Jmax=A_model$Jmax.mean, Vcmax=A_model$Vcmax.mean, Rd=A_model$Rd_pred2, g1=A_model$g1_date)


##compare to Asat
A_means <- read.csv("calculated data/A_treatment_means.csv")
A_means$volume <- as.numeric(c("5", "10", "15", "20", "25", "35", "1000"))
Asat <- A_means[, c(1, 4)]

Acompare <- cbind(Asat, A_sat_pred[,2])
names(Acompare)[3] <- "Aleaf"


require(nlme)
library(multcomp)

#relevel to free to evaluate container effect  
asat$volume <- as.factor(asat$volume)
asat$volume <- relevel(asat$volume, ref="1000")
asat$block <- as.factor(gsub("-[1-9]", "", asat$ID))

#asat
 asat_lm <- lme(Photo ~ volume, random= ~1|ID, data=asat)
#   anova(asat_lm)
#   summary(asat_lm)
asat_lm2 <- lme(Photo ~ volume, random= ~1|block/ID, data=asat)
# anova(asat_lm2)
confint(asat_lm2)
asat_lm3 <- lm(Photo ~ volume, data=asat)

newdata <- data.frame(volume=c(5, 10, 15, 20, 25, 35, 1000))
newdata$volume <- as.factor(newdata$volume)
predict(asat_lm2, newdata, interval="confidence") 

