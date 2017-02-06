#apply photosynthesis model to generate PS over the experiment
#compare to actual measurements

source("functions and packages/startscripts.R")

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
A_model$VPD <- RHtoVPD(A_model$RH, A_model$temp, Pa=101)

#model, should return the aleaf for every 15 minutes. (will retrun Aleaf at 15min interval)
A_sat_pred <- Photosyn(RH=70,Ca=400, PPFD=1800, Tleaf=25, 
                   Jmax=A_model$Jmax.mean, Vcmax=A_model$Vcmax.mean, Rd=A_model$Rd_pred2, g1=A_model$g1_date)


##compare to Asat
A_means <- read.csv("calculated data/A_treatment_means.csv")
A_means$volume <- as.numeric(c("5", "10", "15", "20", "25", "35", "1000"))
Asat <- A_means[, c(1, 4)]

Acompare <- cbind(Asat, A_sat_pred[,2])
names(Acompare)[3] <- "Aleaf"
