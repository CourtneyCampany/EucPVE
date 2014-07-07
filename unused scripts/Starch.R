source("functions and packages/load packages.R")

#read in plot design and TNC data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

starch <- read.csv("raw data/starch_harvest.csv")
names(starch)[1] <- "ID"

#standard means
standard <- starch[1:6,]
standard_agg <- summaryBy( Abs510 ~ ID , data = standard,  FUN=c(mean))
#convert mean of 100 standard from abs to ug
standard100 <- with(standard, (Abs510[4] +Abs510[6])/2)
fvalue <- 100/standard_agg[2,2]

#Data subset
starch1 <- starch[7:150,]

starch_agg <- summaryBy( .~ ID , data = starch1,  FUN=c(mean, sd, length))
starch_agg$abs_SE <- with(starch_agg, Abs510.sd/sqrt(Abs510.length))

starch_agg$mg_glu_sample <- (100*starch_agg$Abs510.mean*100*162)/(1000*180)

starch_agg$g_glu_g_DW <- (100*starch_agg$Abs510.mean*100*162)/(1000*180*starch_agg$mass.mean)
starch_agg$mg_glu_per_gDW <- starch_agg$g_glu_g_DW*1000

starch_agg$percglu <- starch_agg$Abs510.mean * (standard100 / starch_agg$mass.mean) *10*.9
starch_agg$starchcontent <- starch_agg$percglu *10


#merge with plot summary
starch_agg <- merge(starch_agg, plotsumm, by = "ID")
starch_agg$volume <- as.factor(starch_agg$volume)


#Stats
getP <- function(x)anova(x)[[5]][1]

#POT effect
starchlm <- lm(starchcontent ~ volume, data=starch_agg)
getP(starchlm)

# Pot size effect.
starch_potsize <- lm(starchcontent ~ volume, data=starch_agg, subset=volume != "1000")
getP(starch_potsize)


#Plotting
with(starch_agg, bargraph.CI(as.factor(volume), starchcontent))
                                         

