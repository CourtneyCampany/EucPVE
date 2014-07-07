library(doBy)
library(sciplot)

#read in plot design and TNC data
plotsumm <- read.csv("plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

starch <- read.csv("starch_ps3.csv")
#names(starch)[1] <- "ID"
starch$ID <- gsub ("'", "", starch$ID)

#standard means
standard <- starch[1:6,]
standard_agg <- summaryBy( abs510 ~ ID , data = standard,  FUN=c(mean))
#convert mean of 100 standard from abs to ug
standard100 <- with(standard, (abs510[4] + abs510[6])/2)
fvalue <- 100/standard_agg[2,2]

#Data subset
starch1 <- starch[7:150,]

######remove bad data
starch1 <- subset(starch1, ID != "4-6" & ID != "5-4")

#calculate starch content
starch_agg <- summaryBy( .~ ID , data = starch1,  FUN=c(mean, sd, length))
starch_agg$abs_SE <- with(starch_agg, abs510.sd/sqrt(abs510.length))

starch_agg$mg_glu_sample <- (100*starch_agg$abs510.mean*100*162)/(1000*180)

starch_agg$g_glu_g_DW <- (100*starch_agg$abs510.mean*100*162)/(1000*180*starch_agg$mass.mean)
starch_agg$mg_glu_per_gDW <- starch_agg$g_glu_g_DW*1000

starch_agg$percglu <- starch_agg$abs510.mean * (standard100 / starch_agg$mass.mean) *10*.9
starch_agg$starchcontent <- starch_agg$percglu *10


#merge with plot summary
starch_agg <- merge(starch_agg, plotsumm, by = "ID")
starch_agg$volume <- as.factor(starch_agg$volume)

starch_means <- aggregate(starchcontent ~ volume, FUN = mean, data = starch_agg)


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
  box ()                                      

