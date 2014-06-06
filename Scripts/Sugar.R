source("functions and packages/load packages")

#read in plot design and TNC data
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

sugar_ps3_2 <- read.csv("raw data/sugars_ps3.2.csv")

#build loop to run through both sugar files then merge data
#sugar_ps3_1 <- read.csv("sugars_ps3.1.csv")

#make standard curve and extract equation vales from linear model
#standard curve
standard2 <- sugar_ps3_2[1:18,]
standard2 <- subset(standard2, select =c("ID", "abs620"))
names(standard2)[1] <- "concentration"
standard2$concentration <- as.character(standard2$concentration)
droplevels(standard2)

standard_agg <- aggregate(abs620 ~ concentration, FUN=mean, data=standard2)
#extract equation vales from linear model
with(standard2, plot(abs620, concentration))

standardcurve2 <- lm(abs620 ~ as.numeric(concentration), data=standard2)

#slope and interept
curveeq2 <- coef(standardcurve2)

#curveeq2$intercept <- curveeq2$coef[1]
#curveeq2$slope <- curveeq2$coef[2]


#Data subset
sugar2 <- sugar_ps3_2[19:92,]
sugar2$ID <- as.character(sugar2$ID)
sugar2$ID <- gsub ("'", "", sugar2$ID)

#average replicates of abs for each ID (n=3)
sugar2_agg <- summaryBy( .~ ID , data = sugar2,  FUN=c(mean, sd, length))
sugar2_agg$abs_SE <- with(sugar2_agg, abs620.sd/sqrt(abs620.length))

sugar2_agg$gluconc <- (curveeq2[[2]]*sugar2_agg$abs620.mean)+curveeq2[[1]]
sugar2_agg$glusample <- with(sugar2_agg, (gluconc*5*5)/1000)
sugar2_agg$gSUG_gTISSUE <- with(sugar2_agg, glusample/mass.mean)
sugar2_agg$mgSUG_gDWT <- with(sugar2_agg, gSUG_gTISSUE*1000)

#merge with plot summary
sugar2_agg <- merge(sugar2_agg, plotsumm, by = "ID")
sugar2_agg$volume <- as.factor(sugar2_agg$volume)

#Plotting
with(sugar2_agg, bargraph.CI(volume, mgSUG_gDWT))
box () 













