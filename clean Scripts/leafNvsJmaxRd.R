source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

###Rdark vs Leaf N on an area basis


###nitro

photo_chem <- read.csv("calculated data/Amax_chem.csv")
#run volume format func
photo_chem<- vollab_func(photo_chem)
#recalculate sla in proper si units (m2/kg)
photo_chem$SLA <- with(photo_chem, (area/10000) / (mass/1000))
#simple leaf N %
photo_chem$leafnperc <- with(photo_chem, Nperc*100)

leaf_param <- photo_chem[,c(1,3:4,11:12)]
leaf_param$volume <- gsub("05", "5", leaf_param$volume)

leaf_param_agg <- summaryBy(Nmass+Narea~ volume, data=leaf_param, FUN=c(mean, se))


###rdark
rd <- read.csv("calculated data/Rd_leaf.csv")

rd_agg <- summaryBy(resppermass ~ volume, data=rd, FUN=c(mean, se))
rd_agg$resppermass.mean <- abs(rd_agg$resppermass.mean)



##jmax vcmax

##2-3: jmax, vcmax
phys <- read.csv("calculated data/jmax_vcmax.csv")

phys_agg <- summaryBy(Jmax.mean+Vcmax.mean ~ volume, data=phys, FUN=c(mean, se))
names(phys_agg)[2:5]<- c("Jmax", "Vcmax", "Jmax_se", "Vcmax_se")


###merge
leafdat <- merge(rd, leaf_param, by=c("ID", "campaign", "volume"))
leafdat$Nmass2 <- leafdat$Nmass*1000


leafdat_agg <- merge(rd_agg, phys_agg, by="volume")
leafdat_agg <- merge(leafdat_agg, leaf_param_agg, by="volume")



#plotting
plot(abs(resppermass) ~ Nmass2, data=leafdat, col=as.factor(volume), pch=16,ylim=c(0,8),xlim=c(0,10),
     xlab="Leaf Nitrogen  (mg/g)", ylab= "Dark Respiration  (nmols g-1 s-1)", cex=1.3)


plot(resppermass.mean ~ Nmass.mean, data=leafdat_agg,ylim=c(0,4), xlim=c(0, 0.01), col=as.factor(volume), pch=pchs,
     xlab=nmasslab, cex=1.3)


plot(resppermass.mean ~ Vcmax, data=leafdat_agg, col=as.factor(volume), pch=pchs,xlim=c(50, 125), ylim=c(0, 4),
     xlab=nmasslab, cex=1.3)

