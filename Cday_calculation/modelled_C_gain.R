source("functions and packages/startscripts.R")

##use modelled C net and gross to determine respiration
##use harvest mass + respiration + unaccounted as fractions of total gross

##read data-----------------------------------------------------------------------------------------------------------------
finalmass <- read.csv("calculated data/harvest_mass_means.csv")

la_pred <- read.csv("Calculated data/LApred_volume.csv")
   names(la_pred)[3] <- "LA"
   la_pred$Date <- as.Date(la_pred$Date)
   
Cday_net <- read.csv("calculated data/Aleaf_model/cday_120_clean.csv")
   Cday_net$Date <- as.Date(Cday_net$Date)
  
Cday_gross <- read.csv("calculated data/Aleaf_model/cday_120_clean_gross.csv")
    Cday_gross$Date <- as.Date(Cday_gross$Date)
    
##Cday with leaf area needs self shading (use slope intercept, from 5-free vol)
sigma <- read.csv("calculated data/M_leafarea_model.csv")
      
##function to generate total plant daily C gain-----------------------------------------------------------------------------
modelledC_func <- function(leafarea, shading, Cday){ #leafarea dfr, self shading dfr, and modelled C gain (gm2)
          dailyCnet <- merge(leafarea,Cday)
            ##this needs to include self shadeing (M as a linear function of leaf area)
            dailyCnet <- merge(dailyCnet, shading[, c(2,3,5)], by="volume")
                dailyCnet$M <- with(dailyCnet, b*LA+intercept)
            #calculate total daily C gain with self shading
            dailyCnet$tdcg <- with(dailyCnet, LA * carbon_day * M)
              print("successfully calculated total daily carbon with modelled Cgain, leaf area and self shading")

            dailyCnet$volume <- as.factor(dailyCnet$volume)
              return(dailyCnet)
            }

##calculate net and gross C gain, then quantify leaf respiration--------------------------------------------------------------
dailyCnet <- modelledC_func(la_pred, sigma, Cday_net)
    names(dailyCnet)[8] <- "tdc_net"
    names(dailyCnet)[4] <- "cday_net"
    
dailyCgross <- modelledC_func(la_pred, sigma, Cday_gross)
    names(dailyCgross)[8] <- "tdc_gross"
    names(dailyCgross)[4] <- "cday_gross"
    
dailyC <- merge(dailyCgross[, c(1:2, 4, 8)], dailyCnet[,c(1:2,4,8)], by=c("Date", "volume"))
  dailyC$Rleaf <- with(dailyC, tdc_gross-tdc_net)
  
write.csv(dailyC, "calculated data/modelleddailycarbon.csv", row.names = FALSE) 

#Calculate total seedling C gain over experiment (120d) and compare to final harevst mass C----------------------------------
plantCnet <- doBy::summaryBy(tdc_net+Rleaf+tdc_gross ~ volume, FUN=sum, data=dailyC)

write.csv(plantCnet, "calculated data/CUEdaily.csv", row.names = FALSE) 

