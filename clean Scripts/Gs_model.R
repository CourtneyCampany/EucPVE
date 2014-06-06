#Stomatal conducatnce model (use nls)

source("functions and packages/load packages.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")

#this script reads in a caluclated Asat dataframe
#Read in A spot measurements and create plot IDs
A_obs <- read.csv("calculated data/Asat_obs.csv")
A_obs$ID <- paste(A_obs$plot, A_obs$pot, sep = "-")
A_obs$Date <- as.Date(A_obs$Date)
A_obs$Date <-strftime(A_obs$Date ,"%Y/%m/%d")

#subset conductance measurements of Asat data
cond_data <- subset(A_obs, CO2=="400", select = c("Date", "Cond", "VpdL", "Photo", "CO2R","Ci","ID", "volume"))
#write.csv(cond_data, "calculated data/conductance.csv", row.names=FALSE)

#mean of raw data (5 observations per pot per date)
cond_agg <- summaryBy(. ~ ID + Date, data = cond_data, FUN = c(mean))
names(cond_agg)[3:8] <- c("gs", "D", "A", "Ca", "Ci", "volume")
cond_agg$volume <- as.factor(cond_agg$volume)

#------------------------------------------------------------------------------------------------------
# #test if gs is different by volume or by time
# boxplot(gs ~ volume, data = cond_agg)
# boxplot(Cond ~ volume, data = cond_data)
# boxplot(gs ~ Date, data = cond_agg)
# #models
# 
# volumegs <- lm(gs ~ volume, data = cond_agg)
# gs_Date<- lm(gs ~ volume+Date, data = cond_agg)
# summary(gs_Date)
# anova(gs_Date)
# #gs different by volume and date

#------------------------------------------------------------------------------------------------------
#fit optimal conductance model on means and date
#dataframe that contains the variables A,Ca and D.
#generate a g0 and g1 for treatment means by Date, then predict
#rename.vars


#use nls list instead of  loop
library(nlme)
nlsfits <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) | volume,
                   start=list(g1=8),data=cond_agg)
g1_vol <- as.data.frame(coef(nlsfits))
g1_vol$volume <- as.numeric(rownames(g1_vol))
names(g1_vol)[1] <- "g1_vol"
cond_agg <- merge(cond_agg,g1_vol)
cond_agg$gspred_vol <- with(cond_agg, 1.6*(1+g1_vol/sqrt(D))*(A/Ca))

with(cond_agg, plot(gspred_vol,gs))
abline(0,1)
with(cond_agg, plot(A/(sqrt(D)*Ca), gs))

#run model by date+volume, interpolate parameters across experiment dates

cond_agg$uniqueID <- paste(cond_agg$volume, cond_agg$Date, sep="-")
#IDfactor <-unique(cond_agg$uniqueID)

nlsfits_pve <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) | uniqueID,
                   start=list(g1=8),data=cond_agg)
g1_date <- as.data.frame(coef(nlsfits_pve))
g1_date$uniqueID <- as.character(rownames(g1_date))
names(g1_date)[1] <- "g1_date"

cond_pred <- merge(cond_agg,g1_date)
cond_pred$gspred_date <- with(cond_pred, 1.6*(1+g1_date/sqrt(D))*(A/Ca))


with(cond_pred, plot(gspred_vol,gs, col=volume))
with(cond_pred, points(gspred_date, gs, pch=16, col=volume))

write.csv(cond_pred[,c(2:5, 10, 12)], "calculated data/g1_pred.csv", row.names=FALSE)


#interpolate across dates (parameters g0 and g1)---------------------------------------------------

#gs_parm_pred <- function(dfr){
ID_sp <- split(cond_pred, cond_pred$ID)

date_sp <- lapply(ID_sp, function(z){
  apfun_g1vol <- approxfun(x=z$Date, y=z$g1_vol)
  
  z$g1_vol <- apfun_g1vol(z$Date)
  
  return(z)
})

g0g1_pred <- do.call(rbind,date_sp)
write.csv(g0g1_pred, "calculated data/g0g1_pred.csv", row.names=FALSE)


#------------------------------------------------------------------------------------------------------------
#now look at the fit of the model versus observed data

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

#merge g0 and g1 from model to cond data set
names(gs_param)[3]<- "volume"

cond_fit <- merge(cond_agg, gs_param, by="volume")
names(cond_fit)[4:7] <- c("gs", "D", "A", "Ca")
cond_fit$gs_func <- with(cond_fit, (A/(Ca*sqrt(D))))



#quick plot
plot(gs~gs_func, col=volume, ylim=c(0,3), xlim=c(0,.15), cex=1.3, data=cond_fit)

#with(cond_fit, plot(gs~gs_func, col=volume, ylim=c(0,3), xlim=c(0,3), cex=1.3))
points(gs_pred~gs_func, data=cond_fit, pch=16, col=volume)
abline(0,1)



#loop through dataframe by volume, return coef for each volume 
#return one set of parameters for all dates and IDs for each volume

# volumefactor <- unique(cond_agg$volume)
# gsmodel <- list()
# 
# for (i in 1:length(volumefactor)){
#   #gsmodel[[i]] stores all seven models results in a one list
#   gsmodel[[i]] <- nls(gs ~
#                         1.6*(1+g1/sqrt(D))*(A/Ca),start=list(g1=8),subset=volume==volumefactor[i],data=cond_agg)
# }
# 
# #use sapply to return the results of the coef fun for each element in the list(gsmodel)
# gs_res <- data.frame(t(sapply(gsmodel,coef)))
# #add in volume ID, specific to the order of the for loop
# gs_param <- cbind(gs_res, volumefactor)