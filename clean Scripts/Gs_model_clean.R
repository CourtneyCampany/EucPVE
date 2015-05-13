#Stomatal conducatnce model (use nls)

###for stats this now cleans outliers, needs to be represented in stats table

source("functions and packages/startscripts.R")

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
  cond_agg$Date <- as.Date(cond_agg$Date)
  cond_agg$cica <- with(cond_agg, Ci/Ca )

------------------------------------------------------------------------------------------------------
#test if gs is different by volume or by time
boxplot(gs ~ volume, data = cond_agg)
boxplot(gs ~ Date, data = cond_agg)
  
###looks like a few outliers >1, remove and re-run model
cond_agg2 <- cond_agg[cond_agg$gs <= .75, ]

#find where date different and look at overall model

#relevel to free to evaluate container effect  
cond_agg$volume <- relevel(cond_agg$volume, ref="1000")  
cond_agg2$volume <- relevel(cond_agg2$volume, ref="1000") 

  #raw data 
  gs_lm <- lme(gs ~ volume, random= ~1|ID, data=cond_agg)
  anova(gs_lm)
  summary(gs_lm)
  visreg(gs_lm)
  
  tukey_gs<- glht(gs_lm, linfct = mcp(volume = "Tukey"))
  siglets_gs <-cld(tukey_gs)
  siglets_gs <- siglets_gs$mcletters$Letters
  
  #clean data
  gs_lm2 <- lme(gs ~ volume, random= ~1|ID, data=cond_agg2)
  anova(gs_lm2)
  summary(gs_lm2)
  visreg(gs_lm2)

  tukey_gs2<- glht(gs_lm2, linfct = mcp(volume = "Tukey"))
  siglets_gs_clean<-cld(tukey_gs2)
  siglets_gs_clean2 <- siglets_gs_clean$mcletters$Letters
  
  write.csv(siglets_gs_clean2, "master_scripts/sigletters/sl_gs.csv", row.names=FALSE)
  
##overall model with date
# gs_lm2 <- lme(gs ~ volume + Date, random= ~1|ID, data = cond_agg)
# summary(gs_lm2)
# anova(gs_lm2)
# visreg(gs_lm2)
# 
# tukey_gs2<- glht(gs_lm2, linfct = mcp(volume = "Tukey"))

#remove possible outlier in vol 15, 2013-03-07, 5-4???????????????

#overall seedling gs to show that not stressed
gs_mean <- mean(cond_agg2$gs)

##also test ci/ca
# cica_mean <- mean(cond_agg$cica)
# 
# boxplot(cica ~ volume, data = cond_agg)
# 
# cica_lm <- lme(cica ~ volume, random= ~1|ID, data=cond_agg)
# anova(cica_lm)
# summary(cica_lm)
# 
# tukey_cica<- glht(cica_lm, linfct = mcp(volume = "Tukey"))
# cld(tukey_cica)
# visreg(cica_lm)


#------------------------------------------------------------------------------------------------------
#fit optimal conductance model on means and date
#dataframe that contains the variables A,Ca and D.
#generate a g0 and g1 for treatment means by Date, then predict
#rename.vars


#use nls list instead of  loop
library(nlme)
nlsfits <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) | volume,
                   start=list(g1=8),data=cond_agg2)

#extract g1 parameter values from nls list
g1_vol <- data.frame(coef(nlsfits))
  g1_vol$volume <- as.numeric(rownames(g1_vol))
  row.names(g1_vol) <- NULL
  g1_vol$volume <- as.factor(g1_vol$volume)
  names(g1_vol)[1] <- "g1_vol"

cond_agg3 <- merge(cond_agg2,g1_vol)
cond_agg3$gspred_vol <- with(cond_agg3, 1.6*(1+g1_vol/sqrt(D))*(A/Ca))

with(cond_agg3, plot(gspred_vol,gs, xlim=c(0,1), ylim=c(0,1)))
abline(0,1)
with(cond_agg3, plot(A/(sqrt(D)*Ca), gs))


#test  g1 is different----------------------------------------------------------------------------
###rerun nlslist by id so I can run stats 
nlsfits2 <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) | ID,
                   start=list(g1=8),data=cond_agg2)

g1_vol2 <- data.frame(coef(nlsfits2))
  g1_vol2$ID <- as.character(rownames(g1_vol2))
  row.names(g1_vol2) <- NULL
  names(g1_vol2)[1] <- "g1_ID"
###merge with plotsumm to get treatment
#read in plot design and harvest data
plotsumm <- read.csv("raw data/plot_summary.csv")
  plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

g1_vol2 <- merge(g1_vol2, plotsumm[,3:4])
  g1_vol2$volume <- as.factor(g1_vol2$volume)

#stats
g1_lm <- lme(g1_ID ~ volume, random= ~1|ID, data=g1_vol2)
anova(g1_lm)
summary(g1_lm)
visreg(g1_lm)

tukey_g1<- glht(g1_lm, linfct = mcp(volume = "Tukey"))
siglets_g1 <- cld(tukey_g1)
siglets_g1_2 <- siglets_g1$mcletters$Letters

write.csv(siglets_g1_2, "master_scripts/sigletters/sl_g1_vol.csv", row.names=FALSE)


#run model by date+volume, interpolate parameters across experiment dates-----------------------------------------

cond_date <- cond_agg2

cond_date$uniqueID <- paste(cond_date$volume, cond_date$Date, sep="-")
#IDfactor <-unique(cond_agg$uniqueID)

nlsfits_pve <- nlsList(gs ~  1.6*(1+g1/sqrt(D))*(A/Ca) | uniqueID,
                   start=list(g1=8),data=cond_date)
g1_date <- as.data.frame(coef(nlsfits_pve))
g1_date$uniqueID <- as.character(rownames(g1_date))
names(g1_date)[1] <- "g1_date"

cond_pred <- merge(cond_date,g1_date)
cond_pred$gspred_date <- with(cond_pred, 1.6*(1+g1_date/sqrt(D))*(A/Ca))

g1_all <- merge(cond_pred[,2:12], cond_agg3)


with(g1_all, plot(gspred_date, gs, pch=pchs, col=volume, xlim=c(0,1), ylim=c(0,1)))
with(g1_all, points(gspred_vol, gs, pch=1, col=volume))
abline(0,1)

write.csv(g1_all[, c("volume", "ID", "Date", "gs", "g1_vol", "g1_date")], "calculated data/g1_pred.csv", row.names=FALSE)

##stats on g1_date
g1_date_lm <- lme(g1_date ~ volume, random= ~1|ID, data=cond_pred)
anova(g1_date_lm)
summary(g1_date_lm)
visreg(g1_date_lm)

tukey_g1_date<- glht(g1_date_lm, linfct = mcp(volume = "Tukey"))

siglets_g1_date <- cld(tukey_g1_date)
siglets_g1_date2 <- siglets_g1_date$mcletters$Letters

write.csv(siglets_g1_date2, "master_scripts/sigletters/sl_g1_date.csv", row.names=FALSE)


#interpolate across dates (parameters g0 and g1)---------------------------------------------------

#gs_parm_pred <- function(dfr){
ID_sp <- split(cond_pred, cond_pred$ID)

date_sp <- lapply(ID_sp, function(z){
  apfun_g1vol <- approxfun(x=z$Date, y=z$g1_vol)
  
  z$g1_vol <- apfun_g1vol(z$Date)
  
  return(z)
})

g0g1_pred <- do.call(rbind,date_sp)
#write.csv(g0g1_pred, "calculated data/g0g1_pred.csv", row.names=FALSE)


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
