# standard error function
se <- function(x) sd(x)/sqrt(length(x))

#-----------------------------------------------------------------------------------
#fit aci curve function
acifunction <- function(x) {
  
  aci_model <- fitacis(x, "ID",varnames = list(ALEAF="Photo", Tleaf = "Temp", Ci="Ci", PPFD="PPFD"), 
                       Tcorrect=TRUE)
  
  aci_coef <- coef(aci_model)
  aci_coef <- merge(aci_coef, plotsumm, by = "ID")
  aci_means <- summaryBy(Vcmax+Jmax+Rd ~ volume , data = aci_coef,  FUN=c(mean,se))
  #aci1_means$Date <- as.Date("2013-03-14")
  
  return(aci_means)
}
#-----------------------------------------------------------------------------------
#format raw leaf CN data from ANU
leafCN_format <- function(dfr){
  
  dfr$ID <- as.character(dfr$ID)
  
  idsp <- strsplit(dfr$ID, "")
  
  plot <- sapply(idsp, "[", 1)
  pot <- sapply(idsp, "[",2)
  campaign <- sapply(idsp, "[",3)
  
  leaf_perCN <- cbind(dfr[2:3], plot)
  leaf_perCN <- cbind(leaf_perCN, pot)
  leaf_perCN <- cbind(leaf_perCN, campaign)
  leaf_perCN$ID <-  paste(leaf_perCN$plot, leaf_perCN$pot, sep = "-")
  leaf_perCN <- leaf_perCN[ -c(3:4) ]
  leaf_perCN$Nperc <- leaf_perCN$Nperc/100
  leaf_perCN$Cperc <- leaf_perCN$Cperc/100
  leaf_perCN$campaign <- as.integer(leaf_perCN$campaign)
  return(leaf_perCN)
}
#------------------------------------------------------------------------------------

#generate basic summary statistics
summary_stats <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                          conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
#---------------------------------------------------------------------
#adds campaign Date and orders by ID and Date
add_campaign_date <- function(x){
  
  x$Date <-ifelse(x$campaign == 1, "2013/03/07", x$campaign)
  x$Date <-ifelse(x$campaign == 2, "2013/03/22", x$Date )
  x$Date <-ifelse(x$campaign == 3, "2013/04/08", x$Date )
  x$Date <-ifelse(x$campaign == 4, "2013/04/17", x$Date )
  x$Date <-ifelse(x$campaign == 5, "2013/05/01", x$Date )
  x$Date <-ifelse(x$campaign == 6, "2013/05/16", x$Date )
  
  x$Date <- as.Date(x$Date)
  
  dateorder<-order(x$ID, by=x$Date)
  x <- x[dateorder,]
  return(x)
}
#----------------------------------------------------------------------
#adds campaign Date when means are used instead of ID
add_campaign_noID <- function(x){
  
  x$Date <-ifelse(x$campaign == 1, "2013/03/07", x$campaign)
  x$Date <-ifelse(x$campaign == 2, "2013/03/22", x$Date )
  x$Date <-ifelse(x$campaign == 3, "2013/04/08", x$Date )
  x$Date <-ifelse(x$campaign == 4, "2013/04/17", x$Date )
  x$Date <-ifelse(x$campaign == 5, "2013/05/01", x$Date )
  x$Date <-ifelse(x$campaign == 6, "2013/05/16", x$Date )
  
  x$Date <- as.Date(x$Date)
  
  #dateorder<-order(x$ID, by=x$Date)
  #x <- x[dateorder,]
  return(x)
}

#----------------------------------------------------------------------
#to.pdf function
to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

#-----------------------------------------------------------------------
add_trend_line <- function(x,y,d,col, ...){
  
  fit <- lm(d[[y]] ~ d[[x]])
  ablineclip(fit, lwd=2, col=d$volume, lty=ltys[d$volume],
             x1=min(d[[x]]), x2=max(d[[x]]), ...)
}
#------------------------------------------------------------------------


simp_mod<-function(model){
  mod2<-update(model,method="ML") #change method from REML to ML
  stai<-stepAIC(mod2,trace=FALSE) #model simplification by AIC
  dr<-drop1(stai,test="Chisq") #test if removing a factor even more significantly lowers model
  model<-update(stai,method="REML")
  ifelse(all(dr[[4]]<0.05,na.rm=TRUE),anr<-anova(model),anr<-NA) 
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic=stai$anova,drop1=dr,anova.reml=anr,model.reml=model,model.ml=stai))
}
#------------------------------------------------------------------------

summarytodfr <- function(x) {
  if(length(x) == 1) {
    as.data.frame(x[[1]])
  } else {
    lapply(unlist(x, FALSE), as.data.frame)
  }
}

