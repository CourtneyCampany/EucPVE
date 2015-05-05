# standard error function
se <- function(x) sd(x)/sqrt(length(x))

#-----------------------------------------------------------------------------------
getP <- function(x)anova(x)[[5]][1]

getP_lme <- function(x)anova(x)[[4]][1]
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

#------------------------------------------------------------------------
#function to get N pool size in g for pots
npool_func <- function(x){
  #bulk density (g/m3)
  bd<-1.7
  #determin soil mass from bd and volume, then determine N pool size in g
  x$soilmass <- bd*x$volume
  x$soilN <- with(x, soilmass*n_perc)
  return(x)
}
#------------------------------------------------------------------------
#linear model extraction
extract_func <- function(mod){
  data.frame(
    r2=summary(mod)$r.squared, 
    intercept=coef(mod)[[1]],
    b=coef(mod)[[2]], 
    p=anova(mod)[1,"Pr(>F)"])
}

#------------------------------------------------------------------------
#function to interpolate a variable with approxfun..can make even more generic 
leafpred_func <- function(dfr, var) {
  dfr_sp <- split(dfr, dfr$ID)
  #interpolate
  dfr_sp <- lapply(dfr_sp, function(z){
    
    apfun_var <- approxfun(x=z$Date, y=z[,var])
    z$var_pred <- apfun_var(z$Date)
    names(z)[4] <- paste(var, "_pred", sep="")
    return(z)
  })
}
#------------------------------------------------------------------------
#replaces "free" with 1000 and adds 0 to single digit volumes
vollab_func <- function(x){
x$volume <- gsub("free", "1000", x$volume)
x$volume <- gsub("^5", "05", x$volume)
x$volume <- as.factor(x$volume)
return(x)
}

#------------------------------------------------------------------------
#simple add date and volume as factor 
datevol_func <- function(x){
  x$volume <- as.factor(x$volume)
  x$Date <- as.Date(x$Date)
  return(x)
}

#bar plot function--------------------------------------------------------------------------------

bar <- function(dv, factors, dataframe, percentage=FALSE, errbar=!percentage, half.errbar=TRUE, conf.level=.95, 
                xlab=NULL, ylab=NULL, main=NULL, names.arg=NULL, bar.col="black", whisker=.015,args.errbar=NULL,
                legend=TRUE, legend.text=NULL, args.legend=NULL,legend.border=FALSE, box=TRUE, args.yaxis=NULL, 
                mar=c(5,4,3,2),...){
  axes=!percentage
  dv.name<-substitute(dv)
  if(length(dv.name)>1) stop("'dv' only takes one variable")
  dv.name<-as.character(dv.name)
  dv<-dataframe[[dv.name]]
  fnames<-substitute(factors)
  if(length(fnames)==1){
    factors<-as.character(fnames)
    nf<-1
  }else{
    factors<-as.character(fnames[-1L])
    nf<-length(factors)
  }
  if(nf>2) stop("This function accepts no more than 2 factors \n",
                "\t-i.e., it only plots one-way or two-way designs.")
  if(percentage & errbar){
    warning("percentage=TRUE; error bars were not plotted")
    errbar<-FALSE
  }
  if(!percentage) xbars<-tapply(dv, dataframe[,factors], mean, na.rm=TRUE)
  else {
    xbars<-tapply(dv, list(interaction(dataframe[,factors], lex.order=TRUE)), mean, na.rm=TRUE)
    if(sum(na.omit(dv)!=0&na.omit(dv)!=1)>0) 
      stop("Data points in 'dv' need to be 0 or 1 in order to set 'percentage' to TRUE")
    xbars<-rbind(xbars, 1-xbars)*100
  }
  if(errbar){
    se<-tapply(dv, dataframe[,factors], sd, na.rm=TRUE)/sqrt(tapply(dv, dataframe[,factors], length))
    conf.level=1-(1-conf.level)/2
    lo.bar<-xbars-se*qnorm(conf.level)
    hi.bar<-xbars+se*qnorm(conf.level)  
  }
  extras<-list(...)
  if(legend & !percentage){
    if(is.null(legend.text))
      legend.text<-sort(unique(dataframe[[factors[1]]]))
    args.legend.temp<-list(x="topright", bty=if(!legend.border)"n" else "o",
                           inset=c(0,0))
    if(is.list(args.legend))
      args.legend<-modifyList(args.legend.temp, args.legend)
    else 
      args.legend<-args.legend.temp
  } else if(legend & percentage){
    if(is.null(legend.text)) 
      legend.text<-c("1", "0")
    args.legend.temp<-list(x="topright", bty=if(!legend.border)"n" else "o",
                           inset=c(0,0))
    if(is.list(args.legend))
      args.legend<-modifyList(args.legend.temp, args.legend)
    else 
      args.legend<-args.legend.temp
  } else if(!legend){
    args.legend<-NULL
    legend.text<-NULL
  }
  if(errbar && legend && !percentage) ymax<-max(hi.bar)+max(hi.bar)/20
  else if(errbar && legend && percentage) ymax<-115
  else if(errbar && !legend) ymax <- max(xbars)
  else if(!errbar && legend && percentage) ymax<-110	
  else if(!errbar) ymax<-max(xbars) + max(xbars)/20
  if(!percentage){
    args.barplot<-list(beside=TRUE, height=xbars, ylim=c(0, ymax), main=main, names.arg=names.arg,
                       col=hcl(h=seq(0,270, 270/(length(unique(dataframe[[factors[1]]]))))[-length(unique(dataframe[[factors[1]]]))]),
                       legend.text=legend.text, args.legend=args.legend, xpd=TRUE,
                       xlab=if(is.null(xlab)) factors[length(factors)] else xlab,
                       ylab=if(is.null(ylab)) dv.name else ylab, axes=axes)
  }else{
    args.barplot<-list(beside=TRUE, height=xbars, ylim=c(0, ymax),  main=main, names.arg=names.arg,
                       col=hcl(h=seq(0,270, 270/(length(unique(dataframe[[factors[1]]]))))[-length(unique(dataframe[[factors[1]]]))]),
                       legend.text=legend.text, args.legend=args.legend, xpd=TRUE,
                       xlab=if(is.null(xlab)) " "[length(factors)] else xlab,
                       ylab=if(is.null(ylab)) "percentage" else ylab, axes=axes)		
  }
  args.barplot<-modifyList(args.barplot, extras)
  errbars = function(xvals, cilo, cihi, whisker, nc, args.errbar = NULL, half.errbar=TRUE) {
    if(half.errbar){
      cilo<-(cihi+cilo)/2
    }
    fixedArgs.bar = list(matlines, x=list(xvals), 
                         y=lapply(split(as.data.frame(t(do.call("rbind", 
                                                                list(cihi, cilo)))),1:nc),matrix, 
                                  nrow=2, byrow=T))
    allArgs.bar = c(fixedArgs.bar, args.errbar)
    whisker.len = whisker*(par("usr")[2] - par("usr")[1])/2
    whiskers = rbind((xvals - whisker.len)[1,],
                     (xvals + whisker.len)[1,])
    fixedArgs.lo = list(matlines, x=list(whiskers), 	
                        y=lapply(split(as.data.frame(t(do.call("rbind", 
                                                               list(cilo, cilo)))), 1:nc), matrix, nrow=2, byrow=T))
    allArgs.bar.lo = c(fixedArgs.lo, args.errbar)
    fixedArgs.hi = list(matlines, x=list(whiskers), 
                        y=lapply(split(as.data.frame(t(do.call("rbind", 
                                                               list(cihi, cihi)))), 1:nc), matrix, nrow=2, byrow=T))
    allArgs.bar.hi = c(fixedArgs.hi, args.errbar)  
    invisible(do.call(mapply, allArgs.bar))
    if(!half.errbar) invisible(do.call(mapply, allArgs.bar.lo))
    invisible(do.call(mapply, allArgs.bar.hi))
  }
  par(mar=mar)
  errloc<-as.vector(do.call(barplot, args.barplot))
  if(errbar){
    errloc<-rbind(errloc, errloc)
    lo.bar<-matrix(as.vector(lo.bar))
    hi.bar<-matrix(as.vector(hi.bar))
    args.errbar.temp<-list(col=bar.col, lty=1)
    args.errbar<-if(is.null(args.errbar)|!is.list(args.errbar)) 
      args.errbar.temp
    else if(is.list(args.errbar)) 
      modifyList(args.errbar.temp, args.errbar)
    errbars(errloc, cilo=lo.bar, cihi=hi.bar, nc=1, whisker=whisker, 
            args.errbar=args.errbar, half.errbar=half.errbar)
  }
  if(box) box()
  if(percentage){
    args.yaxis.temp<-list(at=seq(0,100, 20), las=1)
    args.yaxis<-if(!is.list(args.yaxis)) args.yaxis.temp else modifyList(args.yaxis.temp, args.yaxis)
    do.call(axis, c(side=2, args.yaxis))
  }
}


#####function for plotting model stuff


addpoly <- function(x,y1,y2,col=alpha("lightgrey",0.8),...){
  ii <- order(x)
  y1 <- y1[ii]
  y2 <- y2[ii]
  x <- x[ii]
  polygon(c(x,rev(x)), c(y1, rev(y2)), col=col, border=NA,...)
}

predline <- function(fit, from=NULL, to=NULL, ...){
  
  if(is.null(from))from <- min(fit$model[,2], na.rm=TRUE)
  if(is.null(to))to <- max(fit$model[,2], na.rm=TRUE)
  
  newdat <- data.frame(X = seq(from,to, length=101))
  names(newdat)[1] <- names(coef(fit))[2]
  
  pred <- as.data.frame(predict(fit, newdat, se.fit=TRUE, interval="confidence")$fit)
  
  addpoly(newdat[[1]], pred$lwr, pred$upr)
  ablinepiece(fit, from=from, to=to, ...)
  
}
#'@title Add a line to a plot
#'@description As \code{abline}, but with \code{from} and \code{to} arguments. 
#'If a fitted linear regression model is used as asn argument, it uses the min and max values of the data used to fit the model.
#'@param a Intercept (optional)
#'@param b Slope (optional)
#'@param reg A fitted linear regression model (output of \code{\link{lm}}).
#'@param from Draw from this X value
#'@param to Draw to this x value
#'@param \dots Further parameters passed to \code{\link{segments}}
#'@export
ablinepiece <- function(a=NULL,b=NULL,reg=NULL,from=NULL,to=NULL,...){
  
  # Borrowed from abline
  if (!is.null(reg)) a <- reg
  
  if (!is.null(a) && is.list(a)) {
    temp <- as.vector(coefficients(a))
    from <- min(a$model[,2], na.rm=TRUE)
    to <- max(a$model[,2], na.rm=TRUE)
    
    if (length(temp) == 1) {
      a <- 0
      b <- temp
    }
    else {
      a <- temp[1]
      b <- temp[2]
    }
  }
  
  segments(x0=from,x1=to,
           y0=a+from*b,y1=a+to*b,...)
  
}


# Simple function for placing labels on a figure.--------------------------------------------------------------------
# for example, plotlabel("(a)", "topright")
plotlabel <- function(txt, where, inset=0.08, ...){
  u <- par()$usr
  if(grepl("left",where))x <- u[1] + inset*(u[2]-u[1])
  if(grepl("right",where))x <- u[2] - inset*(u[2]-u[1])
  if(grepl("bottom",where))y <- u[3] + inset*(u[4]-u[3])
  if(grepl("top",where))y <- u[4] - inset*(u[4]-u[3])
  
  text(x,y,txt,...)
}
