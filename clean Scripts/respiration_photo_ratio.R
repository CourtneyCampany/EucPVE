source("functions and packages/startscripts.R")


#gross read
cgross <- read.csv("calculated data/Aleaf_model/cday_120_clean_gross.csv")
  names(cgross)[3] <- "cday_gross"


##net read
cnet <- read.csv("calculated data/Aleaf_model/cday_120_clean.csv")
  names(cnet)[3] <- "cday_net"

##merge to get respiration and ratio
cday <- merge(cnet, cgross)
  cday$respiration <- with(cday, cday_gross- cday_net)
  cday$ratio <- with(cday, respiration/cday_net)
  cday$volume <- gsub(1000, "free", cday$volume)
  cday$volume <- gsub("^5", "05", cday$volume)
  cday$volume <- as.factor(cday$volume)

#means
cgross_agg <- doBy::summaryBy(cday_gross ~ volume, data=cgross, FUN=mean, keep.names=TRUE )
cnet_agg <- doBy::summaryBy(cday_net ~ volume, data=cnet, FUN=mean, keep.names=TRUE )


boxplot(ratio~volume, data=cday, col=palette(), outline=FALSE, ylab="R:A")
