source("functions and packages/functions.R")
source("functions and packages/load packages.R")

#read in plot design, lma data, merge
plotsumm <- read.csv("raw data/plot_summary.csv")
plotsumm$ID <- paste(plotsumm$plot, plotsumm$pot, sep = "-")

lma <- read.csv("raw data/seedling leaf mass area.csv")
lma$ID <- paste(lma$plot, lma$pot, sep = "-")
lma <- merge(lma, plotsumm[3:4], all=TRUE)
lma$volume <- as.factor(lma$volume)

#run function to add campaign dates, caluclate sla
lma <- add_campaign_date(lma)
lma$sla <- with(lma, area/mass)

#remove missing values
lma_noNA <- subset(lma, !is.na(sla))

#subset with only sla and parameters of lm
lma_lm <- subset(lma_noNA, select = c("ID", "volume", "campaign", "sla"))


#-----------------------------------------------------------------------------------------------
#TEST with subset of one volume
lma15 <- lma_lm[lma_lm$volume == "15",]
lma15 <- subset(lma15, select = c("ID", "campaign", "sla"))
droplevels(lma15)

lma15.new <- groupedData( sla ~ campaign | ID, data = as.data.frame(lma15))
plot(lma15.new)
getGroups(lma15.new)
getGroupsFormula(lma15.new)

lm_lma_15 <- lmList(sla ~ campaign,  data=lma15.new)
coef(lm_lma_15)
intervals(lm_lma_15)
plot(intervals(lm_lma_15))


#----------------------------------------------------------------------------------------------
#Visualize changes in SLA across campaigns (Date messes things up so stay with campaign# (interval=2wks))

#function to create groupData set
grp_func <- function(dfr){
  
  sub_dfr <- subset(dfr, select = c("ID", "campaign", "sla"))
  lma.new <- groupedData(sla ~ campaign|ID, data=sub_dfr)
}
#create groupdData objects by volume
groupData_ls<- dlply(lma_lm, .(volume) , grp_func)
#generate trellis plots
sla_plots <- lapply(groupData_ls, plot)
print(sla_plots)
dev.copy2pdf(file="output/stats_plots/lma_date.pdf")

#----------------------------------------------------------------------------------------------------
examples of plotBY

window()
par(mfrow=c(3,4))
plotBy(sla ~ campaign | ID, how="panel", type='o', data=lma_lm)

par(mfrow=c(3,4))
plotBy(sla ~ campaign | ID, how="col", type='o', data=subset(lma_lm,volume=='5'), legend=F,
       enhance="lm", lty=5)


gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
ltys <- c(rep(1,6),4)

par(mfrow=c(3,4))
#run plotBY through plyr by volume
sla_plots<- dlply(lma_lm, .(volume), function(x) plotBy(sla ~ campaign | ID, how="col", type='o', 
                                                        legend=F,enhance="lm", lty=5, data=x))    


