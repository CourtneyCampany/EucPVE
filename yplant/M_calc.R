require(plyr)
require(doBy)

#use simulations from sunny day and A parameters for each pot volume to calc M
#M = Aplant/Aobs

#read in summary files for all volumes into a list
eucs_summs <- list.files(path = "yplant/simulation_summary/", pattern="*.csv", full.names = TRUE)
eucs_list <- lapply(eucs_summs, function(x) read.csv(x))

#name list elements
listnames <- c("10", "15", "20", "25", "35", "5", "free")
names(eucs_list) <- listnames

#function to calculate M (twp arguments:1st is dfr, 2nd is the name of each list element)
Mcalc <- function(dfr, vol){
dfr$plant_id <- gsub("yplant/euc_plfiles/", "", dfr$pfile)
dfr$plant_id <- gsub(".p", "", dfr$plant_id)
dfr$M <- dfr$totA/dfr$totA0
M_dfr <- subset(dfr, select = c("plant_id", "M", "totPARleaf"))
M_dfr$volume <- as.factor(vol)
return(M_dfr)
}

#run the function on each element of the list 
#call the names of the list into plyr, run the function on each element (eucs_list[[x]]) where vol argument = names
M_eucs <- ldply(names(eucs_list), function(x) Mcalc(dfr = eucs_list[[x]], vol = x))
M_eucs$plant_id <- as.factor(M_eucs$plant_id)
write.csv(M_eucs, "calculated data/M_eucs.csv", row.names=FALSE)
#means
M_agg <- summaryBy(M+totPARleaf~ volume, data=M_eucs, FUN=mean, keep.names=TRUE)

#plot bits
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)
pchs3 <- c(16,17,18,15,5,6,7)

#plot
plot(M~totPARleaf, pch=pchs[volume], col=volume, data=M_eucs)


n<- as.numeric(length(unique(M_eucs$plant_id)))
col2 <- rainbow(n)

plot(M~totPARleaf, pch=pchs3[volume], col=col2[plant_id], data=M_eucs)

#should I pull out plants that have a similar height, leaf area, etc?
#there is significant variation in the range of M, not all values may be suitable for my seedlings









#plot M against leaf#-----------------------------------------------------------------------------

leaf_no <- read.csv("calculated data/leaf_number.csv")
leafM <- merge(leaf_no, M_agg, by="volume")




