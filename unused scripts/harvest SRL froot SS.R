
srlmass <- read.csv("SRLmass.csv")
srlmass$ID <- paste(srlmass$plot, srlmass$pot, sep = "-")
srlmass <- subset(srlmass, !is.na(ss_fw))

#use % water to calculate SRLdw
srlmass$srl_dw <- with(srlmass, srl_fw * (1-(ss_fw - ss_dw)/ss_fw))
 
#add extramass from doubles of plot one SS
extramass <- data.frame(ID = c("1-1", "1-2", "1-4", "1-5", "1-6", "1-7", "1-8"), 
                        extramass = c(.27, .30, .53, 1.2, .96, .91, .58))
                   
#new data frame with just total dry mass from froot subsample
srlmass <- merge(srlmass, extramass, by = "ID", all=TRUE)

srlmass$extramass <- ifelse(is.na(srlmass$extramass), 0, srlmass$extramass)

srlmass$frootSS_dw <- with(srlmass, srl_dw + ss_dw + extramass)


frootSS <- subset(srlmass, select = c("ID", "frootSS_dw"))

write.csv(frootSS, "harvest froot SS", row.names = FALSE)