source("functions and packages/load packages.R")
source("functions and packages/functions.R")
source("functions and packages/plot objects.R")
source("read data scripts/survey read data.R")

#HEIGHT

#format data
height <- melt(height, id=c("plot", "pot"))
height$variable <- gsub ("X", "", height$variable)
names(height)[3:4] <- c("Date", "height")
height$Date <- as.Date(height$Date, format = "%m.%d.%Y")
height <- subset(height, !is.na(height))
height$ID <- paste(height$plot, height$pot, sep = "-")
#merge with plot summary
height <- merge(height, plotsumm, by = c("pot", "plot", "ID"))
height$volume <- as.factor(height$volume)

write.csv(height, "calculated data/height.csv", row.names=FALSE)

#DIAMETER

#format data
diam <- subset(diam, select = -c(Base..Time.0.))
diam <- melt(diam, id=c("plot",  "pot"))
names(diam)[3:4] <- c("Date", "diameter")
diam$Date <- gsub ("X", "", diam$Date)
diam$Date <- as.Date(diam$Date, format = "%m.%d.%Y")
diam <- subset(diam, !is.na(diameter))
diam$ID <- paste(diam$plot, diam$pot, sep = "-")
#merge with plot summary
diam <- merge(diam, plotsumm, by = c("pot", "plot", "ID"))
diam$volume <- as.factor(diam$volume)

write.csv(diam, "calculated data/diameter.csv", row.names=FALSE)

#LEAF COUNT

#format data
leafno$X1.21.2013   <- as.integer(leafno$X1.21.2013)
leafno <- melt(leafno, id=c("plot", "pot"))
names(leafno)[3:4] <- c("Date", "count")
leafno$Date <- gsub ("X", "", leafno$Date)
leafno$Date <- as.Date(leafno$Date, format = "%m.%d.%Y")
leafno <- subset(leafno, !is.na(count))
leafno$ID <- paste(leafno$plot, leafno$pot, sep = "-")
#merge with plot summary and sort
leafno <- merge(leafno, plotsumm, by = c("pot", "plot", "ID"))
leafno$volume <- as.factor(leafno$volume)

write.csv(leafno, "calculated data/leaf_number.csv", row.names=FALSE)

#stats----------------------------------------------------------------------------------------------
# 'Being in a pot' effect.
diam_last <- subset(diam, Date == max(Date))
height_last <- subset(height, Date == max(Date))
leafno_last <- subset(leafno, Date == max(Date))

diam_pot <- lm(diameter ~ as.factor(volume), data=diam_last)
height_pot <- lm(height ~ as.factor(volume), data=height_last)
leafno_pot <- lm(count ~ as.factor(volume), data=leafno_last)

diam_stat <- extract_func(diam_pot)
hieght_stat <- extract_func(height_pot)
leafno_stat <- extract_func(leafno_pot)

# Pot size effect.
diam_potsize <- lm(diameter ~ as.factor(volume), data=diam_last, subset=volume != "1000")
height_potsize <- lm(height ~ as.factor(volume), data=height_last, subset=volume != "1000")
leafno_potsize <- lm(count ~ as.factor(volume), data=leafno_last, subset=volume != "1000")

diam_pot_stat<- extract_func(diam_potsize)
height_pot_stat<-extract_func(height_potsize)
leafno_pot_stat<-extract_func(leafno_potsize)

