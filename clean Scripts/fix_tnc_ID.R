leaf_tnc <- read.csv("raw data/leaf_tnc.csv")

a <- strsplit(as.character(leaf_tnc$ID), "-")
a.df <- ldply(a)

#function to change abbreviated month to numeric, pulls number from month.abb vector
motonum <- function(x) match(tolower(x), tolower(month.abb))
#run function on vector of month names
a.df$n.month <- motonum(a.df$V2)

leaf_tnc$ID <- paste(a.df$V1, a.df$n.month, sep = "-")

write.csv(leaf_tnc, "raw data/leaf_tnc_corr.csv", row.names=FALSE)

