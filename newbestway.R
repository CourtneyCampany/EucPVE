

finalmass <- read.csv("calculated data/harvest_mass_means.csv")

lainterp <- read.csv("Calculated data/LApred_volume.csv")
Aleaf <- read.csv("calculated data/model_runs/cday_120_clean.csv")

names(lainterp)[3] <- "LA"

df <- merge(lainterp,Aleaf)

df$tdcg <- with(df, LA * carbon_day)

# need to worry about sigma!

# total carbon gain over 120 days
dft <- summaryBy(tdcg ~ volume, FUN=sum, data=df)

dat <- merge(finalmass, dft)


with(dat, plot(tdcg.sum, mass, xlim=c(0,250), ylim=c(0,250)))
abline(0,1)


with(dat, plot(tdcg.sum, mass/tdcg.sum))



aleafvol <- summaryBy(. ~ volume, data=Aleaf, FUN=mean)
dat <- merge(dat, aleafvol)

with(dat, plot(carbon_day.mean, mass/tdcg.sum))
