#plot objects

#colors
gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))

#lines and symbols
lwds<- 2
pchs <- c(rep(16,6),17)
pch2 <- c(rep(1,6),2)
ltys <- c(rep(1,6),4)
ypos <- c(2.5,1,0)
PTcex <- 1.6
LABcex <- 1.3
#legend and axis labels for all data

vollab <- expression(Soil~volume~(l))
leglab <- c(5, 10, 15, 20, 25, 35, "Free")
leglab2 <- as.character(c(5, 10, 15, 20, 25, 35))

rdlab <-expression(Respiration[dark]~~(mu*mol~m^-2~s^-1))
suglab<- expression(Leaf~Soluble~Sugars~~(g~g^-1))
starchlab <- expression(Leaf~Starch~~(g~g^-1))
tnclab <- expression(Total~Non-Structural~Carbohydrates~~(g~g^-1))
slalab<- expression(SLA[TNC~free]~~(m^2~g^-1))
lmalab <- expression(LMA[TNC~free]~~(g~cm^-2))
maxlab <-expression(A[max]~~(mu*mol~m^-2~s^-1))
satlab <- expression(italic(A)[sat]~~(mu*mol~m^-2~s^-1))
nitro <- expression(Leaf~Nitrogen~~(g~g^-1))
nfree <- expression(Leaf~Nitrogen[TNC~free]~~(g~g^-1))
nmasslab<- expression(Leaf~Nitrogen[mass]~~(g~g^-1))
nmass_noTNC<- expression(Leaf~Nitrogen[TNC[free]]~~(g~g^-1))
narealab <- expression(Leaf~Nitrogen[area]~~(g~m^-2))
treelab<- "Seedling Mass (g)"
leaflab <-expression(Leaf~Area~~(cm^-2))
condlab <- expression(g[s]~~(mu*mol~m^-2~s^-1))
Amasslab <- expression(italic(A)[mass]~~(n*mol~g^-1~s^-1))
cdaylab <- expression(Carbon~gain~~(g~d^-1))
LAm2 <- expression(Leaf~Area~~(m^2))
anet <- expression(italic(A)[net] ~ ~(mu * mol ~ m^-2 ~ s^-1))
cilab <- expression(C[i]~~(mu*mol~mol^-1))
srllab <- expression(Specific~Root~Length~~(m~g^1))
c13lab <-expression(paste(delta^{13}, "C (\u2030)"))

#labels for mtext
raw <- "Raw Data"
volmean <- "Volume Means"
datemean <- "Date x Volume Means"
