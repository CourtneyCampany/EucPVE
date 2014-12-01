#plot objects----------------------------------------------------------------------------------------------------
numdays <- as.numeric(as.Date("2013-05-21") - as.Date("2013-01-21"))

gradient <- colorRampPalette(c("red", "blue"))
palette(gradient(7))
pchs = c(rep(16,6),17)
pchsO <- c(rep(1,6),2)
ypos <- c(2.5,1,0)
vollab <- expression(Pot~volume~(l))
leglab <- c(5, 10, 15, 20, 25, 35, "free")

cols <- as.vector(palette())
require(scales)
cols1 <- alpha(cols[1], 0.45)
cols2 <- alpha(cols[2], 0.45)
cols3 <- alpha(cols[3], 0.45)
cols4 <- alpha(cols[4], 0.45)
cols5 <- alpha(cols[5], 0.45)
cols6 <- alpha(cols[6], 0.45)
cols7 <- alpha(cols[7], 0.45)

col_bl <- alpha("black", .50)

col_exude <- alpha("darkorange2", .25)
col_resp <- alpha("green4", .5)

treelab<- paste("Seedling Mass Production over ",numdays," days (g)", sep="")
sub35 <- expression(Scaled[35])
subfree <- expression(Scaled[free])
treelab35 <- paste("Seedling Mass Production over ",numdays," days ",sub35,"  (g)", sep="")
treelabfree <- paste("Seedling Mass Production over ",numdays," days ",subfree,"  (g)", sep="")
treelabfree <- paste("Relative Mass Production over ",numdays," days ","  (g)", sep="")

cdaylab <- expression(Daily~Carbon~Gain~~(g~m^-2~d^-1))
cday35lab <- expression(Daily~Carbon~Gain~Scaled[35]~~(g~m^-2~d^-1))
cdayfreelab <- expression(Daily~Carbon~Gain~Scaled[free]~~(g~m^-2~d^-1))