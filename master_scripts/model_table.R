
###make model parameter table

r1 = c("L~0~", "initial leaf area",	0.035,	"m^2^",	"this study")
r2 = c("Leaf~0~", "initial leaf mass",	3.45,	"g",	"this study")
r3 = c("Stem~0~", "initial stem mass",	1.51,	"g",	"this study")
r4 = c("Root~0~", "initial root mass",	0.99,	"g",	"this study")
r5 = c("$\\epsilon$",	"biomass conversion efficiency",	0.65,	"g C g mass^-1^",	"Mäkelä (1997)")
r6 = c("*R*~coarse~ ~root~",	"coarse root respiration",	0.00124,	"g C g root^-1^ d^-1^",	"Marden et al. (2008)")
r7 = c("*R*~fine~ ~root~",	"fine root respiration",	0.01037,	"g C g root^-1^ d^-1^","Ryan et al. (2010)")
r8 = c("*R*~stem~",	"stem respiration",	0.00187,	"g C g stem^-1^ d^-1^", "Drake et al. (unpublished)")
r9 = c("*P*",	"daily leaf carbon assimilation",	'5.4 - 7.6',	"g C m^-2^ d^-1^", "this study")
r10 = c("$\\Lambda$",	"leaf or root turnover", 	1,	"yr^-1^",	"theoretical")

modeltable <- rbind(r1, r2,r3,r4,r5,r6,r7,r8,r9,r10)


colnames(modeltable) <- c("Variable","Description", "Default Value", "Units", "Source")

write.csv(modeltable, "master_scripts/modeltable.csv", row.names=FALSE)
