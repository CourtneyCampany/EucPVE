#complex model mass based--------------------------------------------------------------------
#if comparing models with different structure then use ML method

#model1 (volume as factor, ID is random effect, and all others are fixed effects)
Amass_model <- lme(A_mass~ starch*Nmass_notnc*volume, random=~1|ID, data=photo_chem, method="ML")
#model2 (adds campaign as random effect)
Amass_model2 <- lme(A_mass~ starch*Nmass_notnc*volume, random=~campaign|ID, data=photo_chem, method="ML")

#model3 (adds campaign as fixed effect)
Amass_model3 <- lme(A_mass~ campaign*starch*Nmass_notnc*volume, random=~1|ID, data=photo_chem,method="ML")

anova(Amass_model, Amass_model2, Amass_model3)

#USE simply model function to step and delete down to the best model (choosing model with time)
timemod2 <- simp_mod(Amass_model3)
timemod3 <- timemod2$model.ml
timemod4 <- update(timemod3,~. -campaign:starch:Nmass_notnc)
anova(timemod3, timemod4)

timemod5 <- simp_mod(timemod4)
timemod6 <- update(timemod5$model.ml,~. -starch:Nmass_notnc)
Anova(timemod6)

timemod7 <- simp_mod(timemod6)
finalmod <- timemod7$model.reml
Anova(finalmod)

#plot the two seperate interactions
plot(allEffects(finalmod), "campaign:starch:volume", x.var = "starch")
plot(allEffects(finalmod), "campaign:Nmass_notnc:volume", x.var = "Nmass_notnc")
xyplot(A_mass ~ starch| volume + factor(campaign), data = photo_chem, type = c("p", "r"))
xyplot(A_mass ~ Nmass_notnc| volume + factor(campaign), data = photo_chem, type = c("p", "r"))