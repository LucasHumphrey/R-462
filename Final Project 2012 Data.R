yards2012=read.csv("yards2012.csv",header=TRUE)
attach(yards2012)
summary(yards2012)
lm_yards=lm(Pts~Int.Yds+Punt.Yds+KO.Yds+Lost.Fumbles+Penalties.Yds)
summary(lm_yards)
plot(lm_yards)
shapiro.test(lm_yards$residuals)

lm_x1=lm(Int.Yds~Punt.Yds+KO.Yds+Lost.Fumbles+Penalties.Yds)
R2_1=summary(lm_x1)$r.squared
vif_1=1/(1-R2_1)
lm_x2=lm(Punt.Yds~Int.Yds+KO.Yds+Lost.Fumbles+Penalties.Yds)
R2_2=summary(lm_x2)$r.squared
vif_2=1/(1-R2_2)
lm_x3=lm(KO.Yds~Int.Yds+Punt.Yds+Lost.Fumbles+Penalties.Yds)
R2_3=summary(lm_x3)$r.squared
vif_3=1/(1-R2_3)
lm_x4=lm(Lost.Fumbles~Int.Yds+Punt.Yds+KO.Yds+Penalties.Yds)
R2_4=summary(lm_x4)$r.squared
vif_4=1/(1-R2_4)
lm_x5=lm(Penalties.Yds~Int.Yds+Punt.Yds+KO.Yds+Lost.Fumbles)
R2_5=summary(lm_x5)$r.squared
vif_5=1/(1-R2_5)

lm_sub1=lm(Pts~Int.Yds+Punt.Yds+Lost.Fumbles+Penalties.Yds)
summary(lm_sub1)
plot(lm_sub1)
shapiro.test(lm_sub1$residuals)

lm_sub2=lm(Pts~Int.Yds+Punt.Yds+Penalties.Yds)
summary(lm_sub2)
plot(lm_sub2)
shapiro.test(lm_sub2$residuals)

anova(lm_yards,lm_sub1,lm_sub2)

confidence2013=predict(lm_sub2,yards2012,interval='confidence')
confidence2013
predict2013=predict(lm_sub2,yards2012,interval='prediction')
predict2013
