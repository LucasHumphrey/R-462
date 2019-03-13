##Lucas Humphrey STAT 462 Homework 10

##Part 1
data=read.table("bears.txt",header=TRUE)
bears=data[data$Obs.No==1,]
attach(bears)
lm_bears=lm(Weight~Head.L+Head.W+Neck.G+Length+Chest.G)
summary(lm_bears)
round(cor(bears[5:9]),digits=3)
lm_x1=lm(Head.L~Head.W+Neck.G+Length+Chest.G)
R2_1=summary(lm_x1)$r.squared
vif_1=1/(1-R2_1)

lm_x2=lm(Head.W~Head.L+Neck.G+Length+Chest.G)
R2_2=summary(lm_x2)$r.squared
vif_2=1/(1-R2_2)

lm_x3=lm(Neck.G~Head.L+Head.W+Length+Chest.G)
R2_3=summary(lm_x3)$r.squared
vif_3=1/(1-R2_3)

lm_x4=lm(Length~Head.L+Head.W+Neck.G+Chest.G)
R2_4=summary(lm_x4)$r.squared
vif_4=1/(1-R2_4)

lm_x5=lm(Chest.G~Head.L+Head.W+Neck.G+Length)
R2_5=summary(lm_x5)$r.squared
vif_5=1/(1-R2_5)

###Part 1.5
lm_new=lm(Weight~Head.L+Head.W+Length+Chest.G)
summary(lm_new)
lm_x1=lm(Head.L~Head.W+Length+Chest.G)
R2_1=summary(lm_x1)$r.squared
vif_1=1/(1-R2_1)

lm_x2=lm(Head.W~Head.L+Length+Chest.G)
R2_2=summary(lm_x2)$r.squared
vif_2=1/(1-R2_2)

lm_x4=lm(Length~Head.L+Head.W+Chest.G)
R2_4=summary(lm_x4)$r.squared
vif_4=1/(1-R2_4)

lm_x5=lm(Chest.G~Head.L+Head.W+Length)
R2_5=summary(lm_x5)$r.squared
vif_5=1/(1-R2_5)


##Part 2
data(divusa, package="faraway")
attach(divusa)
lm_div=lm(divorce~unemployed+femlab+marriage+birth+military)
summary(lm_div)
round(cor(divusa[3:7]),digits=3)
lm_y1=lm(unemployed~femlab+marriage+birth+military)
R2_6=summary(lm_y1)$r.squared
vif_6=1/(1-R2_6)

lm_y2=lm(femlab~unemployed+marriage+birth+military)
R2_7=summary(lm_y2)$r.squared
vif_7=1/(1-R2_7)

lm_y3=lm(marriage~unemployed+femlab+birth+military)
R2_8=summary(lm_y3)$r.squared
vif_8=1/(1-R2_8)

lm_y4=lm(birth~unemployed+femlab+marriage+military)
R2_9=summary(lm_y4)$r.squared
vif_9=1/(1-R2_9)

lm_y5=lm(military~unemployed+femlab+marriage+birth)
R2_10=summary(lm_y5)$r.squared

cor(residuals(lm_div)[-1], residuals(lm_div)[-length(residuals(lm_div))])

require(nlme)
gldiv=gls(divorce~unemployed+femlab+marriage+birth+military, correlation = corAR1(form = ~year), method="ML", data = na.omit(divusa))
summary(gldiv)

###testing for collinearity in sub model
lm_div2=lm(divorce~femlab+marriage+birth)
summary(lm_div2)
round(cor(divusa[4:6]),digits=3)
lm_y2=lm(femlab~marriage+birth)
R2_7=summary(lm_y2)$r.squared
vif_7=1/(1-R2_7)

lm_y3=lm(marriage~femlab+birth)
R2_8=summary(lm_y3)$r.squared
vif_8=1/(1-R2_8)

lm_y4=lm(birth~femlab+marriage)
R2_9=summary(lm_y4)$r.squared
vif_9=1/(1-R2_9)

