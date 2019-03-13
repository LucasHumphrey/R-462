##Lucas Humphrey STAT 462 Homework 8

##Part 1
data=read.table("bears.txt",header=TRUE)
bears=data[data$Obs.No==1,]
attach(bears)
lm_bears=lm(Weight~Length)
plot(lm_bears)
lm_bears2=lm(log(Weight)~Length)
plot(lm_bears2)
shapiro.test(lm_bears2$residuals)

##Part 2
data(teengamb, package="faraway")
attach(teengamb)
lm_gamb=lm(gamble~sex+status+income+verbal)
hist(lm_gamb$residuals)
qqnorm(lm_gamb$residuals)
qqline(lm_gamb$residuals)
shapiro.test(lm_gamb$residuals)
X = model.matrix(lm_gamb)
H = X%*%solve(t(X)%*%X)%*%t(X)
h = diag(H)
epshat=residuals(lm_gamb)
yhat=fitted(lm_gamb)
plot(yhat,epshat)
sig2hat=summary(lm_gamb)$sigma^2
r=epshat/(sqrt((1-h)*sig2hat))
n=length(r); p=5
t = r*((n-p-1)/(n-p-r^2))^(1/2)
plot(yhat,t)
D = (1/p)*r^2*h/(1-h)
plot(yhat,D)
plot(lm_gamb)

##Part 3
data(prostate, package="faraway")
attach(prostate)
lm_pro=lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
hist(lm_pro$residuals)
qqnorm(lm_pro$residuals)
qqline(lm_pro$residuals)
shapiro.test(lm_pro$residuals)
X = model.matrix(lm_pro)
H = X%*%solve(t(X)%*%X)%*%t(X)
h = diag(H)
epshat=residuals(lm_pro)
yhat=fitted(lm_pro)
plot(yhat,epshat)
sig2hat=summary(lm_pro)$sigma^2
r=epshat/(sqrt((1-h)*sig2hat))
n=length(r); p=9
t = r*((n-p-1)/(n-p-r^2))^(1/2)
plot(yhat,t)
D = (1/p)*r^2*h/(1-h)
plot(yhat,D)
plot(lm_pro)
