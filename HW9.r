##Lucas Humphrey STAT 462 Homework 9

##Part 1
swiss=read.table("swiss.txt",header=TRUE)
attach(swiss)
lm_swiss=lm(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality)
X = model.matrix(lm_swiss)
H = X%*%solve(t(X)%*%X)%*%t(X)
h = diag(H)
plot(h)
abline(h=c(2,3)*mean(h),lty=2,col=c("blue","red"))
head(swiss[order(h,decreasing=TRUE),])
epshat=residuals(lm_swiss)
yhat=fitted(lm_swiss)
sig2hat=summary(lm_swiss)$sigma^2
r=epshat/(sqrt((1-h)*sig2hat))
n=length(r); p=6
t=r*((n-p-1)/(n-p-r^2))^(1/2)
plot(t)
head(swiss[order(t,decreasing=TRUE),])
D=(1/p)*r^2*h/(1-h)
plot(D)
head(swiss[order(D,decreasing=TRUE),])


##Part 2
data(cheddar, package="faraway")
attach(cheddar)
summary(cheddar)
plot(cheddar)
cor(taste,Acetic)
cor(taste,H2S)
cor(taste,Lactic)
boxplot(taste)
boxplot(Acetic)
boxplot(H2S)
boxplot(Lactic)
lm_ched=lm(taste~Acetic+H2S+Lactic)
summary(lm_ched)
plot(lm_ched)
shapiro.test(lm_ched$residuals)
