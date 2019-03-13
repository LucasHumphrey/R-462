##Lucas Humphrey STAT462 Homework 4

##Part 1
bears=read.table("bears.txt",header=TRUE)
head(bears)
attach(bears)

bear_iid=bears[bears$Obs.No == 1,]
bear_iid

###
y=bear_iid$Weight
x1=bear_iid$Chest.G
x2=bear_iid$Head.W

b1=sum((y-mean(y))*(x1-mean(x1)))/sum((x1-mean(x1))^2)
b0=mean(y)-mean(x1)*b1
c(b0,b1)

b1=sum((y-mean(y))*(x2-mean(x2)))/sum((x2-mean(x2))^2)
b0=mean(y)-mean(x2)*b1
c(b0,b1)

###
lm_fit=lm(bear_iid$Weight~bear_iid$Chest.G)
lm_sum=summary(lm_fit)
lm_sum
X1=model.matrix(~bear_iid$Chest.G)
X1inv=solve(t(X1)%*%X1)
H=X1%*%X1inv%*%t(X1)
yhat=H%*%y
e_hat=y-yhat
n=dim(X1)[1]
p=dim(X1)[2]
sig2_hat=sum(e_hat^2)/(n-p)
sig2_hat
lm_sum$sigma^2
r2=1-(sum(e_hat^2)/sum((y-mean(y))^2))
r2
lm_sum$r.squared

lm_fit=lm(bear_iid$Weight~bear_iid$Head.W)
lm_sum=summary(lm_fit)
lm_sum
X2=model.matrix(~bear_iid$Head.W)
X2inv=solve(t(X2)%*%X2)
H=X2%*%X2inv%*%t(X2)
yhat=H%*%y
e_hat=y-yhat
n=dim(X2)[1]
p=dim(X2)[2]
sig2_hat=sum(e_hat^2)/(n-p)
sig2_hat
lm_sum$sigma^2
r2=1-(sum(e_hat^2)/sum((y-mean(y))^2))
r2
lm_sum$r.squared

###
lm_fit=lm(bear_iid$Weight~bear_iid$Chest.G+bear_iid$Head.W)
lm_sum=summary(lm_fit)
lm_sum
X=model.matrix(~bear_iid$Chest.G+bear_iid$Head.W)
Xinv=solve(t(X)%*%X)
H=X%*%Xinv%*%t(X)
yhat=H%*%y
e_hat=y-yhat
n=dim(X)[1]
p=dim(X)[2]
sig2_hat=sum(e_hat^2)/(n-p)
sig2_hat
lm_sum$sigma^2
r2=1-(sum(e_hat^2)/sum((y-mean(y))^2))
r2
lm_sum$r.squared


##Part 2
airdata=scan()
airdata=matrix(airdata,10,3,byrow=T)
dim(airdata)
airdata

x=c(1,0,2,0,3,1,0,1,2,0)
y=c(16,9,17,12,22,13,8,15,19,11)
airdata=data.frame(x,y)
airdata

###
lm_fit=lm(y~x)
lm_sum=summary(lm_fit)
lm_sum
X=model.matrix(~x)
Xinv=solve(t(X)%*%X)
H=X%*%Xinv%*%t(X)
yhat=H%*%y
plot(lm_fit$fitted.values,yhat)
e_hat=y-yhat
plot(lm_fit$residuals,e_hat)
n=dim(X)[1]
p=dim(X)[2]
sig2_hat=sum(e_hat^2)/(n-p)
sig2_hat
lm_sum$sigma^2


##Part 3
install.packages("faraway")
data(teengamb,package="faraway")
head(teengamb)
attach(teengamb)

###
lm_fit=lm(gamble~sex+status+income+verbal)
lm_sum=summary(lm_fit)
lm_sum
lm_sum$r.squared
plot(lm_fit)
mean(lm_fit$residuals)
median(lm_fit$residuals)
cor(lm_fit$fitted.values,lm_fit$residuals)
cor(income,lm_fit$residuals)
