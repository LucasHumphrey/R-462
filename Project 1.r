##Lucas Humphrey Project 1

install.packages("lars")
library(lars)
data(diabetes)
data.all=data.frame(cbind(diabetes$x, y = diabetes$y))
attach(data.all)

set.seed(38723)
y_all=data.all$y
X_all=data.all[,c("age","sex","bmi","map","tc","ldl","hdl","tch","ltg","glu")]
smp_size = floor(0.75 * nrow(data.all))
train_smp = sample(seq_len(nrow(data.all)), size = smp_size, replace = FALSE)
X_all=as.matrix(X_all)
X_all=model.matrix(~X_all)
y = y_all[train_smp]
y0 = y_all[-train_smp]
X = X_all[train_smp,]
X0 = X_all[-train_smp,]

lm_full=lm(y~X-1)
summary(lm_full)
sum_full=summary(lm_full)
coefficients(sum_full)

sig_sub1=c(1,3,4,5,10)
lm_sub1=lm(y~X[,sig_sub1]-1)
sum_sub1=summary(lm_sub1)
coefficients(sum_sub1)

sig_sub2=c(1,4,5)
lm_sub2=lm(y~X[,sig_sub2]-1)
sum_sub2=summary(lm_sub2)
coefficients(sum_sub2)

anova(lm_full,lm_sub1,lm_sub2, test="F")

X0_sub1 = X0[,sig_sub1]
X0_sub2 = X0[,sig_sub2]
bhat=lm_full$coefficients
bhat_sub1=lm_sub1$coefficients
bhat_sub2=lm_sub2$coefficients
yhat = X0%*%bhat
yhat_sub1 = X0_sub1%*%bhat_sub1
yhat_sub2 = X0_sub2%*%bhat_sub2
mean((yhat - y0)^2)
mean((yhat_sub1 - y0)^2)
mean((yhat_sub2 - y0)^2)

plot(lm_full$fitted.values,lm_full$residuals)
qqnorm(lm_full$residuals)
qqline(lm_full$residuals)
plot(lm_full)
