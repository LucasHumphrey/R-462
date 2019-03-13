##Lucas Humphrey STAT 462 Homework 7

##Part 1
data(prostate,package="faraway")
head(prostate,n=3)
attach(prostate)
set.seed(2017)
n_all<-dim(prostate)[1]
y_all<-prostate$lpsa
X_all<-prostate[,c("lcavol", "lweight", "age", "lbph", "lcp", "gleason",
              "pgg45")]
X_all<-as.matrix(X_all)
X_all<-model.matrix(~X_all)
pred_sample = sample(1:n_all,1,replace=FALSE)
y = y_all[-pred_sample]
y0 = y_all[pred_sample]
X = X_all[-pred_sample,]
X0 = X_all[pred_sample,]
lm_full<-lm(y~X-1)
sum_full<-summary(lm_full)
sum_full
coefficients(sum_full)
sig_pred<-c(1, 2, 3, 4)
lm_sub<-lm(y~X[,sig_pred]-1)
sum_sub <- summary(lm_sub)
sum_sub
coefficients(sum_sub)
X0_sub <- X0[sig_pred]
bhat<-lm_full$coefficients
bhat_sub<-lm_sub$coefficients
yhat = X0%*%bhat
yhat_sub = X0_sub%*%bhat_sub
sum((yhat - y0)^2)
sum((yhat_sub - y0)^2)
anova(lm_full,lm_sub)

lm_full=lm(lpsa~lcavol+lweight+age+lbph+lcp+gleason+pgg45-1)
x=model.matrix(lm_full)
(x1=apply(x, 2, function(x) quantile(x, 0.95)))
predict(lm_full, new = data.frame(t(x1)), interval = "prediction")
lm_sub=lm(lpsa~lcavol+lweight+age)
x2=model.matrix(lm_sub)
(x3=apply(x2, 2, function(x2) quantile(x2, 0.95)))
predict(lm_sub, new = data.frame(t(x3)), interval = "prediction")


###Part 2
cement=read.table("cement.txt",header=TRUE)
attach(cement)
cement

lm_alum=lm(Hardness_cement~Aluminum)
x=model.matrix(lm_alum)
q1=quantile(Aluminum,0.50)
q2=quantile(Aluminum,0.90)
x1=apply(x,2,function(x) quantile(q1,0.95))
predict(lm_alum, new=data.frame(t(x1)), interval = "confidence")
x2=apply(x,2,function(x) quantile(q2,0.95))
predict(lm_alum, new=data.frame(t(x2)), interval = "confidence")
predict(lm_alum, new=data.frame(t(x1)), interval = "prediction")
predict(lm_alum, new=data.frame(t(x2)), interval = "prediction")

lm_full=lm(Hardness_cement~Aluminum+Silicate+Aluminum_ferrite+Silicate_bic)
x=model.matrix(lm_full)
q3=quantile(Aluminum+Silicate+Aluminum_ferrite+Silicate_bic,0.50)
q4=quantile(Aluminum+Silicate+Aluminum_ferrite+Silicate_bic,0.75)
x3=apply(x,2,function(x) quantile(q3,0.95))
predict(lm_full, new=data.frame(t(x3)), interval = "confidence")
x4=apply(x,2,function(x) quantile(q4,0.95))
predict(lm_full, new=data.frame(t(x4)), interval = "confidence")
predict(lm_full, new=data.frame(t(x3)), interval = "prediction")
predict(lm_full, new=data.frame(t(x4)), interval = "prediction")
