##Lucas Humphrey STAT 462 Homework 5

##Part 1
bears=read.table("bears.txt",header=TRUE)
head(bears)
attach(bears)

bear_iid=bears[bears$Obs.No == 1,]
bear_iid

###
y=bear_iid$Weight
x1=bear_iid$Neck.G
lm_fit=lm(y~x1)
lm_sum=summary(lm_fit)
lm_sum
X1=model.matrix(~x1)
X1inv=solve(t(X1)%*%X1)
H=X1%*%X1inv%*%t(X1)
yhat=H%*%y
e_hat=y-yhat
n=dim(X1)[1]
p=dim(X1)[2]
sig2_hat=sum(e_hat^2)/(n-p)
sig2_hat
lm_sum$sigma^2
beta_hat=solve(t(X1)%*%X1, t(X1)%*%y)
beta_hat
cv_mat=sig2_hat * solve(t(X1)%*%X1)
round(cv_mat,digits=5)
se = sqrt(diag(cv_mat))
mytable<-cbind(beta_hat,se)
colnames(mytable)<-c("Est","se")
mytable
T=beta_hat/se
pval = 2*pt(abs(T),df=n-p,lower.tail = FALSE)
mytable<-cbind(mytable,T,pval)
colnames(mytable)<-c("Est","se","t-stat","p-val")
mytable
lm_sum


##Part 2
y=bear_iid$Weight
x1=bear_iid$Neck.G
x2=bear_iid$Head.W
lm_fit=lm(y~x1+x2)
lm_sum=summary(lm_fit)
lm_sum

###
TSS=sum((y-mean(y))^2)
TSS
yhat=lm_fit$fitted.values
RSS=sum((y-yhat)^2)
RSS
n=length(y)
p=length(lm_fit$coefficients)
SSREG=TSS-RSS
SSREG

X3=model.matrix(~x1+x2)
X3inv=solve(t(X3)%*%X3)
H=X3%*%X3inv%*%t(X3)
yhat=H%*%y
e_hat=y-yhat
sig2_hat=sum(e_hat^2)/(n-p)
sig2_hat
lm_sum$sigma^2
beta_hat=solve(t(X3)%*%X3, t(X3)%*%y)
beta_hat
cv_mat=sig2_hat * solve(t(X3)%*%X3)
round(cv_mat,digits=5)
se = sqrt(diag(cv_mat))
mytable<-cbind(beta_hat,se)
colnames(mytable)<-c("Est","se")
T=beta_hat/se
pval = 2*pt(abs(T),df=n-p,lower.tail = FALSE)
mytable<-cbind(mytable,T,pval)
colnames(mytable)<-c("Est","se","t-stat","p-val")
mytable

F=(SSREG/(p-1))/(RSS/(n-p))
F
Pvalue=pf(F,p-1,n-p,lower.tail=FALSE)
Pvalue
anova(lm_fit)


##Part 3
y=bear_iid$Weight
x1=bear_iid$Head.L
x2=bear_iid$Head.W
x3=bear_iid$Neck.G
x4=bear_iid$Length
x5=bear_iid$Chest.G
lm_fit=lm(y~x1+x2+x3+x4+x5)
lm_sum=summary(lm_fit)
lm_sum

TSS=sum((y-mean(y))^2)
TSS
yhat=lm_fit$fitted.values
RSS=sum((y-yhat)^2)
RSS
n=length(y)
p=length(lm_fit$coefficients)
SSREG=TSS-RSS
SSREG
F=(SSREG/(p-1))/(RSS/(n-p))
F
Pvalue=pf(F,p-1,n-p,lower.tail=FALSE)
Pvalue
anova(lm_fit)

###
lm_fit=lm(y~x2+x4)
lm_sum=summary(lm_fit)
lm_sum
TSS=sum((y-mean(y))^2)
TSS
yhat=lm_fit$fitted.values
RSS=sum((y-yhat)^2)
RSS
SSREG=TSS-RSS
SSREG
n=length(y)
p=length(lm_fit$coefficients)
F=(SSREG/(p-1))/(RSS/(n-p))
F
Pvalue = pf(F,p-1,n-p,lower.tail=FALSE)
Pvalue
anova(lm_fit)
