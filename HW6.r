##Lucas Humphrey STAT 462 Homework 6

##Part 1
require("faraway")
data(sat)
attach(sat)
sat
plot(sat)
lm_full=lm(total~expend+ratio+salary+takers)
lm_sum=summary(lm_full)
lm_sum
lm_sub=lm(total~takers)
RSS_sub = sum(lm_sub$residuals^2)
RSS_sub
RSS_full = sum(lm_full$residuals^2)
RSS_full
p = length(lm_full$coefficients)
q = length(lm_sub$coefficients)
n = length(total)
F = ((RSS_sub-RSS_full)/(p-q))/(RSS_full/(n-p))
pvalue = pf(F,p-q,n-p,lower.tail=FALSE)
pvalue
anova(lm_full)
anova(lm_sub)
anova(lm_full,lm_sub)

y=total
X1=model.matrix(~expend+ratio+salary+takers)
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

beta_hat=lm_full$coefficients
resid_hat=lm_full$residuals
y<-total
X<-model.matrix(~expend+ratio+salary+takers)
n=50
p=5
boot_reps <- 10000
boot_est<- matrix(nrow=boot_reps,ncol=p)
for(i in 1:boot_reps){
  new_sample<-sample(1:n,n,replace=TRUE)
  eps_new<-resid_hat[new_sample]
  y_new = X%*%beta_hat + eps_new
  boot_est[i,] = solve(t(X)%*%X,t(X)%*%y_new)
}
colnames(boot_est) = colnames(X)
boot_est = data.frame(boot_est)
head(round(boot_est,digits=4))

low<-quantile(boot_est$salary,0.025)
high<-quantile(boot_est$salary,0.975)
beta_sal<-beta_hat["salary"]
sum_full<-summary(lm_full)
se_sal<-coefficients(sum_full)["salary","Std. Error"]
t_mult<-qt(0.975,n-p)
low_t<-beta_sal - t_mult*se_sal
high_t<-beta_sal + t_mult*se_sal
c(low,high)
c(low_t,high_t)


##Part 2
require("faraway")
data(cheddar)
attach(cheddar)
cheddar
plot(cheddar)
lm_full=lm(taste~Acetic+H2S+Lactic)
lm_sum=summary(lm_full)
lm_sum
anova(lm_full)
lm_sub=lm(taste~H2S+Lactic)
anova(lm_sub)
anova(lm_full,lm_sub)

y=taste
TSS=sum((y-mean(y))^2)
TSS
X2=model.matrix(~Acetic+H2S+Lactic)
X2inv=solve(t(X2)%*%X2)
H=X2%*%X2inv%*%t(X2)
yhat=H%*%y
RSS=sum((y-yhat)^2)
RSS
n=length(y)
p=length(lm_full$coefficients)
F = ((TSS-RSS)/(p-1))/(RSS/(n-p))
F
anova(lm_full)

y=taste
X2=model.matrix(~Acetic+H2S+Lactic)
X2inv=solve(t(X2)%*%X2)
H=X2%*%X2inv%*%t(X2)
yhat=H%*%y
e_hat=y-yhat
n=dim(X2)[1]
p=dim(X2)[2]
sig2_hat=sum(e_hat^2)/(n-p)
sig2_hat
lm_sum$sigma^2
beta_hat=solve(t(X2)%*%X2, t(X2)%*%y)
beta_hat
cv_mat=sig2_hat * solve(t(X2)%*%X2)
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

beta_hat=lm_full$coefficients
resid_hat=lm_full$residuals
y<-taste
X<-model.matrix(~Acetic+H2S+Lactic)
boot_reps <- 10000
boot_est<- matrix(nrow=boot_reps,ncol=p)
for(i in 1:boot_reps){
  new_sample<-sample(1:n,n,replace=TRUE)
  eps_new<-resid_hat[new_sample]
  y_new = X%*%beta_hat + eps_new
  boot_est[i,] = solve(t(X)%*%X,t(X)%*%y_new)
}
colnames(boot_est) = colnames(X)
boot_est = data.frame(boot_est)
head(round(boot_est,digits=4))

low<-quantile(boot_est$H2S,0.025)
high<-quantile(boot_est$H2S,0.975)
beta_H<-beta_hat["H2S"]
sum_full<-summary(lm_full)
se_H<-coefficients(sum_full)["H2S","Std. Error"]
t_mult<-qt(0.975,n-p)
low_t<-beta_H - t_mult*se_H
high_t<-beta_H + t_mult*se_H
c(low,high)
c(low_t,high_t)
