##Lucas Humphrey Project 2

ames=read.table("AmesHousing.txt",header=TRUE,sep="\t")
attach(ames)

###Grouping continuous variables, removing NA values, computing correlations
DeckPool=Wood.Deck.SF+Open.Porch.SF+Enclosed.Porch+X3Ssn.Porch+Screen.Porch+Pool.Area
Property=Lot.Frontage+Lot.Area
Floors=X1st.Flr.SF+X2nd.Flr.SF+Low.Qual.Fin.SF
y=SalePrice
x1=na.omit(DeckPool)
x2=na.omit(Property)
x3=na.omit(Floors)
x4=na.omit(Misc.Val)
summary(x1)
summary(x2)
summary(x3)
summary(x4)
cor(y,x1)
cor(y,x2)
cor(y,x3)
cor(y,x4)
boxplot(x3)

###Creating random test sample, fitting a model with chosen variables, checking diagnostics
set.seed(7736)
y_all=ames$SalePrice
X_all=ames[,c("X1st.Flr.SF","X2nd.Flr.SF","Low.Qual.Fin.SF")]
testindices=sample(2930,round(2930/4))
X_all=as.matrix(X_all)
X_all=model.matrix(~X_all)
y = y_all[-testindices]
y0 = y_all[testindices]
X = X_all[-testindices,]
X0 = X_all[testindices,]
lm_ames=lm(y~X-1)
summary(lm_ames)
plot(lm_ames)
shapiro.test(lm_ames$residuals)

###Checking for possible collinearity problems
lm_x1=lm(X1st.Flr.SF~X2nd.Flr.SF+Low.Qual.Fin.SF)
R2_1=summary(lm_x1)$r.squared
vif_1=1/(1-R2_1)
lm_x2=lm(X2nd.Flr.SF~X1st.Flr.SF+Low.Qual.Fin.SF)
R2_2=summary(lm_x2)$r.squared
vif_2=1/(1-R2_2)
lm_x3=lm(Low.Qual.Fin.SF~X1st.Flr.SF+X2nd.Flr.SF)
R2_3=summary(lm_x3)$r.squared
vif_3=1/(1-R2_3)

###Transforming the original model to check for a better fit
lm_ames2=lm(sqrt(y)~X-1)
summary(lm_ames2)
plot(lm_ames2)
lm_ames3=lm(log(y)~X-1)
summary(lm_ames3)
plot(lm_ames3)

###Performing diagnostics on the chosen transformed model to detect outliers
X1 = model.matrix(lm_ames2)
H = X1%*%solve(t(X1)%*%X1)%*%t(X1)
h = diag(H)
plot(h)
abline(h=c(2,3)*mean(h),lty=2,col=c("blue","red"))
head(X_all[order(h,decreasing=TRUE),])
epshat=residuals(lm_ames2)
yhat=fitted(lm_ames2)
sig2hat=summary(lm_ames2)$sigma^2
r=epshat/(sqrt((1-h)*sig2hat))
n=length(r); p=4
t=r*((n-p-1)/(n-p-r^2))^(1/2)
plot(t)
head(X_all[order(t,decreasing=TRUE),])
D=(1/p)*r^2*h/(1-h)
plot(D)
head(X_all[order(D,decreasing=TRUE),])

###Calculating mean prediction error in the testing dataset
bhat=lm_ames2$coefficients
yhat = X0%*%bhat
mean((yhat - y0)^2)
