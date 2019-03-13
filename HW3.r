##Lucas Humphrey STAT462 Homework 3

##Part 1
###Load the dataset record.txt in R, using the function read.table
record=read.table("record.txt",header=TRUE,sep=' ')
head(record)

###Draw  a  scatterplot  and  compute  the  correlation  for  all  pairs  of  variables  in  the  dataset.
plot(record)
cor(record)

###Using the equations, compute the least square estimators for the coefficients of a single linear  regression  model,  with  response  m400  and  predictor  m100.
y=m400
x=m100
b1=sum((y-mean(y))*(x-mean(x)))/sum((x-mean(x))^2)
b0=mean(y)-mean(x)*b1
c(b0,b1)

###Produce a scatter plot for m400 vs m100 with the fitted regression line superimposed.
plot(m100,m400)
abline(-4.032628,4.943686,col='red')

###Re-compute the least square estimators of beta0 and beta1 using the R function lm, and display the summary of the regression.
X=cbind(1,m100)
b_v2=solve(t(X)%*%X, t(X)%*%y)
b_v2

lm_fit=lm(y~x)
lm_fit

###Compute the fitted values and the residuals, using the estimated regression line.
Xinv=solve(t(X)%*%X)
H=X%*%Xinv%*%t(X)
yhat=H%*%y
yhat
e_hat=y-yhat
e_hat

###Compute the 25th and 75th percentiles of variable m100 on the dataset. Use the estimated regression line to estimate the mean of variable m400 at each of these two percentiles.
quantile(m100,probs=0.25)
quantile(m100,probs=0.75)
y2=-4.033+(4.944*11.27)
y2
y3=-4.033+(4.944*11.92)
y3


##Part 2
set.seed(123)
###Create in R the observations for the predictor X.
X=rnorm(n=100,mean=0,sd=1)
plot(X)

###Generate the response values, Y
e=rnorm(n=100,mean=-2*X,sd=1.25)
Y=3+(-2*X)+e
plot(Y)

###Create the scatterplot of Y vs X and compute the correlation coefficient of Y and X.
plot(X,Y)
cor(X,Y)

###Form the least squares estimate and add both the estimated regression line and true regression line to your scatterplot.
b1=sum((Y-mean(Y))*(X-mean(X)))/sum((X-mean(X))^2)
b0=mean(Y)-mean(X)*b1
c(b0,b1)

abline(3,-2,col='red')
abline(2.871496,-4.0655590,col='green')


##Part 3
###Load the dataset "Diamond Rings" into R.
diamond=read.csv("DiamondRings.csv")
head(diamond)
attach(diamond)

###Compute the correlation coefficient between Carats and Price.
cor(Carats,Price)

###Fit a simple linear regression and interpret the results.
y=Price
x=Carats
b1=sum((y-mean(y))*(x-mean(x)))/sum((x-mean(x))^2)
b0=mean(y)-mean(x)*b1
c(b0,b1)

X=cbind(1,Carats)
b_v4=solve(t(X)%*%X, t(X)%*%y)
b_v4

lm_fit=lm(y~x)
lm_fit

###Create a scatterplot and add the estimated regression line into the plot.
plot(diamond)
abline(-259.6,3721,col='red')
